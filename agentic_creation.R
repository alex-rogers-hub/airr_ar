library(httr2)
library(jsonlite)

# ----------------------------
# Helper: clean Markdown and extract JSON
# ----------------------------
extract_json <- function(text) {
  # Remove markdown fences
  text <- gsub("^```json\\s*", "", text)
  text <- gsub("^```\\s*", "", text)
  text <- gsub("```$", "", text)
  
  # Extract only JSON braces content
  start <- regexpr("\\{", text)
  end   <- regexpr("\\}[^\"]*$", text)
  if (start == -1 || end == -1) stop("No JSON object found in text")
  text <- substr(text, start, end + attr(end, "match.length") - 1)
  
  trimws(text)
}

# ----------------------------
# Claude JSON chat wrapper
# ----------------------------
claude_chat_json <- function(messages,
                             system = NULL,
                             model = "claude-sonnet-4-20250514",
                             max_tokens = 1024) {
  
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") stop("ANTHROPIC_API_KEY not set")
  
  body <- list(
    model = model,
    max_tokens = max_tokens,
    messages = messages
  )
  if (!is.null(system)) body$system <- system
  
  req <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |>
    httr2::req_body_json(body)
  
  resp <- httr2::req_perform(req)
  text <- httr2::resp_body_json(resp)$content[[1]]$text
  
  text_clean <- extract_json(text)
  jsonlite::fromJSON(text_clean, simplifyVector = FALSE)
}

# ----------------------------
# Write R code to a file safely
# ----------------------------
write_function_file <- function(code, file = "generated_function.R") {
  
  if (is.null(code) || !nzchar(as.character(code))) {
    stop("No code provided to write_function_file")
  }
  if (is.null(file) || !nzchar(as.character(file))) {
    stop("No file path provided to write_function_file")
  }
  
  writeLines(as.character(code), con = file)
  
  # Test if it sources correctly
  tryCatch({
    source(file, local = TRUE)
    paste0("Function written and sourced successfully: ", file)
  }, error = function(e) {
    paste0("Function written, but source failed: ", e$message)
  })
}

# ----------------------------
# Test function file helper
# ----------------------------
test_function <- function(file) {
  tryCatch({
    source(file, local = TRUE)
    paste0("Tested file successfully: ", file)
  }, error = function(e) {
    paste0("Error testing file: ", e$message)
  })
}

# ----------------------------
# System prompt for agentic behavior
# ----------------------------
agent_system_prompt <- "
You are an R function-writing agent.

You MUST respond **only in JSON**. Do not include markdown, explanations, or text outside JSON.

The JSON MUST have:
- action: string
- args: object

Allowed actions:
- write_function_file: args must include 'code' (non-empty string) and 'file'
- test_function: args must include 'file'
- return_success: args must include 'message'

The R function you write must run without error.
"

# ----------------------------
# Robust agent loop
# ----------------------------
run_function_agent <- function(task, file = "generated_function.R", max_retries = 3) {
  
  messages <- list(
    list(role = "user", content = task)
  )
  
  retry_count <- 0
  
  repeat {
    
    plan <- claude_chat_json(
      messages = messages,
      system   = agent_system_prompt
    )
    
    # Validate JSON plan
    if (is.null(plan$action) || is.null(plan$args)) {
      stop("Claude returned invalid JSON plan: missing 'action' or 'args'")
    }
    
    action <- plan$action
    args   <- plan$args
    
    # If code is required but missing, fail gracefully
    if (action == "write_function_file" && 
        (is.null(args$code) || !nzchar(args$code))) {
      
      retry_count <- retry_count + 1
      if (retry_count > max_retries) {
        stop("Claude returned no code after ", max_retries, " retries")
      }
      
      messages <- append(
        messages,
        list(
          list(role = "assistant", content = jsonlite::toJSON(plan, auto_unbox = TRUE)),
          list(role = "user", content = "You must provide a valid 'code' field for write_function_file.")
        )
      )
      next
    }
    
    # Execute action safely
    result <- switch(
      action,
      write_function_file = write_function_file(args$code, args$file %||% file),
      test_function       = test_function(args$file %||% file),
      return_success      = args$message,
      stop("Unknown action returned by Claude: ", action)
    )
    
    # Append result for next iteration
    messages <- append(
      messages,
      list(
        list(role = "assistant",
             content = jsonlite::toJSON(plan, auto_unbox = TRUE)),
        list(role = "user",
             content = paste("Result:", result))
      )
    )
    
    if (action == "return_success") {
      return(result)
    }
  }
}

run_function_agent(
  "Write an R function called `trimmed_mean` that takes a numeric vector `x` and a trim proportion `trim`, returning the trimmed mean."
)

run_function_agent(
  "Write an R function called show_me that takes an input string and submits it as a query using the ask_chatgpt() function appending both the query and output into the table query_results
  create the table if it doesn't exist already"
)

run_function_agent(
  "Write an R function which queries chatGPT with a temperature of 0.7 and repeats the question 10 times, returning each reply into the query_results table alongside the query"
)

run_function_agent(
  "re-write the R functions called show_me query_chatgpt_multiple so that they work together,
  requiring a text and a numeric input with the text being the question and the number being the repetitions, 
  with each response returning to a separate row into a master output table query_results"
)

ttt <- show_me('how good is the brand Nike', 10)
ttt2 <- query_chatgpt_multiple('how good is the brand Nike',10)