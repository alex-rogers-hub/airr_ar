


install.packages(c("httr2", "jsonlite", "glue"))

library(httr)
library(httr2)
library(jsonlite)
library(glue)

# r-script-access
OPENAI_API_KEY <- Sys.getenv(OPENAI_API_KEY)
ANTHROPIC_API_KEY <- Sys.getenv(ANTHROPIC_API_KEY)

call_llm_api <- function(
    url,
    headers,
    body,
    response_parser
) {
  resp <- request(url) |>
    req_headers(!!!headers) |>
    req_body_json(body) |>
    req_perform()
  
  if (resp_status(resp) >= 400) {
    stop(resp |> resp_body_string())
  }
  
  response_parser(resp)
}

ask_chatgpt <- function(
    prompt,
    model = "gpt-4o-mini",
    temperature = 0.7
) {
  call_llm_api(
    url = "https://api.openai.com/v1/chat/completions",
    headers = list(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))
    ),
    body = list(
      model = model,
      messages = list(
        list(role = "user", content = prompt)
      ),
      temperature = temperature
    ),
    response_parser = function(resp) {
      parsed <- resp |> resp_body_json()
      parsed$choices[[1]]$message$content
    }
  )
}

ask_chatgpt("Explain Bayesian updating in one paragraph")






api_key <- Sys.getenv("ANTHROPIC_API_KEY")

req <- request("https://api.anthropic.com/v1/messages") |>
  req_headers(
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01",
    "content-type" = "application/json"
  ) |>
  req_body_json(list(
    model = "claude-3-5-sonnet-20241022",
    max_tokens = 1000,
    messages = list(
      list(
        role = "user",
        content = "Explain linear regression like I'm five."
      )
    )
  ))

resp <- req |> req_perform()
content(resp)

claude_chat <- function(prompt,
                        model = "claude-sonnet-4-20250514",
                        max_tokens = 1024) {
  
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") stop("ANTHROPIC_API_KEY not set")
  
  req <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      max_tokens = max_tokens,
      messages = list(
        list(role = "user", content = prompt)
      )
    ))
  
  resp <- httr2::req_perform(req)
  json <- httr2::resp_body_json(resp)
  json$content[[1]]$text
}


claude_chat("Say hello in one sentence.")

extract_json <- function(text) {
  # Remove markdown-style ```json or ```
  text <- gsub("^```json\\s*", "", text)
  text <- gsub("^```\\s*", "", text)
  text <- gsub("```$", "", text)
  text <- trimws(text)
  text
}

claude_chat_json <- function(messages,
                             system = NULL,
                             model = "claude-sonnet-4-20250514",
                             max_tokens = 1024) {
  
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  
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



agent_system_prompt <- "
You are an R function-writing agent.

You must:
- Write a single R function
- Ensure it runs without error
- Save it as a standalone .R script

You may only respond with JSON.

Allowed actions:
- write_function_file(code, file)
- test_function(file)
- return_success(message)

Rules:
- The script must contain ONLY valid R code
- No markdown, no explanations
- No side effects outside the function
"

# Write R code to a file safely
write_function_file <- function(code, file = "generated_function.R") {
  if (missing(code) || !nzchar(code)) stop("No code provided")
  if (missing(file) || !nzchar(file)) stop("No file path provided")
  
  # Write the code to the file
  writeLines(code, con = file)
  
  # Optional: test if it sources correctly
  tryCatch({
    source(file, local = TRUE)
    paste0("Function written and sourced successfully: ", file)
  }, error = function(e) {
    paste0("Function written, but source failed: ", e$message)
  })
}

test_function <- function(file) {
  tryCatch({
    source(file, local = TRUE)
    paste0("Tested file successfully: ", file)
  }, error = function(e) {
    paste0("Error testing file: ", e$message)
  })
}



run_function_agent <- function(task, file = "generated_function.R") {
  
  messages <- list(
    list(role = "user", content = task)
  )
  
  repeat {
    
    plan <- claude_chat_json(
      messages = messages,
      system   = agent_system_prompt
    )
    
    action <- plan$action
    args   <- plan$args
    
    result <- switch(
      action,
      write_function_file = write_function_file(args$code, args$file %||% file),
      test_function       = test_function(args$file %||% file),
      return_success      = return_success(args$message),
      stop("Unknown action")
    )
    
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

claude_chat_json(
  messages = list(list(role = "user", content = "Return JSON {\"ok\": true}")),
  system = "You must respond with JSON only."
)


run_function_agent(
  "Write an R function called trimmed_mean that takes a numeric vector and a trim proportion."
)




