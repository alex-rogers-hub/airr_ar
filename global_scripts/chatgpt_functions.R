# Function to query ChatGPT once


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


# Function to query ChatGPT multiple times and return results in a table
query_chatgpt_multiple <- function(text, repetitions) {
  # Validate inputs
  if (!is.character(text) || length(text) != 1 || nchar(text) == 0) {
    stop("Text must be a non-empty character string")
  }
  
  if (!is.numeric(repetitions) || length(repetitions) != 1 || repetitions <= 0) {
    stop("Repetitions must be a positive numeric value")
  }
  
  # Convert repetitions to integer
  repetitions <- as.integer(repetitions)
  
  # Initialize results list
  responses <- character(repetitions)
  
  # Query ChatGPT multiple times
  for (i in 1:repetitions) {
    responses[i] <- ask_chatgpt(text)
  }
  
  # Create master output table
  query_results <- data.frame(
    query_number = 1:repetitions,
    question = rep(text, repetitions),
    response = responses,
    timestamp = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  return(query_results)
}




ask_chatgpt_async <- function(
    prompts,
    model = "gpt-4o-mini",
    temperature = 0.7
) {
  require(httr2)
  
  n_prompts <- length(prompts)
  
  # Expand model to match prompts length
  if (length(model) == 1) {
    models <- rep(model, n_prompts)
  } else if (length(model) == n_prompts) {
    models <- model
  } else {
    stop(paste0(
      "Length of 'model' must be either 1 or equal to length of 'prompts' (", 
      n_prompts, "). Got ", length(model), "."
    ))
  }
  
  # Expand temperature to match prompts length
  if (length(temperature) == 1) {
    temperatures <- rep(temperature, n_prompts)
  } else if (length(temperature) == n_prompts) {
    temperatures <- temperature
  } else {
    stop(paste0(
      "Length of 'temperature' must be either 1 or equal to length of 'prompts' (", 
      n_prompts, "). Got ", length(temperature), "."
    ))
  }
  
  # Create multiple requests
  requests <- lapply(seq_along(prompts), function(i) {
    request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        `Content-Type` = "application/json"
      ) |>
      req_body_json(list(
        model = models[i],
        messages = list(
          list(role = "user", content = prompts[i])
        ),
        temperature = temperatures[i]
      ))
  })
  
  # Execute all requests in parallel
  responses <- req_perform_parallel(requests)
  
  # Parse responses
  results <- lapply(responses, function(resp) {
    if (resp_is_error(resp)) {
      return(list(error = resp_status(resp)))
    }
    parsed <- resp_body_json(resp)
    parsed$choices[[1]]$message$content
  })
  
  return(results)
}

# # Example 1: Single model and temperature for all
# prompts <- c("What is 2+2?", "Explain photosynthesis", "Write a haiku")
# responses <- ask_chatgpt_async(prompts)
# 
# # Example 2: Different models for each query
# prompts <- c("Simple math", "Complex analysis", "Creative writing")
# models <- c("gpt-4o-mini", "gpt-4o", "gpt-4o-mini")
# responses <- ask_chatgpt_async(prompts, model = models)
# 
# # Example 3: Different temperatures for each query
# prompts <- c("Factual question", "Creative task", "Another factual")
# temps <- c(0.1, 1.2, 0.1)
# responses <- ask_chatgpt_async(prompts, temperature = temps)
# 
# # Example 4: Different models AND temperatures
# prompts <- c("Query 1", "Query 2", "Query 3")
# models <- c("gpt-4o-mini", "gpt-4o", "gpt-4o-mini")
# temps <- c(0.3, 0.7, 1.0)
# responses <- ask_chatgpt_async(prompts, model = models, temperature = temps)
