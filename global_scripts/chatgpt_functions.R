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
    temperature = 0.7,
    max_concurrent = 450,
    max_retries = 3,
    retry_delay = 2,
    fallback_models = c("gpt-4o", "gpt-5-mini")
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
  
  # --- Check rate limits before starting ---
  current_model <- model[1]
  limits <- check_rate_limits(current_model)
  
  if (!is.null(limits)) {
    message(sprintf("Rate limits for %s: %s/%s requests, %s/%s tokens remaining",
                    current_model,
                    limits$requests_remaining, limits$requests_limit,
                    limits$tokens_remaining, limits$tokens_limit))
    
    # Estimate tokens needed (rough: ~100 tokens per prompt + ~200 per response)
    estimated_tokens_needed <- n_prompts * 300
    
    # Check token availability
    if (!is.null(limits$tokens_remaining) && 
        limits$tokens_remaining < estimated_tokens_needed) {
      
      message(sprintf("⚠ Estimated need ~%d tokens but only %d remaining for %s",
                      estimated_tokens_needed, limits$tokens_remaining, current_model))
      
      new_model <- find_available_model(fallback_models, estimated_tokens_needed, current_model)
      
      if (!is.null(new_model)) {
        message(sprintf("→ Switching to fallback model: %s", new_model))
        models <- rep(new_model, n_prompts)
      } else {
        if (!is.null(limits$requests_reset)) {
          message(sprintf("⏳ All models constrained. Limits reset in: %s", limits$requests_reset))
        }
        message("Proceeding with original model - retries will handle 429s")
      }
    }
    
    # Check request count availability
    if (!is.null(limits$requests_remaining) && 
        limits$requests_remaining < n_prompts) {
      
      message(sprintf("⚠ Need %d requests but only %d remaining for %s",
                      n_prompts, limits$requests_remaining, current_model))
      
      new_model <- find_available_model(fallback_models, estimated_tokens_needed, current_model)
      
      if (!is.null(new_model)) {
        message(sprintf("→ Switching to fallback model: %s", new_model))
        models <- rep(new_model, n_prompts)
      }
    }
  }
  
  # --- Build requests ---
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
      )) |>
      req_retry(
        max_tries = max_retries,
        is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503),
        backoff = ~ retry_delay * 2^(.x - 1)
      )
  })
  
  # --- Execute in batches ---
  batch_size <- min(max_concurrent, n_prompts)
  batch_indices <- split(seq_along(requests),
                         ceiling(seq_along(requests) / batch_size))
  
  all_results <- list()
  batch_start_time <- Sys.time()
  requests_sent <- 0
  failed_indices <- c()
  
  for (batch_num in seq_along(batch_indices)) {
    idx <- batch_indices[[batch_num]]
    batch_t0 <- Sys.time()
    
    message(sprintf("Batch %d/%d (%d requests, model: %s)...",
                    batch_num, length(batch_indices), length(idx), models[idx[1]]))
    
    batch_requests <- requests[idx]
    
    # Execute with max_active = batch size for true parallelism
    responses <- req_perform_parallel(
      batch_requests,
      on_error = "continue",
      max_active = length(batch_requests)
    )
    
    batch_elapsed <- as.numeric(difftime(Sys.time(), batch_t0, units = "secs"))
    message(sprintf("  Batch %d: %d requests in %.1fs (%.0f req/s)",
                    batch_num, length(idx), batch_elapsed,
                    length(idx) / max(batch_elapsed, 0.1)))
    
    # Parse responses and track failures
    batch_results <- lapply(seq_along(responses), function(j) {
      resp <- responses[[j]]
      if (resp_is_error(resp)) {
        warning(sprintf("Request %d failed with status %d", idx[j], resp_status(resp)))
        failed_indices <<- c(failed_indices, idx[j])
        return(NA_character_)
      }
      parsed <- resp_body_json(resp)
      parsed$choices[[1]]$message$content
    })
    
    all_results <- c(all_results, batch_results)
    requests_sent <- requests_sent + length(idx)
    
    # Smart rate-limit pause: only wait if we're going too fast
    if (batch_num < length(batch_indices)) {
      total_elapsed <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))
      min_elapsed <- requests_sent / (480 / 60)  # seconds we should have taken at 480 RPM
      wait_time <- max(0, min_elapsed - total_elapsed)
      
      if (wait_time > 0) {
        message(sprintf("  Rate limit pause: %.1fs", wait_time))
        Sys.sleep(wait_time)
      }
    }
  }
  
  # --- Retry failed requests with fallback model ---
  if (length(failed_indices) > 0) {
    message(sprintf("\n%d requests failed. Attempting retry with fallback model...",
                    length(failed_indices)))
    
    Sys.sleep(2)
    
    fallback_model <- find_available_model(fallback_models,
                                           length(failed_indices) * 300,
                                           models[1])
    
    if (!is.null(fallback_model)) {
      message(sprintf("Retrying %d requests with %s", length(failed_indices), fallback_model))
      
      retry_requests <- lapply(failed_indices, function(i) {
        request("https://api.openai.com/v1/chat/completions") |>
          req_headers(
            Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
            `Content-Type` = "application/json"
          ) |>
          req_body_json(list(
            model = fallback_model,
            messages = list(
              list(role = "user", content = prompts[i])
            ),
            temperature = temperatures[i]
          )) |>
          req_retry(
            max_tries = 2,
            is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503),
            backoff = ~ 3
          )
      })
      
      retry_responses <- req_perform_parallel(
        retry_requests,
        on_error = "continue",
        max_active = length(retry_requests)
      )
      
      for (j in seq_along(failed_indices)) {
        resp <- retry_responses[[j]]
        if (!resp_is_error(resp)) {
          parsed <- resp_body_json(resp)
          all_results[[failed_indices[j]]] <- parsed$choices[[1]]$message$content
          message(sprintf("  ✓ Retry %d succeeded", failed_indices[j]))
        } else {
          message(sprintf("  ✗ Retry %d failed", failed_indices[j]))
        }
      }
    }
  }
  
  total_time <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))
  message(sprintf("✓ %d requests in %.1fs (%.0f req/min, model: %s)",
                  n_prompts, total_time,
                  n_prompts / max(total_time, 0.1) * 60,
                  models[1]))
  
  return(all_results)
}


# ============================================
# Helper: Check rate limits for a model
# ============================================
check_rate_limits <- function(model = "gpt-4o-mini") {
  
  tryCatch({
    resp <- request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        `Content-Type` = "application/json"
      ) |>
      req_body_json(list(
        model = model,
        messages = list(list(role = "user", content = "hi")),
        temperature = 0,
        max_tokens = 1
      )) |>
      req_perform()
    
    headers <- resp_headers(resp)
    
    list(
      requests_limit = as.integer(headers$`x-ratelimit-limit-requests`),
      requests_remaining = as.integer(headers$`x-ratelimit-remaining-requests`),
      requests_reset = headers$`x-ratelimit-reset-requests`,
      tokens_limit = as.integer(headers$`x-ratelimit-limit-tokens`),
      tokens_remaining = as.integer(headers$`x-ratelimit-remaining-tokens`),
      tokens_reset = headers$`x-ratelimit-reset-tokens`,
      model = model
    )
  }, error = function(e) {
    warning(sprintf("Could not check rate limits for %s: %s", model, e$message))
    return(NULL)
  })
}


# ============================================
# Helper: Find an available fallback model
# ============================================
find_available_model <- function(fallback_models, 
                                 estimated_tokens_needed,
                                 exclude_model = NULL) {
  
  candidates <- fallback_models[fallback_models != exclude_model]
  
  for (candidate in candidates) {
    message(sprintf("  Checking availability of %s...", candidate))
    
    limits <- check_rate_limits(candidate)
    
    if (!is.null(limits)) {
      has_requests <- is.null(limits$requests_remaining) || limits$requests_remaining > 10
      has_tokens <- is.null(limits$tokens_remaining) || limits$tokens_remaining > estimated_tokens_needed
      
      if (has_requests && has_tokens) {
        message(sprintf("  ✓ %s available: %s requests, %s tokens remaining",
                        candidate,
                        ifelse(is.null(limits$requests_remaining), "unknown", limits$requests_remaining),
                        ifelse(is.null(limits$tokens_remaining), "unknown", limits$tokens_remaining)))
        return(candidate)
      } else {
        message(sprintf("  ✗ %s insufficient: %s requests, %s tokens remaining",
                        candidate,
                        ifelse(is.null(limits$requests_remaining), "unknown", limits$requests_remaining),
                        ifelse(is.null(limits$tokens_remaining), "unknown", limits$tokens_remaining)))
      }
    }
  }
  
  message("  ✗ No fallback models available with sufficient capacity")
  return(NULL)
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
