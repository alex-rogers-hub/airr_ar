# ============================================
# Rate limit cache — updated from response headers
# ============================================

.rate_limit_cache <- new.env(parent = emptyenv())
.rate_limit_cache$requests_remaining <- NULL
.rate_limit_cache$tokens_remaining   <- NULL
.rate_limit_cache$requests_limit     <- NULL
.rate_limit_cache$tokens_limit       <- NULL
.rate_limit_cache$last_updated       <- NULL

update_rate_limit_cache <- function(headers) {
  tryCatch({
    if (!is.null(headers$`x-ratelimit-remaining-requests`)) {
      .rate_limit_cache$requests_remaining <- 
        as.integer(headers$`x-ratelimit-remaining-requests`)
    }
    if (!is.null(headers$`x-ratelimit-remaining-tokens`)) {
      .rate_limit_cache$tokens_remaining <- 
        as.integer(headers$`x-ratelimit-remaining-tokens`)
    }
    if (!is.null(headers$`x-ratelimit-limit-requests`)) {
      .rate_limit_cache$requests_limit <- 
        as.integer(headers$`x-ratelimit-limit-requests`)
    }
    if (!is.null(headers$`x-ratelimit-limit-tokens`)) {
      .rate_limit_cache$tokens_limit <- 
        as.integer(headers$`x-ratelimit-limit-tokens`)
    }
    .rate_limit_cache$last_updated <- Sys.time()
  }, error = function(e) invisible(NULL))
}

# ============================================
# Single call (unchanged, used for simple cases)
# ============================================

ask_chatgpt <- function(prompt,
                        model       = "gpt-4o-mini",
                        temperature = 0.7) {
  resp <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(
      model       = model,
      messages    = list(list(role = "user", content = prompt)),
      temperature = temperature
    )) |>
    req_retry(
      max_tries    = 3,
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503),
      backoff      = \(i) 2 * 2^(i - 1)
    ) |>
    req_perform()
  
  headers <- resp_headers(resp)
  update_rate_limit_cache(headers)
  
  parsed <- resp_body_json(resp)
  sanitise_text(parsed$choices[[1]]$message$content)
}

# ============================================
# Async batch — no blocking pre-check
# Limits are read from response headers and cached
# ============================================

ask_chatgpt_async <- function(prompts,
                              model          = "gpt-4.1-mini",
                              temperature    = 0.7,
                              max_retries    = 3,
                              retry_delay    = 2) {
  require(httr2)
  
  n_prompts <- length(prompts)
  
  # Expand model/temperature to match prompt count
  models <- if (length(model) == 1) rep(model, n_prompts) else model
  temps  <- if (length(temperature) == 1) rep(temperature, n_prompts) else temperature
  
  if (length(models) != n_prompts) {
    stop(paste0("Length of 'model' must be 1 or ", n_prompts, ". Got ", length(model)))
  }
  if (length(temps) != n_prompts) {
    stop(paste0("Length of 'temperature' must be 1 or ", n_prompts, ". Got ", length(temperature)))
  }
  
  # Determine max_active from cache if available, otherwise use a safe default
  # OpenAI tier limits vary — we use the cached limit if we have it,
  # otherwise start at 500 and let the retry logic handle 429s
  max_active <- if (!is.null(.rate_limit_cache$requests_limit)) {
    # Use 90% of the known limit to stay safe
    max(1, floor(.rate_limit_cache$requests_limit * 0.9))
  } else {
    500  # Safe default — will be corrected after first batch via cache
  }
  
  # Cap at n_prompts (no point having more workers than requests)
  max_active <- min(max_active, n_prompts)
  
  message(sprintf("=== ask_chatgpt_async: %d prompts, model: %s, max_active: %d ===",
                  n_prompts, models[1], max_active))
  
  # Build all requests
  requests <- lapply(seq_along(prompts), function(i) {
    request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        `Content-Type` = "application/json"
      ) |>
      req_body_json(list(
        model       = models[i],
        messages    = list(list(role = "user", content = prompts[i])),
        temperature = temps[i]
      )) |>
      req_retry(
        max_tries    = max_retries,
        is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503),
        backoff      = \(i) retry_delay * 2^(i - 1)
      )
  })
  
  t0 <- Sys.time()
  
  # Send all at once — req_perform_parallel handles the concurrency
  responses <- req_perform_parallel(
    requests,
    on_error   = "continue",
    max_active = max_active
  )
  
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("  Parallel execution: %d requests in %.1fs (%.0f req/min)",
                  n_prompts, elapsed, n_prompts / max(elapsed, 0.01) * 60))
  
  # Parse responses and update rate limit cache from last successful response
  failed_indices <- c()
  
  results <- lapply(seq_along(responses), function(j) {
    resp <- responses[[j]]
    
    if (inherits(resp, "httr2_response") && !resp_is_error(resp)) {
      # Update cache from headers of successful responses
      update_rate_limit_cache(resp_headers(resp))
      parsed <- resp_body_json(resp)
      sanitise_text(parsed$choices[[1]]$message$content)
    } else {
      status <- tryCatch(resp_status(resp), error = function(e) "unknown")
      message(sprintf("  Request %d failed (status: %s)", j, status))
      failed_indices <<- c(failed_indices, j)
      NA_character_
    }
  })
  
  # Retry any failures
  if (length(failed_indices) > 0) {
    message(sprintf("  Retrying %d failed requests...", length(failed_indices)))
    Sys.sleep(retry_delay)
    
    retry_requests <- lapply(failed_indices, function(i) {
      request("https://api.openai.com/v1/chat/completions") |>
        req_headers(
          Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
          `Content-Type` = "application/json"
        ) |>
        req_body_json(list(
          model       = models[i],
          messages    = list(list(role = "user", content = prompts[i])),
          temperature = temps[i]
        )) |>
        req_retry(
          max_tries    = 2,
          is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503),
          backoff      = \(i) 3
        )
    })
    
    retry_responses <- req_perform_parallel(
      retry_requests,
      on_error   = "continue",
      max_active = length(retry_requests)
    )
    
    for (j in seq_along(failed_indices)) {
      resp <- retry_responses[[j]]
      if (inherits(resp, "httr2_response") && !resp_is_error(resp)) {
        update_rate_limit_cache(resp_headers(resp))
        parsed <- resp_body_json(resp)
        results[[failed_indices[j]]] <- sanitise_text(parsed$choices[[1]]$message$content)
        message(sprintf("  ✓ Retry %d succeeded", failed_indices[j]))
      } else {
        message(sprintf("  ✗ Retry %d failed permanently", failed_indices[j]))
      }
    }
  }
  
  total <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("✓ %d requests completed in %.1fs (%.0f req/min) — cache: %s/%s requests, %s/%s tokens",
                  n_prompts, total,
                  n_prompts / max(total, 0.01) * 60,
                  .rate_limit_cache$requests_remaining %||% "?",
                  .rate_limit_cache$requests_limit     %||% "?",
                  .rate_limit_cache$tokens_remaining   %||% "?",
                  .rate_limit_cache$tokens_limit       %||% "?"))
  
  return(results)
}

# ============================================
# check_rate_limits — now reads from cache first,
# only makes a live call if cache is empty or stale (> 60s)
# ============================================

check_rate_limits <- function(model = "gpt-4o-mini", force_refresh = FALSE) {
  
  cache_age <- if (!is.null(.rate_limit_cache$last_updated)) {
    as.numeric(difftime(Sys.time(), .rate_limit_cache$last_updated, units = "secs"))
  } else {
    Inf
  }
  
  # Return cache if fresh enough and not forcing refresh
  if (!force_refresh && cache_age < 60 && !is.null(.rate_limit_cache$requests_limit)) {
    return(list(
      requests_limit     = .rate_limit_cache$requests_limit,
      requests_remaining = .rate_limit_cache$requests_remaining,
      tokens_limit       = .rate_limit_cache$tokens_limit,
      tokens_remaining   = .rate_limit_cache$tokens_remaining,
      model              = model,
      from_cache         = TRUE
    ))
  }
  
  # Live check
  tryCatch({
    resp <- request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        `Content-Type` = "application/json"
      ) |>
      req_body_json(list(
        model       = model,
        messages    = list(list(role = "user", content = "hi")),
        temperature = 0,
        max_tokens  = 1
      )) |>
      req_perform()
    
    headers <- resp_headers(resp)
    update_rate_limit_cache(headers)
    
    list(
      requests_limit     = .rate_limit_cache$requests_limit,
      requests_remaining = .rate_limit_cache$requests_remaining,
      tokens_limit       = .rate_limit_cache$tokens_limit,
      tokens_remaining   = .rate_limit_cache$tokens_remaining,
      model              = model,
      from_cache         = FALSE
    )
  }, error = function(e) {
    warning(sprintf("Could not check rate limits for %s: %s", model, e$message))
    return(NULL)
  })
}

# ============================================
# find_available_model — unchanged
# ============================================

find_available_model <- function(fallback_models,
                                 estimated_tokens_needed,
                                 exclude_model = NULL) {
  
  candidates <- fallback_models[fallback_models != exclude_model]
  
  for (candidate in candidates) {
    message(sprintf("  Checking availability of %s...", candidate))
    limits <- check_rate_limits(candidate, force_refresh = TRUE)
    
    if (!is.null(limits)) {
      has_requests <- is.null(limits$requests_remaining) || limits$requests_remaining > 10
      has_tokens   <- is.null(limits$tokens_remaining)   || 
        limits$tokens_remaining > estimated_tokens_needed
      
      if (has_requests && has_tokens) {
        message(sprintf("  ✓ %s available", candidate))
        return(candidate)
      } else {
        message(sprintf("  ✗ %s insufficient", candidate))
      }
    }
  }
  
  message("  No fallback models available")
  return(NULL)
}

# ============================================
# query_chatgpt_multiple — kept for compatibility
# ============================================

query_chatgpt_multiple <- function(text, repetitions) {
  if (!is.character(text) || length(text) != 1 || nchar(text) == 0) {
    stop("Text must be a non-empty character string")
  }
  repetitions <- as.integer(repetitions)
  responses   <- ask_chatgpt_async(rep(text, repetitions))
  
  data.frame(
    query_number = seq_len(repetitions),
    question     = rep(text, repetitions),
    response     = unlist(responses),
    timestamp    = Sys.time(),
    stringsAsFactors = FALSE
  )
}