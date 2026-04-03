# recalculate_persistence.R
# Recalculates all persistence scores using the updated exponential weighting
# Run standalone: Rscript /home/aarogers/airr/recalculate_persistence.R

setwd("/home/aarogers/airr")
source("global.R")

con <- make_con()
on.exit(dbDisconnect(con), add = TRUE)

message("=== Persistence Recalculation Started: ", Sys.time(), " ===")

# ============================================
# 1. Brand-level persistence (fact_persistence_history)
# ============================================

message("\n--- Recalculating brand persistence ---")

brand_pairs <- dbGetQuery(con, "
  SELECT DISTINCT brand_id, login_id
  FROM fact_presence_history
  ORDER BY login_id, brand_id
")

message(sprintf("Found %d brand/user pairs", nrow(brand_pairs)))

brand_success <- 0
brand_failed  <- 0

for (i in seq_len(nrow(brand_pairs))) {
  bid <- brand_pairs$brand_id[i]
  lid <- brand_pairs$login_id[i]
  
  tryCatch({
    
    # Get full presence history for this brand/user
    presence_history <- dbGetQuery(con, "
      SELECT date, overall_score
      FROM fact_presence_history
      WHERE brand_id = $1 AND login_id = $2
      ORDER BY date",
                                   params = list(bid, lid))
    
    if (nrow(presence_history) < 2) {
      # Not enough history — skip, leave as-is
      next
    }
    
    # Recalculate persistence for each date that has enough history
    for (d in seq_len(nrow(presence_history))) {
      
      # Only use data up to and including this date (no look-ahead)
      history_to_date <- presence_history[seq_len(d), ]
      
      if (nrow(history_to_date) < 2) next
      
      score_date <- presence_history$date[d]
      
      persistence_result <- if (nrow(history_to_date) < 5) {
        # Not enough history — use latest presence score
        list(
          score                    = tail(history_to_date$overall_score, 1),
          coefficient_of_variation = 0,
          interpretation           = "Not enough data",
          daily_perc_change        = 0
        )
      } else {
        calculate_persistence_score("placeholder", 
                                    history_to_date %>%
                                      rename(overall_score = overall_score))
      }
      
      # Upsert into fact_persistence_history
      dbExecute(con, "
        INSERT INTO fact_persistence_history (
          brand_id, login_id, date, persistence_score,
          coefficient_of_variation, interpretation, daily_perc_change
        ) VALUES ($1, $2, $3, $4, $5, $6, $7)
        ON CONFLICT (brand_id, login_id, date) DO UPDATE SET
          persistence_score        = EXCLUDED.persistence_score,
          coefficient_of_variation = EXCLUDED.coefficient_of_variation,
          interpretation           = EXCLUDED.interpretation,
          daily_perc_change        = EXCLUDED.daily_perc_change",
                params = list(
                  bid, lid, score_date,
                  persistence_result$score,
                  persistence_result$coefficient_of_variation,
                  persistence_result$interpretation,
                  persistence_result$daily_perc_change
                ))
    }
    
    brand_success <- brand_success + 1
    if (i %% 10 == 0) {
      message(sprintf("  [%d/%d] brand_id %d / login_id %d done",
                      i, nrow(brand_pairs), bid, lid))
    }
    
  }, error = function(e) {
    brand_failed <<- brand_failed + 1
    message(sprintf("  ✗ brand_id %d / login_id %d: %s", bid, lid, e$message))
  })
}

message(sprintf("\n✓ Brand persistence: %d updated, %d failed",
                brand_success, brand_failed))

# ============================================
# 2. Recalculate AIRR scores for brands
#    (since persistence is a component of AIRR)
# ============================================

message("\n--- Recalculating brand AIRR scores ---")

airr_pairs <- dbGetQuery(con, "
  SELECT DISTINCT brand_id, login_id
  FROM fact_airr_history
  ORDER BY login_id, brand_id
")

airr_success <- 0
airr_failed  <- 0

for (i in seq_len(nrow(airr_pairs))) {
  bid <- airr_pairs$brand_id[i]
  lid <- airr_pairs$login_id[i]
  
  tryCatch({
    
    # Get all dates for this brand/user
    dates <- dbGetQuery(con, "
      SELECT date FROM fact_airr_history
      WHERE brand_id = $1 AND login_id = $2
      ORDER BY date",
                        params = list(bid, lid))$date
    
    for (score_date in dates) {
      
      scores <- dbGetQuery(con, "
        SELECT 
          COALESCE(fps.persistence_score, 0) as persistence_score,
          COALESCE(fpres.overall_score, 0)    as presence_score,
          COALESCE(fperc.perception_score, 0) as perception_score,
          COALESCE(fprest.prestige_score, 0)  as prestige_score
        FROM (SELECT $3::date as d) x
        LEFT JOIN fact_persistence_history fps
          ON fps.brand_id = $1 AND fps.login_id = $2 AND fps.date = $3
        LEFT JOIN fact_presence_history fpres
          ON fpres.brand_id = $1 AND fpres.login_id = $2 AND fpres.date = $3
        LEFT JOIN fact_perception_history fperc
          ON fperc.brand_id = $1 AND fperc.login_id = $2 AND fperc.date = $3
        LEFT JOIN fact_prestige_history fprest
          ON fprest.brand_id = $1 AND fprest.login_id = $2 AND fprest.date = $3",
                           params = list(bid, lid, score_date))
      
      airr_score <- scores$perception_score  * AIRR_WEIGHTS$perception  +
        scores$presence_score     * AIRR_WEIGHTS$presence    +
        scores$prestige_score     * AIRR_WEIGHTS$prestige    +
        scores$persistence_score  * AIRR_WEIGHTS$persistence
      
      dbExecute(con, "
        UPDATE fact_airr_history
        SET airr_score = $1
        WHERE brand_id = $2 AND login_id = $3 AND date = $4",
                params = list(airr_score, bid, lid, score_date))
    }
    
    airr_success <- airr_success + 1
    
  }, error = function(e) {
    airr_failed <<- airr_failed + 1
    message(sprintf("  ✗ AIRR brand_id %d / login_id %d: %s", 
                    bid, lid, e$message))
  })
}

message(sprintf("✓ Brand AIRR: %d updated, %d failed",
                airr_success, airr_failed))

# ============================================
# 3. Prompt-level persistence (fact_query_history)
# ============================================

message("\n--- Recalculating prompt persistence ---")

query_pairs <- dbGetQuery(con, "
  SELECT DISTINCT brand_id, query_id
  FROM fact_query_history
  ORDER BY brand_id, query_id
")

message(sprintf("Found %d brand/query pairs", nrow(query_pairs)))

query_success <- 0
query_failed  <- 0

for (i in seq_len(nrow(query_pairs))) {
  bid <- query_pairs$brand_id[i]
  qid <- query_pairs$query_id[i]
  
  tryCatch({
    
    presence_history <- dbGetQuery(con, "
      SELECT date, presence_score
      FROM fact_query_history
      WHERE brand_id = $1 AND query_id = $2
      ORDER BY date",
                                   params = list(bid, qid))
    
    if (nrow(presence_history) < 2) next
    
    for (d in seq_len(nrow(presence_history))) {
      
      history_to_date <- presence_history[seq_len(d), ]
      if (nrow(history_to_date) < 2) next
      
      score_date <- presence_history$date[d]
      
      persistence_score <- if (nrow(history_to_date) < 5) {
        tail(history_to_date$presence_score, 1)
      } else {
        calculate_daily_persistence_sep(history_to_date)
      }
      
      # Recalculate AIRR for this prompt/date
      existing <- dbGetQuery(con, "
        SELECT presence_score, perception_score, prestige_score
        FROM fact_query_history
        WHERE brand_id = $1 AND query_id = $2 AND date = $3",
                             params = list(bid, qid, score_date))
      
      if (nrow(existing) == 0) next
      
      airr_score <- existing$perception_score * AIRR_WEIGHTS$perception +
        existing$presence_score   * AIRR_WEIGHTS$presence   +
        existing$prestige_score   * AIRR_WEIGHTS$prestige   +
        persistence_score         * AIRR_WEIGHTS$persistence
      
      dbExecute(con, "
        UPDATE fact_query_history
        SET persistence_score = $1,
            airr_score        = $2
        WHERE brand_id = $3 AND query_id = $4 AND date = $5",
                params = list(persistence_score, airr_score, bid, qid, score_date))
    }
    
    query_success <- query_success + 1
    if (i %% 20 == 0) {
      message(sprintf("  [%d/%d] brand_id %d / query_id %d done",
                      i, nrow(query_pairs), bid, qid))
    }
    
  }, error = function(e) {
    query_failed <<- query_failed + 1
    message(sprintf("  ✗ brand_id %d / query_id %d: %s", bid, qid, e$message))
  })
}

message(sprintf("\n✓ Prompt persistence: %d updated, %d failed",
                query_success, query_failed))

# ============================================
# 4. Profile brand persistence (fact_profile_brand_history)
# ============================================

message("\n--- Recalculating persona brand persistence ---")

profile_brand_pairs <- dbGetQuery(con, "
  SELECT DISTINCT brand_id, profile_id
  FROM fact_profile_brand_history
  ORDER BY profile_id, brand_id
")

message(sprintf("Found %d profile/brand pairs", nrow(profile_brand_pairs)))

pb_success <- 0
pb_failed  <- 0

for (i in seq_len(nrow(profile_brand_pairs))) {
  bid <- profile_brand_pairs$brand_id[i]
  pid <- profile_brand_pairs$profile_id[i]
  
  tryCatch({
    
    presence_history <- dbGetQuery(con, "
      SELECT date, presence_score
      FROM fact_profile_brand_history
      WHERE brand_id = $1 AND profile_id = $2
      ORDER BY date",
                                   params = list(bid, pid))
    
    if (nrow(presence_history) < 2) next
    
    for (d in seq_len(nrow(presence_history))) {
      
      history_to_date <- presence_history[seq_len(d), ]
      if (nrow(history_to_date) < 2) next
      
      score_date <- presence_history$date[d]
      
      persistence_score <- if (nrow(history_to_date) < 5) {
        tail(history_to_date$presence_score, 1)
      } else {
        calculate_daily_persistence_sep(history_to_date)
      }
      
      existing <- dbGetQuery(con, "
        SELECT presence_score, perception_score, prestige_score
        FROM fact_profile_brand_history
        WHERE brand_id = $1 AND profile_id = $2 AND date = $3",
                             params = list(bid, pid, score_date))
      
      if (nrow(existing) == 0) next
      
      airr_score <- existing$perception_score * AIRR_WEIGHTS$perception +
        existing$presence_score   * AIRR_WEIGHTS$presence   +
        existing$prestige_score   * AIRR_WEIGHTS$prestige   +
        persistence_score         * AIRR_WEIGHTS$persistence
      
      dbExecute(con, "
        UPDATE fact_profile_brand_history
        SET persistence_score = $1,
            airr_score        = $2
        WHERE brand_id = $3 AND profile_id = $4 AND date = $5",
                params = list(persistence_score, airr_score, bid, pid, score_date))
    }
    
    pb_success <- pb_success + 1
    
  }, error = function(e) {
    pb_failed <<- pb_failed + 1
    message(sprintf("  ✗ brand_id %d / profile_id %d: %s", bid, pid, e$message))
  })
}

message(sprintf("✓ Persona brand persistence: %d updated, %d failed",
                pb_success, pb_failed))

# ============================================
# 5. Profile query persistence (fact_profile_query_history)
# ============================================

message("\n--- Recalculating persona prompt persistence ---")

profile_query_pairs <- dbGetQuery(con, "
  SELECT DISTINCT brand_id, query_id, profile_id
  FROM fact_profile_query_history
  ORDER BY profile_id, brand_id, query_id
")

message(sprintf("Found %d profile/brand/query combinations", 
                nrow(profile_query_pairs)))

pq_success <- 0
pq_failed  <- 0

for (i in seq_len(nrow(profile_query_pairs))) {
  bid <- profile_query_pairs$brand_id[i]
  qid <- profile_query_pairs$query_id[i]
  pid <- profile_query_pairs$profile_id[i]
  
  tryCatch({
    
    presence_history <- dbGetQuery(con, "
      SELECT date, presence_score
      FROM fact_profile_query_history
      WHERE brand_id = $1 AND query_id = $2 AND profile_id = $3
      ORDER BY date",
                                   params = list(bid, qid, pid))
    
    if (nrow(presence_history) < 2) next
    
    for (d in seq_len(nrow(presence_history))) {
      
      history_to_date <- presence_history[seq_len(d), ]
      if (nrow(history_to_date) < 2) next
      
      score_date <- presence_history$date[d]
      
      persistence_score <- if (nrow(history_to_date) < 5) {
        tail(history_to_date$presence_score, 1)
      } else {
        calculate_daily_persistence_sep(history_to_date)
      }
      
      existing <- dbGetQuery(con, "
        SELECT presence_score, perception_score, prestige_score
        FROM fact_profile_query_history
        WHERE brand_id = $1 AND query_id = $2 
          AND profile_id = $3 AND date = $4",
                             params = list(bid, qid, pid, score_date))
      
      if (nrow(existing) == 0) next
      
      airr_score <- existing$perception_score * AIRR_WEIGHTS$perception +
        existing$presence_score   * AIRR_WEIGHTS$presence   +
        existing$prestige_score   * AIRR_WEIGHTS$prestige   +
        persistence_score         * AIRR_WEIGHTS$persistence
      
      dbExecute(con, "
        UPDATE fact_profile_query_history
        SET persistence_score = $1,
            airr_score        = $2
        WHERE brand_id = $3 AND query_id = $4 
          AND profile_id = $5 AND date = $6",
                params = list(persistence_score, airr_score, bid, qid, pid, score_date))
    }
    
    pq_success <- pq_success + 1
    
  }, error = function(e) {
    pq_failed <<- pq_failed + 1
    message(sprintf("  ✗ brand_id %d / query_id %d / profile_id %d: %s",
                    bid, qid, pid, e$message))
  })
}

message(sprintf("✓ Persona prompt persistence: %d updated, %d failed",
                pq_success, pq_failed))

# ============================================
# Summary
# ============================================

message("\n=== Persistence Recalculation Complete: ", Sys.time(), " ===")
message(sprintf("  Brand persistence:        %d updated, %d failed", 
                brand_success, brand_failed))
message(sprintf("  Brand AIRR:               %d updated, %d failed",
                airr_success, airr_failed))
message(sprintf("  Prompt persistence:       %d updated, %d failed",
                query_success, query_failed))
message(sprintf("  Persona brand:            %d updated, %d failed",
                pb_success, pb_failed))
message(sprintf("  Persona prompt:           %d updated, %d failed",
                pq_success, pq_failed))