

# ============================================
# STEP 1: Rename dim_customer -> dim_brand
#         and customer_id -> brand_id, customer_name -> brand_name
# ============================================

# Rename the table
dbExecute(con, "ALTER TABLE dim_customer RENAME TO dim_brand;")

# Rename the columns
dbExecute(con, "ALTER TABLE dim_brand RENAME COLUMN customer_id TO brand_id;")
dbExecute(con, "ALTER TABLE dim_brand RENAME COLUMN customer_name TO brand_name;")

# Drop columns that are moving to dim_user
dbExecute(con, "ALTER TABLE dim_brand DROP COLUMN IF EXISTS email;")
dbExecute(con, "ALTER TABLE dim_brand DROP COLUMN IF EXISTS password_hash;")
dbExecute(con, "ALTER TABLE dim_brand DROP COLUMN IF EXISTS date_added;")

# Rename the sequence
dbExecute(con, "ALTER SEQUENCE dim_customer_id_seq RENAME TO dim_brand_id_seq;")

# ============================================
# STEP 2: Rename customer_id -> brand_id in ALL fact tables
# ============================================

# fact_presence_history
dbExecute(con, "ALTER TABLE fact_presence_history RENAME COLUMN customer_id TO brand_id;")

# fact_perception_history
dbExecute(con, "ALTER TABLE fact_perception_history RENAME COLUMN customer_id TO brand_id;")

# fact_prestige_history
dbExecute(con, "ALTER TABLE fact_prestige_history RENAME COLUMN customer_id TO brand_id;")

# fact_persistence_history
dbExecute(con, "ALTER TABLE fact_persistence_history RENAME COLUMN customer_id TO brand_id;")

# fact_airr_history
dbExecute(con, "ALTER TABLE fact_airr_history RENAME COLUMN customer_id TO brand_id;")

# dim_cust_query -> dim_brand_query
dbExecute(con, "ALTER TABLE dim_cust_query RENAME TO dim_brand_query;")
dbExecute(con, "ALTER TABLE dim_brand_query RENAME COLUMN customer_id TO brand_id;")

# fact_query_history
dbExecute(con, "ALTER TABLE fact_query_history RENAME COLUMN customer_id TO brand_id;")

# fact_comp_history
dbExecute(con, "ALTER TABLE fact_comp_history RENAME COLUMN customer_id TO brand_id;")

# fact_query_comp_history
dbExecute(con, "ALTER TABLE fact_query_comp_history RENAME COLUMN customer_id TO brand_id;")

# ============================================
# STEP 3: Drop old customer_log and fact_customer_level
# ============================================

dbExecute(con, "DROP TABLE IF EXISTS customer_log;")
dbExecute(con, "DROP TABLE IF EXISTS fact_customer_level;")

# ============================================
# STEP 4: Create dim_user
# ============================================

dbExecute(con, "
CREATE TABLE dim_user (
    login_id SERIAL PRIMARY KEY,
    date_added DATE NOT NULL DEFAULT CURRENT_DATE,
    email VARCHAR(100) UNIQUE NOT NULL,
    password_hash VARCHAR(256) NOT NULL
);
")

# ============================================
# STEP 5: Recreate dim_subscription (drop and recreate cleanly)
# ============================================

dbExecute(con, "DROP TABLE IF EXISTS dim_subscription CASCADE;")

dbExecute(con, "
CREATE TABLE dim_subscription (
    subscription_level_id INTEGER NOT NULL,
    subscription_name VARCHAR(255) NOT NULL,
    num_prompts_included INTEGER NOT NULL,
    num_competitors_included INTEGER NOT NULL,
    PRIMARY KEY (subscription_level_id)
);
")

# ============================================
# STEP 6: Create fact_user_sub_level
# ============================================

dbExecute(con, "
CREATE TABLE fact_user_sub_level (
    login_id INTEGER NOT NULL,
    subscription_level_id INTEGER NOT NULL,
    extra_prompts_added INTEGER NOT NULL DEFAULT 0,
    extra_competitors_added INTEGER NOT NULL DEFAULT 0,
    date_valid_from DATE NOT NULL,
    date_valid_to DATE NOT NULL,
    PRIMARY KEY (login_id, subscription_level_id, date_valid_from),
    FOREIGN KEY (login_id) REFERENCES dim_user(login_id) ON DELETE CASCADE,
    FOREIGN KEY (subscription_level_id) REFERENCES dim_subscription(subscription_level_id) ON DELETE CASCADE
);
")

# ============================================
# STEP 7: Create fact_user_brands_tracked
# ============================================

dbExecute(con, "
CREATE TABLE fact_user_brands_tracked (
    login_id INTEGER NOT NULL,
    main_brand_flag BOOLEAN NOT NULL DEFAULT FALSE,
    brand_id INTEGER NOT NULL,
    date_valid_from DATE NOT NULL,
    date_valid_to DATE,
    PRIMARY KEY (login_id, brand_id, date_valid_from),
    FOREIGN KEY (login_id) REFERENCES dim_user(login_id) ON DELETE CASCADE,
    FOREIGN KEY (brand_id) REFERENCES dim_brand(brand_id) ON DELETE CASCADE
);
")

# ============================================
# STEP 8: Rename indexes
# ============================================

# Drop old indexes and recreate with new names
dbExecute(con, "DROP INDEX IF EXISTS idx_presence_customer_date;")
dbExecute(con, "DROP INDEX IF EXISTS idx_perception_customer_date;")
dbExecute(con, "DROP INDEX IF EXISTS idx_prestige_customer_date;")
dbExecute(con, "DROP INDEX IF EXISTS idx_persistence_customer_date;")
dbExecute(con, "DROP INDEX IF EXISTS idx_airr_customer_date;")
dbExecute(con, "DROP INDEX IF EXISTS idx_comp_history_customer;")
dbExecute(con, "DROP INDEX IF EXISTS idx_query_history_customer;")
dbExecute(con, "DROP INDEX IF EXISTS idx_query_comp_history_customer;")
dbExecute(con, "DROP INDEX IF EXISTS idx_cust_query_query_id;")

dbExecute(con, "CREATE INDEX idx_presence_brand_date ON fact_presence_history(brand_id, date);")
dbExecute(con, "CREATE INDEX idx_perception_brand_date ON fact_perception_history(brand_id, date);")
dbExecute(con, "CREATE INDEX idx_prestige_brand_date ON fact_prestige_history(brand_id, date);")
dbExecute(con, "CREATE INDEX idx_persistence_brand_date ON fact_persistence_history(brand_id, date);")
dbExecute(con, "CREATE INDEX idx_airr_brand_date ON fact_airr_history(brand_id, date);")
dbExecute(con, "CREATE INDEX idx_comp_history_brand ON fact_comp_history(brand_id);")
dbExecute(con, "CREATE INDEX idx_query_history_brand ON fact_query_history(brand_id);")
dbExecute(con, "CREATE INDEX idx_query_comp_history_brand ON fact_query_comp_history(brand_id);")
dbExecute(con, "CREATE INDEX idx_brand_query_query_id ON dim_brand_query(query_id);")

# New indexes for new tables
dbExecute(con, "CREATE INDEX idx_user_brands_login ON fact_user_brands_tracked(login_id);")
dbExecute(con, "CREATE INDEX idx_user_brands_brand ON fact_user_brands_tracked(brand_id);")
dbExecute(con, "CREATE INDEX idx_user_sub_login ON fact_user_sub_level(login_id);")


dbExecute(con, "
CREATE TABLE fact_user_queries_tracked (
    login_id INTEGER NOT NULL,
    query_id INTEGER NOT NULL,
    date_valid_from DATE NOT NULL,
    date_valid_to DATE,
    PRIMARY KEY (login_id, query_id, date_valid_from),
    FOREIGN KEY (login_id) REFERENCES dim_user(login_id) ON DELETE CASCADE,
    FOREIGN KEY (query_id) REFERENCES dim_query(query_id) ON DELETE CASCADE
);
")


dbExecute(con, "CREATE INDEX idx_user_queries_login ON fact_user_queries_tracked(login_id);")
dbExecute(con, "CREATE INDEX idx_user_queries_query ON fact_user_queries_tracked(query_id);")


dbExecute(con, "
  INSERT INTO fact_user_queries_tracked (login_id, query_id, date_valid_from)
  SELECT DISTINCT ubt.login_id, dbq.query_id, dbq.date_added
  FROM dim_brand_query dbq
  JOIN fact_user_brands_tracked ubt ON dbq.brand_id = ubt.brand_id
  WHERE ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE
  ON CONFLICT (login_id, query_id, date_valid_from) DO NOTHING;
")


# Login IDs 4-9: Pro tier (subscription_level_id = 2)
dbExecute(con, "
  INSERT INTO fact_user_sub_level (
    login_id, subscription_level_id, extra_prompts_added, extra_competitors_added,
    date_valid_from, date_valid_to
  )
  VALUES 
    (4, 2, 0, 0, '2025-01-01', '2099-12-31'),
    (5, 2, 0, 0, '2025-01-01', '2099-12-31'),
    (6, 2, 0, 0, '2025-01-01', '2099-12-31'),
    (7, 2, 0, 0, '2025-01-01', '2099-12-31'),
    (8, 2, 0, 0, '2025-01-01', '2099-12-31'),
    (9, 2, 0, 0, '2025-01-01', '2099-12-31')
  ON CONFLICT (login_id, subscription_level_id, date_valid_from) DO NOTHING;
")

# Login ID 3: Enterprise tier (subscription_level_id = 3)
dbExecute(con, "
  INSERT INTO fact_user_sub_level (
    login_id, subscription_level_id, extra_prompts_added, extra_competitors_added,
    date_valid_from, date_valid_to
  )
  VALUES (3, 3, 0, 0, '2025-01-01', '2099-12-31')
  ON CONFLICT (login_id, subscription_level_id, date_valid_from) DO NOTHING;
")

# Login ID 1: Enterprise tier + 1000 extra prompts + 1000 extra competitors
dbExecute(con, "
  INSERT INTO fact_user_sub_level (
    login_id, subscription_level_id, extra_prompts_added, extra_competitors_added,
    date_valid_from, date_valid_to
  )
  VALUES (1, 3, 1000, 1000, '2025-01-01', '2099-12-31')
  ON CONFLICT (login_id, subscription_level_id, date_valid_from) DO NOTHING;
")

# Verify
# fact_user_sub_level <- dbGetQuery(con, "SELECT * FROM fact_user_sub_level ORDER BY login_id")

tracked_prompts <- dbGetQuery(con, "
  SELECT 
    u.login_id,
    mb.brand_name as main_brand,
    uqt.query_id,
    dq.query_string,
    uqt.date_valid_from
  FROM dim_user u
  -- Get main brand
  LEFT JOIN (
    SELECT ubt.login_id, b.brand_name
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.main_brand_flag = TRUE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
  ) mb ON u.login_id = mb.login_id
  -- Get tracked queries
  LEFT JOIN fact_user_queries_tracked uqt ON u.login_id = uqt.login_id
    AND uqt.date_valid_from <= CURRENT_DATE
    AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to >= CURRENT_DATE)
  LEFT JOIN dim_query dq ON uqt.query_id = dq.query_id
  ORDER BY u.login_id, dq.query_string
")

# View it
View(tracked_prompts)

# To remove specific ones, use:
# dbExecute(con, "
#   UPDATE fact_user_queries_tracked
#   SET date_valid_to = CURRENT_DATE
#   WHERE login_id = $1 AND query_id = $2
#     AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
#   params = list(<login_id>, <query_id>))

# Or to bulk remove by a vector of query_ids for a specific user:
# ids_to_remove <- c(97	,
#                    98	,
#                    99	,
#                    100	,
#                    104	,
#                    105	,
#                    106	,
#                    107	,
#                    108	,
#                    109	,
#                    110	,
#                    111	,
#                    112	,
#                    113	,
#                    114	,
#                    115	,
#                    116	,
#                    117	,
#                    118	)
# for (qid in ids_to_remove) {
#   dbExecute(con, "
#     UPDATE fact_user_queries_tracked
#     SET date_valid_to = CURRENT_DATE
#     WHERE login_id = $1 AND query_id = $2
#       AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
#     params = list(9, qid))
# }

