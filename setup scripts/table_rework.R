

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
ids_to_remove <- c(86	,
                   87	,
                   89	,
                   90	,
                   91	,
                   92	,
                   93	,
                   94	,
                   95	,
                   97	,
                   98	,
                   99	,
                   100	,
                   104	,
                   105	,
                   106	,
                   107	,
                   108	,
                   109	,
                   110	,
                   111	,
                   112	,
                   113	,
                   114	,
                   115	,
                   116	,
                   117	,
                   118	)
for (qid in ids_to_remove) {
  dbExecute(con, "
    UPDATE fact_user_queries_tracked
    SET date_valid_to = CURRENT_DATE
    WHERE login_id = $1 AND query_id = $2
      AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
    params = list(4, qid))
}
dbExecute(pool, "
INSERT INTO fact_user_queries_tracked (login_id, query_id, date_valid_from, date_valid_to)
SELECT 10, query_id, date_valid_from, date_valid_to
FROM fact_user_queries_tracked
WHERE login_id = 3;")


dbExecute(pool, "
  UPDATE dim_user 
  SET email = 'ua@ua.com' 
  WHERE email = 'ue@ue.com';
")

# -- Add reach/region columns to dim_brand
dbExecute(pool, "
ALTER TABLE dim_brand 
ADD COLUMN IF NOT EXISTS brand_reach VARCHAR(20) DEFAULT 'global',
ADD COLUMN IF NOT EXISTS reach_country VARCHAR(100),
ADD COLUMN IF NOT EXISTS reach_region VARCHAR(200),
ADD COLUMN IF NOT EXISTS industry VARCHAR(200);")

# -- Add onboarding_complete flag to dim_user
dbExecute(pool, "
ALTER TABLE dim_user
ADD COLUMN IF NOT EXISTS onboarding_complete BOOLEAN DEFAULT FALSE;")

dbExecute(pool, "
ALTER TABLE dim_brand
ADD COLUMN IF NOT EXISTS reach_postcode VARCHAR(20);")


# -- Profiles table
dbExecute(pool, "
CREATE TABLE IF NOT EXISTS dim_customer_profile (
  profile_id        SERIAL PRIMARY KEY,
  profile_name      VARCHAR(100) NOT NULL,
  profile_descriptor TEXT NOT NULL,
  is_standard       BOOLEAN DEFAULT TRUE,
  created_by_login  INTEGER REFERENCES dim_user(login_id),
  date_created      DATE DEFAULT CURRENT_DATE
);")

# -- Link profiles to users
dbExecute(pool, "
CREATE TABLE IF NOT EXISTS fact_user_profiles_tracked (
  login_id          INTEGER REFERENCES dim_user(login_id),
  profile_id        INTEGER REFERENCES dim_customer_profile(profile_id),
  date_valid_from   DATE NOT NULL DEFAULT CURRENT_DATE,
  date_valid_to     DATE,
  PRIMARY KEY (login_id, profile_id, date_valid_from)
);")

# -- Profile scores for brand-level
dbExecute(pool, "
CREATE TABLE IF NOT EXISTS fact_profile_brand_history (
  profile_id        INTEGER REFERENCES dim_customer_profile(profile_id),
  brand_id          INTEGER REFERENCES dim_brand(brand_id),
  date              DATE NOT NULL,
  presence_score    NUMERIC(6,2),
  perception_score  NUMERIC(6,2),
  prestige_score    NUMERIC(6,2),
  persistence_score NUMERIC(6,2),
  airr_score        NUMERIC(6,2),
  PRIMARY KEY (profile_id, brand_id, date)
);")

# -- Profile scores for prompt-level
dbExecute(pool, "
CREATE TABLE IF NOT EXISTS fact_profile_query_history (
  profile_id        INTEGER REFERENCES dim_customer_profile(profile_id),
  brand_id          INTEGER REFERENCES dim_brand(brand_id),
  query_id          INTEGER REFERENCES dim_query(query_id),
  date              DATE NOT NULL,
  presence_score    NUMERIC(6,2),
  perception_score  NUMERIC(6,2),
  prestige_score    NUMERIC(6,2),
  persistence_score NUMERIC(6,2),
  airr_score        NUMERIC(6,2),
  PRIMARY KEY (profile_id, brand_id, query_id, date)
);")

# -- Insert standard profiles
dbExecute(pool, "
INSERT INTO dim_customer_profile (profile_name, profile_descriptor, is_standard) VALUES
('Athletic & Active',    'I am an active person aged 18-35 who exercises at least 4 times per week. I am health-conscious with a mid-to-high income and tend to prioritise performance and quality', TRUE),
('Young Professional',   'I am an urban professional aged 22-32 with a college degree earning between $45,000 and $80,000 per year. I am career focused and either single or newly in a relationship', TRUE),
('Family Focused',       'I am a married parent aged 30-45 with two or more children. My household income is between $60,000 and $100,000 per year and I live in the suburbs and prioritise value for money', TRUE),
('Budget Conscious',     'I am a price-sensitive shopper with a household income under $40,000 per year. I always look for the best deal and prioritise affordability over brand prestige', TRUE),
('Luxury Seeker',        'I am a high earner aged 30-55 with a household income over $150,000 per year. I am brand conscious and always prioritise quality and prestige over price', TRUE),
('Tech Enthusiast',      'I am typically male aged 18-40 and an early adopter of new technology. I have a mid-to-high income and prefer to shop online and research products thoroughly before buying', TRUE),
('Senior',               'I am aged 55 or over and either retired or approaching retirement. I am on a fixed or pension income, tend to be brand loyal and am increasingly health conscious', TRUE),
('Student',              'I am aged 18-24 and currently in higher education. My income is under $25,000 per year. I am trend aware but highly price sensitive and often shop online', TRUE),
('Small Business Owner', 'I am self-employed and aged between 28 and 50. My income is variable and I am often time poor. I tend to focus on return on investment and practical value', TRUE),
('Health & Wellness',    'I prioritise health and wellness in all purchasing decisions. I prefer organic and natural products, am willing to pay a premium for quality and am conscious of my diet and fitness', TRUE),
('Eco Conscious',        'I am aged 20-40 and environmentally motivated. I will actively choose and pay more for sustainable and ethical brands and am very aware of environmental impact', TRUE),
('Urban Millennial',     'I am a city dweller aged 25-38 who rents rather than owns. My income is between $50,000 and $90,000 per year and I tend to prioritise experiences over possessions', TRUE),
('Suburban Parent',      'I am a homeowner aged 32-48 with children in school. I own two cars and my household income is between $75,000 and $130,000 per year. Convenience and family value are important to me', TRUE),
('High Earner',          'I have a household income over $200,000 per year and am aged 35-55. I am status conscious, time poor and willing to pay significantly more for the best quality and service', TRUE),
('Retiree',              'I am aged 62 or over and living on pension or savings income. I am health focused, brand loyal and tend to research purchases carefully before committing', TRUE)
ON CONFLICT DO NOTHING;")



dbExecute(pool, "
ALTER TABLE fact_user_brands_tracked
  ADD COLUMN IF NOT EXISTS industry TEXT;")



dbExecute(pool, "ALTER TABLE fact_presence_history    ADD COLUMN IF NOT EXISTS login_id INTEGER REFERENCES dim_user(login_id);")
dbExecute(pool, "ALTER TABLE fact_perception_history  ADD COLUMN IF NOT EXISTS login_id INTEGER REFERENCES dim_user(login_id);")
dbExecute(pool, "ALTER TABLE fact_prestige_history    ADD COLUMN IF NOT EXISTS login_id INTEGER REFERENCES dim_user(login_id);")
dbExecute(pool, "ALTER TABLE fact_persistence_history ADD COLUMN IF NOT EXISTS login_id INTEGER REFERENCES dim_user(login_id);")
dbExecute(pool, "ALTER TABLE fact_airr_history        ADD COLUMN IF NOT EXISTS login_id INTEGER REFERENCES dim_user(login_id);")

# -- Drop old unique constraints and add new composite ones
# -- (adjust constraint names to match your actual schema)

# -- fact_presence_history
dbExecute(pool, "ALTER TABLE fact_presence_history
DROP CONSTRAINT IF EXISTS fact_presence_history_brand_id_date_key;")
dbExecute(pool, "ALTER TABLE fact_presence_history
ADD CONSTRAINT fact_presence_history_brand_login_date_key 
UNIQUE (brand_id, login_id, date);")

# -- Repeat for the other tables:
dbExecute(pool, "  ALTER TABLE fact_perception_history
DROP CONSTRAINT IF EXISTS fact_perception_history_brand_id_date_key;")
dbExecute(pool, "ALTER TABLE fact_perception_history
ADD CONSTRAINT fact_perception_history_brand_login_date_key
UNIQUE (brand_id, login_id, date);")

dbExecute(pool, "ALTER TABLE fact_prestige_history
DROP CONSTRAINT IF EXISTS fact_prestige_history_brand_id_date_key;")
dbExecute(pool, "ALTER TABLE fact_prestige_history
ADD CONSTRAINT fact_prestige_history_brand_login_date_key
UNIQUE (brand_id, login_id, date);")

dbExecute(pool, "ALTER TABLE fact_persistence_history
DROP CONSTRAINT IF EXISTS fact_persistence_history_brand_id_date_key;")
dbExecute(pool, "ALTER TABLE fact_persistence_history
ADD CONSTRAINT fact_persistence_history_brand_login_date_key
UNIQUE (brand_id, login_id, date);")

dbExecute(pool, "ALTER TABLE fact_airr_history
DROP CONSTRAINT IF EXISTS fact_airr_history_brand_id_date_key;")
dbExecute(pool, "ALTER TABLE fact_airr_history
ADD CONSTRAINT fact_airr_history_brand_login_date_key
UNIQUE (brand_id, login_id, date);")

dbExecute(pool, "CREATE TABLE dim_api_keys (
  api_key_id       SERIAL PRIMARY KEY,
  login_id         INTEGER NOT NULL REFERENCES dim_user(login_id),
  api_key          VARCHAR(64) NOT NULL UNIQUE,
  key_name         VARCHAR(100),
  date_created     DATE NOT NULL DEFAULT CURRENT_DATE,
  date_last_used   TIMESTAMP,
  date_revoked     TIMESTAMP,
  is_active        BOOLEAN NOT NULL DEFAULT TRUE
);")


dbExecute(pool, "CREATE INDEX idx_api_keys_login    ON dim_api_keys(login_id);")
dbExecute(pool, "CREATE INDEX idx_api_keys_key      ON dim_api_keys(api_key);")
dbExecute(pool, "CREATE INDEX idx_api_keys_active   ON dim_api_keys(is_active);")

dbExecute(pool, "
CREATE TABLE user_purchases (
  id               SERIAL PRIMARY KEY,
  email            TEXT NOT NULL,
  access_level     TEXT NOT NULL,  -- 'basic', 'premium', 'enterprise'
  amount           NUMERIC,
  purchase_date    TIMESTAMP DEFAULT NOW(),
  stripe_session_id TEXT UNIQUE,
  is_active        BOOLEAN DEFAULT TRUE
);
")


dbExecute(pool, "
  ALTER TABLE dim_subscription 
  ADD COLUMN num_personas_included INT;
")

# Step 2: Delete existing rows so we can reinsert with correct data
dbExecute(pool, "
  DELETE FROM dim_subscription;
")

# Step 3: Reset the sequence if using SERIAL/auto-increment
dbExecute(pool, "
  ALTER SEQUENCE dim_subscription_subscription_level_id_seq RESTART WITH 1;
")

# Step 4: Insert the new correct data
dbExecute(pool, "
  INSERT INTO dim_subscription 
    (subscription_level_id, subscription_name, num_prompts_included, num_competitors_included, num_personas_included)
  VALUES
    (1, 'Starter', 1,  5,  1),
    (2, 'Pro',     15, 10, 7),
    (3, 'Custom',  15, 10, 7);
")

dbExecute(pool, "
  INSERT INTO dim_subscription 
    (subscription_level_id, subscription_name, num_prompts_included, num_competitors_included, num_personas_included)
  VALUES
    (4, 'AiRR test', 1,  1,  1);
")