

# Execute the CREATE TABLE statements
dbExecute(con, "
CREATE TABLE dim_customer (
    customer_id SERIAL PRIMARY KEY,
    customer_name VARCHAR(255) NOT NULL
    date_added DATE NOT NULL
);
")


dbExecute(con, "CREATE SEQUENCE IF NOT EXISTS dim_customer_id_seq;")
dbExecute(con, "
  SELECT setval('dim_customer_id_seq', 
    COALESCE((SELECT MAX(customer_id) FROM dim_customer), 0));
")
dbExecute(con, "
  ALTER TABLE dim_customer 
  ALTER COLUMN customer_id SET DEFAULT nextval('dim_customer_id_seq');
")

dbExecute(con, "
  ALTER TABLE dim_customer 
  add column email varchar(100);
")

dbExecute(con, "
  ALTER TABLE dim_customer 
  add column password_hash VARCHAR(256);
")


dbExecute(con, "
CREATE TABLE fact_presence_history (
    customer_id INTEGER NOT NULL,
    date DATE NOT NULL,
    overall_score NUMERIC(10, 4),
    simple_mention_rate NUMERIC(10, 4),
    total_responses INTEGER,
    responses_with_mentions INTEGER,
    interpretation TEXT,
    stability NUMERIC(10, 4),
    PRIMARY KEY (customer_id, date),
    FOREIGN KEY (customer_id) REFERENCES dim_customer(customer_id) ON DELETE CASCADE
);
")

dbExecute(con, "
CREATE TABLE fact_perception_history (
    customer_id INTEGER NOT NULL,
    date DATE NOT NULL,
    perception_score NUMERIC(10, 4),
    perception_accuracy_score NUMERIC(10, 4),
    perception_sentiment_score NUMERIC(10, 4),
    prestige_accuracy_interpretation TEXT,
    PRIMARY KEY (customer_id, date),
    FOREIGN KEY (customer_id) REFERENCES dim_customer(customer_id) ON DELETE CASCADE
);
")

dbExecute(con, "
CREATE TABLE fact_prestige_history (
    customer_id INTEGER NOT NULL,
    date DATE NOT NULL,
    prestige_score NUMERIC(10, 4),
    prestige_rank_score NUMERIC(10, 4),
    prestige_rank_comps_brands TEXT,
    prestige_authority_score NUMERIC(10, 4),
    prestige_leadership_score NUMERIC(10, 4),
    PRIMARY KEY (customer_id, date),
    FOREIGN KEY (customer_id) REFERENCES dim_customer(customer_id) ON DELETE CASCADE
);
")

dbExecute(con, "
CREATE TABLE fact_persistence_history (
    customer_id INTEGER NOT NULL,
    date DATE NOT NULL,
    persistence_score NUMERIC(10, 4),
    coefficient_of_variation NUMERIC(10, 4),
    interpretation TEXT,
    daily_perc_change TEXT,
    PRIMARY KEY (customer_id, date),
    FOREIGN KEY (customer_id) REFERENCES dim_customer(customer_id) ON DELETE CASCADE
);
")

dbExecute(con, "
CREATE TABLE fact_airr_history (
    customer_id INTEGER NOT NULL,
    date DATE NOT NULL,
    airr_score NUMERIC(10, 4),
    PRIMARY KEY (customer_id, date),
    FOREIGN KEY (customer_id) REFERENCES dim_customer(customer_id) ON DELETE CASCADE
);
")

dbExecute(con, "
          CREATE TABLE customer_log (
    customer_id INTEGER NOT NULL,
    customer_name VARCHAR(50) UNIQUE NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL,
    password_hash VARCHAR(256) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
          ")


dbExecute(con, "
CREATE TABLE dim_query (
    query_id SERIAL PRIMARY KEY,
    query_string VARCHAR(256) NOT NULL
);
")

dbExecute(con, "
CREATE TABLE dim_cust_query (
    customer_id INTEGER NOT NULL,
    query_id INTEGER NOT NULL,
    date_added DATE NOT NULL,
    PRIMARY KEY (customer_id, date_added, query_id),
    FOREIGN KEY (customer_id) REFERENCES dim_customer(customer_id) ON DELETE CASCADE,
    FOREIGN KEY (query_id) REFERENCES dim_query(query_id) ON DELETE CASCADE
);
")

dbExecute(con,"
          ALTER TABLE dim_cust_query 
ADD CONSTRAINT dim_cust_query_unique 
UNIQUE (customer_id, query_id);
          ")


dbExecute(con, "
CREATE TABLE fact_query_history (
    customer_id INTEGER NOT NULL,
    query_id INTEGER NOT NULL,
    date DATE NOT NULL,
    presence_score NUMERIC(10, 4),
    perception_score NUMERIC(10, 4),
    prestige_score NUMERIC(10, 4),
    persistence_score NUMERIC(10, 4),
    airr_score NUMERIC(10, 4),
    PRIMARY KEY (customer_id, date, query_id),
    FOREIGN KEY (customer_id) REFERENCES dim_customer(customer_id) ON DELETE CASCADE,
    FOREIGN KEY (query_id) REFERENCES dim_query(query_id) ON DELETE CASCADE
);
")


dbExecute(con, "
CREATE TABLE fact_comp_history (
    customer_id INTEGER NOT NULL,
    date DATE NOT NULL,
    comp_brand VARCHAR(255) NOT NULL,
    presence_score NUMERIC(10, 4),
    perception_score NUMERIC(10, 4),
    prestige_score NUMERIC(10, 4),
    persistence_score NUMERIC(10, 4),
    airr_score NUMERIC(10, 4),
    PRIMARY KEY (customer_id, date, comp_brand),
    FOREIGN KEY (customer_id) REFERENCES dim_customer(customer_id) ON DELETE CASCADE
);
")

dbExecute(con, "
CREATE TABLE fact_query_comp_history (
    customer_id INTEGER NOT NULL,
    query_id INTEGER NOT NULL,
    date DATE NOT NULL,
    comp_brand VARCHAR(255) NOT NULL,
    presence_score NUMERIC(10, 4),
    perception_score NUMERIC(10, 4),
    prestige_score NUMERIC(10, 4),
    persistence_score NUMERIC(10, 4),
    airr_score NUMERIC(10, 4),
    PRIMARY KEY (customer_id, date, query_id, comp_brand),
    FOREIGN KEY (customer_id) REFERENCES dim_customer(customer_id) ON DELETE CASCADE
);
")

dbExecute(con, "
CREATE TABLE dim_subscription (
    subscription_level_id INTEGER NOT NULL,
    subscription_name VARCHAR(255) NOT NULL,
    num_prompts_included INTEGER NOT NULL,
    num_competitors_included INTEGER NOT NULL,
    PRIMARY KEY (subscription_level_id)
);
")

dbExecute(con, "
CREATE TABLE fact_customer_level (
    customer_id INTEGER NOT NULL,
    subscription_level_id INTEGER NOT NULL,
    extra_prompts_added INTEGER NOT NULL,
    date_valid_from DATE NOT NULL,
    date_valid_to DATE NOT NULL,
    PRIMARY KEY (customer_id, subscription_level_id, extra_prompts_added, date_valid_from),
    FOREIGN KEY (customer_id) REFERENCES dim_customer(customer_id) ON DELETE CASCADE
);
")


dbExecute(con, "
  INSERT INTO dim_subscription (subscription_level_id, subscription_name, num_prompts_included, num_competitors_included)
  VALUES 
    (1, 'Free', 1, 1),
    (2, 'Pro', 5, 3),
    (3, 'Enterprise', 20, 10)
  ON CONFLICT (subscription_level_id) DO NOTHING;
")




# Create indexes
dbExecute(con, "CREATE INDEX idx_presence_customer_date ON fact_presence_history(customer_id, date);")
dbExecute(con, "CREATE INDEX idx_perception_customer_date ON fact_perception_history(customer_id, date);")
dbExecute(con, "CREATE INDEX idx_prestige_customer_date ON fact_prestige_history(customer_id, date);")
dbExecute(con, "CREATE INDEX idx_persistence_customer_date ON fact_persistence_history(customer_id, date);")
dbExecute(con, "CREATE INDEX idx_airr_customer_date ON fact_airr_history(customer_id, date);")
dbExecute(con, "CREATE INDEX idx_presence_date ON fact_presence_history(date);")
dbExecute(con, "CREATE INDEX idx_perception_date ON fact_perception_history(date);")
dbExecute(con, "CREATE INDEX idx_prestige_date ON fact_prestige_history(date);")
dbExecute(con, "CREATE INDEX idx_persistence_date ON fact_persistence_history(date);")
dbExecute(con, "CREATE INDEX idx_airr_date ON fact_airr_history(date);")
dbExecute(con, "CREATE INDEX idx_query_query_id ON dim_query(query_id);")
dbExecute(con, "CREATE INDEX idx_cust_query_query_id ON dim_cust_query(query_id);")
dbExecute(con, "CREATE INDEX idx_query_history_query_id ON fact_query_history(query_id);")
dbExecute(con, "CREATE INDEX idx_query_history_customer ON fact_query_history(customer_id);")
dbExecute(con, "CREATE INDEX idx_comp_history_customer ON fact_comp_history(customer_id);")
dbExecute(con, "CREATE INDEX idx_query_comp_history_customer ON fact_query_comp_history(customer_id);")


# Verify tables were created
dbListTables(con)

# Check table structure
dbGetQuery(con, "
  SELECT column_name, data_type, is_nullable
  FROM information_schema.columns
  WHERE table_name = 'dim_customer'
  ORDER BY ordinal_position;
")

dbExecute(con, "
  INSERT INTO dim_customer (customer_id, customer_name) 
  VALUES (1, 'Nike'), (2, 'Under Armour');
")

add_customer <- function(con, customer_name) {
  query <- "
    INSERT INTO dim_customer (customer_name)
    VALUES ($1)
    RETURNING customer_id;
  "
  result <- dbGetQuery(con, query, params = list(customer_name))
  return(result$customer_id)
}

# add_customer(con, 'Fila')
# 
# add_customer(con, 'Adidas')
# ttt <- dbGetQuery(con, 'select * from dim_customer')
# hash_password("Adidas")
# 
# cname <- "Umbro"
# dbExecute(con,paste0("update dim_customer
#                       set password_hash = '",hash_password(cname),
#                      "' where customer_name = '", cname,
#                      "';"))

# dbExecute(con, "
# delete from dim_query
# where query_id < 83;
# ")


# cust_list <- c('Wix', 'Squarespace', 'Webflow', 'Wordpress',
#                'Uber Eats', 'Instacart', 'Doordash', 'Deliveroo',
#                'Ro', 'Hims & Hers', 'Keeps', 'Lemonaid Health',
#                'Bud Light', 'Michelob Ultra', 'Coors Light', 'Miller Light',
#                'Rocket', 'Redfin', 'Zillow', 'Opendoor')
# 
# cust_list <- c('Shopify',
#                'Amazon Fresh', 'Walmart Delivery',
#                'Roman', 'One Medical','Teladoc Health',
#                'Corona Extra', 'Miller Lite',
#                'Zillow','Realtor.com','SoFi')
# 
# for (customer_name in cust_list){
#   customer_id <- create_user(customer_name, paste0(substring(customer_name,1,2),"@",substring(customer_name,1,3),".com"), customer_name)
#   user_create_airr(customer_name)
# }
# 
# 

# dbExecute(con,paste0("delete from fact_airr_history
#                       where date = '2026-02-12';"))
# dbExecute(con,paste0("delete from fact_presence_history
#                       where date = '2026-02-12';"))
# dbExecute(con,paste0("delete from fact_perception_history
#                       where date = '2026-02-12';"))
# dbExecute(con,paste0("delete from fact_prestige_history
#                       where date = '2026-02-12';"))
# dbExecute(con,paste0("delete from fact_persistence_history
#                       where date = '2026-02-12';"))


dbExecute(pool,paste0("update fact_presence_history
                      set date = '2026-02-17' where date = '2026-02-18';"))
dbExecute(pool,paste0("update fact_perception_history
                      set date = '2026-02-17' where date = '2026-02-18';"))
dbExecute(pool,paste0("update fact_prestige_history
                      set date = '2026-02-17' where date = '2026-02-18';"))
dbExecute(pool,paste0("update fact_persistence_history
                      set date = '2026-02-17' where date = '2026-02-18';"))
dbExecute(pool,paste0("update fact_airr_history
                      set date = '2026-02-17' where date = '2026-02-18';"))
dbExecute(pool,paste0("update fact_query_history
                      set date = '2026-02-17' where date = '2026-02-18';"))
# 
# 
# dim_customer <- dbGetQuery(con,'select * from dim_customer')
# 
# # 
# 
# fact_airr_history <- dbGetQuery(pool,'select * from fact_airr_history')
# fact_presence_history <- dbGetQuery(pool,'select * from fact_presence_history')
# fact_query_history <- dbGetQuery(pool,'select * from fact_query_history')
# dim_user <- dbGetQuery(pool,'select * from dim_user')
# dim_brand <- dbGetQuery(pool,'select * from dim_brand')
# dim_subscription <- dbGetQuery(pool,'select * from dim_subscription')
# fact_user_sub_level <- dbGetQuery(pool,'select * from fact_user_sub_level')
# fact_user_queries_tracked <- dbGetQuery(pool,'select * from fact_user_queries_tracked')


