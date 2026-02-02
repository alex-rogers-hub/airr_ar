

# Execute the CREATE TABLE statements
dbExecute(con, "
CREATE TABLE dim_customer (
    customer_id SERIAL PRIMARY KEY,
    customer_name VARCHAR(255) NOT NULL
    date_added DATE NOT NULL
);
")
# dbExecute(con, "
#   alter table dim_customer ADD COLUMN date_added DATE DEFAULT CURRENT_DATE;
# ")


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


