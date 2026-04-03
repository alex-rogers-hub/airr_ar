
# Find Living Group's brand_id
brand <- dbGetQuery(pool, "
  SELECT brand_id, brand_name FROM dim_brand
  WHERE lower(brand_name) LIKE '%living%'
")
print(brand)

# Add the alias
add_brand_alias(brand$brand_id[1], "Living Ratings")

# Verify
get_brand_aliases(brand$brand_id[1])
