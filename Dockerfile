# Dockerfile
FROM rocker/r-ver:4.4.0

# System dependencies for R packages
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    autossh \
    openssh-client \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c(
  'DBI', 'RPostgres', 'httr2', 'jsonlite', 
  'future', 'promises', 'dplyr', 'stringr',
  'lubridate', 'logger', 'aws.s3',
  'R6', 'parallel', 'plotly', 'purrr', 'tibble'
), repos='https://cran.rstudio.com/')"

RUN R -e "install.packages(c('DBI','RPostgres','httr2','jsonlite','future','promises','dplyr','stringr','lubridate','logger', 'R6','parallel','purrr','tibble','magrittr'), repos='https://cran.rstudio.com/')"

# Copy scoring code
WORKDIR /app
COPY global_scripts/ ./global_scripts/
COPY worker_entry.R ./worker_entry.R
COPY setup_tunnel.sh ./setup_tunnel.sh

# SSH key will be injected at runtime via AWS Secrets Manager
RUN chmod +x setup_tunnel.sh

ENTRYPOINT ["Rscript", "worker_entry.R"]