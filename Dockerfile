FROM rocker/r-ver:4.3.1

# Set CRAN mirror for R package installations
ENV CRAN_MIRROR https://cran.rstudio.com

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libudunits2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libglu1-mesa-dev \
    libx11-dev \
    libxt-dev \
    git \
    build-essential \
    libsodium-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    pkg-config \
    cron \ # <--- Added cron here (good)
    && rm -rf /var/lib/apt/lists/*

# Install R packages.
RUN R -e "install.packages(c('remotes', 'plumber', 'readxl', 'SpadeR', 'DT', 'plotly', 'fmsb', 'tidyverse', 'ggplot2', 'archetypes', 'Anthropometry', 'shinyBS', 'formatR', 'jsonlite', 'httr', 'vegan', 'dplyr', 'indicspecies', 'shiny', 'dotenv'), repos='${CRAN_MIRROR}', Ncpus = parallel::detectCores() -1)" && \
    R -e "remotes::install_github('jeffkimbrel/jakR')"

# Verify plumber installation
RUN R -e "if (!requireNamespace('plumber', quietly = TRUE)) { stop('plumber not installed') }"

# Copy code to the Docker container
WORKDIR /app
COPY . /app

# Script for Cron job, to run ./Extras/Main_script_for_results.R
RUN chmod +x ./starting_script.sh # <--- Correct

# Copy the crontab file and add it to cron
COPY crontab_job /etc/cron.d/crontab_job
RUN chmod 0644 /etc/cron.d/crontab_job

# Expose API port
EXPOSE 8870

## Custom entrypoint script to start Cron script and Service plumber API
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]