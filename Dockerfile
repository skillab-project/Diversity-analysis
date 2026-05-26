# Use the official R base image. Using a specific version is generally better for reproducibility.
FROM rocker/tidyverse:4.3.1

# Set CRAN mirror for R package installations
ENV CRAN_MIRROR https://cran.rstudio.com

# Install system dependencies for R packages
# Group related apt-get commands to reduce image layers
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
    # Potentially needed for graphics and some ragg dependencies (uncomment if issues persist)
    # libglpk-dev \
    # libgmp-dev \
    # libxft-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages.
# Combine all R package installations into a single RUN command to optimize Docker layers.
# Using 'remotes::install_github' for jakR with the corrected repository.
# Use Ncpus argument to speed up installation.
RUN R -e "install.packages(c('remotes', 'plumber', 'readxl', 'SpadeR', 'DT', 'plotly', 'fmsb', 'tidyverse', 'ggplot2', 'archetypes', 'Anthropometry', 'shinyBS', 'formatR', 'jsonlite', 'httr', 'vegan', 'dplyr', 'indicspecies', 'shiny', 'dotenv'), repos='${CRAN_MIRROR}', Ncpus = parallel::detectCores() -1)" && \
    R -e "remotes::install_github('jeffkimbrel/jakR')"


# Verify plumber installation (good practice)
RUN R -e "if (!requireNamespace('plumber', quietly = TRUE)) { stop('plumber not installed') }"

# Copy your application code to the Docker container
WORKDIR /app
COPY . /app
COPY ./data /app/data

# Expose the API ports
EXPOSE 8870

# Default command to run the APIs on port 8870
CMD ["Rscript", "-e", "plumber::plumb('/app/plumber_diversity_script.R')$run(host='0.0.0.0', port=8870)"]