# Use the official R base image. Using a specific version is generally better for reproducibility.
FROM rocker/r-ver:4.3.1

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
    # NEW: Added libsodium-dev for the 'sodium' R package
    libsodium-dev \
    # NEW: Added libharfbuzz-dev and libfribidi-dev for 'textshaping' R package
    libharfbuzz-dev \
    libfribidi-dev \
    # NEW: Potentially needed for graphics and some ragg dependencies
    pkg-config \
    # Consider adding these for full graphics capability if needed, though for headless API, not strictly essential for build
    # libglpk-dev \
    # libgmp-dev \
    # libxft-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages.
# Combine all R package installations into a single RUN command to optimize Docker layers.
# Use 'remotes::install_github' for packages not on CRAN.
# NOTE: ggradar is now installed from CRAN.
# For jakR, 'remotes::install_github' is the correct approach.
# Use Ncpus argument to speed up installation.
RUN R -e "install.packages(c('remotes', 'plumber', 'readxl', 'SpadeR', 'DT', 'plotly', 'fmsb', 'tidyverse', 'ggplot2', 'archetypes', 'Anthropometry', 'shinyBS', 'formatR', 'jsonlite', 'httr', 'vegan', 'dplyr', 'indicspecies', 'shiny', 'dotenv', 'ggradar'), repos='${CRAN_MIRROR}', Ncpus = parallel::detectCores() -1)" && \
    R -e "remotes::install_github('eubatool/jakR')"


# Verify plumber installation (good practice)
RUN R -e "if (!requireNamespace('plumber', quietly = TRUE)) { stop('plumber not installed') }"

# Copy your application code to the Docker container
WORKDIR /app
COPY . /app

# Expose the API ports
EXPOSE 8870

# Default command to run the APIs on port 8870
CMD ["Rscript", "-e", "plumber::plumb('/app/plumber_diversity_script.R')$run(host='0.0.0.0', port=8870)"]