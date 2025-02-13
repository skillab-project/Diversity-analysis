# Use the official R base image
FROM rocker/rstudio:latest

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libudunits2-dev \
    libfontconfig1-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libglu1-mesa-dev \
    libx11-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('remotes', repos='https://cran.rstudio.com')"
RUN R -e "remotes::install_github('ricardo-bion/ggradar')"

RUN R -e "install.packages('plumber')"
RUN R -e "install.packages('ggradar')"
RUN R -e "install.packages('httr')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('readxl')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('archetypes')"
RUN R -e "install.packages('Anthropometry')"
RUN R -e "install.packages('SpadeR')"
RUN R -e "install.packages('fmsb')"
RUN R -e "install.packages('jsonlite')"
RUN R -e "install.packages('formatR')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('shinyBS')"

RUN R -e "if (!requireNamespace('plumber', quietly = TRUE)) { stop('plumber not installed') }"

# Copy your application code to the Docker container
WORKDIR /app
COPY . /app

# Expose the API ports
EXPOSE 8870

# Default command to run the APIs on ports 8870
CMD ["Rscript", "-e", "plumber::plumb('/app/plumber_diversity_new.R')$run(host='0.0.0.0', port=8870)"]