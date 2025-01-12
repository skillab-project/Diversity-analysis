# plumber.R
# Load necessary libraries
library(plumber)
library(readxl)
library(SpadeR)
library(DT)
library(plotly)
library(fmsb)
library(ggradar)
library(tidyverse)
library(ggplot2)
library(archetypes)
library(Anthropometry)
library(shinyBS)
library(formatR)
library(jsonlite)





source('./api_datacollection.R')
# Include all the required functions and definitions
source("./Diversity.R")  

#* @apiTitle Diversity Microservice
#* @apiDescription API for calculating biodiversity functionality

#* Calculate Diversity Indices
#* @param occup_level:int Occupation level (1 to 4).
#* @param skill_layer:int Skill layer (e.g., 2).
#* @param pillar_selection:string Pillar selection (e.g., 'S').
#* @post /Diversity
function(occup_level, skill_layer, pillar_selection) {
  
  # Ensure arguments are in the correct format
  occup_level <- as.character(occup_level)
  skill_layer <- as.character(skill_layer)
  pillar_selection <- as.character(pillar_selection)
  
  # Call the main diversity function
  tryCatch({
    
    matrix <- get_jobs(occup_level=occup_level,pillar=pillar_selection,level = skill_layer)
    
    res <- diversity(matrix)
    
    data = res
    
  }, error = function(e) {
      message = e$message
    
  })
}