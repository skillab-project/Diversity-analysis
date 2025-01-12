# plumber_archetypal.R
# Load necessary libraries
library(plumber)
library(readxl)
library(DT)
library(plotly)
library(fmsb)
library(ggradar)
library(tidyverse)
library(ggplot2)
library(archetypes)
library(Anthropometry)
library(formatR)
library(jsonlite)

# Include all the required functions and definitions
source("./archetypal.R")  # Replace with the path to your script or directly include the necessary functions
source('./api_datacollection.R')


#* @apiTitle Archetypal Microservice
#* @apiDescription API for archetypal calculations in SKILLAB.

#* Calculate Archetypes
#* @param pillar_selection:string Pillar selection (e.g., 'S').
#* @param skill_layer:int Skill layer (e.g., 1).
#* @param occupation_id:string Occupation ID selection (e.g., "http://data.europa.eu/esco/isco/C01").
#* @param num_archetypes:int Number of archetypes (e.g., 3).
#* @post /archetypal
function(pillar_selection, skill_layer, occupation_id, num_archetypes) {
  # Ensure arguments are in the correct format
  pillar_selection <- as.character(pillar_selection)
  
  skill_layer <- as.numeric(skill_layer)
  occup_id<-occupation_id
  #occup_id <-"http://data.europa.eu/esco/isco/C01"
  
  num_archetypes <- as.numeric(num_archetypes)
  
  
  # Call the main archetypal function
  tryCatch({
    
    matrix <-get_jobs(occup_level='',pillar=pillar_selection,level=skill_layer,occup_id=occup_id)
    
    res <- archetypal(matrix,num_archetypes)
    
    archetypal_table = res[[1]]      # Archetypal table
    
  }, error = function(e) {
    
      message = e$message

  })
}

# Run the API using plumber
# To run, save this file and run: plumber::plumb("plumber_archetypal.R")$run(port = 8001)




#* Calculate Archetypes and Return Plot
#* @param pillar_selection:string Pillar selection (e.g., 'S').
#* @param skill_layer:int Skill layer (e.g., 1).
#* @param occupation_id:string Occupation ID selection (e.g., "http://data.europa.eu/esco/isco/C01").
#* @param num_archetypes:int Number of archetypes (e.g., 3).
#* @get /archetypal_plot
#* @serializer contentType list(type="image/png")
function(pillar_selection, skill_layer, occupation_id, num_archetypes) {
  # Ensure arguments are in the correct format
  pillar_selection <- as.character(pillar_selection)
  
  skill_layer <- as.numeric(skill_layer)
  occup_id <- occupation_id
  #occup_id <-"http://data.europa.eu/esco/isco/C01"
  
  num_archetypes <- as.numeric(num_archetypes)
  
  # Call the main archetypal function
  tryCatch({
    
    matrix <-get_jobs(occup_level='',pillar=pillar_selection,level=skill_layer,occup_id=occup_id)
    
    res <- archetypal(matrix,num_archetypes)
    
    archetypes_obj <- res[[2]]  # Archetypes object for plotting
    
    # Generate the plot and return it as PNG
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 800, height = 800)  # Set the plot dimensions
    simplexplot(archetypes_obj)         # Generate the simplex plot
    dev.off()                           # Close the graphics device
    # Read the image file and return it
    readBin(tmp, "raw", file.info(tmp)$size)
    
    
    
  }, error = function(e) {
    stop("Error in generating archetypal plot: ", e$message)
  })
}