############# Function for diversity results ###################
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
library(ggplot2)


load('./Extras/New_occupation_table.Rda')
source('./Extras/Diversity_analysis_KUs.R')
source('./Extras/Required_skill_matching.R')
source('./Extras/Diversity_analysis_organization_KUs.R')


#* @apiTitle Diversity Microservice
#* @apiDescription API for diversity calculators

#* @param occupation_name:string Example: Software developers
#* @post /diversity
function(occupation_name) {

  # Call the main diversity function
  tryCatch({
    occupational_code<-New_occupation_table$Codes[match(occupation_name,New_occupation_table$Label4)]
    load(paste0('./data/',occupational_code,'/Diversity_results.Rda'))
    
    res_alpha<-results_list$Alpha_results
    res_beta<-results_list$Beta_results
    res_isa<-results_list$ISA_all_groups
    res_pcoa<-results_list$PCoA_results
    
    
    data = list(res_alpha,res_beta,res_pcoa,res_isa)
    
  }, error = function(e) {
    message = e$message
    
  })
}

#* @apiTitle Diversity-Kus Microservice
#* @apiDescription API for diversity calculators for KUs Analysis


#* @param start_date:string (Optional) Start of timestamp filter, in year-month-day format (e.g. "2025-06")
#* @param end_date:string (Optional) End of timestamp filter, in year-month-day format(e.g. "2025-07")
#* @post /diversity_kus
function(start_date=NULL,end_date=NULL) {
  
  
 
  # Call the main diversity function
  tryCatch({
    res_alpha<-alpha_kus_results(start_date,end_date)
    
    data = list(res_alpha)
    
  }, error = function(e) {
    message = e$message
    
  })
}


#* @apiTitle Diversity-Kus-Organization Microservice
#* @apiDescription API for diversity calculators for KUs Analysis


#* @param start_date:string (Optional) Start of timestamp filter, in year-month-day format (e.g. "2025-06")
#* @param end_date:string (Optional) End of timestamp filter, in year-month-day format(e.g. "2025-07")
#* @post /diversity_kus_organization
function(start_date=NULL,end_date=NULL) {
  
  
  
  # Call the main diversity function
  tryCatch({
    res<-kus_results(start_date,end_date)
    
    data = list(res)
    
  }, error = function(e) {
    message = e$message
    
  })
}



#* @apiTitle Required Skills Microservice
#* @apiDescription API for required skill for occupation functionality

#* Find required skills for occupation requested
#* @param occupation_name:string Occupation name selection (e.g."Applications programmers","Software developers","Systems analysts","Web and multimedia developers").
#* @post /required_skills_service
function(occupation_name) {
  
  # Call the main diversity function
  tryCatch({
    
    occupational_code<-New_occupation_table$Codes[match(occupation_name,New_occupation_table$Label4)]
    load(paste0('./data/',occupational_code,'/Diversity_results.Rda'))
    
    res<-results_list$ISA_seperate
    results<-res[!is.na(match(res$Group,occupation_name)),]
    results<-results[order(results$stat,decreasing = TRUE),]
    colnames(results)<-c('Role','Skill','Pillar','Value','SkillId')
    data = results
    
  }, error = function(e) {
    message = e$message
    
  })
}




#* @apiTitle Skill matching Microservice
#* @apiDescription API for required skill for occupation functionality

#* Find required skills for occupation requested
#* @param skill_list:list Set of skills to search for related occupations.
#* @param matching_number:integer Number of matches to return.
#* @post /required_skill_recommender
function(skill_list,matching_number=1) {
  # Call the main diversity function
  tryCatch({
    
    
    data = required_skill_map(skill_list,matching_number,New_occupation_table)
    
  }, error = function(e) {
    message = e$message
    
  })
}
