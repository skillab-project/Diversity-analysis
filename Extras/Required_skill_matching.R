#### Packages ######
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



required_skill_map<-function(skills_set,matching_number,New_occupation_table){
occupations_all<-unique(New_occupation_table$Label4)

occupation_mapping<-list()
potential_role<-c()
mapping_coefficient<-c()
for (i in seq_along(occupations_all)) {
  occupation_name  <- occupations_all[i]
  occupational_code <- New_occupation_table$Codes[
    match(occupation_name, New_occupation_table$Label4)
  ]
  path <- file.path("data", occupational_code, "Diversity_results.Rda")
  
  if (inherits(try(load(path), silent = TRUE), "try-error")) next
  
  
  res<-results_list$ISA_seperate
  results<-res[!is.na(match(res$Group,occupation_name)),]
  results<-results[order(results$stat,decreasing = TRUE),]
  colnames(results)<-c('Role','Skill','Pillar','Value','SkillId')
  data = results
  
  
  
  cumulative_sum<-sum(data$Value)
  
  
  skills_in_occupation<-skills_set[skills_set %in% data$Skill]  
  
  if(sum(skills_set %in% data$Skill) >= matching_number){
    values_for_skills<-data$Value[data$Skill %in% skills_set]
    sum_values<-sum(values_for_skills)
    
    coeff<-sum_values/cumulative_sum
    
    potential_role<-c(potential_role,occupation_name)
    mapping_coefficient<-c(mapping_coefficient,coeff)
    
    occupation_mapping[[occupation_name]]<-skills_in_occupation
  }else{
    next
  }
  
  
  
  }

res<-data.frame(Occupation=potential_role,Matching=round(mapping_coefficient*100,3))
res<-res[order(res$Matching,decreasing = TRUE),]

occupation_mapping<-occupation_mapping[match(res$Occupation, names(occupation_mapping))]

return(list(Results=res,Skills=occupation_mapping))

}