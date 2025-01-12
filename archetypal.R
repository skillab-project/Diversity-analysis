######################## SKILLAB Biodiversity and Archetypal functionalities

library(readxl)
library(SpadeR)
library(DT)
library(shiny)
library(plotly)
library(fmsb)
library(ggradar)
library(tidyverse)
library(ggplot2)
library(archetypes)
library(Anthropometry)
library(shinyBS)
library(DT)
library(formatR)
library(jsonlite)




########################## PREVIOUS #########################
# ### Functions for Diversity indices calculation
# #dataset reading --- Another function - API CALL
# skill_dataset <-function(pillar,layer,path_skills,demand){
#   if(demand==TRUE){
#     
#     if(pillar=='K'){
#       name<-'jobs_matrix_knowledge_layer_'
#     }else if(pillar=='L'){
#       name<-'jobs_matrix_language_layer_'
#     }else if(pillar=='S'){
#       name<-'jobs_matrix_skills_layer_'
#     }else if(pillar=='T'){
#       name<-'jobs_matrix_transversal_layer_'
#     }
#     path<-paste0(path_skills,'\\',pillar,'\\',name,layer,'_new','.xlsx')
#     dataset<-read_excel(path)
#     dataset$ID <-dataset$Job_ID
#     dataset<-subset(dataset,select=-c(Job_ID))
#   }
#   else{
#     if(pillar=='K'){
#       name<-'matrix_knowledge_layer_'
#     }else if(pillar=='L'){
#       name<-'matrix_language_layer_'
#     }else if(pillar=='S'){
#       name<-'matrix_skills_layer_'
#     }else if(pillar=='T'){
#       name<-'matrix_transversal_layer_'
#     }
#     path<-paste0(path_skills,'\\',pillar,'\\',name,layer,'_new','.xlsx')
#     dataset<-read_excel(path)
#     
#     dataset$ID <-dataset$profile_id
#     dataset<-subset(dataset,select=-c(profile_id))
#     
#     
#   }
#   return(dataset)
# }
# #dataset transformation
# skill_transformation<-function(skills){
#   cols <-colnames(skills)[1:length(colnames(skills))]
#   counts <-c()
#   for (k in cols){
#     counts<-c(counts,sum(skills[k]))
#   }
#   skill_matrix <-data.frame(counts,row.names=cols)
#   return(skill_matrix)
# }
# 
# filter_occupations_fnc <- function(data_occupation,label_id){
#   data <- data_occupation[data_occupation$iscoGroup_1 %in% as.character(label_id),]
#   return(data)
# }
# 
# skill_taxonomy_fnc <- function(pillar,layer,branch){
#   skills_taxonomy <-read_excel(paste0('C:\\Users\\30697\\Desktop\\SKILLAB\\Code\\Archetypoids_analysis\\ESCO_Taxonomy\\concept_paths_',pillar,'.xlsx'))
#   skills <-skills_taxonomy[skills_taxonomy$layer == layer,]
#   
#   if (pillar=='S'){
#     inds<-c()
#     for(k in 1:length(skills$code)){
#       if(as.numeric(strsplit(skills$code[k],split="*")[[1]][4]) == branch){
#         inds<-c(inds,k)
#       }
#     }
#   }
#   else if(pillar=='T'){
#     inds<-c()
#     for(k in 1:length(skills$code)){
#       if(as.numeric(strsplit(skills$code[k],split="*")[[1]][4]) == branch){
#         inds<-c(inds,k)
#       }
#     }
#   }
#   else if(pillar=='K'){
#     inds<-c()
#     for(k in 1:length(skills$code)){
#       if(as.numeric(strsplit(skills$pathString_pos[k],split="/")[[1]][2]) == branch){
#         inds<-c(inds,k)
#       }
#     }
#   }
#   else{
#     inds<-c()
#     for(k in 1:length(skills$conceptPL)){
#       if(as.numeric(strsplit(skills$pathString_pos[k],split="/")[[1]][2])==branch){
#         inds<-c(inds,k)
#       }
#     }
#   }
#   
#   names <- skills$conceptPL[inds]
#   names<-names[complete.cases(names)]
#   
#   return(names)
# }


####################################### ARCHETYPES ###################################################
archetypes_main <-function(data,num_archetypes){
  
  skill_current<- data 
  
  archetypes_res <-archetypal_calculation(skill_current,num_archetypes)
  
  archetypes_obj <-archetypes_res[[2]]
  
  archetypes_tab <-data.frame(archetypes_res[[1]])
  
  colnames(archetypes_tab)<-colnames(archetypes_res[[1]])
  
  json_data1 <-archetypes_tab
  
  ################################## Simplex plot --- need save as an HTML or a PNG directly? #############################
  #Plot command 
  #simplexplot(archetypes_obj)
  
  #1. Save as PNG method 
  ##png("simplex_plot.png", width = 800, height = 800)
  #simplexplot(archetypes_obj)  # Generate the plot
  #dev.off()           
  
  #2. Save as PNG and then as HTML method 
  #png("simplex_plot.png", width = 800, height = 800)
  #simplexplot(archetypes_obj)
  #dev.off()
  # Create an HTML file embedding the PNG
  #html_code <- '
   #   <html>
    #  <head><title>Simplex Plot</title></head>
     # <body>  
      #<h1>Simplex Plot</h1>
      #<img src="simplex_plot.png" alt="Simplex Plot">
      #</body>
      #</html>
    #'
  
  #writeLines(html_code, "simplex_plot.html")
  
  ################################ MATCHING MECHANISM -- LATER ####################################################################
  
  
  return(list(json_data1,archetypes_obj))
  
}

archetypal_calculation <-function(data,num_archetypes){
  zero_variance <- apply(data, 2, var) == 0
  data<-data[,!zero_variance]
  data <- apply(data, 2, as.numeric)  # Ensure all columns are numeric
  
  as <- stepArchetypes (as.matrix(data),k=num_archetypes,nrep=10,verbose=FALSE)
  abest<-bestModel(as)
  table_archetypes <- t(apply(abest$archetypes,1,round))
  
  names_archetypes<-c()
  for(k in 1:num_archetypes){
    name <-paste('Archetype',k)
    names_archetypes<-c(names_archetypes,name)
  }
  
  rownames(table_archetypes)<-names_archetypes
  return(list(table_archetypes,abest))
}

# archetypal_table_preparation <-function(data_skills,data_occupation,occupation_selection,occupation_group_isco){
#   common_ids <- intersect(data_occupation$id_1,data_skills$ID)
#   data_occupation <-data_occupation[data_occupation$id_1 %in% common_ids,]
#   data_occupation<-data_occupation[!duplicated(data_occupation$id_1),]
#   data_occupation<-data_occupation[order(data_occupation$id_1),]
#   if(occupation_group_isco==1){
#     occupations <- data_occupation$preferredLabel_1
#   }else if(occupation_group_isco==2){
#     occupations <- data_occupation$preferredLabel_2
#   }else if(occupation_group_isco==3){
#     occupations <- data_occupation$preferredLabel_3
#   }else{
#     occupations <- data_occupation$preferredLabel_4
#   }
#   data_skills <- data_skills[data_skills$ID %in% common_ids,]
#   data_skills<-data_skills[!duplicated(data_skills$ID),]
#   data_skills<-data_skills[order(data_skills$ID),]
#   data_skills <-subset(data_skills,select=-c(ID))
#   
#   skill_current <- data_skills[occupations == occupation_selection,]
#   
#   return(skill_current)
# }


archetypal <-function(matrix,num_archetypes){
  ### input matrix
  data_skills<-matrix[,2:ncol(matrix)]
  # Matrix for archetypal calcualtion, Matrix_second for matching mechanism...
  res <- archetypes_main(data_skills,num_archetypes)
  ## res[[1]] -- wanted table
  ## simplexplot(res[[2]]) -- wanted graph
   return(res)
}







############################ Descriptions --- we will see if it will be implemented ####################################
# descriptions <- read_excel('C:/Users/30697/Desktop/SKILLAB/Code/Skills_dataset/main_data.xlsx')
# 
# descr_wanted <-descriptions[descriptions$id %in% common_ids,]
# descr_want<-descr_wanted[!duplicated(descr_wanted$id),]
# descr<-descr_want$description[occupations==un]
# #abest is the archetypes model
# which <- apply(coef(abest, "alphas"), 2, which.max)
# final_descriptions <- descr[which]
 
################################ Matching mechanism ----------- we will see how we will implement it (NOT NOW)################################
## Same procedure before calculating archetypes --- supply or demand
# cols<-colnames(archetypes_tab)
# data_occupation2 <-read_excel(path_data_occupations_supply) ##### Provide path for the exce- occupations
# data_skills2 <- skill_dataset(pillar_selection,skill_layer,path_skills_supply_Lnk,FALSE) ### Provide  Pillar, Skill-layer, path and TRUE/FALSE because of different file names
# 
# 
# common_ids <- intersect(data_occupation2$id_1,data_skills2$ID)
# data_occupation2 <-data_occupation2[data_occupation2$id_1 %in% common_ids,]
# data_occupation2<-data_occupation2[!duplicated(data_occupation2$id_1),]
# data_occupation2<-data_occupation2[order(data_occupation2$id_1),]
# 
# data_skills2 <- data_skills2[data_skills2$ID %in% common_ids,]
# data_skills2<-data_skills2[!duplicated(data_skills2$ID),]
# data_skills2<-data_skills2[order(data_skills2$ID),]
# 
# ids<-subset(data_skills2,select=c(ID))
# 
# 
# if(occupation_group_isco==1){
#   occupations <- data_occupation2$preferredLabel_1
# }else if(occupation_group_isco==2){
#   occupations <- data_occupation2$preferredLabel_2
# }else if(occupation_group_isco==3){
#   occupations <- data_occupation2$preferredLabel_3
# }else{
#   occupations <- data_occupation2$preferredLabel_4
# }
# 
# ID <- ids$ID[occupations==occupation_selection]
# skills_current<-data_skills2[,colnames(data_skills2) %in% cols]
# skills<-skills_current[occupations==occupation_selection,]
# #### The result will be the calculation of archetypal coefficients for the supply / demand (second dataset of the first archetypes)
# res <-predict(archetypes_obj,skills)

