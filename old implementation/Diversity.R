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




########################################## DIVERSITY ################################################
######## Data request and send back ##############
### Functions for Diversity indices calculation
calculated_diversity <- function(data,q1=c(0,1,2)){
  abudance <- data
  
  pi<-abudance$counts/sum(abudance$counts)
  abudance$pi <-pi
  ###Richness
  richness <- length(abudance$counts[abudance$counts>0])
  
  if (!is.finite(richness)){
    richness <- 0
  }
  ### Exponential Shannon entropy
  shannon <- exp(- sum(abudance$pi[abudance$counts>0]*log(abudance$pi[abudance$counts>0])))
  
  if (!is.finite(shannon)){
    shannon <- 0
  }
  
  ### Inverse Simpson
  simpson <- 1/sum(abudance$pi^2)
  
  if (!is.finite( simpson)){
    simpson <- 0
  }
  
  #diversity_profile <- Diversity(data,"abundance",q=q1)
  
  hill_numbers <- c(richness,round(shannon,3),round(simpson,3))#diversity_profile$Hill_numbers$Empirical
  
  evenness1 <- log(hill_numbers[2])/log(hill_numbers[1])
  
  
  if (!is.finite( evenness1)){
    evenness1 <- 0
  }
  
  
  evenness2 <- (hill_numbers[2]-1)/(hill_numbers[1]-1)
  
  if (!is.finite(evenness2)){
    evenness2  <- 0
  }
  
  evenness4 <- (hill_numbers[3]-1)/(hill_numbers[2]-1)
  
  
  if (!is.finite(evenness4)){
    evenness4 <- 0
  }
  
  N <- sum(data)
  margalef <- (hill_numbers[1]-1)/log(N)
  
  if (!is.finite(margalef)){
    margalef<- 0
  }
  
  
  menhinick <- (hill_numbers[1]-1)/sqrt(N)
  
  if (!is.finite(menhinick)){
    menhinick <- 0
  }
  
  indices <- c(hill_numbers,round(evenness1,3),round(evenness2,3),round(evenness4,3),round(margalef,3),round(menhinick,3))
  
  names <-c("Richness","Exponential Shannon Entropy","Inverse Simpson Index","Evenness Pielous J","Evenness N1-1/N0-1","Evenness N2-1/N1-1","Margalef Index","Menhinick Index")
  final <-data.frame(indices,row.names=names)
  
  return(final)
}
#dataset reading --- Another function - API CALL
skill_dataset <-function(pillar,layer,path_skills,demand){
  if(demand==TRUE){
    
    if(pillar=='K'){
      name<-'jobs_matrix_knowledge_layer_'
    }else if(pillar=='L'){
      name<-'jobs_matrix_language_layer_'
    }else if(pillar=='S'){
      name<-'jobs_matrix_skills_layer_'
    }else if(pillar=='T'){
      name<-'jobs_matrix_transversal_layer_'
    }
    path<-paste0(path_skills,'\\',pillar,'\\',name,layer,'_new','.xlsx')
    dataset<-read_excel(path)
    dataset$ID <-dataset$Job_ID
    dataset<-subset(dataset,select=-c(Job_ID))
  }
  else{
    if(pillar=='K'){
      name<-'matrix_knowledge_layer_'
    }else if(pillar=='L'){
      name<-'matrix_language_layer_'
    }else if(pillar=='S'){
      name<-'matrix_skills_layer_'
    }else if(pillar=='T'){
      name<-'matrix_transversal_layer_'
    }
    path<-paste0(path_skills,'\\',pillar,'\\',name,layer,'_new','.xlsx')
    dataset<-read_excel(path)
    
    dataset$ID <-dataset$profile_id
    dataset<-subset(dataset,select=-c(profile_id))
    
    
  }
  return(dataset)
}
#dataset transformation

skill_transformation<-function(skills){
  cols <-colnames(skills)[1:length(colnames(skills))]
  skills[] <- lapply(skills, as.numeric)
  
  counts <-c()
  for (k in cols){
    counts<-c(counts,sum(skills[k]))
  }
  skill_matrix <-data.frame(counts,row.names=cols)
  return(skill_matrix)
}

calculation_diversity_per_occupation <- function(matrix){
  ### NEED IDS here also modify the initial...
 
  unique_occupations <- unique(matrix$Occupation)
  unique_occupations<-unique_occupations[order(unique_occupations)]
  
  final_df <- data.frame()
  percentages_df <-data.frame()
  
  data_skills<-matrix[,2:ncol(matrix)]
  
  for (k in unique_occupations){
    
    skill_current <- data_skills[matrix$Occupation == k,]
    
    skill_matrix <-skill_transformation(skill_current)
    
    indices <- calculated_diversity(skill_matrix)
    
    if(ncol(final_df)==0){
      final_df <- indices
      final_df[[k]]<-final_df$indices
      final_df <- subset(final_df,select=-indices)
    }else{
      final_df[[k]]<-indices$indices
    }
    
    s_max <- ncol(data_skills)
    r1_max <-(s_max-1)/log(s_max)
    r2_max <-(s_max-1)/sqrt(s_max)
    
    inds_all <-indices[[1]]
    
    S<- inds_all[1]
    
    ind1<-round(S/s_max *100,3)
    
    H <-inds_all[2]
    
    ind2<-round(H/s_max*100,3)
    
    D <-inds_all[3]
    
    ind3<-round(D/s_max*100,3)
    
    Ej <-inds_all[4]
    
    ind4<-round(Ej*100,3)
    
    E2 <-inds_all[5]
    
    ind5<-round(E2*100,3)
    
    E4 <-inds_all[6]
    
    ind6 <- round(E4*100,3)
    
    R1 <-inds_all[7]
    
    ind7 <- round(R1/r1_max*100,3)
    
    R2 <- inds_all[8]
    
    ind8 <- round(R2/r2_max*100,3)
    data <- data.frame(
      indices=c(ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8)
    )
    row.names(data)<-c('S','H','D','Ej','E2','E4','R1','R2')
    if(ncol(percentages_df)==0){
      percentages_df <- data
      percentages_df[[k]]<-percentages_df$indices
      percentages_df <- subset(percentages_df,select=-indices)
    }else{
      percentages_df[[k]]<-data$indices
    }
  }
  
  return(list(final_df,percentages_df))
  
}

filter_occupations_fnc <- function(data_occupation,label_id){
  data <- data_occupation[data_occupation$iscoGroup_1 %in% as.character(label_id),]
  return(data)
}

skill_taxonomy_fnc <- function(pillar,layer,branch){
  skills_taxonomy <-read_excel(paste0('C:\\Users\\30697\\Desktop\\SKILLAB\\Code\\Archetypoids_analysis\\ESCO_Taxonomy\\concept_paths_',pillar,'.xlsx'))
  skills <-skills_taxonomy[skills_taxonomy$layer == layer,]
  
  if (pillar=='S'){
    inds<-c()
    for(k in 1:length(skills$code)){
      if(as.numeric(strsplit(skills$code[k],split="*")[[1]][4]) == branch){
        inds<-c(inds,k)
      }
    }
  }
  else if(pillar=='T'){
    inds<-c()
    for(k in 1:length(skills$code)){
      if(as.numeric(strsplit(skills$code[k],split="*")[[1]][4]) == branch){
        inds<-c(inds,k)
      }
    }
  }
  else if(pillar=='K'){
    inds<-c()
    for(k in 1:length(skills$code)){
      if(as.numeric(strsplit(skills$pathString_pos[k],split="/")[[1]][2]) == branch){
        inds<-c(inds,k)
      }
    }
  }
  else{
    inds<-c()
    for(k in 1:length(skills$conceptPL)){
      if(as.numeric(strsplit(skills$pathString_pos[k],split="/")[[1]][2])==branch){
        inds<-c(inds,k)
      }
    }
  }
  
  names <- skills$conceptPL[inds]
  names<-names[complete.cases(names)]
  
  return(names)
}


diversity<-function(matrix){
  ###Calculation of diversity indices
  #need of demand/supply info
  json_res  <- main_function_diversity(matrix)
  ##### 4. Returns the output somehow
  return(json_res)
}

####### MAIN FUNCTION for call here ###############  ------------------ care about the labels -- discussion
main_function_diversity<-function(matrix){
  ########### WORK FINE EITHER WAY NEED ADJUSTMENT FOR THE LABEL FOR THE FF3 #############
  final_dataframe_ls <-calculation_diversity_per_occupation(matrix) ### Provide the occupation data and skills data with isco-group to categorize per occupation
  
  ###final_dataframe_ls[[2]] -> Radar plot
  ff<-data.frame(t(final_dataframe_ls[[1]]))
  
  json_data <- ff #list(ff)
  
  ff2 <-data.frame(t(final_dataframe_ls[[2]]))
  json_data2<-ff2 
  
  #final_dataframe_ls2 <-calculation_diversity_per_occupation(data_occupation_second,data_skills_second,occup_level)
  #ff3<-data.frame(t(final_dataframe_ls2[[2]]))
  #json_data3<-ff3 
  return(list(json_data,json_data2))
  ### Format for first json produced [{"Richness":111,"Exponential.Shannon.Entropy":1111,...,"_row":"Clerical support workers"},{},{}...]
  ### Format for second json produced [{"S":111,"H":1111,...,"_row":"Clerical support workers"},{},{}...] --- Radar plot alone
}



# ################ RADAR PLOT --- ONLY DEMAND ##############
# data_for_radar <-ff2
# 
# data_wanted <- data.frame(Metric=colnames(data_for_radar),Value=t(data_for_radar)[,1])
# 
# data_radar <- data_wanted %>% mutate(Group ="Demand")
# 
# data_radar<-data_radar %>%
#   spread(key = Metric, value = Value)
# 
# ggradar(data_radar,
#         axis.labels = row.names(data),  # Show only the labels of the nodes
#         group.colours = c(rgb(0.2, 0.5, 0.5, 0.9)),  # Color of the polygon
#         grid.min = 0,  # Minimum value for grid
#         grid.mid=50,
#         grid.max = 100,  # Maximum value for grid (set according to normalized values)
#         fill = TRUE,  # Fill the polygon
#         fill.alpha = 0.5,  # Set fill transparency
#         legend.position = "none",  # Remove legend as there is only one dataset
#         grid.label.size = 0  # Hide grid labels (percentages)
# )
# 
# ############ RADAR PLOT ---- OVERLAP ################
# 
# data_for_radar_second <-ff3
# 
# data_wanted_second <- data.frame(Metric=colnames(data_for_radar_second),Value=t(data_for_radar_second)[,1])
# 
# data_combined <- bind_rows(
#   data_wanted %>% mutate(Group = "Demand"),
#   data_wanted_second %>% mutate(Group = "Supply")
# )
# data_radar_1 <- data_combined %>%
#   spread(key = Metric, value = Value)
# 
# ggradar(data_radar_1,
#         axis.labels = colnames(data_radar)[-1],  # Show only the labels of the nodes
#         group.colours = c(rgb(0.2, 0.5, 0.5, 0.9), rgb(0.7, 0.3, 0.3, 0.9)),  # Color of polygons
#         grid.min = 0,  # Minimum value for grid
#         grid.mid=50,
#         grid.max = 100,  # Maximum value for grid
#         grid.label.size=0,
#         fill = TRUE,  # Fill polygons
#         fill.alpha = 0.5,  # Set fill transparency
#         legend.position = "bottom"  # Position legend at the bottom
# )






#args <- commandArgs(trailingOnly = TRUE)
#occup_level <-args[1]
#skill_layer <-args[2]
#pillar_selection<-args[3]
#maybe need it
#pillar_selection <- gsub("'", "", pillar_selection)  # Remove single quotes

#print(occup_level)
#print(skill_layer)
#print(pillar_selection)
#diversity(occup_level,skill_layer,pillar_selection)


#setwd(path)
#occup_level <- '1'
#skill_layer <-'2'
#pillar_selection <- 'S'










