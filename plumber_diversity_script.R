
  ############# Function for diversity results ###################
# plumber.R
# Load necessary libraries
library(plumber)
library(readxl)
library(SpadeR)
library(DT)
library(plotly)
library(fmsb)
library(tidyverse)
library(ggplot2)
library(archetypes)
library(Anthropometry)
library(shinyBS)
library(formatR)
library(jsonlite)
library(ggplot2)


load('./Extras/New_occupation_table.Rda')
load('./Extras/New_occupation_table2.Rda')

load('./Extras/Skill_table.Rda')
ls_of_names<-c("artificial intelligence engineer","computer scientist",             
               "data analyst","data scientist", "enterprise architect",
               "green ICT consultant" , "ICT business analysis manager",
               "ICT business analyst" ,"ICT consultant","ICT system analyst" ,
               "ICT system architect","ICT system developer","integration engineer",
               "user experience analyst", "software analyst","software architect",
               "software developer","user interface developer",
               "digital games developer","search engine optimisation expert",            
               "user interface designer","web content manager",                          
               "web developer","ICT application configurator",
               "ICT application developer","numerical tool and process control programmer")
occupation_table3<-New_occupation_table[c('id4',"label4","Codes")]
occupation_table4<-New_occupation_table2[c("id4","label4","Codes")]
occupation_table<-rbind(occupation_table3,occupation_table4)
potential_occ<-read.csv('./Extras/Potential_occupations.csv')
source('./Extras/Diversity_analysis_KUs.R')
source('./Extras/Required_skill_matching.R')
source('./Extras/Diversity_analysis_organization_KUs.R')

############## Functions ###########
Fit_score_fnc<-function(candidate_skills,results_isa,ratio=0.5,Pillar="all"){
  #candidate_skills<-results_isa$Skill_id[c(2,10,12,15)]
  # 2. Matching logic
  required_skills <- results_isa$Skill_id
  
  candidate<- results_isa[c("Skill","Skill_id","Importance","Pillar")]
  if(Pillar=="all"){
    n_skills<-round(nrow(candidate)*ratio)
    candidate<-candidate[1:n_skills,]
    required_skills<-required_skills[1:n_skills]
    candidate$Value<-ifelse(required_skills %in% candidate_skills,1,0) 
  }else{
    candidate<-candidate[candidate$Pillar==Pillar,]
    n_skills<-round(nrow(candidate)*ratio)
    candidate<-candidate[1:n_skills,]
    required_skills<-required_skills[1:n_skills]
    candidate$Value<-ifelse(required_skills %in% candidate_skills,1,0)
  }
  
  
  
  Fit.Score<-sum(candidate$Value*candidate$Importance)/sum(candidate$Importance)
  return(Fit.Score)
}

Fit_score_init<-function(candidate_skill,results_isa,Pillar="all"){
  skills<-results_isa
  if(nrow(skills)>40){
    Fit_all<-Fit_score_fnc(candidate_skill,skills,ratio=0.2,Pillar="all")
  }else if(nrow(skills)<=20){
    Fit_all<-Fit_score_fnc(candidate_skill,skills,ratio=1,Pillar="all")
  }else{
    Fit_all<-Fit_score_fnc(candidate_skill,skills,ratio=0.5,Pillar="all")
  }
  
  return(Fit_all)
}

Percentile_ranking<-function(candidate_score,distribution){
  # Remove missing values
  distribution <- distribution[!is.na(distribution)]
  
  # Check for empty distribution
  if (length(distribution) == 0) {
    stop("distribution contains no valid values")
  }
  
  # Percentile rank (0–100)
  percentile <- round(mean(distribution <= candidate_score) * 100)
  
  return(percentile)
}

Fit_missing_skills<-function(candidate_skill,results_isa){
  skills<-results_isa
  if(nrow(skills)>40){
    n_skills<-round(nrow(skills)*0.2)
    skills_f<-skills[1:n_skills,]
  }else if(nrow(skills)<=20){
    skills_f<-skills
  }else{
    n_skills<-round(nrow(skills)*0.5)
    skills_f<-skills[1:n_skills,]
  }
  
  missing_skills<-Missing_skills_fnc(candidate_skill,skills_f)
  return(missing_skills)
}

Owned_Skills_fnc<-function(candidate_skill,results_isa){
  skill_owned<-results_isa[results_isa$Skill_id %in% candidate_skill,c("Skill","Importance")]
  skill_owned$Importance<-skill_owned$Importance*100
  return(skill_owned)
}

Missing_skills_fnc<-function(candidate_skills,results_isa,Pillar="all"){
  #candidate_skills<-results_isa$Skill_id[c(2,10,12,15)]
  if(Pillar=="all"){
    # 2. Matching logic
    required_skills <- results_isa$Skill_id
    
    #matched <- required_skills[required_skills %in% candidate_skills]
    
    missing <- setdiff(required_skills, candidate_skills) 
  }else if(Pillar=="S"){
    # 2. Matching logic
    required_skills <- results_isa$Skill_id[results_isa$Pillar=="S"]
    
    #matched <- required_skills[required_skills %in% candidate_skills]
    
    missing <- setdiff(required_skills, candidate_skills) 
  }else if(Pillar=="K"){
    # 2. Matching logic
    required_skills <- results_isa$Skill_id[results_isa$Pillar=="K"]
    
    #matched <- required_skills[required_skills %in% candidate_skills]
    
    missing <- setdiff(required_skills, candidate_skills) 
  }
  
  
  missing_df <- results_isa[results_isa$Skill_id %in% missing,c("Skill","Importance","Skill_id")]
  row.names(missing_df)<-NULL
  
  q <- quantile(missing_df$Importance, probs = c(1/3, 2/3), na.rm = TRUE)
  priority<- ifelse(
    missing_df$Importance <= q[1], "Low",
    ifelse(
      missing_df$Importance <= q[2], "Moderate",
      "High"
    )
  )
  missing_df$Priority<-priority
  return(missing_df)
  
}

Important_skills_role<-function(results_isa){
  skills<-results_isa
  if(nrow(skills)>40){
    n_skills<-round(nrow(skills)*0.2)
    skills_f<-skills[1:n_skills,]
  }else if(nrow(skills)<=20){
    skills_f<-skills
  }else{
    n_skills<-round(nrow(skills)*0.5)
    skills_f<-skills[1:n_skills,]
  }
  
  return(skills_f)
}

Return_Fits<-function(occupation_name,ls_of_names,option="single"){
  if(occupation_name %in% ls_of_names){
    load('./data_for_Citizen/C2511_Fits_second_2.Rda')
    fits_C2511<-list_of_fits
    load('./data_for_Citizen/C2512_Fits_second_2.Rda')
    fits_C2512<-list_of_fits
    load('./data_for_Citizen/C2513_Fits_second_2.Rda')
    fits_C2513<-list_of_fits
    load('./data_for_Citizen/C2514_Fits_second_2.Rda')
    fits_C2514<-list_of_fits
    list_all<-list()
    for(name in names(fits_C2511)){
      list_all[[name]]<-fits_C2511[[name]]
    }
    for(name in names(fits_C2512)){
      list_all[[name]]<-fits_C2512[[name]]
    }
    for(name in names(fits_C2513)){
      list_all[[name]]<-fits_C2513[[name]]
    }
    for(name in names(fits_C2514)){
      list_all[[name]]<-fits_C2514[[name]]
    }
    if(option=="single"){
      fits_wanted<-list_all[[occupation_name]]
    }else{
      fits_wanted<-list_all
    }
    return(fits_wanted)
  }else{
    res<-NA
    return(res)
    
  }
}
Return_ISA_res<-function(occupation_name,ls_of_names,option="single"){
  if(occupation_name %in% ls_of_names){
    data<-readxl::read_xlsx('./data_for_Citizen/Core_skills_all.xlsx')
    if(option=="single"){
      wanted<-data[data$Occupation==occupation_name,]
    }else{
      wanted<-data
    }
    return(wanted)
  }else{
    res<-data.frame()
    return(res)
  }
}


Skill_contribution_fnc<-function(skills,skills_tab,distr){
  
  core_skills_owned<-skills[skills %in% skills_tab$Skill]
  core_skills_ids<-skills_tab$Skill_id[skills_tab$Skill %in% core_skills_owned]
  
  fit<-Fit_score_fnc(core_skills_ids,skills_tab,ratio=1)
  oc<-Percentile_ranking(fit,distr)
  sc_all<-c()
  for(skill in core_skills_ids){
    temp_set<-core_skills_ids[!core_skills_ids %in% skill]
    fit_skill<-Fit_score_fnc(temp_set,skills_tab,ratio=1)
    oc_skill<-Percentile_ranking(fit_skill,distr)
    sc_skill<-oc-oc_skill
    sc_all<-c(sc_all,sc_skill)
  }
  df<-data.frame(Skill=core_skills_owned,SC=sc_all)
  if(nrow(df)!=0){
    df<-df[order(df$SC,decreasing = TRUE),]
  }
  return(df)
}
MOCG_iter<-function(skills,skills_rest,distr,skills_tab){
  fit_init<-Fit_score_fnc(skills,skills_tab)
  OC_init<-Percentile_ranking(fit_init,distr)
  
  mocg_skill<-c()
  for(skill in skills_rest){
    skills_new<-c(skills,skill)
    fit_new<-Fit_score_fnc(skills_new,skills_tab)
    OC_new<-Percentile_ranking(fit_new,distr)
    diffOC<-OC_new-OC_init
    
    mocg_skill<-c(mocg_skill,diffOC)
  }
  
  skill_to_learn<-skills_rest[which.max(mocg_skill)]
  val<-mocg_skill[which.max(mocg_skill)]
  return(list(Skill=skill_to_learn,Value=val))
}
MOCG_fnc<-function(skills,skills_tab,distr){
  core_skills_not_owned<-skills_tab$Skill[!skills_tab$Skill %in% skills]
  skills_rest<-core_skills_not_owned
  skills_current<-skills
  path_skill<-c()
  path_mocg<-c()
  for(i in 1:length(core_skills_not_owned)){
    res<-MOCG_iter(skills_current,skills_rest,distr,skills_tab)
    path_skill<-c(path_skill,res$Skill)
    path_mocg<-c(path_mocg,res$Value)
    
    skills_current<-c(skills_current,res$Skill)
    skills_rest<-skills_rest[!skills_rest %in% res$Skill]
  }
  
  df<-data.frame(Skill=path_skill,MOCG=path_mocg)
  return(df)
}
MOCG_exclusive<-function(skills,skills_rest,distr,skills_tab){
  skill_ids<-skills_tab$Skill_id[skills_tab$Skill %in% skills]
  fit_init<-Fit_score_fnc(skill_ids,skills_tab,ratio=1)
  OC_init<-Percentile_ranking(fit_init,distr)
  
  mocg_skill<-c()
  skills_rest_id<-skills_tab$Skill_id[skills_tab$Skill %in% skills_rest]
  for(skill in skills_rest_id){
    skills_new<-c(skill_ids,skill)
    fit_new<-Fit_score_fnc(skills_new,skills_tab,ratio=1)
    OC_new<-Percentile_ranking(fit_new,distr)
    diffOC<-OC_new-OC_init
    
    mocg_skill<-c(mocg_skill,diffOC)
  }
  
  df=data.frame(Skill=skills_rest,MOCG=mocg_skill)
  return(df)
}


Maximization_MOCG<-function(skills,skills_tab,distr,learning_matrix){
  
  core_skills_not_owned<-skills_tab$Skill[!skills_tab$Skill %in% skills]
  skills_rest<-core_skills_not_owned
  skills_current<-skills
  
  
  skills_rest_id<-skills_tab$Skill_id[skills_tab$Skill %in% skills_rest]
  skills_current_id<-skills_tab$Skill_id[skills_tab$Skill %in% skills_current]
  
  ids<-c()
  diff_fit<-c()
  diff_perc<-c()
  skills_all<-c()
  for(i in 1:nrow(learning_matrix)){
    l_o<-learning_matrix[i,]
    id<-l_o$ID
    ids<-c(ids,id)
    
    l_o_skills<-l_o[colnames(l_o) %in% skills_rest]
    skills_acquired<-colnames(l_o_skills)[which(l_o_skills==1)]
    skills_acquired_id<-skills_tab$Skill_id[skills_tab$Skill %in% skills_acquired]
    
    fit_prev<-Fit_score_fnc(skills_current_id,skills_tab,ratio=1)
    perc_prev<-Percentile_ranking(fit_prev,distr)
    
    fit_new<-Fit_score_fnc(c(skills_current_id,skills_acquired_id),skills_tab,ratio=1)
    perc_new<-Percentile_ranking(fit_new,distr)
    
    diff_fit<-c(diff_fit,fit_new-fit_prev)
    diff_perc<-c(diff_perc,perc_new-perc_prev)
    
    skills_all<-c(skills_all,list(skills_acquired))
  }
  
  df <- data.frame(
    Learning.Opportunity = ids,
    Fit.Improvement = diff_fit,
    Competitiveness.Improvement = diff_perc,
    stringsAsFactors = FALSE
  )
  
  df$Skills.Acquired <- I(skills_all)  
  return(df)
}

#tryCatch({
#  source('./Extras/Main_script_for_results.R')
#  
#}, error = function(e) {
#  message = e$message

#})



#* @apiTitle Diversity Microservice
#* @apiDescription API for diversity calculators

#* @param occupation_name:string Example: Software developers
#* @post /diversity
function(occupation_name) {
  
  # Call the main diversity function
  tryCatch({
    occupational_code<-occupation_table$Codes[match(occupation_name,occupation_table$label4)]
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


#* @apiTitle Diversity Microservice
#* @apiDescription API for diversity calculators

#* @get /available_occupation_names
function() {
  
  # Call the main diversity function
  tryCatch({
  load('./Extras/Available_Occupation_names.Rda')
    
   data = list(occupation_list)
    
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
    
    occupational_code<-occupation_table$Codes[match(occupation_name,occupation_table$label4)]
    
    load(paste0('./data/',occupational_code,'/Diversity_results.Rda'))
    
    res<-results_list$ISA_seperate
    results<-res[!is.na(match(res$Occupation,occupation_name)),]
    results<-results[order(results$Importance,decreasing = TRUE),]
    colnames(results)<-c('Role','Skill','Pillar','Importance','SkillId')
    
    
    res<-Return_ISA_res(occupation_name,ls_of_names)
    if(nrow(res)==0){
      data=results
    }else{
      colnames(res)<-c('Role','Skill','Pillar','Importance','SkillId')
      data=res
    }
    
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
    
    print(skill_list)
    data = required_skill_map(skill_list,matching_number,New_occupation_table)
    
  }, error = function(e) {
    message = e$message
    
  })
}





#* Fit and competition
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example ["http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"]
#* @post /fit_score_calculation_service
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(occupation_table$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- occupation_table$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
    res <- results_list$ISA_seperate
    res_role<-res[res$Occupation==occupation_name,]
    
    res_check<-Return_ISA_res(occupation_name,ls_of_names)
    if(nrow(res_check)==0){
      res_role<-res_role
    }else{
      res_role<-res_check
      
    }
    #colnames(results) <- c('Role','Skill','Pillar','Importance','SkillId')
    
    #1. Fit score
    Fit.Score.Candidate<-Fit_score_fnc(candidate_skills,res_role,ratio=1)
    
    #2. Relative standing
    res_check_2<-Return_Fits(occupation_name,ls_of_names)
    if(length(res_check_2)>1){
      distr<-res_check_2
      percentile<-Percentile_ranking(Fit.Score.Candidate,distr)
      quantile.points<-quantile(distr,probs=c(0.25,0.5,0.75,1))
      
      standing<-percentile
      if (standing <= 25) {
        candidate.tier <- "Novice"
      } else if (standing <= 50) {
        candidate.tier <- "Intermediate"
      } else if (standing <= 75) {
        candidate.tier <- "Advanced"
      } else {
        candidate.tier <- "Elite"
      }
    }else{
      load(paste0('./data/',occupational_code,'/FitScores.Rda'))
      if(occupation_name %in% names(res_all)){
        competition.scores<-res_all[[occupation_name]]
        competitive<-competition.scores[competition.scores>0]
        quantile.points<-quantile(competitive,probs=c(0.25,0.5,0.75,1))
        
        standing <- round(100 * mean(competitive < Fit.Score.Candidate))
        
        if (standing <= 25) {
          candidate.tier <- "Novice"
        } else if (standing <= 50) {
          candidate.tier <- "Intermediate"
        } else if (standing <= 75) {
          candidate.tier <- "Advanced"
        } else {
          candidate.tier <- "Elite"
        }
      }
    }
    
    
    list(
      Fit.Score = Fit.Score.Candidate,
      Quantiles = quantile.points,
      Current.Standing=standing,
      Candidate.tier=candidate.tier
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}






#* Skill profile radar
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example ["http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"]
#* @post /skill_profile_radar_data
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(occupation_table$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- occupation_table$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
    res <- results_list$ISA_seperate
    res_role<-res[res$Occupation==occupation_name,]
    
    
    res_check<-Return_ISA_res(occupation_name,ls_of_names)
    if(nrow(res_check)==0){
      res_role<-res_role
    }else{
      res_role<-res_check
      
    }
    #colnames(results) <- c('Role','Skill','Pillar','Importance','SkillId')
    
    #1. Return skills from candidate skillset that is in the required skills
    Skill_df<-Owned_Skills_fnc(candidate_skills,res_role)
    
    
    
    list(
      data= Skill_df
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}





#* Skill and Knowledge gaps
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example ["http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"]
#* @post /missing_skills
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(occupation_table$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- occupation_table$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
    res <- results_list$ISA_seperate
    res_role<-res[res$Occupation==occupation_name,]
    
    res_check<-Return_ISA_res(occupation_name,ls_of_names)
    if(nrow(res_check)==0){
      res_role<-res_role
    }else{
      res_role<-res_check
      
    }
    #colnames(results) <- c('Role','Skill','Pillar','Importance','SkillId')
    
    #1. Return skills from candidate skillset that is in the required skills
    miss_df_knowledge<-Missing_skills_fnc(candidate_skills,res_role,Pillar="K")
    miss_df_skills<-Missing_skills_fnc(candidate_skills,res_role,Pillar="S")
    
    
    list(
      ESCO.Skills.Gaps= miss_df_skills,
      ESCO.Knowledge.Gaps=miss_df_knowledge
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}


#* Skill contribution for candidate position
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example ["http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"]
#* @post /skill_contribution
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(occupation_table$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- occupation_table$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
    res <- results_list$ISA_seperate
    res_role<-res[res$Occupation==occupation_name,]
    
    res_check<-Return_ISA_res(occupation_name,ls_of_names)
    if(nrow(res_check)==0){
      res_role<-res_role
    }else{
      res_role<-res_check
      
    }
    
    
    
    #2. Relative standing
    res_check_2<-Return_Fits(occupation_name,ls_of_names)
    skill_names<-skill_matrix$PreferedLabel[skill_matrix$Skill_id %in% candidate_skills]
    if(length(res_check_2)>1){
      distr<-res_check_2
      res<-Skill_contribution_fnc(skill_names,res_role,distr)
      colnames(res)<-c('Skill','Contribution')
    }else{
      load(paste0('./data/',occupational_code,'/FitScores.Rda'))
      if(occupation_name %in% names(res_all)){
        competition.scores<-res_all[[occupation_name]]
        
        distr<-competition.scores
        res<-Skill_contribution_fnc(skill_names,res_role,distr)
        colnames(res)<-c('Skill','Contribution')
        
        
      }else{
        res<-NA
      }
    }
    
    
    
    
    list(
      Skill.Contribution=res
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}



#* MOCG for each missing skill
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example ["http://data.europa.eu/esco/skill/9d2e926f-53d9-41f5-98f3-19dfaa687f3f","http://data.europa.eu/esco/skill/bd14968e-e409-45af-b362-3495ed7b10e0","http://data.europa.eu/esco/skill/f0de4973-0a70-4644-8fd4-3a97080476f4","http://data.europa.eu/esco/skill/4812a4ea-dc55-4dc6-b9b0-4a59bba2c647","http://data.europa.eu/esco/skill/4707da90-9cfc-46ca-8de0-38a0b7bfb137","http://data.europa.eu/esco/skill/7b5cce4d-c7fe-4119-b48f-70aa05391787","http://data.europa.eu/esco/skill/15d76317-c71a-4fa2-aadc-2ecc34e627b7","http://data.europa.eu/esco/skill/d9013e0e-e937-43d5-ab71-0e917ee882b8","http://data.europa.eu/esco/skill/fd33c66c-70c4-40e6-b87c-5495bd3bf26e","http://data.europa.eu/esco/skill/69bbd53f-fbb0-4476-b4b2-ef7844464e28","http://data.europa.eu/esco/skill/3cd569a2-4f88-4c1e-9995-8dce8c5e51a7","http://data.europa.eu/esco/skill/cb668e89-6ef5-4ff3-ab4a-506010e7e70b","http://data.europa.eu/esco/skill/21d2f96d-35f7-4e3f-9745-c533d2dd6e97","http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0","http://data.europa.eu/esco/skill/9b9de2a4-d8af-4a7b-933a-a8334ae60067","http://data.europa.eu/esco/skill/11430d93-c835-48ed-8e70-285fa69c9ae6","http://data.europa.eu/esco/skill/c4b1f326-224a-420a-b8b3-814a8f13b6cb"]
#* @post /mocg
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(occupation_table$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- occupation_table$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
    res <- results_list$ISA_seperate
    res_role<-res[res$Occupation==occupation_name,]
    
    res_check<-Return_ISA_res(occupation_name,ls_of_names)
    if(nrow(res_check)==0){
      res_role<-res_role
    }else{
      res_role<-res_check
      
    }
    
    
    
    #2. Relative standing
    res_check_2<-Return_Fits(occupation_name,ls_of_names)
    skill_names<-skill_matrix$PreferedLabel[skill_matrix$Skill_id %in% candidate_skills]
    if(length(res_check_2)>1){
      distr<-res_check_2
      skills_rest<-res_role$Skill[!res_role$Skill %in% skill_names]
      res<-MOCG_exclusive(skill_names,skills_rest,distr,res_role)
      colnames(res)<-c('Skill','MOCG')
    }else{
      load(paste0('./data/',occupational_code,'/FitScores.Rda'))
      if(occupation_name %in% names(res_all)){
        competition.scores<-res_all[[occupation_name]]
        
        distr<-competition.scores
        skills_rest<-res_role$Skill[!res_role$Skill %in% skill_names]
        res<-MOCG_exclusive(skill_names,skills_rest,distr,skills_tab)
        
        colnames(res)<-c('Skill','MOCG')
        
        
      }else{
        res<-NA
      }
    }
    
    
    
    
    list(
      MOCG=res
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}


#* Maximization of MOCG through Learning opportunities
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example ["http://data.europa.eu/esco/skill/9d2e926f-53d9-41f5-98f3-19dfaa687f3f","http://data.europa.eu/esco/skill/bd14968e-e409-45af-b362-3495ed7b10e0","http://data.europa.eu/esco/skill/f0de4973-0a70-4644-8fd4-3a97080476f4","http://data.europa.eu/esco/skill/4812a4ea-dc55-4dc6-b9b0-4a59bba2c647","http://data.europa.eu/esco/skill/4707da90-9cfc-46ca-8de0-38a0b7bfb137","http://data.europa.eu/esco/skill/7b5cce4d-c7fe-4119-b48f-70aa05391787","http://data.europa.eu/esco/skill/15d76317-c71a-4fa2-aadc-2ecc34e627b7","http://data.europa.eu/esco/skill/d9013e0e-e937-43d5-ab71-0e917ee882b8","http://data.europa.eu/esco/skill/fd33c66c-70c4-40e6-b87c-5495bd3bf26e","http://data.europa.eu/esco/skill/69bbd53f-fbb0-4476-b4b2-ef7844464e28","http://data.europa.eu/esco/skill/3cd569a2-4f88-4c1e-9995-8dce8c5e51a7","http://data.europa.eu/esco/skill/cb668e89-6ef5-4ff3-ab4a-506010e7e70b","http://data.europa.eu/esco/skill/21d2f96d-35f7-4e3f-9745-c533d2dd6e97","http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0","http://data.europa.eu/esco/skill/9b9de2a4-d8af-4a7b-933a-a8334ae60067","http://data.europa.eu/esco/skill/11430d93-c835-48ed-8e70-285fa69c9ae6","http://data.europa.eu/esco/skill/c4b1f326-224a-420a-b8b3-814a8f13b6cb"]
#* @post /mocg_learning_opportunities
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(occupation_table$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- occupation_table$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
    res <- results_list$ISA_seperate
    res_role<-res[res$Occupation==occupation_name,]
    
    res_check<-Return_ISA_res(occupation_name,ls_of_names)
    if(nrow(res_check)==0){
      res_role<-res_role
    }else{
      res_role<-res_check
      
    }
    
    #2. Distribution of competitors
    res_check_2<-Return_Fits(occupation_name,ls_of_names)
    if(length(res_check_2)>1){
      distr<-res_check_2
    }else{
      load(paste0('./data/',occupational_code,'/FitScores.Rda'))
      if(occupation_name %in% names(res_all)){
        competition.scores<-res_all[[occupation_name]]
        
        distr<-competition.scores
        
        
        
      }else{
        return(NA)
      }
    }
    
    #3. Learning Opportunities
    load('./data_for_Citizen/LearningOpportunities.Rda')
    
    missing_skill_names<-res_role$Skill[!res_role$Skill_id %in% candidate_skills]
    
    targeted_lo<-learning_opportunities_df[,c("ID",colnames(learning_opportunities_df)[colnames(learning_opportunities_df)%in% missing_skill_names])]
    
    lo_role<-targeted_lo[rowSums(targeted_lo[2:ncol(targeted_lo)])>0,]
    
    
    candidate_skill_names<-skill_matrix$PreferedLabel[skill_matrix$Skill_id %in% candidate_skills]
    
    
    res<-Maximization_MOCG(candidate_skill_names,res_role,distr,lo_role)
    
    
    res<-res[order(res$Competitiveness.Improvement,decreasing=TRUE),]
    
    
    list(
      Learning_opportunities=res
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}




#* Skill to learn step by step to increase his Fit
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example ["http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"]
#* @post /skill_learning_lader
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(occupation_table$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- occupation_table$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
    res <- results_list$ISA_seperate
    res_role<-res[res$Occupation==occupation_name,]
    
    #Competition data load
    load(paste0('./data/',occupational_code,'/FitScores.Rda'))
    if(occupation_name %in% names(res_all)){
      competition.scores<-res_all[[occupation_name]]
      competitive<-competition.scores[competition.scores>0]
    }
    
    
    ############ Section for pre-run results #############
    res_check<-Return_ISA_res(occupation_name,ls_of_names)
    if(nrow(res_check)==0){
      res_role<-res_role
    }else{
      res_role<-res_check
      
    }
    
    res_check_2<-Return_Fits(occupation_name,ls_of_names)
    if(length(res_check_2)>1){
      distr<-res_check_2
    }else{
      load(paste0('./data/',occupational_code,'/FitScores.Rda'))
      if(occupation_name %in% names(res_all)){
        competition.scores<-res_all[[occupation_name]]
        
        distr<-competition.scores
        
        
        
      }else{
        return(NA)
      }
    }
    
    
    competitive<-distr[distr>0]
    #Initial Fit score
    Fit.Score.Candidate<-Fit_score_fnc(candidate_skills,res_role,ratio=1)
    
    
    
    list.of.missing.skills<-res_role[!res_role$Skill_id %in% candidate_skills,]
    skill_lader<-c()
    fit_lader<-c()
    competition_lader<-c()
    new_skills<-c()
    for(i in 1:nrow(list.of.missing.skills)){
      new_skills<-c(new_skills,list.of.missing.skills$Skill_id[i])
      skill_lader<-c(skill_lader,list.of.missing.skills$Skill[i])
      
      fit.current<-Fit_score_fnc(c(candidate_skills,new_skills),res_role,ratio=1)
      fit_lader<-c(fit_lader,100*fit.current)
      
      competition.current<- round(100 * mean(competitive < fit.current),2)
      competition_lader<-c(competition_lader,competition.current)
    }
    
    lader_df<-data.frame(Skills=skill_lader,Fit=fit_lader,Competition=competition_lader)
    
    
    list(
      Skill.Learning=lader_df
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}



#* Fit scores and relative standing for different roles
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example ["http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"]
#* @post /alternative_careers
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(occupation_table$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- occupation_table$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
    res <- results_list$ISA_seperate
    res_role<-res[res$Occupation==occupation_name,]
    
    #Competition data load
    load(paste0('./data/',occupational_code,'/FitScores.Rda'))
    if(occupation_name %in% names(res_all)){
      competition.scores<-res_all[[occupation_name]]
      competitive<-competition.scores[competition.scores>0]
    }
    
    
    potential_alternative_roles<-as.character(unique(res$Occupation)[unique(res$Occupation)%in% names(res_all)])
    
    
    
    res_check<-Return_ISA_res(occupation_name,ls_of_names,option="all")
    if(nrow(res_check)==0){
      fits<-c()
      competitions<-c()
      roles<-c()
      for(role in potential_alternative_roles){
        roles<-c(roles,role)
        res_role<-res[res$Occupation==role,]
        
        fit_role<-Fit_score_init(candidate_skills,res_role)
        
        competition.scores<-res_all[[role]]
        competitive<-competition.scores[competition.scores>0]
        
        standing <- round(100 * mean(competitive < fit_role))
        
        fits<-c(fits,100*fit_role)
        competitions<-c(competitions,standing)
      }
    }else{
      res_role<-res_check
      res_check_2<-Return_Fits(occupation_name,ls_of_names,option="all")
      
      fits_all<-res_check_2
      IVs_all<-res_role
      
      same_occupations<-intersect(names(fits_all),unique(IVs_all$Occupation))
      
      fits_wanted<-fits_all[names(fits_all) %in% same_occupations]
      IVs_wanted<-IVs_all[IVs_all$Occupation %in% same_occupations,]
      
      
      fits<-c()
      competitions<-c()
      roles<-c()
      potential_alternative_roles<-unique(names(fits_wanted))
      for(role in potential_alternative_roles){
        roles<-c(roles,role)
        res_role<-IVs_wanted[IVs_wanted$Occupation==role,]
        
        fit_role<-Fit_score_fnc(candidate_skills,res_role,ratio=1)
        
        competition.scores<-fits_wanted[[role]]
        competitive<-Percentile_ranking(fit_role,competition.scores)
        
        
        fits<-c(fits,fit_role)
        competitions<-c(competitions,competitive)
      }
      
    }
    
    
    
    
    Potential.Careers<-data.frame(Roles=roles,Fit=fits,Competition=competitions)
    
    Potential.Careers<-Potential.Careers[order(Potential.Careers$Competition,Potential.Careers$Fit,decreasing=TRUE),]
    
    Potential.Careers<-Potential.Careers[Potential.Careers$Fit!=0,]    
    list(
      alternative.careers=Potential.Careers
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}


#* Transferable skills of candidates to alternative occupations
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example ["http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"]
#* @post /Transferable_skills
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(occupation_table$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- occupation_table$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
    res <- results_list$ISA_seperate
    res_role<-res[res$Occupation==occupation_name,]
    
    #Competition data load
    load(paste0('./data/',occupational_code,'/FitScores.Rda'))
    if(occupation_name %in% names(res_all)){
      competition.scores<-res_all[[occupation_name]]
      competitive<-competition.scores[competition.scores>0]
    }
    
    
    potential_alternative_roles<-as.character(unique(res$Occupation)[unique(res$Occupation)%in% names(res_all)])
    
    candidate_skill_info <- skill_matrix[
      skill_matrix$Skill_id %in% candidate_skills,
      c("Skill_id", "PreferedLabel", "Pillar")
    ]
    
    
    
    res_check<-Return_ISA_res(occupation_name,ls_of_names,option="all")
    
    if(nrow(res_check)==0){
      skills_wanted<-c()
      pillar_wanted<-c()
      roles<-c()
      importances<-c()
      for(i in 1:nrow(candidate_skill_info)){
        skill<-candidate_skill_info$PreferedLabel[i]
        skills_wanted<-c(skills_wanted,skill)
        pillar<-candidate_skill_info$Pillar[i]
        pillar_wanted<-c(pillar_wanted,pillar)
        
        roles_skill<-c()
        importance<-0
        for(role in potential_alternative_roles){
          res_role<-res[res$Occupation==role,]
          important.skills<-res_role
          
          if(skill %in% important.skills$Skill){
            importance<-important.skills$Importance[important.skills$Skill %in% skill]
            roles_skill<-c(roles_skill,role)
          }
        }
        roles_all<-paste0(roles_skill,collapse=",")
        roles<-c(roles,roles_all)
        importances<-c(importances,importance)
      }
      
      Transferable.Skills<-data.frame(Skill=skills_wanted,Roles=roles,Pillar=pillar_wanted,Importance=importances)
      Transferable.Skills <- Transferable.Skills[Transferable.Skills$Roles != "", ]
      
      
      
      ##### Connections for cleaner presentation #########
      skill_nodes<-c()
      pillar_nodes<-c()
      roles_nodes<-c()
      importances<-c()
      for(i in 1:nrow(Transferable.Skills)){
        skill<-Transferable.Skills$Skill[i]
        pillar<-Transferable.Skills$Pillar[i]
        importance<-Transferable.Skills$Importance[i]
        
        roles<-Transferable.Skills$Roles[i]
        seperated<-strsplit(roles,split=",")[[1]]
        if(length(seperated)>1){
          for(role in seperated){
            skill_nodes<-c(skill_nodes,skill)
            pillar_nodes<-c(pillar_nodes,pillar)
            roles_nodes<-c(roles_nodes,role)
            importances<-c(importances,importance)
          }
        }else{
          skill_nodes<-c(skill_nodes,skill)
          pillar_nodes<-c(pillar_nodes,pillar)
          roles_nodes<-c(roles_nodes,roles)
          importances<-c(importances,importance)
        }
      }
      
      graph_df<-data.frame(Skills=skill_nodes,Roles=roles_nodes,Pillar=pillar_nodes,Importance=importances)
      graph_df<-graph_df[graph_df$Pillar!="T",]
      
    }else{
      res_role<-res_check
      res_check_2<-Return_Fits(occupation_name,ls_of_names,option="all")
      
      fits_all<-res_check_2
      IVs_all<-res_role
      
      same_occupations<-intersect(names(fits_all),unique(IVs_all$Occupation))
      
      fits_wanted<-fits_all[names(fits_all) %in% same_occupations]
      IVs_wanted<-IVs_all[IVs_all$Occupation %in% same_occupations,]
      potential_alternative_roles <- same_occupations
      
      skills_wanted<-c()
      pillar_wanted<-c()
      roles<-c()
      importances<-c()
      for(i in 1:nrow(candidate_skill_info)){
        skill<-candidate_skill_info$PreferedLabel[i]
        skills_wanted<-c(skills_wanted,skill)
        pillar<-candidate_skill_info$Pillar[i]
        pillar_wanted<-c(pillar_wanted,pillar)
        
        roles_skill<-c()
        importance<-0
        for(role in potential_alternative_roles){
          res_role<-IVs_wanted[IVs_wanted$Occupation==role,]
          important.skills<-res_role
          
          if(skill %in% important.skills$Skill){
            importance<-important.skills$Importance[important.skills$Skill %in% skill]
            roles_skill<-c(roles_skill,role)
          }
        }
        roles_all<-paste0(roles_skill,collapse=",")
        roles<-c(roles,roles_all)
        importances<-c(importances,importance)
      }
      
      Transferable.Skills<-data.frame(Skill=skills_wanted,Roles=roles,Pillar=pillar_wanted,Importance=importances)
      Transferable.Skills <- Transferable.Skills[Transferable.Skills$Roles != "", ]
      
      
      ##### Connections for cleaner presentation #########
      skill_nodes<-c()
      pillar_nodes<-c()
      roles_nodes<-c()
      importances<-c()
      for(i in 1:nrow(Transferable.Skills)){
        skill<-Transferable.Skills$Skill[i]
        pillar<-Transferable.Skills$Pillar[i]
        importance<-Transferable.Skills$Importance[i]
        
        roles<-Transferable.Skills$Roles[i]
        seperated<-strsplit(roles,split=",")[[1]]
        if(length(seperated)>1){
          for(role in seperated){
            skill_nodes<-c(skill_nodes,skill)
            pillar_nodes<-c(pillar_nodes,pillar)
            roles_nodes<-c(roles_nodes,role)
            importances<-c(importances,importance)
          }
        }else{
          skill_nodes<-c(skill_nodes,skill)
          pillar_nodes<-c(pillar_nodes,pillar)
          roles_nodes<-c(roles_nodes,roles)
          importances<-c(importances,importance)
        }
      }
      
      graph_df<-data.frame(Skills=skill_nodes,Roles=roles_nodes,Pillar=pillar_nodes,Importance=importances)
      
      graph_df<-graph_df[graph_df$Pillar!="T",]
    }
    
    
    
    
    
    
    list(
      Transferable.Skills=Transferable.Skills,
      Graph.Transferable.Skills=graph_df
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}




#* Job description examples
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example ["http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"]
#* @post /job_description
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(occupation_table$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- occupation_table$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Skill_job_matrix.Rda'))
    jobs<-skill_matrix_job[skill_matrix_job$Occupation_Label4==occupation_name,]
    
    
    candidate_skill_info <- skill_matrix[
      skill_matrix$Skill_id %in% candidate_skills,
      c("PreferedLabel")
    ]
    
    skill_cols <- names(jobs)[6:ncol(jobs)]
    candidate_vec <- as.numeric(skill_cols %in% candidate_skill_info)
    job_matrix <- as.matrix(jobs[, skill_cols])
    
    
    dot_products <- job_matrix %*% candidate_vec
    
    # vector norms
    job_norms <- sqrt(rowSums(job_matrix^2))
    candidate_norm <- sqrt(sum(candidate_vec^2))
    
    # cosine similarities
    cos_sim <- as.vector(dot_products / (job_norms * candidate_norm))
    
    results <- data.frame(
      Job_id = jobs$Job_id,
      Occupation = jobs$Occupation_Label4,
      CosineSimilarity = cos_sim
    )
    
    results <- results[order(-results$CosineSimilarity), ]
    
    for(id in results$Job_id){
      headers = c(
        accept = "application/json",
        `Content-Type` = "application/x-www-form-urlencoded",
        `Authorization` = paste("Bearer",tokenb)
      )
      params = list(
        page = "1"
      )
      data = list(
        ids = id,
        sources="eures"
      )
      
      res <- httr::POST(url = paste0(URL_base,'/jobs'), httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
      parsed_content <- content(res, "parsed")
      
      item<-parsed_content$items[[1]]
      if(length(item$description)>0){
        descr_wanted<-item$description
        break
      }
    }
    
    list(
      Description=descr_wanted
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}




#* Recommended role based on candidate skills
#* @param candidate_skills:list #Example ["http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"]
#* @post /role_recommendation
function(candidate_skills) {
  
  tryCatch({
    
    occupations_all<-potential_occ
    fit_scores<-c()
    for(occupation_name in potential_occ$Roles){
      occupation_names <- trimws(occupation_table$label4)
      # 1. Load occupation data (same as before)
      occupational_code <- occupation_table$Codes[
        match(occupation_name, occupation_names)
      ]
      
      load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
      res <- results_list$ISA_seperate
      res_role<-res[res$Occupation==occupation_name,]
      
      #colnames(results) <- c('Role','Skill','Pillar','Importance','SkillId')
      
      #1. Fit score
      Fit.Score.Candidate<-Fit_score_init(candidate_skills,res_role)
      fit_scores<-c(fit_scores,Fit.Score.Candidate)
    }
    
    df<-data.frame(Roles=potential_occ$Roles,Fit=fit_scores)
    
    ordered_df<-df[order(df$Fit,decreasing = TRUE),]
    
    Recommended_roles<-ordered_df[ordered_df$Fit>0,]
    if(nrow(Recommended_roles)>5){
      recommended_occupations<-Recommended_roles$Roles[1:5]
    }else if(nrow(Recommended_roles)>0){
      recommended_occupations<-Recommended_roles$Roles[1:nrow(Recommended_roles)]
    }else{
      recommended_occupations<-NA
    }
    list(
      Recommended.Occupations=recommended_occupations
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}







