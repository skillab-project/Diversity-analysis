############## Libraries ###########
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
library(httr)
library(dotenv)
################ Authetication ##########################
get_valid_token <- function(API_BASE_URL,USERNAME,PASSWORD) {
  response <- POST(
    paste0(API_BASE_URL, "/login"),
    body = list(username = USERNAME, password = PASSWORD),
    encode = "json",
    add_headers(
      "accept" = "application/json",
      "Content-Type" = "application/json"
    ),
    verbose()  # Debug authentication
  )
  
  if (status_code(response) == 200) {
    token <- gsub('"', "", content(response, "text"))
    cat("Token acquired:", substr(token, 1, 20), "...\n")  # Log partial token
    return(token)
  } else {
    stop(sprintf("Auth failed (Status %d): %s", 
                 status_code(response),
                 content(response, "text")))
  }
}


dotenv::load_dot_env(".env")
user <- Sys.getenv("USERNAME")
pass <- Sys.getenv("PASSWORD")
URL_base<-Sys.getenv("URL")
tokenb<-get_valid_token(URL_base,user,pass)

############# Data ##########
load('./Extras/New_occupation_table2.Rda')
load('./Extras/Skill_table.Rda')
potential_occ<-read.csv('./Extras/Potential_occupations.csv')
candidate_skills<-c("http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d",
                     "http://data.europa.eu/esco/skill/b633eb55-8f1f-4ae6-ab4c-2022ffe2cb7f",
                    "http://data.europa.eu/esco/skill/4da171e5-779c-4983-a76f-91c16751e99f",
                     "http://data.europa.eu/esco/skill/bd14968e-e409-45af-b362-3495ed7b10e0",
                     "http://data.europa.eu/esco/skill/7111b95d-0ce3-441a-9d92-4c75d05c4388")

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
  
  
  
  Fit.Score<-round(sum(candidate$Value*candidate$Importance)/sum(candidate$Importance),2)
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

####### Services #############
#* Fit and competition
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example "http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"
# "http://data.europa.eu/esco/skill/b633eb55-8f1f-4ae6-ab4c-2022ffe2cb7f"
#"http://data.europa.eu/esco/skill/4da171e5-779c-4983-a76f-91c16751e99f"
# "http://data.europa.eu/esco/skill/bd14968e-e409-45af-b362-3495ed7b10e0"
# "http://data.europa.eu/esco/skill/7111b95d-0ce3-441a-9d92-4c75d05c4388"
#* @post /fit_score_calculation_service
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(New_occupation_table2$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- New_occupation_table2$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
    res <- results_list$ISA_seperate
    res_role<-res[res$Occupation==occupation_name,]
    
    #colnames(results) <- c('Role','Skill','Pillar','Importance','SkillId')
    
    #1. Fit score
    Fit.Score.Candidate<-Fit_score_init(candidate_skills,res_role)
    
    #2. Relative standing
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
#* @param candidate_skills:list #Example "http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"
#* @post /skill_profile_radar_data
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(New_occupation_table2$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- New_occupation_table2$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
    res <- results_list$ISA_seperate
    res_role<-res[res$Occupation==occupation_name,]
    
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
#* @param candidate_skills:list #Example "http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"
#* @post /missing_skills
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(New_occupation_table2$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- New_occupation_table2$Codes[
      match(occupation_name, occupation_names)
    ]
    
    load(paste0('./data/', occupational_code, '/Diversity_results.Rda'))
    res <- results_list$ISA_seperate
    res_role<-res[res$Occupation==occupation_name,]
    
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



#* Skill to learn step by step to increase his Fit
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example "http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"
#* @post /skill_learning_lader
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(New_occupation_table2$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- New_occupation_table2$Codes[
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

    #Initial Fit score
    Fit.Score.Candidate<-Fit_score_init(candidate_skills,res_role)
    
    
    
    list.of.missing.skills<-Fit_missing_skills(candidate_skills,res_role)
    skill_lader<-c()
    fit_lader<-c()
    competition_lader<-c()
    new_skills<-c()
    for(i in 1:nrow(list.of.missing.skills)){
      new_skills<-c(new_skills,list.of.missing.skills$Skill_id[i])
      skill_lader<-c(skill_lader,list.of.missing.skills$Skill[i])
      
      fit.current<-Fit_score_init(c(candidate_skills,new_skills),res_role)
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
#* @param candidate_skills:list #Example "http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"
#* @post /alternative_careers
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(New_occupation_table2$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- New_occupation_table2$Codes[
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
    
    Potential.Careers<-data.frame(Roles=roles,Fit=fits,Competition=competitions)
    Potential.Careers<-Potential.Careers[order(Potential.Careers$Competition,Potential.Careers$Fit,decreasing=TRUE),]
    
    list(
      alternative.careers=Potential.Careers
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}


#* Transferable skills of candidates to alternative occupations
#* @param occupation_name:string #Example "software developer"
#* @param candidate_skills:list #Example "http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"
#* @post /Transferable_skills
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(New_occupation_table2$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- New_occupation_table2$Codes[
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
#* @param candidate_skills:list #Example "http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"
#* @post /job_description
function(occupation_name, candidate_skills) {
  
  tryCatch({
    occupation_names <- trimws(New_occupation_table2$label4)
    # 1. Load occupation data (same as before)
    occupational_code <- New_occupation_table2$Codes[
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
#* @param candidate_skills:list #Example "http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389","http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"
#* @post /role_recommendation
function(candidate_skills) {
  
  tryCatch({
    
    occupations_all<-potential_occ
    fit_scores<-c()
    for(occupation_name in potential_occ$Roles){
      occupation_names <- trimws(New_occupation_table2$label4)
      # 1. Load occupation data (same as before)
      occupational_code <- New_occupation_table2$Codes[
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


