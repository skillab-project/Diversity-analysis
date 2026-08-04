############ Description: occupation selection and table construction for biodiversity analysis  --- all occupations################
#################### load data #######
load('./Extras/Skill_table.Rda')

########### libraries #######
library(httr)
######### Functions ########
jobs_gathering<-function(occupation_id,tokenb,URL_base){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded",
    `Authorization` = paste("Bearer",tokenb)
  )
  params = list(
    page = "1"
  )
  data = list(
    occupation_ids = occupation_id,
    sources="eures-escox"
  )
  
  res <- httr::POST(url = paste0(URL_base,'/jobs'), httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  parsed_content <- content(res, "parsed")
  jobs_page1<-parsed_content$items
  count_of_jobs<-parsed_content$count
  
  
  indicator<-count_of_jobs<10000
  if(indicator){
      if(count_of_jobs>100){
        iterations<-ceiling((count_of_jobs-100)/100)
        jobs<-jobs_page1
        for (j in 2:(iterations+1)){
          print(j)
          params = list(
            page = as.character(j)
          )
          data = list(
            occupation_ids = occupation_id,
            sources="eures-escox"
          )
          res <- httr::POST(url = paste0(URL_base,'/jobs'), httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
          parsed_content <- content(res, "parsed")
          jobs_pagej<-parsed_content$items
          jobs<-c(jobs,jobs_pagej)
          
        }
        
      }else{
        jobs<-jobs_page1
      }
    }else{
      total_iterations<-ceiling((count_of_jobs)/100)
      min<-total_iterations-100
      max<-total_iterations+1
      jobs<-c()
      for(j in min:max){
        print(j)
        params = list(
          page = as.character(j)
        )
        data = list(
          occupation_ids = occupation_id,
          sources="eures-escox"
        )
        res <- httr::POST(url = paste0(URL_base,'/jobs'), httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
        parsed_content <- content(res, "parsed")
        jobs_pagej<-parsed_content$items
        jobs<-c(jobs,jobs_pagej)
      }
    }
  
  
  
  
  
  return(jobs)
}
job_skill_list_construction<-function(jobs){
  skill_list<-list()
  for(job in jobs){
    skill_list[[as.character(job$id)]]<-unlist(job$skills)
  }
  return(skill_list)
}
job_skill_matrix_construction<-function(unique_skills,occupations_jobs,arr,skill_matrix,occupation_table){
  jobs_ids<-c()
  occupations_id4<-c()
  for(occupation in names(occupations_jobs)){
    occ<-rep(occupation,length(occupations_jobs[[occupation]]))
    ids<-names(occupations_jobs[[occupation]])
    occupations_id4<-c(occupations_id4,occ)
    jobs_ids<-c(jobs_ids,ids)
  }
  df<-data.frame(`Job_id`=jobs_ids,`Occupation_ID4`=occupations_id4)
  df$`Occupation_Label4`<-occupation_table$label4[
    match(df$Occupation_ID4, occupation_table$id4)
  ]
  df$`Occupation_ID3`<-occupation_table$id3[
    match(df$Occupation_ID4, occupation_table$id4)]
  df$`Occupation_Label3`<-occupation_table$label3[
    match(df$Occupation_ID4, occupation_table$id4)]
  
  df_skills<-data.frame(`Skills`=unique_skills)
  skill_names<-skill_matrix$PreferedLabel[
    match(df_skills$Skills,skill_matrix$Skill_id)]
  
  for(skill in skill_names){
    df[[skill]]<-NA
  }
  df[is.na(df)]<-0
  

  
   for(name in names(arr)){
     skill_collection<-arr[[name]]
     skill_col_i<-skill_matrix$PreferedLabel[
       match(skill_collection,skill_matrix$Skill_id)]
     df[df$Job_id==name,skill_col_i[!is.na(skill_col_i)]]<-1
   }
  return(df)
}
####### Whole pipeline function ############ argument :: groups_all -from: New_occupation_table.Rda
jobs_skill_analysis<-function(groups_all,token,URL_base,occupation_table){
  for(group_i in groups_all){
    code<-occupation_table$Codes[match(group_i,occupation_table$Group)]
    print(code)
    
    occupations_table<-occupation_table[occupation_table$Group==group_i,]
    potential_occupations<-occupations_table$id4
    arr<-c()
    occupations_jobs<-list()
    ##gather advertisements for the occupations in potential occupations
    for(occupation in potential_occupations){
      jobs<-jobs_gathering(occupation,token=token,URL_base=URL_base)
      ##list of job_id and skills
      jobs_skills_list<-job_skill_list_construction(jobs)
      occupations_jobs[[occupation]]<-jobs_skills_list
      arr<-c(arr,jobs_skills_list)
    }
    ## find all the unique skills and make a dataframe for all the jobs/occupations
    all_skills<-unlist(arr)
    unique_skills<-unique(all_skills)
    un_skills<-unique_skills[unique_skills %in% skill_matrix$Skill_id]
    ## Matrix construction job_id, occupation_label4,occupation_id3,occupation_label4,occupation_id4,code_occupation
    skill_matrix_job<-job_skill_matrix_construction(un_skills,occupations_jobs,arr,skill_matrix,occupation_table)
    
    occ_table<-occupation_table[occupation_table$Group==group_i,]
    if(length(unique(occ_table$Codes))>1){
      for(code in unique(occ_table$Codes)){
        save(skill_matrix_job,file=paste0('./data/',code,'/Skill_job_matrix.Rda'))
      }
    }else{
      save(skill_matrix_job,file=paste0('./data/',code,'/Skill_job_matrix.Rda'))
    }
    
    load(paste0('./data/',code,'/Skill_job_matrix.Rda'))
         
    rm(list = setdiff(ls(), c("groups_all", "token", "URL_base", "occupation_table","skill_matrix")))
    gc()
  }
  return('Complete')
}








