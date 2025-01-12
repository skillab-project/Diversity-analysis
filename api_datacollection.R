#################### API for data collection ---- dataframe construction ###########

##libraries

library(httr)
library(dplyr)

######### Get all the possible skills based on the pillar and the skill level
get_all_skills<-function(pillar,level){
  #input pillar & level
  level<-as.character(level)
  page<-"1"
  
  
  all_skills_list<-c()
  while(TRUE){
    headers = c(
      accept = "application/json",
      `Content-Type` = "application/x-www-form-urlencoded"
    )
    params = list(
      page = page
    )
    if(pillar=='S'){
    data = list(
      min_skill_level = level,
      max_skill_level = level
    )
    }else if(pillar=='K'){
      data = list(
        min_knowledge_level = level,
        max_knowledge_level = level
      )
    }else if(pillar=='L'){
      data=list(
      min_language_level = level,
      max_language_level = level
      )
    }else if(pillar=='T'){
      data=list(
      min_traversal_level=level,
      max_traversal_level=level
      )
    }
    res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/skills", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
    
    parsed_content <- content(res, "parsed")
    # Convert the parsed content to a list of items
    all_skill_ls <- as.list(parsed_content)
    
    all_skills_list<-c(all_skills_list,all_skill_ls$items)
    if(length(all_skill_ls$items)==0){
      break
    }
  
    page<-as.numeric(page)+1
  }
  #predefine headers - params (modification for pages) - 
  
  skills <-c()
  for (item in all_skills_list){
    skill <-item$label
    skills<-c(skills,skill)
  }
  
  return(skills)
}
######### Construct the dataframe with the 0s and as columns the skills
initial_dataframe_construction <-function(occupations,skills){
  df <- as.data.frame(matrix(
    0,  # Default value to fill the dataframe (can be changed to 0 or another value)
    nrow = length(occupations),
    ncol = length(skills)
  ))
  colnames(df) <- skills
  df$Occupation <-occupations
  df <- df[, c("Occupation", skills)]
  return(df)
}
#Utility
### need options c('label','alternative_labels','description')
get_skill_info <-function(skill_id,options=c()){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded"
  )
  
  params = list(
    page = "1"
  )
  
  data = list(
    ids = skill_id
  )
  res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/skills", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  parsed_content <- content(res, "parsed")
  # Convert the parsed content to a list of items
  all_occup_ls <- as.list(parsed_content)
  
  skill<-all_occup_ls$items[[1]]
  
  info<-list()
 
  if('label'%in% options){
    info$label<-skill$label
  }else if('alternative_labels' %in% options){
    info$alternative_labels <-skill$alternative_labels
  }else if('description' %in% options){
    info$description <-skill$description
  }
  
  
  if(length(skill$knowledge_levels)==0){
    info$knowledge_level<-0
  }else{
  info$knowledge_level <-skill$knowledge_levels[[1]]}
  
  if(length(skill$language_levels)==0){
    info$language_level<-0
  }else{
    info$language_level<-skill$language_levels[[1]]
  }
  
  if(length(skill$traversal_levels)==0){
    info$traversal_level<-0
  }else{
    info$traversal_level<-skill$traversal_levels[[1]]
  }
  
  if(length(skill$skill_levels)==0){
    info$skill_level <- 0
  }else{
    info$skill_level <- skill$skill_levels[[1]]
  }
  
  info$knowledge_ancestors <-skill$knowledge_ancestors
  
  info$skill_ancestors<-skill$skill_ancestors
  info$tranversal_ancestors<-skill$tranversal_ancestors
  info$language_ancestors<-skill$language_ancestors
  
  return(info)
}

get_occupation_info<-function(occupation_id){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded"
  )
  
  params = list(
    page = '1'
  )
  
  data = list(
    ids=occupation_id
  )
  
  res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  parsed_content <- content(res, "parsed")
  # Convert the parsed content to a list of items
  occupation_list <- as.list(parsed_content)
  
  name<-occupation_list$items[[1]]$label
  return(name)
}
### propagate untill desire level
propagate_skill <-function(skill_id,pillar,level){
  skill_info <-get_skill_info(skill_id,options=c('label'))

  
  if(pillar=='S'){
    info <-skill_info$skill_level
    info_ancestors<-skill_info$skill_ancestors
    
    
    if(info==0){
      return(NULL)
    }else if(info<=level){
      return(skill_info$label)
    }else{
      headers = c(
        accept = "application/json",
        `Content-Type` = "application/x-www-form-urlencoded"
      )
      data = list(
        ids = skill_id
      )
      
      res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/utility/back-propagation", httr::add_headers(.headers=headers), body = data, encode = "form")
      parsed_content <- content(res, "parsed")
      # Convert the parsed content to a list of items
      info_ancestors<- as.list(parsed_content)
      desired_skills<-c()
      
      for (ancestor_id in info_ancestors){
        ancestor_skill <-get_skill_info(ancestor_id,options=c('label'))
        if (ancestor_skill$skill_level==level){
          desired_skills<-c(desired_skills,ancestor_skill$label)
        }
      }
      
      return(desired_skills)
    }
    
  }else if(pillar=='T'){
    info <-skill_info$tranversal_level
    info_ancestors<-skill_info$tranversal_ancestors
    if(info==0){
      return(NULL)
    }else if(info<=level){
      return(skill_info$label)
    }else{
      headers = c(
        accept = "application/json",
        `Content-Type` = "application/x-www-form-urlencoded"
      )
      data = list(
        ids = skill_id
      )
      
      res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/utility/back-propagation", httr::add_headers(.headers=headers), body = data, encode = "form")
      parsed_content <- content(res, "parsed")
      # Convert the parsed content to a list of items
      info_ancestors<- as.list(parsed_content)
      desired_skills<-c()
      
      for (ancestor_id in info_ancestors){
        ancestor_skill <-get_skill_info(ancestor_id,options=c('label'))
        if (ancestor_skill$tranversal_level==level){
          desired_skills<-c(desired_skills,ancestor_skill$label)
        }
      }
      
      return(desired_skills)
    }
    
    
  }else if(pillar=='K'){
    info <-skill_info$knowledge_level
    info_ancestors<-skill_info$knowledge_ancestors
    if(info==0){
      return(NULL)
    }else if(info<=level){
      return(skill_info$label)
    }else{
      headers = c(
        accept = "application/json",
        `Content-Type` = "application/x-www-form-urlencoded"
      )
      data = list(
        ids = skill_id
      )
      
      res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/utility/back-propagation", httr::add_headers(.headers=headers), body = data, encode = "form")
      parsed_content <- content(res, "parsed")
      # Convert the parsed content to a list of items
      info_ancestors<- as.list(parsed_content)
      desired_skills<-c()
      
      for (ancestor_id in info_ancestors){
        
        ancestor_skill <-get_skill_info(ancestor_id,options=c('label'))
        if (ancestor_skill$knowledge_level==level){
          desired_skills<-c(desired_skills,ancestor_skill$label)
        }
      }
      
      return(desired_skills)
    }
  }else if(pillar=='L'){
    info <-skill_info$language_level
    info_ancestors<-skill_info$language_ancestors
    
    if(info==0){
      return(NULL)
    }else if(info<=level){
      return(skill_info$label)
    }else{
      headers = c(
        accept = "application/json",
        `Content-Type` = "application/x-www-form-urlencoded"
      )
      data = list(
        ids = skill_id
      )
      
      res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/utility/back-propagation", httr::add_headers(.headers=headers), body = data, encode = "form")
      parsed_content <- content(res, "parsed")
      # Convert the parsed content to a list of items
      info_ancestors<- as.list(parsed_content)
      desired_skills<-c()
      for (ancestor_id in info_ancestors){
        ancestor_skill <-get_skill_info(ancestor_id,options=c('label'))
        if (ancestor_skill$language_level==level){
          desired_skills<-c(desired_skills,ancestor_skill$label)
        }
      }
      
      return(desired_skills)
    }
  }
  
  
  
}
#1.data occupation
get_all_occupations<-function(occup_level){
  page<-'1'
  page<-as.numeric(page)
  occup_level<-as.character(occup_level)
  
  occupations_list<-c()
  while(TRUE){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded"
  )
  
  params = list(
    page = page
  )
  
  data = list(
    min_level = occup_level,
    max_level = occup_level
  )
  
  res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  parsed_content <- content(res, "parsed")
  # Convert the parsed content to a list of items
  all_occup_ls <- as.list(parsed_content)
  
  if(length(all_occup_ls$items)==0){
    break
  
  }
  occupations_list<-c(occupations_list,all_occup_ls$items)
  
  
  page<-as.numeric(page)+1
  }
  occupations<-c()
  occupations_id<-c()
  for(occupation in occupations_list){
    label<-occupation$label
    occupations<-c(occupations,label)
    id<-occupation$id
    occupations_id<-c(occupations_id,id)
  }
  return(list(occupations,occupations_id))
}

#2.data skills for the occupations of interest
get_jobs<-function(occup_level,pillar,level,occup_id='no-id'){

  
  skills_all <-get_all_skills(pillar,level)
  
  skill_matrix <-initial_dataframe_construction(rep(0,1),skills_all)
  
  
  
  
  
  
  if(occup_id=='no-id'){
    occupations_info <- get_all_occupations(occup_level)
    occupation_ids<-occupations_info[2][[1]]
    all_occupations<-occupations_info[1][[1]]
  for(i in 1:length(occupation_ids)){
    id<-occupation_ids[i]
    
    occupation_i <-all_occupations[i]
    
    #print(occupation_i)
    
    headers = c(
      accept = "application/json",
      `Content-Type` = "application/x-www-form-urlencoded"
    )
    
    params = list(
      page = "1"
    )
    
    data = list(
      occupation_ids = id
    )
    
    res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/jobs", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
    
    parsed_content <- content(res, "parsed")
    # Convert the parsed content to a list of items
    all_occup_ls <- as.list(parsed_content)
    if(length(all_occup_ls$items)==0){
      next
    }
    job_list<-all_occup_ls$items
    
    for(j in 1:3){
      if(length(job_list)<j){
        next
      }
      job<-job_list[[j]]
      #print('New job')
      skills_job<-job$skills
      skills_job_wanted<-c()
      
      for(skill in skills_job){
        desired_skills<-propagate_skill(skill,pillar,level)
        skills_job_wanted<-c(skills_job_wanted,desired_skills)
      }
      
      final_skills_per_job <-unique(skills_job_wanted)
      
      binary_skill_array <-as.numeric(skills_all %in% final_skills_per_job)
      
      array_f<-c(occupation_i,binary_skill_array)
      
      skill_matrix<-rbind(skill_matrix,array_f)
      
    }
    
  }
  
  }else{
    id<-occup_id
    occupation_i <-get_occupation_info(id)
    
    headers = c(
      accept = "application/json",
      `Content-Type` = "application/x-www-form-urlencoded"
    )
    
    params = list(
      page = "1"
    )
    
    data = list(
      occupation_ids = id
    )
    
    res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/jobs", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
    
    parsed_content <- content(res, "parsed")
    # Convert the parsed content to a list of items
    all_occup_ls <- as.list(parsed_content)
    
    if(length(all_occup_ls$items)==0){
      next
    }
    
    job_list<-all_occup_ls$items
    
    for(j in 1:50){
      if(length(job_list)<j){
        next
      }
      job<-job_list[[j]]
      #print('New job')
      skills_job<-job$skills
      skills_job_wanted<-c()
      
      for(skill in skills_job){
        desired_skills<-propagate_skill(skill,pillar,level)
        skills_job_wanted<-c(skills_job_wanted,desired_skills)
      }
      
      final_skills_per_job <-unique(skills_job_wanted)
      
      binary_skill_array <-as.numeric(skills_all %in% final_skills_per_job)
      
      array_f<-c(occupation_i,binary_skill_array)
      
      skill_matrix<-rbind(skill_matrix,array_f)
      
    }
    
  }
  f_skill <- skill_matrix[2:nrow(skill_matrix),]
  return(f_skill)
  
}



