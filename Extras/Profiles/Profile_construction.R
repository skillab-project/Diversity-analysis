#install.packages("arrow")
library(arrow)
rm(list=ls())
load('./Skill_table.Rda')
load('./New_occupation_table2.Rda')

d<-read_parquet("C:/Users/30697/Desktop/SKILLAB/Services/Services_current/Diversity_service_working/Extras/Profiles/profiles_C2514_with_skills.parquet")

d_wanted<-d[c(1,24,25)]

find_cands_with_zero_skills <- which(lengths(d_wanted$profile_skills) == 0L)


profiles<-d_wanted[-find_cands_with_zero_skills,]



tmp <- vapply(profiles$esco_uris, function(x) {
  c(x[1], x[2])
}, character(2))

profiles$occupation_1 <- tmp[1, ]
profiles$occupation_2 <- tmp[2, ]



profile_skill_list_construction<-function(jobs){
  skill_list <- setNames(
    lapply(jobs$profile_skills, unlist),
    as.character(jobs$user_id)
  )
  return(skill_list)
}

occupation_profiles<-list()
arr<-c()
for(occupation in unique(profiles$occupation_1)){
  profile_occupation<-profile_skill_list_construction(profiles[profiles$occupation_1==occupation,])
  occupation_profiles[[occupation]]<-profile_occupation
  arr<-c(arr,profile_occupation)
}



## find all the unique skills and make a dataframe for all the jobs/occupations
all_skills<-unlist(arr)
unique_skills<-unique(all_skills)
un_skills<-unique_skills[unique_skills %in% skill_matrix$Skill_id]


job_skill_matrix_construction<-function(unique_skills,occupations_jobs,arr,skill_matrix,occupation_table){
  # -----------------------------
  # 1. Flatten occupations_jobs (FAST)
  # -----------------------------
  occupations_id4 <- rep(
    names(occupations_jobs),
    lengths(occupations_jobs)
  )
  
  jobs_ids <- unlist(lapply(occupations_jobs, names), use.names = FALSE)
  
  df <- data.frame(
    Job_id = jobs_ids,
    Occupation_ID4 = occupations_id4,
    stringsAsFactors = FALSE
  )
  
  # -----------------------------
  # 2. Fast lookup maps (NO match() loops)
  # -----------------------------
  id4 <- occupation_table$id4
  
  label4_map <- setNames(occupation_table$label4, id4)
  label3_map <- setNames(occupation_table$label3, id4)
  id3_map    <- setNames(occupation_table$id3, id4)
  
  df$Occupation_Label4 <- label4_map[df$Occupation_ID4]
  df$Occupation_Label3 <- label3_map[df$Occupation_ID4]
  df$Occupation_ID3    <- id3_map[df$Occupation_ID4]
  
  # -----------------------------
  # 3. Skill name mapping (vectorized)
  # -----------------------------
  skill_names <- skill_matrix$PreferedLabel[
    match(unique_skills, skill_matrix$Skill_id)
  ]
  skill_names <- skill_names[!is.na(skill_names)]
  
  
  skill_index <- setNames(seq_along(skill_names), skill_names)
  job_index <- setNames(seq_len(nrow(df)), df$Job_id)
  
  arr_idx <- lapply(arr, function(skills) {
    mapped <- skill_matrix$PreferedLabel[
      match(skills, skill_matrix$Skill_id)
    ]
    skill_index[mapped]
  })
  
  job_ids <- rep(names(arr_idx), lengths(arr_idx))
  skill_ids <- unlist(arr_idx, use.names = FALSE)
  
  valid <- !is.na(skill_ids)
  
  i <- job_index[job_ids[valid]]
  j <- skill_ids[valid]
  
  skill_mat <- matrix(0L, nrow(df), length(skill_names))
  skill_mat[cbind(i, j)] <- 1L
  
  df <- cbind(df, skill_mat)
  colnames(df)[6:ncol(df)]<-skill_names
  
  return(df)
}

profile_matrix_occupation<-job_skill_matrix_construction(un_skills,occupation_profiles,arr,skill_matrix,New_occupation_table2)



save(profile_matrix_occupation,file='./Profiles/C2514.Rda')

rm(list=ls())
gc()
