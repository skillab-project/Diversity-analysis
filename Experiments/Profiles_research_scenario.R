############################ Calculation of the competition ##########################
######### Function ############
Fit_score_fnc<-function(candidate_skills,results_isa,ratio=0.5,Pillar="all"){
  #candidate_skills<-results_isa$Skill_id[c(2,10,12,15)]
  # 2. Matching logic
  required_skills <- results_isa$Skill
  
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


##################### Define the function ################
Fit.Competition<-function(target_occupation,level,profile_matrix_occupation,skills,ratio1=1,ratio2=0.5,
                          ratio3=0.2,minimum.number.of.skills=1,skill.threshold.1=20,skill.threshold.2=40,Pillar="all"){
  
  fits<-c()
  if(level==3){
    profiles_occupation<-profile_matrix_occupation[profile_matrix_occupation$Occupation_Label3==target_occupation,]
    skills_occupation<-skills[skills$Occupation==target_occupation,]
    if(length(skills_occupation)==0){
      message("No skills for that occupation")
      return(fits)
    }
    
    if(all(skills_occupation$Skill %in% colnames(profiles_occupation))){
      profiles_f<-profiles_occupation[skills_occupation$Skill]
      cands<-profiles_f[rowSums(profiles_f)>minimum.number.of.skills,]

      if(nrow(cands)==0){
        message("No candidates with more than 2 required skills")
        return(fits)
      }
      
      for(i in 1:nrow(cands)){
        if(ncol(cands)>skill.threshold.2){
          r<-Fit_score_fnc(colnames(cands[i,cands[i,]==1]),skills_occupation,ratio=ratio3,Pillar=Pillar)
          fits<-c(fits,r)
        }else if(ncol(cands)<=skill.threshold.1){
          r<-Fit_score_fnc(colnames(cands[i,cands[i,]==1]),skills_occupation,ratio=ratio1,Pillar=Pillar)
          fits<-c(fits,r)
        }else{
          r<-Fit_score_fnc(colnames(cands[i,cands[i,]==1]),skills_occupation,ratio=ratio2,Pillar=Pillar)
          fits<-c(fits,r)
        }
        
      }
      
      return(fits)
      
    }else{
      skills_not_in<-skills_occupation$Skill[!skills_occupation$Skill %in% colnames(profiles_occupation)]
      for(skill in skills_not_in){
        profiles_occupation[[skill]]<-0
      }
      profiles_f<-profiles_occupation[skills_occupation$Skill]
      cands<-profiles_f[rowSums(profiles_f)>minimum.number.of.skills,]

      if(nrow(cands)==0){
        message("No candidates with more than 2 required skills")
        return(fits)
      }
      for(i in 1:nrow(cands)){
        if(ncol(cands)>skill.threshold.2){
          r<-Fit_score_fnc(colnames(cands[i,cands[i,]==1]),skills_occupation,ratio=ratio3,Pillar=Pillar)
          fits<-c(fits,r)
        }else if(ncol(cands)<=skill.threshold.1){
          r<-Fit_score_fnc(colnames(cands[i,cands[i,]==1]),skills_occupation,ratio=ratio1,Pillar=Pillar)
          fits<-c(fits,r)
        }else{
          r<-Fit_score_fnc(colnames(cands[i,cands[i,]==1]),skills_occupation,ratio=ratio2,Pillar=Pillar)
          fits<-c(fits,r)
        }
      }
      return(fits)
    }
    
    
  }else{
    profiles_occupation<-profile_matrix_occupation[profile_matrix_occupation$Occupation_Label4==target_occupation,]
    skills_occupation<-skills[skills$Occupation==target_occupation,]
    if(length(skills_occupation)==0){
      message("No skills for that occupation")
      return(fits)
    }
    
    
    
    
    if(all(skills_occupation$Skill %in% colnames(profiles_occupation))){
      profiles_f<-profiles_occupation[skills_occupation$Skill]
      cands<-profiles_f[rowSums(profiles_f)>minimum.number.of.skills,]
      
      if(nrow(cands)==0){
        message("No candidates with more than 2 required skills")
        return(fits)
      }
      
      for(i in 1:nrow(cands)){
        if(ncol(cands)>skill.threshold.2){
          r<-Fit_score_fnc(colnames(cands[i,cands[i,]==1]),skills_occupation,ratio=ratio3,Pillar=Pillar)
          fits<-c(fits,r)
        }else if(ncol(cands)<=skill.threshold.1){
          r<-Fit_score_fnc(colnames(cands[i,cands[i,]==1]),skills_occupation,ratio=ratio1,Pillar=Pillar)
          fits<-c(fits,r)
        }else{
          r<-Fit_score_fnc(colnames(cands[i,cands[i,]==1]),skills_occupation,ratio=ratio2,Pillar=Pillar)
          fits<-c(fits,r)
        }
        
      }
      
      return(fits)
      
    }else{
      skills_not_in<-skills_occupation$Skill[!skills_occupation$Skill %in% colnames(profiles_occupation)]
      for(skill in skills_not_in){
        profiles_occupation[[skill]]<-0
      }
      profiles_f<-profiles_occupation[skills_occupation$Skill]
      cands<-profiles_f[rowSums(profiles_f)>minimum.number.of.skills,]
      
      if(nrow(cands)==0){
        message("No candidates with more than 2 required skills")
        return(fits)
      }
      for(i in 1:nrow(cands)){
        if(ncol(cands)>skill.threshold.2){
          r<-Fit_score_fnc(colnames(cands[i,cands[i,]==1]),skills_occupation,ratio=ratio3,Pillar=Pillar)
          fits<-c(fits,r)
        }else if(ncol(cands)<=skill.threshold.1){
          r<-Fit_score_fnc(colnames(cands[i,cands[i,]==1]),skills_occupation,ratio=ratio1,Pillar=Pillar)
          fits<-c(fits,r)
        }else{
          r<-Fit_score_fnc(colnames(cands[i,cands[i,]==1]),skills_occupation,ratio=ratio2,Pillar=Pillar)
          fits<-c(fits,r)
        }
      }
      
      return(fits)
    }
  }
  
}





load('./Extras/Profiles/C2512.Rda')
load('./data/C2512/Diversity_results.Rda')
skills<-results_list$ISA_seperate
unique_occupations<-unique(skills$Occupation)
unique_profiles<-unique(profile_matrix_occupation$Occupation_Label4)
common<-unique_profiles[unique_profiles %in% unique_occupations]
res_all<-list()
for(label in common){
  print(label)
  res<-Fit.Competition(label,level=4,profile_matrix_occupation,skills,Pillar="K")
  res_all[[label]]<-res
}


save(res_all,file='./Experiments/C2512_FitScores_K.Rda')
rm(list=ls())
gc()





load('./data/C251/Diversity_results.Rda')
skills<-results_list$ISA_seperate
unique_occupations<-unique(skills$Occupation)
#unique_profiles<-unique(profile_matrix_occupation$Occupation_Label4)
common<-unique_occupations[c(1,3,4,5)]
res_all<-list()
for(label in common){
  print(label)
  if(label=="Applications programmers"){
    load('./Extras/Profiles/C2514.Rda')
  }else if(label=="Software developers"){
    load('./Extras/Profiles/C2512.Rda')
  }else if(label=="Systems analysts"){
    load('./Extras/Profiles/C2511.Rda')
  }else if(label=="Web and multimedia developers"){
    load('./Extras/Profiles/C2513.Rda')
  }
  
  res<-Fit.Competition(label,level=3,profile_matrix_occupation,skills)
  
  res_all[[label]]<-res
  rm(list=ls(profile_matrix_occupation))
  gc()
}


save(res_all,file='./Experiments/C251_FitScores.Rda')
rm(list=ls())
gc()
