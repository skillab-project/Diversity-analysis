############ Diversity analysis + ISA #########

#### Libraries
library(readxl)
library(SpadeR)
library(DT)
library(shiny)
library(plotly)
library(fmsb)
library(tidyverse)
library(ggplot2)
library(Anthropometry)
library(shinyBS)
library(DT)
library(formatR)
library(jsonlite)
library(dplyr)
library(vegan)
library(jakR)
library(indicspecies)
library(data.table)
library(dplyr)
#### Function
Preparation_fnc<-function(res,groups,nskills=20,all_skills=FALSE){
  groups_order<-unique(groups)[order(unique(groups))]
  wanted_cols<-c("stat")
  df<-NULL
  if(ncol(res)>3){
    ind<-ncol(res)-3
    occupations<-1:ind
    for(i in occupations){
      res_wanted<-res[c(colnames(res)[i],wanted_cols)]
      
      res_f<-res_wanted[res_wanted[colnames(res)[i]]==1,]
      
      if(nrow(res_f)==0){
        next
      }
      res_f$Group<-groups_order[i]
      res_f<-res_f[,c(2,3)]
      colnames(res_f)<-c("Importance","Occupation")
      res_f$Skill<-row.names(res_f)
      res_f<-res_f[order(res_f$Importance,decreasing=TRUE),]
      
      if(!all_skills){
      if(nrow(res_f)>nskills){
        res_f<-res_f[1:nskills,]
        res_f$Importance<-round(res_f$Importance,2)
      }else{
        sum_all<-sum(res_f$Importance)
        res_f$Importance<-round(res_f$Importance,2)
      }
      }else{
        res_f$Importance<-round(res_f$Importance,2)
      }
      df<-rbind(df,res_f)
    }
  }
  row.names(df)<-NULL
  
  return(df)
}
alpha_diversity <- function(dataset){
  
  roles <- unique(dataset$Role)
  
  #months<-unique(dataset$Month)
  
  matrix<-c()
  
  for (role in roles){
    
    dataset_role <-dataset[dataset$Role==role,]
    
    skill_counts <-dataset_role[,2:ncol(dataset_role)]
    
    #final_role<-data.frame(colSums(skill_counts))
    final_role<-skill_counts
    #Sum_all
    N<-sum(final_role)
    ##Shannon Index
    H<-round(diversity(final_role),3)
    ##Simpson Index 1-D
    D<-round(diversity(final_role,"simpson"),3)
    ##Inverse Simpson index 1/D
    invD<-round(diversity(final_role,"invsimpson"),3)
    ##Species Richness
    S <- apply(final_role, 1, function(row) sum(row > 0))
    
    ## Pielous J
    J <- round(H/log(S),3)
    ## Margalef
    R <- round(S/log(N),3)
    
    df_indices <- data.frame(cbind(mean(S),mean(H),mean(D),mean(invD)))
    df_indices$Role<-role
    
    colnames(df_indices)<-c('Richness','Shannon','Simpson','Inverse_Simpson','Role')
    
    
    if (length(matrix)==0){
      matrix<-df_indices
    }else{
      matrix <-rbind(matrix,df_indices)
      
    }
    
    
    
    
  }
  return(matrix)
  
  
}
biodiversity_analysis <-function(code,option=0,code_list=NULL){
  load('./Extras/Skill_table.Rda')
  load(paste0('./data/',code,'/Skill_job_matrix.Rda'))
  results_list<-list()
  data_init<-skill_matrix_job
  
  if(nrow(data_init)==0){
    return(NULL)
  }
  data_init$Occupation_Label3<-NA
  data_init<-data_init[,c("Job_id", "Occupation_ID4","Occupation_Label4","Occupation_ID3","Occupation_Label3",
                          setdiff(colnames(data_init),c("Job_id", "Occupation_ID4","Occupation_Label4","Occupation_ID3","Occupation_Label3")))]
  if(option==1){
    for(nm in names(code_list)){
      code1<-code_list[[nm]]
      load(paste0('./data/',code1,'/Skill_job_matrix.Rda'))
      data_role<-skill_matrix_job
      data_role$Occupation_Label4<-data_role$Occupation_Label3
      data_role$Occupation_ID4<-data_role$Occupation_ID3
      
      
      data_init <- bind_rows(data_role, data_init)
      data_init[is.na(data_init)]<-0
     
    }
  }
  
  data_init <- data_init %>%
    distinct(Job_id, .keep_all = TRUE)
  
  data<-data_init
  colnames(data)[3]<-'Role'
  roles <- unique(data$Role)
  
  if(length(roles)<2){
    return(NULL)
  }else {
    nums<-table(data$Role)
    if(sum(nums>2) < 2){
      return(NULL)
    }
  }
  
  # #### Numeric matrix preparation
  # matrix <- c()
  # for(i in 1:10){
  #   for(role in roles){
  #     
  #     data_role <-data[data$Role==role,]
  #     
  #     no_selection<-max(c(round(nrow(data_role)/5),1))
  #     
  #     if(no_selection>1000){
  #       no_selection<-1000
  #     }
  #     
  # 
  #     data_sample<-sample_n(data_role,no_selection)
  #     skills <-data_sample[,6:ncol(data_sample)]
  #     numeric_counts <- sapply(skills, function(column) sum(column))
  #     final_role <-data.frame(t(numeric_counts))
  #     colnames(final_role)<-colnames(skills)
  #     final_role$Role <- role
  #     
  #     if (length(matrix)==0){
  #       matrix <- final_role
  #     }else{
  #       matrix<-rbind(matrix,final_role)
  #     }
  #     
  #   }
  # }
  # 
  # 
  
  dt <- as.data.table(data)
  result <- rbindlist(lapply(1:10, function(iter) {
    
    dt[, {
      
      no_selection <- min(max(round(.N / 5), 1), 1000)
      
      sampled <- .SD[sample(.N, no_selection)]
      
      skills <- sampled[, 6:ncol(sampled), with = FALSE]
      
      as.list(colSums(skills))
      
    }, by = Role]
    
  }), use.names = TRUE, fill = TRUE)
  matrix<-data.frame(result)
  data_aggregated<- matrix[, c("Role", setdiff(names(matrix), c("Role")))]
  dataset_sorted <-data_aggregated[order(data_aggregated$Role),]
  dataset_sorted<-dataset_sorted[!rowSums(dataset_sorted[2:ncol(dataset_sorted)])==0,]
  row_sums <- rowSums(dataset_sorted[2:ncol(dataset_sorted)])
  dataset_sorted_freq<-dataset_sorted
  dataset_sorted_freq[2:ncol(dataset_sorted)]<-dataset_sorted[2:ncol(dataset_sorted)]/row_sums
  
  
  ##### Alpha diversity function #######
  
  
  
  res_alpha<-alpha_diversity(dataset_sorted)
  res_alpha<- res_alpha[, c("Role", setdiff(names(res_alpha), c("Role")))]
  
  
  results_list$`Alpha_results`=res_alpha
  
  #### Beta diversity indices ####
 
  
  
  num<-nrow(dataset_sorted_freq)
  rownames(dataset_sorted_freq) <-1:num
  ### pairwise -- thats what we present
  res<-pairwiseAdonis(dataset_sorted_freq[2:ncol(dataset_sorted_freq)],factors=dataset_sorted_freq$Role,"bray")
  
  results_list$`Beta_results`=res
  
  #### PCOA ########
  bray_dist <- vegdist(dataset_sorted_freq[,2:ncol(dataset_sorted_freq)], method = "bray")
  pcoa_result <- cmdscale(bray_dist, k = 2, eig = TRUE)
  pcoa_df <- data.frame(PC1 = pcoa_result$points[, 1],
                        PC2 = pcoa_result$points[, 2],
                        Role = dataset_sorted_freq$Role)
  
  results_list$`PCoA_results`=pcoa_df
  

  
  


  
  ### ISA results:All ########

  data_init <- data_init[rowSums(data_init[, 6:ncol(data_init), drop = FALSE]) > 0, ]

  # Clean occupation names (important!)
  data_init$Occupation_Label4 <- trimws(data_init$Occupation_Label4)
  
  skill_cols <- 6:ncol(data_init)
  
  result <- data_init %>%
    group_by(Occupation_Label4) %>%
    group_split() %>%
    setNames(unique(data_init$Occupation_Label4)) %>%
    lapply(function(df) {
      
      freq <- colSums(df[, skill_cols, drop = FALSE])
      freq_sorted <- sort(freq, decreasing = TRUE)
      cum_prop <- cumsum(freq_sorted) / sum(freq_sorted)
      
      keep_names <- names(freq_sorted)[cum_prop <= 0.8]
      
      return(keep_names)
    })
  keep_names<-unique(unlist(result))


  # Subset dataframe
  data_init <- data_init[, c(1:5, match(keep_names, names(data_init)))]
  
  
  # Keep first 5 columns + filtered skill columns
  #data_init <- data_init[, c(1:5, skill_cols[keep_cols])]
  
  data_init$Role<-data_init$Occupation_Label4
  data_init2<-data_init[6:ncol(data_init)]
  data_init<-data_init[c('Role',setdiff(colnames(data_init2),'Role'))]
  groups <-as.factor(data_init$Role)
  
  if(length(unique(groups))>10){
    indval <- multipatt(data_init[2:ncol(data_init)], groups,
                        control = how(nperm=1),max.order=3)
  }else{
    indval <- multipatt(data_init[2:ncol(data_init)], groups,
                        control = how(nperm=1))
  }
   
  
  res<-indval$sign
  
  
  groups_all_isa<-unique(res$index)
  groups_all_isa<-groups_all_isa[!is.na(groups_all_isa)]
  
  
  res_list_chemist<-list()
  for(group in groups_all_isa){
    res_group<-res[res$index==group,]
    a<-indval$A[res$index==group,group]
    b<-indval$B[res$index==group,group]
    res_group$A<-a
    res_group$B<-b
    group_names<-colnames(indval$comb)
    res_group<-res_group[c('A','B','stat','p.value')]
    res_group<-round(res_group,3)
    res_group$Group<-group_names[group]
    res_list_chemist[[as.character(group)]]<-res_group
  }
  
  
  names_all<-names(res_list_chemist)
  name1<-names_all[1]
  df<-res_list_chemist[[name1]]
  for(name in names_all[2:length(names_all)]){
    df_n<-res_list_chemist[[name]]
    df<-rbind(df,df_n)
  }
  df_isa<-df[!is.na(df$stat),]
  
  df_isa$Skill<-rownames(df_isa)
  rownames(df_isa)<-NULL
  p<-skill_matrix$Pillar[match(df_isa$Skill,skill_matrix$PreferedLabel)]
  df_isa$Pillar<-p
  df_isa$Skill_id<-skill_matrix$Skill_id[match(df_isa$Skill,skill_matrix$PreferedLabel)]
  
  
  df_isa<-df_isa[,c('Group','Skill','Pillar','stat','Skill_id')]
  
  
  results_list$`ISA_all_groups`=df_isa
  
  ### ISA results:Seperate #########
  df_isa<-Preparation_fnc(res,groups,nskills = 15,all_skills = TRUE)
  
  p<-skill_matrix$Pillar[match(df_isa$Skill,skill_matrix$PreferedLabel)]
  df_isa$Pillar<-p
  df_isa$Skill_id<-skill_matrix$Skill_id[match(df_isa$Skill,skill_matrix$PreferedLabel)]
  
  ##### Required_skills
  df_isa<-df_isa[,c('Occupation','Skill','Pillar','Importance','Skill_id')]
  results_list$`ISA_seperate`=df_isa
  
  save(results_list,file=paste0('./data/',code,'/Diversity_results.Rda'))
  
  
  rm(list=ls())
  gc()
  return(0)
}



