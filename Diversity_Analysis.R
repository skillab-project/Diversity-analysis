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

#### Function
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
biodiversity_analysis <-function(code){
  load('./Extras/New_occupation_table.Rda')
  load('./Extras/Skill_table.Rda')
  load(paste0('./data/',code,'/Skill_job_matrix.Rda'))
  results_list<-list()
  data<-skill_matrix_job
  colnames(data)[3]<-'Role'
  roles <- unique(data$Role)
  
  if(length(roles)>1){
    #### Numeric matrix preparation
    matrix <- c()
    for(i in 1:100){
      for(role in roles){
        
        data_role <-data[data$Role==role,]
        
        no_selection<-max(c(round(nrow(data_role)/5),1))
        
        if(no_selection>1000){
          no_selection<-1000
        }
        
        
        
        
        data_sample<-sample_n(data_role,no_selection)
        skills <-data_sample[,6:ncol(data_sample)]
        numeric_counts <- sapply(skills, function(column) sum(column))
        final_role <-data.frame(t(numeric_counts))
        colnames(final_role)<-colnames(skills)
        final_role$Role <- role
        
        if (length(matrix)==0){
          matrix <- final_role
        }else{
          matrix<-rbind(matrix,final_role)
        }
        
      }
    }
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
    bray_dist <- vegdist(dataset_sorted_freq[,2:ncol(dataset_sorted_freq)], method = "bray",na.rm=TRUE)
    dataset_sorted_freq$Role<-as.factor(dataset_sorted_freq$Role)
    res_role <-adonis2( bray_dist ~ Role, data = dataset_sorted_freq, permutations = 999)    
    
    
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
    groups <-as.factor(dataset_sorted$Role)
    indval <- multipatt(dataset_sorted[2:ncol(dataset_sorted)], groups,
                        control = how(nperm=99)) 
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
    groups <-as.factor(dataset_sorted$Role)
    indval <- multipatt(dataset_sorted[2:ncol(dataset_sorted)], groups,duleg=TRUE,
                        control = how(nperm=99)) 
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
    
    ##### Required_skills
    df_isa<-df_isa[,c('Group','Skill','Pillar','stat','Skill_id')]
    
    results_list$`ISA_seperate`=df_isa
    
    save(results_list,file=paste0('./data/',code,'/Diversity_results.Rda')) 
  }else{
    results_list$`Alpha_results`=NULL
    results_list$`ISA_seperate`=NULL
    results_list$`ISA_all_groups`=NULL
    results_list$`PCoA_results`=NULL
    results_list$`Beta_results`=NULL
    save(results_list,file=paste0('./data/',code,'/Diversity_results.Rda')) 
    
    }
  
  return(0)
}

