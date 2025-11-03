################# KU code alpha diversity ##############
# Load required libraries
library(httr)
library(jsonlite)
library(vegan)
library(dplyr)
library(jakR)
library(indicspecies)


options(rgl.useNULL = TRUE) # for headless error


# Set base URL and endpoint
################## Alpha diversity analysis ###############
alpha_diversity_kus <- function(dataset){
  
  roles <- unique(dataset$Name)
  
  #months<-unique(dataset$Month)
  
  matrix<-c()
  
  for (role in roles){
    
    dataset_role <-dataset[dataset$Name==role,]
    
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
    df_indices$Name<-role
    
    colnames(df_indices)<-c('Richness','Shannon','Simpson','Inverse_Simpson','Name')
    
    
    if (length(matrix)==0){
      matrix<-df_indices
    }else{
      matrix <-rbind(matrix,df_indices)
      
    }
    
    
    
    
  }
  return(matrix)
  
  
}
kus_results<-function(start_time,end_time){
  results_list<-list()
  BASE_URL <- "https://portal.skillab-project.eu/ku-detection"
  ENDPOINT <- "/developer_ku_overview"
  api_url <- paste0(BASE_URL, ENDPOINT)
  
  # Add query parameters
  query_params <- list(
    start_date = start_time,
    end_date = end_time
  )
  
  # Make GET request with query parameters
  response <- GET(api_url, query = query_params, timeout(60))
  
  content_json <- content(response, as = "text", encoding = "UTF-8")
  
  # Convert JSON to data frame
  data_df <- fromJSON(content_json)
  
  kus_data<-data_df$ku_vector
  
  
  analysis_data<-data.frame(Organization=data_df$organization)
  
  for(col in colnames(kus_data)){
    analysis_data[[col]]<-kus_data[[col]]
  }
  
  
  
  
  
  colnames(analysis_data)[1]<-'Name'
  
  data_wanted<-analysis_data
  
  names<-unique(data_wanted$Name)
  matrix <- c()
  data<-data_wanted
  
  for(i in 1:10){
    for(name in names){
      
      data_role <-data[data$Name==name,]
      
      no_selection<-max(c(round(nrow(data_role)/5),1))
      
      data_sample<-sample_n(data_role,no_selection)
      
      skills <-data_sample[,2:ncol(data_sample)]
      
      numeric_counts <- sapply(skills, function(column) sum(column))
      
      final_role <-data.frame(t(numeric_counts))
      
      colnames(final_role)<-colnames(skills)
      
      final_role$Name <- name
      
      if (length(matrix)==0){
        matrix <- final_role
      }else{
        matrix<-rbind(matrix,final_role)
      }
      
    }
  }
  
  data_aggregated<- matrix[, c("Name", setdiff(names(matrix), c("Name")))]
  
  dataset_sorted <-data_aggregated[order(data_aggregated$Name),]
  
  dataset_sorted<-dataset_sorted[!rowSums(dataset_sorted[2:ncol(dataset_sorted)])==0,]
  
  row_sums <- rowSums(dataset_sorted[2:ncol(dataset_sorted)])
  
  ##### Alpha diversity function #######
  
  res_alpha<-alpha_diversity_kus(dataset_sorted)
  
  res_alpha<- res_alpha[,c("Name",setdiff(names(res_alpha),c("Name")))]
  colnames(res_alpha)[1]<-'Organization'
  
  res_alpha[2:ncol(res_alpha)]<-round(res_alpha[2:ncol(res_alpha)],2)
  
  results_list$Alpha_Diversity=res_alpha
  
  #### Beta diversity indices ####
  dataset_sorted_freq<-dataset_sorted
  dataset_sorted_freq[2:ncol(dataset_sorted)]<-dataset_sorted[2:ncol(dataset_sorted)]/row_sums
  
  bray_dist <- vegdist(dataset_sorted_freq[,2:ncol(dataset_sorted_freq)], method = "bray",na.rm=TRUE)
  dataset_sorted_freq$Name<-as.factor(dataset_sorted_freq$Name)
  
  res_role <-adonis2( bray_dist ~ Name, data = dataset_sorted_freq, permutations = 999)    
  
  num<-nrow(dataset_sorted_freq)
  rownames(dataset_sorted_freq) <-1:num
  ### pairwise -- thats what we present
  res<-pairwiseAdonis(dataset_sorted_freq[2:ncol(dataset_sorted_freq)],factors=dataset_sorted_freq$Name,"bray")
  
  results_list$`Beta_results`=res
  
  
  
  #### PCOA ########
  bray_dist <- vegdist(dataset_sorted_freq[,2:ncol(dataset_sorted_freq)], method = "bray")
  pcoa_result <- cmdscale(bray_dist, k = 2, eig = TRUE)
  pcoa_df <- data.frame(PC1 = pcoa_result$points[, 1],
                        PC2 = pcoa_result$points[, 2],
                        Organization = dataset_sorted_freq$Name)
  
  results_list$`PCoA_results`=pcoa_df
  
  
  groups <-as.factor(dataset_sorted$Name)
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
  if(length(names_all)>1){
    name1<-names_all[1]
    df<-res_list_chemist[[name1]]
    for(name in names_all[2:length(names_all)]){
      df_n<-res_list_chemist[[name]]
      df<-rbind(df,df_n)
    }
    df_isa<-df[!is.na(df$stat),]
    
    df_isa$KU<-rownames(df_isa)
    rownames(df_isa)<-NULL
    df_isa<-df_isa[c('KU',setdiff(colnames(df_isa),'KU'))]
  }else{
    name1<-names_all
    df<-res_list_chemist[[name1]]
    
    
    df_isa<-df[!is.na(df$stat),]
    
    df_isa$KU<-rownames(df_isa)
    rownames(df_isa)<-NULL
    df_isa<-df_isa[c('KU',setdiff(colnames(df_isa),'KU'))]
    
  }
  
  
  results_list$ISA=df_isa
  return(results_list)
}



