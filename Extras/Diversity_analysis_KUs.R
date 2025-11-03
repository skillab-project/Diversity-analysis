################# KU code alpha diversity ##############
# Load required libraries
library(httr)
library(jsonlite)
library(vegan)
library(dplyr)
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
alpha_kus_results<-function(start_time,end_time){
  BASE_URL <- "https://portal.skillab-project.eu/ku-detection"
  ENDPOINT <- "/analysis_results"
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

  kus_data<-data_df$detected_kus
  

  analysis_data<-data.frame(repo_name=data_df$repo_name)
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
colnames(res_alpha)[1]<-'Repo_name'

res_alpha[2:ncol(res_alpha)]<-round(res_alpha[2:ncol(res_alpha)],2)
return(res_alpha)
}

