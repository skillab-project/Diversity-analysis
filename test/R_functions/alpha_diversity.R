################# Alpha diversity #####################

#### Libraries
library(readxl)
library(SpadeR)
library(DT)
library(shiny)
library(plotly)
library(fmsb)
library(ggradar)
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


###Function
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
