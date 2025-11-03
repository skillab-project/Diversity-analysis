########################## Beta diversity #############################
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



options(rgl.useNULL = TRUE) # for headless error

beta_diversity<-function(dataset){
  dataset<-dataset[!rowSums(dataset[2:ncol(dataset)])==0,]
  row_sums <- rowSums(dataset[2:ncol(dataset)])
  dataset_freq<-dataset
  dataset_freq[2:ncol(dataset)]<-dataset[2:ncol(dataset)]/row_sums
  
  bray_dist <- vegdist(dataset_freq[,2:ncol(dataset_freq)], method = "bray",na.rm=TRUE)
  dataset_freq$Role<-as.factor(dataset_freq$Role)
  res_role <-adonis2( bray_dist ~ Role, data = dataset_freq, permutations = 999)    

  num<-nrow(dataset_freq)
  rownames(dataset_freq) <-1:num
  ### pairwise -- thats what we present
  res<-pairwiseAdonis(dataset_freq[2:ncol(dataset_freq)],factors=dataset_freq$Role,"bray")
  
  return(res)
}