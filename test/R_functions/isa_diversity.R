###################### ISA script ######################
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
set.seed(123)
isa_function<-function(dataset){
  load('./Extras/Skill_table.Rda')
  groups <-as.factor(dataset$Role)
  indval <- multipatt(dataset[2:ncol(dataset)], groups,
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
  
  return(df_isa)
}
