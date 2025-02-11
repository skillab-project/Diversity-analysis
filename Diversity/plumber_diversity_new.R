############# Function for diversity results ###################
# plumber.R
# Load necessary libraries
library(plumber)
library(readxl)
library(SpadeR)
library(DT)
library(plotly)
library(fmsb)
library(ggradar)
library(tidyverse)
library(ggplot2)
library(archetypes)
library(Anthropometry)
library(shinyBS)
library(formatR)
library(jsonlite)
library(ggplot2)

### ALPHA
load('./Results_Alpha_final_with_repetitions.Rda')
# Define months
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September")
# Loop through each month and replace in the Month column
for (month in months) {
  res_alpha$Month <- gsub(paste0(month, "_1"), paste0(month, " (First Half)"), res_alpha$Month)
  res_alpha$Month <- gsub(paste0(month, "_2"), paste0(month, " (Second Half)"), res_alpha$Month)
}
### BETA
load('./Results_Beta_pairwise_role.Rda')
###PCOA
load('./PCOA_result.Rda')
plt<-ggplot(pcoa_df, aes(x = PC1, y = PC2, color = Role)) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(title = "PCoA of Ecosystems Based on Bray-Curtis Dissimilarity",
       x = "PC1", y = "PC2")
ggsave("./PCOA_plot.png",plot=plt,width=8,height=6,dpi=300)
### ISA
load('./isa_all.Rda')
load('./isa_singlets_pairs_triplets.Rda')
groups<-unique(data$Group)
unique_occupations <- unique(unlist(strsplit(groups, "\\+")))
group_all<-paste(unique_occupations,collapse= "+")
rest_skills$Group<-rep(group_all,nrow(rest_skills))


rest_skills<-rest_skills[,colnames(rest_skills) %in% c('Group','Species','Pillar','stat')]
colnames(rest_skills)<-c('Skills','Pillar','Value','Group')
data<-data[,colnames(data) %in% c('Group','Species','Pillar','Stat')]
colnames(data)<-c('Group','Skills','Pillar','Value')
rest_skills<-rest_skills[,colnames(data)]

all_df<-rbind(data,rest_skills)
all_df$Value<-round(as.numeric(all_df$Value),3)



#* @apiTitle Diversity Microservice
#* @apiDescription API for diversity calculators

#* Find required skills for occupation requested
#* @param occupation_name:string Optional for this stage
#* @post /diversity
function(occupation_name) {
  
  res_alpha<-res_alpha
  res_beta<-res
  res_isa<-all_df
  res_pcoa<-pcoa_df
  
  # Call the main diversity function
  tryCatch({
    
    
    data = list(res_alpha,res_beta,res_pcoa,res_isa)
    
  }, error = function(e) {
    message = e$message
    
  })
}


