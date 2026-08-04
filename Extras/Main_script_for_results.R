############ Call this script for run the analysis for all the codes ##############
load('./Extras/New_occupation_table2.Rda')
load('./Extras/New_occupation_table.Rda')
load('./Extras/Skill_table.Rda')
source('./Extras/Diversity_Analysis.R')
source('./Extras/Skill_job_matrix_construction.R')
library(httr)
library(dotenv)
################ Authetication ##########################
get_valid_token <- function(API_BASE_URL,USERNAME,PASSWORD) {
  response <- POST(
    paste0(API_BASE_URL, "/login"),
    body = list(username = USERNAME, password = PASSWORD),
    encode = "json",
    add_headers(
      "accept" = "application/json",
      "Content-Type" = "application/json"
    ),
    verbose()  # Debug authentication
  )
  
  if (status_code(response) == 200) {
    token <- gsub('"', "", content(response, "text"))
    cat("Token acquired:", substr(token, 1, 20), "...\n")  # Log partial token
    return(token)
  } else {
    stop(sprintf("Auth failed (Status %d): %s", 
                 status_code(response),
                 content(response, "text")))
  }
}


dotenv::load_dot_env(".env")
user <- Sys.getenv("USERNAME")
pass <- Sys.getenv("PASSWORD")
url<-Sys.getenv("URL")
tokenb<-get_valid_token(url,user,pass)


########################## Upper level ##########################
##### Job gathering from Skill_job_matrix_construction.R
codes<-unique(New_occupation_table2$Codes)
groups<-unique(New_occupation_table2$Group)
for(code in codes){
  if (!dir.exists(paste0("data/",code))) {
    dir.create(paste0("data/",code), recursive = TRUE)
  }
}
### get_token
groups2<-c(107,108,109,110,111)
groups<-groups[!groups %in% groups2]

jobs_skill_analysis(groups,token=tokenb,URL_base=url,occupation_table=New_occupation_table2)
##### Diversity analysis for all the groups ########
#codes<-c("C2511","C2512","C2513","C2514","C2519")
for(code in codes){
  print(code)
  biodiversity_analysis(code)
}


########################## Level 4 ##########################
##### Job gathering from Skill_job_matrix_construction.R
codes<-unique(New_occupation_table$Codes)
groups<-unique(New_occupation_table$Group)

for(code in codes){
  if (!dir.exists(paste0("data/",code))) {
    dir.create(paste0("data/",code), recursive = TRUE)
  }
}
### get_token
jobs_skill_analysis(groups,token=tokenb,URL_base=url,occupation_table=New_occupation_table)
##### Diversity analysis for all the groups ########
for(code in codes){
  print(code)
  names_all<-New_occupation_table$Label4[New_occupation_table$Codes == code]
  codes<-New_occupation_table2[New_occupation_table2$label3 %in% names_all,c("label3","Codes")]
  code_list<-list()
  for(code1 in unique(codes$Codes)){
    nm<-codes$label3[codes$Codes==code1]
    code_list[nm]<-code1
  }
  biodiversity_analysis(code,option=1,code_list)
}





############################# Profiles query (based on availabilty on ) ####################



