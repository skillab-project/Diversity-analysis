############ Call this script for run the analysis for all the codes ##############
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

##### Job gathering from Skill_job_matrix_construction.R
codes<-unique(New_occupation_table$Codes)
groups<-unique(New_occupation_table$Group)

for(code in codes){
if (!dir.exists(paste0("data/",code))) {
  dir.create(paste0("data/",code), recursive = TRUE)
}
}
### get_token
jobs_skill_analysis(groups,token=tokenb,URL_base=url)
##### Diversity analysis for all the groups ########
for(code in codes){
  print(code)
  biodiversity_analysis(code)
}

