load('./Extras/New_occupation_table2.Rda')
ids_all<-New_occupation_table2$id4




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


dotenv::load_dot_env("../.env")
user <- Sys.getenv("USERNAME")
pass <- Sys.getenv("PASSWORD")
url<-Sys.getenv("URL")
tokenb<-get_valid_token(url,user,pass)
ids_all<-New_occupation_table2$id4
names_all<-New_occupation_table2$label4
get_number_profiles<-function(occupation_id,tokenb){
  headers = c(
    accept = "application/json",
    Authorization = paste("Bearer",tokenb),
    `Content-Type` = "application/x-www-form-urlencoded"
  )
  
  params = list(
    page = "1"
  )
  
  data = list(
    occupation_uris = occupation_id,
    sources = "revelio"
  )
  
  res <- httr::POST(url = "https://skillab-tracker.csd.auth.gr/api/profiles", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")

  parsed_content <- content(res, "parsed")
  profiles_page1<-parsed_content$items
  count_of_profiles<-parsed_content$count
  
  return(count_of_profiles)
  }

num<-rep(0,length(ids_all))

for(i in 1:length(ids_all)){
  print(i)
  num1<-get_number_profiles(ids_all[i],tokenb)
  num[i]<-num1
  
}
df<-data.frame(Name=names_all,Counts=num)
