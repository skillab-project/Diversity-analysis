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


dotenv::load_dot_env("C:/Users/30697/Desktop/SKILLAB/Services/Services/.env")
user <- Sys.getenv("USERNAME")
pass <- Sys.getenv("PASSWORD")
url<-Sys.getenv("URL")
tokenb<-get_valid_token(url,user,pass)


########################## Occupations for level 1 ################################
headers = c(
  accept = "application/json",
  `Content-Type` = "application/x-www-form-urlencoded",
  `Authorization` = paste("Bearer",tokenb)
)

params = list(
  page = "1"
)

data = list(
  max_level = "1"
)

res <- httr::POST(url = "https://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
parsed_content <- content(res, "parsed")
# Convert the parsed content to a list of items
occupation_list_lv1 <- as.list(parsed_content)$items
no_occupations<-as.list(parsed_content)$count



ids_lvl1<-c()
labels_lv1<-list()
for(index in 1:length(occupation_list_lv1)){
  occ_i <-occupation_list_lv1[[index]]
  id_i<-occ_i$id
  name_i<-occ_i$label
  
  labels_lv1[[id_i]]<-name_i
  ids_lvl1<-c(ids_lvl1,id_i)
}

###################### Occupations for level 2: from children in lvl 1 ####################
ids_lvl2<-c()
childrens_lv1<-list()
for(id in ids_lvl1){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded",
    `Authorization` = paste("Bearer",tokenb)
  )
  
  params = list(
    page = "1"
  )
  
  
  
  data = list(
    ids = id
  )
  
  res <- httr::POST(url = "https://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  
  parsed_content <- content(res, "parsed")
  # Convert the parsed content to a list of items
  data<-parsed_content$items
  temp_id2<-c()
  for(item in data){
    for(children in item$children){
      id_2<-children
      
      ids_lvl2<-c(ids_lvl2,id_2)
      
      temp_id2<-c(temp_id2,id_2)
    }
  }
  childrens_lv1[[id]] <- temp_id2
  
}
labels_lv2<-list()
for(id2 in ids_lvl2){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded",
    `Authorization` = paste("Bearer",tokenb)
  )
  
  params = list(
    page = "1"
  )
  
  data = list(
    ids=id2
  )
  
  res <- httr::POST(url = "https://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  parsed_content <- content(res, "parsed")
  name<-parsed_content$items[[1]]$label
  labels_lv2[[id2]]<-name
}

#################### Occupations for level 3: from children in lvl 2###############
ids_lvl3<-c()
childrens_lv2<-list()
for(id in ids_lvl2){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded",
    `Authorization` = paste("Bearer",tokenb)
  )
  
  params = list(
    page = "1"
  )
  
  data = list(
    ids = id
  )
  
  res <- httr::POST(url = "https://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  
  parsed_content <- content(res, "parsed")
  # Convert the parsed content to a list of items
  data<-parsed_content$items
  temp_id2<-c()
  for(item in data){
    for(children in item$children){
      id_2<-children
      ids_lvl3<-c(ids_lvl3,id_2)
      temp_id2<-c(temp_id2,id_2)
    }
  }
  childrens_lv2[[id]] <- temp_id2
  
}
labels_lv3<-list()
for(id in ids_lvl3){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded",
    `Authorization` = paste("Bearer",tokenb)
  )
  
  params = list(
    page = "1"
  )
  
  data = list(
    ids=id
  )
  
  res <- httr::POST(url = "https://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  parsed_content <- content(res, "parsed")
  name<-parsed_content$items[[1]]$label
  labels_lv3[[id]]<-name
}

##################### Occupations for level 4: from children in lvl3 ###########

ids_lvl4<-c()
childrens_lv3<-list()
for(id in ids_lvl3){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded",
    `Authorization` = paste("Bearer",tokenb)
  )
  
  params = list(
    page = "1"
  )
  
  data = list(
    ids = id
  )
  
  res <- httr::POST(url = "https://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  
  parsed_content <- content(res, "parsed")
  # Convert the parsed content to a list of items
  data<-parsed_content$items
  temp_id2<-c()
  for(item in data){
    for(children in item$children){
      id_2<-children
      ids_lvl4<-c(ids_lvl4,id_2)
      temp_id2<-c(temp_id2,id_2)
    }
  }
  childrens_lv3[[id]] <- temp_id2
  
}
labels_lv4<-list()
for(id in ids_lvl4){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded",
    `Authorization` = paste("Bearer",tokenb)
  )
  
  params = list(
    page = "1"
  )
  
  data = list(
    ids=id
  )
  
  res <- httr::POST(url = "https://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  parsed_content <- content(res, "parsed")
  name<-parsed_content$items[[1]]$label
  labels_lv4[[id]]<-name
}

##################### Occupations for level 5: from children in lvl4 ###########
ids_lvl5<-c()
childrens_lv4<-list()
for(id in ids_lvl4){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded",
    `Authorization` = paste("Bearer",tokenb)
  )
  
  params = list(
    page = "1"
  )
  
  data = list(
    ids = id
  )
  
  res <- httr::POST(url = "https://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  
  parsed_content <- content(res, "parsed")
  # Convert the parsed content to a list of items
  data<-parsed_content$items
  temp_id2<-c()
  for(item in data){
    for(children in item$children){
      id_2<-children
      ids_lvl5<-c(ids_lvl5,id_2)
      temp_id2<-c(temp_id2,id_2)
    }
  }
  childrens_lv4[[id]] <- temp_id2
  
}
labels_lv5<-list()
for(id in ids_lvl5){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded",
    `Authorization` = paste("Bearer",tokenb)
  )
  
  params = list(
    page = "1"
  )
  
  data = list(
    ids=id
  )
  
  res <- httr::POST(url = "https://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  parsed_content <- content(res, "parsed")
  name<-parsed_content$items[[1]]$label
  labels_lv5[[id]]<-name
}



######################## Occupations table correct #######
ids1<-c()
ids2<-c()
ids3<-c()
ids4<-c()
ids5<-c()
for(id4 in ids_lvl4){
  ids4<-c(ids4,id4)
  matches <- sapply(childrens_lv3, function(x) id4 %in% x)
  # Get the names (keys) where TRUE
  id3<-names(childrens_lv3)[matches]
  ids3<-c(ids3,id3)
  
  matches2 <- sapply(childrens_lv2, function(x) id3 %in% x)
  id2<-names(childrens_lv2)[matches2]
  ids2<-c(ids2,id2)
  
  matches1<-sapply(childrens_lv1,function(x) id2 %in% x)
  id1<-names(childrens_lv1)[matches1]
  ids1<-c(ids1,id1)
}
Occupations_tablelv4<-data.frame(id1=ids1,id2=ids2,id3=ids3,id4=ids4)

ids1<-c()
ids2<-c()
ids3<-c()
ids4<-c()
ids5<-c()
for(id5 in ids_lvl5){
  ids5<-c(ids5,id5)
  matches4<-sapply(childrens_lv4, function(x) id5 %in% x)
  id4<-names(childrens_lv4)[matches4]
  
  
  ids4<-c(ids4,id4)
  matches <- sapply(childrens_lv3, function(x) id4 %in% x)
  # Get the names (keys) where TRUE
  id3<-names(childrens_lv3)[matches]
  ids3<-c(ids3,id3)
  
  matches2 <- sapply(childrens_lv2, function(x) id3 %in% x)
  id2<-names(childrens_lv2)[matches2]
  ids2<-c(ids2,id2)
  
  matches1<-sapply(childrens_lv1,function(x) id2 %in% x)
  id1<-names(childrens_lv1)[matches1]
  ids1<-c(ids1,id1)
}
Occupations_table<-data.frame(id1=ids1,id2=ids2,id3=ids3,id4=ids4,id5=ids5)



df1 <- Occupations_table[, c("id1", "id2", "id3", "id4")]
df2 <- Occupations_tablelv4

diff2 <- df2[!do.call(paste, df2) %in% do.call(paste, df1), ]
diff2$id5<-NA


final<-rbind(Occupations_table,diff2)




library(dplyr)
Occupations_table<-Occupations_tablelv4%>%
  mutate(labe1=unlist(labels_lv1[id1]),label2=unlist(labels_lv2[id2]),
         label3=unlist(labels_lv3[id3]),label4=unlist(labels_lv4[id4]))
r<-data.frame(Occupations_table)



labels_lv1_vec <- unlist(labels_lv1)
labels_lv2_vec <- unlist(labels_lv2)
labels_lv3_vec <- unlist(labels_lv3)
labels_lv4_vec <- unlist(labels_lv4)
labels_lv5_vec <- unlist(labels_lv5)

Occupations_table <- final %>%
  mutate(
    label1 = labels_lv1_vec[id1],
    label2 = labels_lv2_vec[id2],
    label3 = labels_lv3_vec[id3],
    label4 = labels_lv4_vec[id4],
    label5 = labels_lv5_vec[id5]
  )
r2<-data.frame(Occupations_table)

New_occupation_table<-r

#writexl::write_xlsx(r,path='C:/Users/30697/Desktop/SKILLAB/EURES/Occupations_lvl5.xlsx')
#writexl::write_xlsx(r2,path='C:/Users/30697/Desktop/SKILLAB/EURES/Occupations_lvl6.xlsx')

load('New_occupation_table2.Rda')

r<-New_occupation_table
codes<-c()
r$Codes<-NA
for(row in 1:nrow(r)){
  res_all<-strsplit(r[row,"id3"],split="/")[[1]]
  len<-length(res_all)
  res<-res_all[len]
  r[row,"Codes"]<-res
}
codes_un<-unique(r$Codes)
groups<-1
r$Group<-NA
for(code in codes_un){
  r[r$Codes==code,"Group"]<-groups
  groups<-groups+1
}
New_occupation_table2<-r
save(New_occupation_table2,file='C:/Users/30697/Desktop/SKILLAB/Services/Services_current/Diversity_service_working/Extras/New_occupation_table2.Rda')


load('./Extras/New_occupation_table2.Rda')
New_occupation_table2$label4<-trimws(New_occupation_table2$label4)
save(New_occupation_table2,file='./Extras/New_occupation_table2.Rda')


load('./Extras/New_occupation_table.Rda')
New_occupation_table$Label4<-trimws(New_occupation_table$Label4)
save(New_occupation_table,file='./Extras/New_occupation_table.Rda')
