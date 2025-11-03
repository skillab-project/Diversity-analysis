###################### Occupation_Table_Construction ##############
library(httr)

headers = c(
  accept = "application/json",
  `Content-Type` = "application/x-www-form-urlencoded"
)

params = list(
  page = "1"
)

data = list(
  max_level = "4",
  min_level = "4"
)

res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
parsed_content <- content(res, "parsed")
# Convert the parsed content to a list of items
occupation_list_lv1 <- as.list(parsed_content)$items
no_occupations<-as.list(parsed_content)$count





ids_lvl1<-c()
names_lvl1<-c()
for(index in 1:length(occupation_list_lv1)){
  occ_i <-occupation_list_lv1[[index]]
  id_i<-occ_i$id
  name_i<-occ_i$label
  
  
  ids_lvl1<-c(ids_lvl1,id_i)
  names_lvl1<-c(names_lvl1,name_i)
}

ids_lvl2<-c()
names_lvl2<-c()
childrens_lv1<-list()
for(id in ids_lvl1){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded"
  )
  
  params = list(
    page = "1"
  )
  
  data = list(
    ids = id
  )
  
  res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")

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



ids_lvl3<-c()
names_lvl3<-c()
childrens_lv2<-list()
for(id in ids_lvl2){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded"
  )
  
  params = list(
    page = "1"
  )
  
  data = list(
    ids = id
  )
  
  res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  
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


ids_lvl4<-c()
childrens_lv3<-list()
for(id in ids_lvl3){
  headers = c(
    accept = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded"
  )
  
  params = list(
    page = "1"
  )
  
  data = list(
    ids = id
  )
  
  res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
  
  parsed_content <- content(res, "parsed")
  # Convert the parsed content to a list of items
  data<-parsed_content$items
  temp_id2<-c()
  for(item in data){
    for(children in item$children){
      id_2<-children
      ids_lvl4<-c(ids_lvl3,id_2)
      temp_id2<-c(temp_id2,id_2)
    }
  }
  childrens_lv3[[id]] <- temp_id2
  
}

































################################################ PREVIOUS #####################################################
id_lvl_3<-c()
name_lvl_3<-c()
id_lvl_4<-c()
name_lvl_4<-c()
for(index in 1:length(occupation_list_lv3)){
  occ_3_item<-occupation_list_lv3[[index]]
  id3<-occ_3_item$id
  label3<-occ_3_item$label
  
  childrens <-occ_3_item$children
  ids4<-c(unlist(childrens))
  labels4<-c()
  for(id in ids4){
    headers = c(
      accept = "application/json",
      `Content-Type` = "application/x-www-form-urlencoded"
    )
    
    params = list(
      page = "1"
    )
    
    data = list(
      ids = id
    )
    
    res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
    parsed_content <- content(res, "parsed")
    items<-parsed_content$items
    label<-items[[1]]$label
    labels4<-c(labels4,label)
  }
  
  id_lvl_3<-c(id_lvl_3,rep(id3,length(labels4)))
  name_lvl_3<-c(name_lvl_3,rep(label3,length(labels4)))
  id_lvl_4<-c(id_lvl_4,ids4)
  name_lvl_4<-c(name_lvl_4,labels4)
  
  
}







########## In case
id_lvl_3_1<-c()
name_lvl_3_1<-c()
id_lvl_4_1<-c()
name_lvl_4_1<-c()
for(index in 1:length(occupation_list_lv3_300)){
  occ_3_item<-occupation_list_lv3_300[[index]]
  id3<-occ_3_item$id
  label3<-occ_3_item$label
  
  childrens <-occ_3_item$children
  ids4<-c(unlist(childrens))
  labels4<-c()
  for(id in ids4){
    headers = c(
      accept = "application/json",
      `Content-Type` = "application/x-www-form-urlencoded"
    )
    
    params = list(
      page = "1"
    )
    
    data = list(
      ids = id
    )
    
    res <- httr::POST(url = "http://skillab-tracker.csd.auth.gr/api/occupations", httr::add_headers(.headers=headers), query = params, body = data, encode = "form")
    parsed_content <- content(res, "parsed")
    items<-parsed_content$items
    label<-items[[1]]$label
    labels4<-c(labels4,label)
  }
  
  id_lvl_3_1<-c(id_lvl_3_1,rep(id3,length(labels4)))
  name_lvl_3_1<-c(name_lvl_3_1,rep(label3,length(labels4)))
  id_lvl_4_1<-c(id_lvl_4_1,ids4)
  name_lvl_4_1<-c(name_lvl_4_1,labels4)
  
  
}


final_id3<-id_lvl_3
final_name3<-name_lvl_3
final_id4<-id_lvl_4
final_name4<-name_lvl_4



Occupations_table<-data.frame(id3=final_id3,Label3=final_name3,id4=final_id4,Label4=final_name4)
save(Occupations_table,file='C:/Users/30697/Desktop/SKILLAB/Services/Occupations_table.Rda')


load('C:/Users/30697/Desktop/SKILLAB/Services/Occupations_table.Rda')
occupation_name<-'Systems analysts'




########## First find all the highest level occupations 
######### Then go back twice until the second ancestor with labels and names
######## Do the same for the skills all pillars.





############# Folder creation #################
id3<-Occupations_table$id3
mainDir <- "C:/Users/30697/Desktop/SKILLAB/Services/data"

for(id in id3){
  res_id<-strsplit(id,split="/")[[1]]
  subDir<-res_id[length(res_id)]
  dir.create(file.path(mainDir, subDir))
}


id2<-Occupations_table$id2
mainDir <- "C:/Users/30697/Desktop/SKILLAB/Services/data"

for(id in id2){
  res_id<-strsplit(id,split="/")[[1]]
  subDir<-res_id[length(res_id)]
  dir.create(file.path(mainDir, subDir))
}




