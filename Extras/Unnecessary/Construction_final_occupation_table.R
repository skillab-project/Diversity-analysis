############
library(httr)


options(rgl.useNULL = TRUE) # for headless error

######### load ##########
load('./Extras/Skill_table.Rda')
load('./Extras/Occupations_table.Rda')


######
num<-c()
ids<-c()
for(id3 in unique(Occupations_table$id3)){
  occ_temp<-Occupations_table[Occupations_table$id3==id3,]
  num<-c(num,nrow(occ_temp))
  ids<-c(ids,id3)
}

ids_good<-ids[which(num!=1)]
ids_bad<-ids[which(num==1)]
ids_bad_split<-strsplit(ids_bad,split="/")
codes <- sapply(ids_bad_split, function(x) tail(x, 1))

# Extract the group key: the first digit after "C"
group_keys <- substr(codes, 2, 2)

# Group the codes by this key
grouped_codes <- split(codes, group_keys)
for(j in 1:length(grouped_codes)){
  group<-grouped_codes[[j]]
  groupj<-c()
  for(id in ids_bad){
    id_splitted<-strsplit(id,split="/")[[1]]
    id_to_check<-id_splitted[length(id_splitted)]
    if(id_to_check %in% group){
      groupj<-c(groupj,id)
    }
  }
 grouped_codes[[j]]<-groupj
}


Occupation_table_good<-Occupations_table[Occupations_table$id3 %in% ids_good,]
Occupation_table_bad<-Occupations_table[Occupations_table$id3 %in% ids_bad,]

groupings_good<-c()
for(j in 1:length(ids_good)){
  id<-ids_good[j]
  occ_temp<-Occupation_table_good[Occupation_table_good$id3 %in% id,]
  groupings_good<-c(groupings_good,rep(j,nrow(occ_temp)))
}
Occupation_table_good$Group<-groupings_good


groupings_bad<-c()
for(j in 1:length(grouped_codes)){
  ids<-grouped_codes[[j]]
  occ_temp<-Occupation_table_bad[Occupation_table_bad$id3 %in% ids,]
  groupings_bad<-c(groupings_bad,rep(j+101,nrow(occ_temp)))
}
Occupation_table_bad$Group<-groupings_bad


New_occupation_table<-rbind(Occupation_table_good,Occupation_table_bad)
save(New_occupation_table,file='./Extras/New_occupation_table.Rda')


load('./Extras/New_occupation_table.Rda')


codes_all<-strsplit(New_occupation_table$id3,split="/")
codes<-c()
for(code in codes_all){
  codei<-code[length(code)]
  codes<-c(codes,codei)
}
New_occupation_table$Codes<-codes
save(New_occupation_table,file='./Extras/New_occupation_table.Rda')



