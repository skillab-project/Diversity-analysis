##### Libraries ######
library(readxl)
library(stringr)
library(openxlsx)
############## load matrices skill-occupations ##########

mapping<-read_excel('./data/new_ESCO_mapping.xlsx')


convert_python_list_to_r <- function(py_list) {
  # Remove outer brackets and extra quotes
  clean_str <- gsub("\\[|\\]", "", py_list)  # Remove square brackets
  clean_str <- gsub("'", "", clean_str)      # Remove single quotes
  
  # Trim whitespace
  clean_str <- trimws(clean_str)
  
  # Handle empty list scenario
  if (clean_str == "") {
    return(c())  # Return empty vector
  }
  
  # Split by commas and return as character vector
  return(unlist(strsplit(clean_str, ",\\s*")))
}
extract_numbers <- function(skill_pillar) {
  nums <- unlist(strsplit(gsub("\\[|\\]", "", skill_pillar), ","))  
  nums <- nums[nums != ""]  # Remove empty values for "[]"
  
  if (length(nums) == 0) {
    return(0)  # Return 0 if no numbers are found
  }
  
  as.numeric(nums)
}

skill_ids<-c()
levels<-c()
Pillar<-c()
skill_preflabels<-c()
for(i in 1:nrow(mapping)){
  if(!is.null(convert_python_list_to_r(mapping$skill_levels[i]))){
    levs<-as.numeric(convert_python_list_to_r(mapping$skill_levels[i]))
    if(levs[1]==4){
      skill_ids<-c(skill_ids,mapping$conceptUri[i])
      levels<-c(levels,levs[1])
      Pillar<-c(Pillar,'S')
      skill_preflabels<-c(skill_preflabels,mapping$preferredLabel[i])
      next
    }
  }
  if(!is.null(convert_python_list_to_r(mapping$traversal_levels[i]))){
    levs<-as.numeric(convert_python_list_to_r(mapping$traversal_levels[i]))
    if(levs[1]==3){
      skill_ids<-c(skill_ids,mapping$conceptUri[i])
      levels<-c(levels,levs[1])
      Pillar<-c(Pillar,'T')
      skill_preflabels<-c(skill_preflabels,mapping$preferredLabel[i])
      next
    }
  }
  if(!is.null(convert_python_list_to_r(mapping$knowledge_levels[i]))){
    levs<-as.numeric(convert_python_list_to_r(mapping$knowledge_levels[i]))
    if(levs[1]==4){
      skill_ids<-c(skill_ids,mapping$conceptUri[i])
      levels<-c(levels,levs[1])
      Pillar<-c(Pillar,'K')
      skill_preflabels<-c(skill_preflabels,mapping$preferredLabel[i])
      next
    }
  }
}
skill_matrix<-data.frame(`Skill_id`=skill_ids,`PreferedLabel`=skill_preflabels,`Pillar`=Pillar,`Levels`=levels)


save(skill_matrix,file='./data/Skill_table.Rda')
