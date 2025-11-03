################ Test alpha ################
source('./test/R_functions/alpha_diversity.R')



options(rgl.useNULL = TRUE) # for headless error


test_alpha<-function(call){
load('./test/R_functions/dataset.Rda')
res_case<-alpha_diversity(dataset_sorted)

load('./test/R_functions/expected_results_alpha.Rda')
if(identical(res,res_case)){
  msg<-'Alpha Diversity Analysis ------------------------- passed'
  return(list(Message=msg,Pass=1))
}else{
  msg<-'Alpha Diversity Analysis ------------------------- failed'
  return(list(Message=msg,Pass=0))
}
}

