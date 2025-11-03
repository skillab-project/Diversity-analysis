###############Test ISA ##########################
source('./test/R_functions/isa_diversity.R')


options(rgl.useNULL = TRUE) # for headless error

test_isa<-function(call){
  load('./test/R_functions/dataset.Rda')
  res_case<-isa_function(dataset_sorted)
  load('./test/R_functions/expected_results_ISA.Rda')
  if(identical(res[1:10,],res_case[1:10,])){
    msg<-'ISA ------------------------- passed'
    return(list(Message=msg,Pass=1))
  }else{
    msg<-'ISA ------------------------- failed'
    return(list(Message=msg,Pass=0))
  }
}
