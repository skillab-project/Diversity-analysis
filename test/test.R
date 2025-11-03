######################## Whole test

source('./test/test_alpha.R')
source('./test/test_beta.R')
source('./test/test_isa.R')


passed<-0
tests<-c('Alpha Diversity ---------------','Beta Diversity ---------------','ISA ---------------')
test_keys<-c('A','B','I')
for(key in test_keys){
  if(key=='A'){
    r<-test_alpha(0)
    if(r$Pass==1){
      passed<-passed+1
      print(paste(tests[1],'Passed'))
    }else{
      passed<-passed
      print(paste(tests[1],'Failed'))
    }
  }else if(key=='B'){
    r<-test_beta(0)
    if(r$Pass==1){
      passed<-passed+1
      print(paste(tests[2],'Passed'))
    }else{
      passed<-passed
      print(paste(tests[2],'Failed'))
    }
  }else if(key=='I'){
    r<-test_isa(0)
    if(r$Pass==1){
      passed<-passed+1
      print(paste(tests[2],'Passed'))
    }else{
      passed<-passed
      print(paste(tests[2],'Failed'))
    }
  }
}

print(paste0('Successful tests : ',passed,'/3'))