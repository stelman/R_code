library(caret)

runManyModels<-function(formula,d,models=c("rf","gbm","svmRadial","svmPoly","nb","mlp","lm","leapSeq")){

results=list()

for(model in models){
  if(typeof(d[1,all.vars(f)[1]])=="double" && model=="nb"){
    break
  }
  print(model)
  res=list()
  m=train(formula,data=d,method=model,verbose=FALSE)
  res$error=min(m$results[[m$metric]])
  res$bestTune=m$bestTune
  results[length(results)+1]= list(res)
}

return(results)

}
