
#this function computes the pca_matrix for a given alpha parameter
computePCA<-function(data,alpha){
  num_col<-ncol(data)-1
  keep<-logical(num_col)
  
  #select the variables
  for (i in 1:num_col){
    
    m1<-glm(injured~data[[i]],data,family="binomial")
    #first coef is always intercept
    if(abs(coef(m1)[2])>alpha){
      keep[i]=TRUE  
    }
    else{
      
      keep[i]=FALSE
    }
    
  }
  
  inputs<-data[,keep]
  inputs$injured<-NULL
  pca_matrix<-prcomp(inputs)
  
  return(pca_matrix)
  
}

#this function returns a new dataframe, where the covariates are the covariates
#chosen by the selection procedure followed by supervised PCA
computeDataForPCA<-function(data,alpha){
  num_col<-ncol(data)-1
  keep<-logical(num_col)
  
  #select the variables
  for (i in 1:num_col){
    
    m1<-glm(injured~data[[i]],data,family="binomial")
    #first coef is always intercept
    if(abs(coef(m1)[2])>alpha){
      keep[i]=TRUE  
    }
    else{
      
      keep[i]=FALSE
    }
    
  }
  
  inputs<-data[,keep]
  inputs$injured<-NULL
  
  
  return(inputs)
  
}

#tests logistic supervised PCA
spcLogistic<-function(data,alpha,test,components){
  
  num_col<-ncol(data)-1
  keep<-logical(num_col)
  
  #select the variables
  for (i in 1:num_col){
    
    m1<-glm(injured~data[[i]],data,family="binomial")
    #first coef is always intercept
    if(abs(coef(m1)[2])>alpha){
      keep[i]=TRUE  
    }else{
      keep[i]=FALSE
    }
    
  }
  
  inputs<-data[,keep]
  
  if(typeof(inputs)=='double'){
    #inputs=data.frame(inputs)
    return(NA)
    #names=colnames(data)
    #colnames(inputs)=names[keep]
  }
  
  if(ncol(inputs)==0){
    
    return(NA)
  }
  
  inputs$injured<-NULL
  pca_matrix<-prcomp(inputs)
  inputs<-data.frame(predict(pca_matrix,data))
  
  if(components>-1 & components<=ncol(inputs)){
    
    inputs=inputs[,1:components]
  }
  inputs$injured=data$injured
 # inputs$DrillDuration=data$DrillDuration
  
  m1<-glm(injured~.,inputs,family="binomial")
  
  drill=test$DrillDuration
  test=test[,keep]
  test$injured<-NULL
  test_pca=data.frame(predict(pca_matrix,test))
  
 # test_pca$DrillDuration<-drill
  
  res<-predict(m1,test_pca,type="response")
  
  return(res)
  
}

#conducts logistic supervised PCA and returns the model
spcLogisticModel<-function(data,alpha,components){
  
  num_col<-ncol(data)-1
  keep<-logical(num_col)
  
  #select the variables
  for (i in 1:num_col){
    
    m1<-glm(injured~data[[i]],data,family="binomial")
    #first coef is always intercept
    if(abs(coef(m1)[2])>alpha){
      keep[i]=TRUE  
    }else{
      keep[i]=FALSE
    }
    
  }
  
  inputs<-data[,keep]
  
  if(typeof(inputs)=='double'){
    #inputs=data.frame(inputs)
    return(NA)
    #names=colnames(data)
    #colnames(inputs)=names[keep]
  }
  
  if(ncol(inputs)==0){
    
    return(NA)
  }
  
  inputs$injured<-NULL
  pca_matrix<-prcomp(inputs)
  inputs<-data.frame(predict(pca_matrix,data))
  
  if(components>-1 & components<=ncol(inputs)){
    
    inputs=inputs[,1:components]
  }
  inputs$injured=data$injured
  
  
  m1<-glm(injured~.,inputs,family="binomial")
  
  return(m1)
  
}
