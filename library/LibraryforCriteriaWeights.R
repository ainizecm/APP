#LIBRARY WITH FUNCTIONS TO GENERATE THE CRITERIA WEIGHTS
#Script containg the main funcitons used to generate the criteria weights 
#THIS IS NOT IN USE AT THE MOMENT BECAUSE WE HAVE ARE USING THE SAME WEIGHTS OF THE CRITERIA FOR ALL THE PROJECTS SO WE DO NOT NEED TO COMPUTE THEM AGAIN
#MAYBE USEFUL IN THE FUTURE IF WE WANT TO GIVE MORE FLEXIBILITY ON CRITERIAS OR REPEAT THE SURVEYS
#1.ahpvector:Computes the eigenvector from the Matrix for the ahp model
#2.cons: Computes consistancy from a ahp matrix
#3.WeightsbyRespondant: Generate the weights by respondant by creating the needed matrixes from the survey
#4.criteriaweights: Computingthe aggreated weights from the survey sheet straight away using the funtion above


###Standard weights and consistency computation for a generic AHp matrix##

#1.Computes the eigenvector from the Matrix for the ahp model
ahpvector<-function(A){
  Anorm<-matrix(nrow=nrow(A), ncol=ncol(A))
  for(j in 1:ncol(A)){
    s<- sum(A[,j])
    for (i in 1:nrow(A)) Anorm[i,j]<-A[i,j]/s}
  neig<-vector ("numeric")
  for (k in 1:nrow(A)){neig[k]=(1/nrow(A))*sum(Anorm[k,])}
  return(neig)}

#2.Computes consistancy from a ahp matrix
cons<-function(A){
  neig<-ahpvector(A)
  RI<-c(0,0,0.58,0.9,1.12,1.24,1.32,1.41,1.45,1.49)
  ws<-0
  for(j in 1:ncol(A)){
    a<-neig[j]*A[,j]
    ws<-ws+a}
  dws<-0 
  for(i in 1:length(neig)){dws[i]<-ws[i]/neig[i]}
  lambdam<-sum(dws)/ncol(A) 
  CI<-(lambdam-ncol(A))/(ncol(A)-1) 
  CR<-CI/RI[ncol(A)]
  return(CR)}


#3.Generate the weights by respondant by creating the needed matrixes from the survey

WeightsbyRespondant<-function(excel,Respondantnumber,firstrow,lastrow){
  read1 <- read.xlsx(excel,"CRITERIA_Survey",header=FALSE);
  fullsurvey<-as.matrix(read1)
  values<-as.vector(c(9,7,5,3,1,1/3,1/5,1/7,1/9))
  survey<-fullsurvey[firstrow:lastrow,1:10]
  
  #Matrix Improved Health
  biImHealth <- matrix(as.numeric(survey[5:14,2:10]),ncol=9)
  l<-vector(length=nrow(biImHealth))
  for(i in 1:nrow(biImHealth)){
    index<-which(biImHealth[i,]==1,arr.ind=TRUE)
    l[i]<-values[index]}
  ImHealth<-matrix(c(1,l[1:4],
                     1/l[1],1,l[5:7],
                     1/l[2],1/l[5],1,l[8:9],
                     1/l[3],1/l[6],1/l[8],1,l[10],
                     1/l[4],1/l[7],1/l[9],1/l[10],1),nrow=5,ncol=5,byrow=TRUE)
  
  #Matrix Responsiveness
  biRespon <-  matrix(as.numeric(survey[12,2:10]),ncol=9)
  index<-which(biRespon==1,arr.ind=TRUE)
  Respon<-matrix(c(1,values[index],1/values[index],1),nrow=2,ncol=2,byrow=TRUE)
  
  #Matrix Social and Financial protection
  biSFprot <-  matrix(as.numeric(survey[18,2:10]),ncol=9)
  index<-which(biSFprot==1,arr.ind=TRUE)
  SFprot<-matrix(c(1,values[index],1/values[index],1),nrow=2,ncol=2,byrow=TRUE)
  
  #Matrix Health workfore 
  biHealthW <-  matrix(as.numeric(survey[20:22,2:10]),ncol=9)
  l<-vector()
  for(i in 1:nrow(biHealthW)){
    index<-which(biHealthW[i,]==1,arr.ind=TRUE)
    l[i]=values[index]
  }
  HealthW<-matrix(c(1,l[1:2],1/l[1],1,l[3],1/l[2],1/l[3],1),nrow=3,ncol=3,byrow=TRUE)
  
  #Matrix Information Systems
  biInfSis<-  matrix(as.numeric(survey[24,2:10]),ncol=9)
  index<-which(biInfSis==1,arr.ind=TRUE)
  InfSis <-matrix(c(1,values[index],1/values[index],1),nrow=2,ncol=2,byrow=TRUE)
  
  #Matrix Financing
  biFin <-  matrix(as.numeric(survey[26:28,2:10]),ncol=9)
  l<-vector()
  for(i in 1:nrow(biFin)){
    index<-which(biFin[i,]==1,arr.ind=TRUE)
    l[i]=values[index]
  }
  Fin<-matrix(c(1,l[1:2],1/l[1],1,l[3],1/l[2],1/l[3],1),nrow=3,ncol=3,byrow=TRUE)
  
  #Matrix Leadership and governance
  biLead <-  matrix(as.numeric(survey[30:39,2:10]),ncol=9)
  l<-vector()
  for(i in 1:nrow(biLead)){
    index<-which(biLead[i,]==1,arr.ind=TRUE)
    l[i]=values[index]
  }
  Lead<-matrix(c(1,l[1:4],
                 1/l[1],1,l[5:7],
                 1/l[2],1/l[5],1,l[8:9],
                 1/l[3],1/l[6],1/l[8],1,l[10],
                 1/l[4],1/l[7],1/l[9],1/l[10],1),nrow=5,ncol=5,byrow=TRUE)
  
  #MATRIX GENERAL OUTCOMES CRITERIA
  biGenOut <-  matrix(as.numeric(survey[41:46,2:10]),ncol=9)
  l<-vector()
  for(i in 1:nrow(biGenOut)){
    index<-which(biGenOut[i,]==1,arr.ind=TRUE)
    l[i]=values[index]
  }
  GenOut<-matrix(c(1,l[1:3],
                   1/l[1],1,l[4:5],
                   1/l[2],1/l[4],1,l[6],
                   1/l[3],1/l[5],1/l[6],1)
                 ,nrow=4,ncol=4,byrow=TRUE)
  
  #Matrix for GENERAL FEASIBILITY CRITERIA
  biGenFeas <-  matrix(as.numeric(survey[48:62,2:10]),ncol=9)
  l<-vector()
  for(i in 1:nrow(biGenFeas)){
    index<-which(biGenFeas[i,]==1,arr.ind=TRUE)
    l[i]=values[index]
  }
  GenFeas<-matrix(c(1,l[1:5],
                    1/l[1],1,l[6:9],
                    1/l[2],1/l[6],1,l[10:12],
                    1/l[3],1/l[7],1/l[10],1,l[13:14],
                    1/l[4],1/l[8],1/l[11],1/l[13],1,l[15],
                    1/l[5],1/l[9],1/l[12],1/l[14],1/l[15],1)
                  ,nrow=6,ncol=6,byrow=TRUE)
  
  #Eficiency, Service req and Medi and drugs
  Ef<-matrix(c(1))
  Serreq<-matrix(c(1))
  Medi<-matrix(c(1))
  
  #Using now the generic funtions into the constructed matrices:
  
  IHw<-ahpvector(ImHealth)
  Resw<-ahpvector(Respon)
  SFpotw<-ahpvector(SFprot)
  Efw<-ahpvector(Ef)
  GenOutw<-ahpvector(GenOut)
  FinalOutcomesWeights<-c(IHw*GenOutw[1],Resw*GenOutw[2],SFpotw*GenOutw[3],Efw*GenOutw[4])
  
  SRw<-ahpvector(Serreq)
  HealthWw<-ahpvector(HealthW)
  InfSisw<-ahpvector(InfSis)
  Mediw<-ahpvector(Medi)
  Finw<-ahpvector(Fin)
  Leadw<-ahpvector(Lead)
  GenFeasw<-ahpvector(GenFeas)
  FinalFeasibilityWeights<-c(SRw*GenFeasw[1],HealthWw*GenFeasw[2]
                             ,InfSisw*GenFeasw[3],Mediw*GenFeasw[4],Finw*GenFeasw[5],
                             Leadw*GenFeasw[6])
  #Consistencies
  cons<-data.frame(c(cons(ImHealth),cons(HealthW),cons(Fin),cons(Lead),cons(GenOut),cons(GenFeas)))
  colnames(cons)<-c(paste("Cons",Respondantnumber))
  
  #Output data frame
  ResWeights<-as.data.frame(c(FinalOutcomesWeights,""," ",FinalFeasibilityWeights))
  colnames(ResWeights)<-c(paste("Weight",Respondantnumber))
  return(c(ResWeights,cons))
}

#4.Computingthe aggreated weights from the survey sheet straight away using the funtion above

criteriaweights<-function(excel,numberofrespondants){
  read2<-read.xlsx(excel, "AHPWEIGHTS",rowIndex = 2:29);
  subcriteria<-read2[2]
  allweights<-data.frame(subcriteria)
  sum<-vector()
  
  for (i in 1:numberofrespondants){ #Compute weights for each respondant
    weighandcons<-WeightsbyRespondant(excel,i,62*(i-1)+1,62*(i-1)+1+61) 
    allweights[,paste0("weights",i)]<-as.data.frame(weighandcons[1])}
  
  allweights[,2:ncol(allweights)]<-apply(allweights[,2:ncol(allweights)],c(1,2),as.numeric)
  allweights[,"FINAL"]<-rowMeans(allweights[,2:ncol(allweights)]) #Add a column with the mean of the weights
  allweights<-as.data.frame(allweights[c(1:10,13:27),])
  rownames(allweights) <- NULL
  return(allweights)} 

#HOW TO SAVE THEM
#CriteriaWeights<-data.frame(Code=test$Code,FINAL=test$FINAL)
#saveRDS(CriteriaWeights, file="criteria.rds")