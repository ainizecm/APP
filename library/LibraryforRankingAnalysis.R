#LIBRARY FOR RANKING ANALYSIS
#Script containg the mainfuncitons used fir the first analysis in the shiny app
#1.Load the criteria weights previosly generated
#2.AccCompl: Aggregated complexity assesed by the experts to each of the actions
#3.FinalWeights: Combine the criteria weights and the complexity of each action 
#4.resultsbyResp: Gets the excel file, sheet,the rows of the matrix and the criteria weighs and returns Outcomes and Complexity marks for them


#1.Load stored criteria weights

CriteriaWeights <- readRDS("data/criteria.rds")

#excel<-'data/DATA2_v2.xlsx'
#5.Aggregated complexity assesed by the experts to each of the actions
#n_bin_actions<-57
#n_cri_resp<-4


#6.Combine the criteria weights and the complexity of each action by:
##a) Normalize de complexity within all the actions in a criteria
##b) Mulitply the compl by the criteria weight for a final result
##c) Move to the end Financing criteria (B8 B9 B10) for the facilitation of the interventions analysis
AccCompl<-function(excel,n_bin_actions=57,n_cri_resp=4){
  lastrow<-n_bin_actions+1
  lastcolum<-n_cri_resp+4
  compl<-data.frame(read.xlsx(excel, "ACTIONS",
                              rowIndex = c(1:lastrow),
                              colIndex=c(1:lastcolum)))
  compl[,5:ncol(compl)]<-apply(compl[,5:ncol(compl)],c(1,2),as.numeric)
  compl[,"FINAL"]<-rowMeans(compl[,5:ncol(compl)]) #Add a row with the average
  rownames(compl) <- NULL
  return(compl)
}

#6.Combine the criteria weights and the complexity of each action by:
##a) Normalize de complexity within all the actions in a criteria
##b) Mulitply the compl by the criteria weight for a final result
##c) Move to the end Financing criteria (B8 B9 B10) for the facilitation of the interventions analysis
FinalWeights<-function(excel,n_bin_actions,n_cri_resp){
  acc<-AccCompl(excel,n_bin_actions,n_cri_resp) #use the above funtion 5 to get actions complexity
  #weig<-criteriaweights(excel,numberofrespondants) #Only use if the criteia weights are computed again
  cri<-CriteriaWeights #read the stored criteria weights
  
  v_i<-vector()
  
  for(i in c(1:7,11:15,10)){ #normalize the complexity of each action comapring all the actions in the same criteria 15 crietria)
  v<- acc$FINAL[which(acc$Criteria==paste0("B",i))]
  w<-cri$FINAL[which(cri$Code==paste0("B",i))]
  v<-sapply(v,function(x) x/sum(v))
  v_i<-c(v_i,as.numeric(v)*as.numeric(w))
  } #multiply by criteria weights
  
  finalt<-data.frame(Criteria=c(as.vector(cri[1:10,]$Code),as.vector(acc$Code),c("B8","B9")),
                     weights=c(cri$FINAL[1:10],
                                v_i,
                                cri$FINAL[which(cri$Code=="B8"|cri$Code=="B9")]
                               ))
  #final<-c(w[1:10],vi[1:38],vi[43:57],w[18:19],vi[39:42]) #rearrange the columns for an easier analysis
  #final<-data.frame(weights=apply(as.array(final),1,as.numeric))
  return(finalt)
  }
  


#7.Compute the results for each respondant. Uses the weights computed with funtion 6
#NOTE:The financing scores do not take into account the choosen way of financing

resultsbyResp<-function(excel,blocksheet,rowindex,weights=criandstr,n_bin_actions=57){
  #lastcolum<-n_bin_actions+15
  #Read the matrix on the selected rows
  PERM<-read.xlsx(excel,sheetIndex = blocksheet,
                  rowIndex =c(5,rowindex), 
                  ##colIndex = 1:lastcolum,
                  header=TRUE)
  PERM[is.na(PERM)] <- 0
  
  #Outcomes Mark, simply multiply the results witht the weights
  outmark<-as.matrix(PERM[,4:13])%*%as.numeric(t(weights[1:10,]$weights))
  
  #Complexity Mark is divided in 3 marks, Actions Complexity, Cost Complexity and Financial Source Complexity
  #The finalcial source complexity does not take into account which is the financial source needed,
  #it is penalized just if some kind of fin source is needed.
  ActionsComplmark<-as.matrix(PERM[,14:(n_bin_actions+9)])%*%as.numeric(t(weights[11:(n_bin_actions+6),]$weights)) #Same as for outcomes, just multply results by weights
  
  #for financial source make it one if any of the options is selected(what if the financial acctions change?)
  #Which are the finantial accions?Read them from the actions sheet

  lastrow<-n_bin_actions+1

  factions<-data.frame(read.xlsx(excel, "ACTIONS",
                              rowIndex = c(1:lastrow),
                              colIndex=c(1:4)))
  factions<-as.vector(factions[which(factions$Criteria=='B10'),3])
  fsourcemarks<-rowSums(PERM[,factions])/rowSums(PERM[,factions])
  fsourcemarks[is.na(fsourcemarks)] <- 0
  fsourcemarks<-as.matrix(fsourcemarks)*sum(weights[which(weights$Criteria %in% factions),]$weights) #mutiply that result with the sum of the complexity of the financial sources
  #cost mars
  costimpmarks<-as.matrix(PERM[,c('B8','B9')])%*%as.numeric(t(weights[which(weights$Criteria=="B8"
                                                                              |weights$Criteria=="B9"),]$weights)) #Cost
  
  
  
  #Generate a data.frame with all the Marks
  RESULTS<-data.frame(Block=PERM$Block,
                      Description=PERM$Decription,
                      Code=PERM$Code,OutcomesMark=round(outmark,2),
                      ActionsComplMark=round(ActionsComplmark,2),
                      FinancingMark=round(costimpmarks+fsourcemarks,2),
                      ComplexityMark=round(ActionsComplmark+costimpmarks+fsourcemarks,2))
  return(RESULTS)
}


##Graph

grafico<-function(results){
    inp<-results$Average_Complexity
    outp<-results$Average_Outcomes
    eff<-dea(inp,outp)
    DEA<-data.frame(eff=eff$eff)
    dea.plot.frontier(inp,outp,txt=results$Code,xlab="Complexity",ylab="Outcomes",
                    pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1),xlim=c(0,max(inp)+0.05),ylim=c(0,max(outp)+0.2))
      axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
      axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)}

