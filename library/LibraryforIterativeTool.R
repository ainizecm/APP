#LIBRARY FUNCTIONS SCRIPT
#Script containg the mainfuncitons used in the shiny app-Iterative part


#Computes which actions are nedeed and their complexity (special computation for financing)
#The sum of the vecctor returns the combined complexity score
needed_actions_complexity<-function(x=rep(0,n_totalint),y=rep(0,n_bin_actions),weights_vector=criandstr$weights,PM,n_bin_actions){
  print('needed actions start')
  lastcolumn<-(14+n_bin_actions+1)
  #print(PM)
  PMfeas<-as.matrix(PM[14:lastcolumn]) #Read second part of the matrix(complexity)
  PMfeas<-apply(PMfeas,c(1,2),as.numeric)
  PMfilter<-x*PMfeas #Only keep the interventions that are in the strategy
  PMfilter<-apply(PMfilter,c(1,2),as.numeric)

  
  column_sum<-apply(PMfilter,2,sum) #sum to see if the action is needed
  
  PM[,c(n_bin_actions+14,n_bin_actions+15)]<-apply(PM[,c(n_bin_actions+14,n_bin_actions+15)],c(1,2),as.numeric)
  #normalize it, first financial non binary variables, then make all the ones that are bigger than one one
  column_sum_normalized<-c(column_sum[1:n_bin_actions],
            sum(PMfilter[,n_bin_actions+1])/sum(PM[,n_bin_actions+14]),#%of the total sum of estimated unitari cost
            sum(PMfilter[,n_bin_actions+2])/sum(PM[,n_bin_actions+15]))#%of the total sum of esimated impact)
 
  column_sum_normalized[is.na(column_sum_normalized)] <- 0

  for(i in 1:n_bin_actions){if (column_sum_normalized[i]<y[i]){column_sum_normalized[i]<-y[i]}}#(not sure it is going to be used)iff any action is forced to be done by the user
  column_sum_normalized[1:n_bin_actions][which(column_sum_normalized[1:n_bin_actions]>1)]<-1
  

  
  #multiply by the weights of each action
  accwe<-as.data.frame(column_sum_normalized*t(weights_vector[11:(11+n_bin_actions+1)]))
  #accwe[,54]<-sum(PMfilter[,54])/sum(PM[,65])*weights_vector[64]#how much from the total possible
  #accwe[,55]<-sum(PMfilter[,55])/sum(PM[,66])*weights_vector[65]#how much of the possible total impact
  #accwe[,56]<-sum(PMfilter[,56])/sum(PM[,67])*weights_vector[66]#how much from the total possible
  #accwe[,57]<-sum(PMfilter[,57])/sum(PM[,68])*weights_vector[67]#how much of the possible total impact
  accwe[,n_bin_actions+1]<-column_sum_normalized[n_bin_actions+1]*weights_vector[11+n_bin_actions]#how much from the total possible
  accwe[,n_bin_actions+2]<-column_sum_normalized[n_bin_actions+2]*weights_vector[11+n_bin_actions+1]#how much of the possible total impact
  accwe[is.na(accwe)] <- 0

  numb=sapply(column_sum[1:n_bin_actions],function(x) round(x,0))
  df_needed_actios<-data.frame(Code=colnames(PM[,14:(14+n_bin_actions+1)]),
             need=column_sum_normalized,
                               weight=t(accwe),
                               Int=c(numb,column_sum_normalized[c(n_bin_actions+1,n_bin_actions+2)]))
  return(df_needed_actios)
}





# #computes list of yet non included interventions and the new needed actions and combined complexity
extra_intervention<-function(x=rep(0,n_totalint),y=rep(0,n_bin_actions),PM,outmark,weights_vector,n_bin_actions){
        lastcolumn<-(14+n_bin_actions+1)
        
        #See how the situation is at that point
        #needed actions for that strategy alreagy
        df_actions<-needed_actions_complexity(x,y,weights_vector,PM,n_bin_actions)
        y<-df_actions$need[1:n_bin_actions] #binary vector for actions
        
        print('see situation ok')
        
        #percentage of outcomes
        outpercen<-sum(x*outmark)/sum(outmark) #%of improvemnt if we do all intvertion we will get 100%
        #percentage of complexity
        complepercen<-sum(df_actions$weight) #%of complexity, if we so all intervention we will get 100%
        print('percentages ok ok')
        
        #Compute the new outocmes and complexity compared to the already selected strategy
        extra_interventions<-data.frame(PM[c(1:3)])
        #trim the descrption no to be too long
        extra_interventions$Description<-sapply(extra_interventions$Description,function(t) paste0(strtrim(t,20),'...'))
        
        #See which actions we need appart from the ones already selected
        extraac<-PM[,c(1:3,14:(14+n_bin_actions-1))]#get the complexity part of the matrix
        extraac[,which(y==1)+3]<-0#Remove the actions that where already done(we add 3 because the pm has 3 columns at the brgining)
        print('extra actions ok')
        
        
        extra_interventions<-data.frame(extra_interventions[which(x==0),])
        extraac<-data.frame(extraac[which(x==0),])
        extraac<-apply(extraac,c(1,2),as.numeric)
        print('starting the loop')
        for (i in 1:nrow(extra_interventions)) {
          xnew<-x
          xnew[as.numeric(rownames(extra_interventions)[i])]<-1 #imagina that you choose it
          df_actions<-needed_actions_complexity(xnew,y,weights_vector,PM,n_bin_actions)
          print('needed action run')
          
          #extra_interventions$outcomes[i]<-outmark[as.numeric(rownames(extra_interventions)[i])]
          extra_interventions$Out[i]<-round(outmark[as.numeric(rownames(extra_interventions)[i])]/sum(outmark)*100,2)
          extra_interventions$Compl[i]<-round((sum(df_actions$weight)-complepercen)*100,2)
          extra_interventions$R[i]<-round(extra_interventions$Out[i]/extra_interventions$Compl[i],2)}
        
        #number of needed actions
        extra_interventions$extraaction<-rowSums(extraac[,4:(3+n_bin_actions)])
        
        #Convert the inpact measure into a value
          extra_interventions$Imp<-PM[which(x==0),]$B9
          extra_interventions$Imp[which(extra_interventions$Imp==0.25)]<-"<1%"
          extra_interventions$Imp[which(extra_interventions$Imp==0.5)]<-"1-5%"
          extra_interventions$Imp[which(extra_interventions$Imp==0.75)]<-"5-10%"
          extra_interventions$Imp[which(extra_interventions$Imp==0.1)]<-">10%"
          
          return(extra_interventions)}






#Using the previous funtions computes a summary of the strategy, with the current situation and the information about the possible addings
strategy<-function(x=rep(0,n_totalint),y=rep(0,n_bin_actions),PM,outmark,weights_vector,n_bin_actions,n_totalint){
  #add to x the interventions possible by y actions
  x[which(x>1)]<-1 
  
  #the total outcomes
  outpercen<-sum(x*outmark)/sum(outmark)
  print('total outcomes ok')
  
  
  #compute the actions that are needed for the current srategy
  df_actions<-needed_actions_complexity(x,y,weights_vector,PM,n_bin_actions)
  print('needed actions ok')
  y<-df_actions$need[1:n_bin_actions] #see which are the needed actions
  print('actions ok')
  
  #the total complexity
  complepercen<-sum(df_actions$weight)
  print('total compls ok')
  
  #data frame for the interventions
  #Compute the outcomes and complexity of interventions not in the strategy
  extra_interventions<-extra_intervention(x=x,y=y,PM,outmark,weights_vector,n_bin_actions) 
  print('extra int ok')
  
  df_actions$extraint<-rep(0,nrow(df_actions))
  #Compute the new actions complexity and how many interventins they support
  for (i in 1:n_bin_actions){
    ynew<-y
    ynew[i]<-1
    
    #compute how many interventions are possible with the new action
    x_pos_new<-rep(0,n_totalint)
    for (j in 1:n_totalint) {if (all(PM[j,14:(14+n_bin_actions-1)]<=y)){x_pos_new[j]<-1}}
    
    print(paste0('new',i))
    #compute how many interventions were already possible with the new action
    x_pos_old<-rep(0,n_totalint)
    for (k in 1:n_totalint) {if (all(PM[k,14:(14+n_bin_actions-1)]<=y)){x_pos_old[k]<-1}}
    print(paste0('old',i))
    
    df_actions$extraint[i]<-sum(x_pos_new-x_pos_old) #the difference between them is the new possible int
    df_actions$extraint[c(n_bin_actions+1,n_bin_actions+2)]<-""}
    
  return(list(outcomespercen=outpercen*100,
              complepercen=complepercen*100,
              Int=data.frame(PM$Code,PM$Description,choose=x),
              df_actions=df_actions,
              extra_interventions=extra_interventions))
}

