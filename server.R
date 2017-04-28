function(input, output, session) {
 
  
  
  observe ({
  #If the file is null do nothing, otherwise start reading the inputs
  file <-input$file.upload
  if (is.null(file)) 
    return(NULL)
  
  ##################################################
  ##########READ PERFORMANCE MATRIX#################
  ################################################
  
  EXCEL<-file$datapath

  #Performance Matrix 
  PM<-read.xlsx(EXCEL,"PerformanceMatrix" ,startRow=5,encoding='UTF-8',header=TRUE)
  output$PM<-renderDataTable({PM})


  #Names of the interventins and actions
  #Read the actions names from the top of of the Performance Matrix NEED TO CHANCHE IF ACTIONS CHANGE
  #Actnames<-t(read.xlsx(EXCEL,"PerformanceMatrix",rowIndex =c(4,5), colIndex = c(13:65,68:72),encoding='UTF-8',header=FALSE))
  #Actnames<-data.frame(Action=Actnames[,1],Code=Actnames[,2])
  
  #Read the Interventions names from the Performance Matrx
  #Intnames<-as.data.frame(read.xlsx(EXCEL,"PerformanceMatrix",
  #rowIndex =c(5, 7:(7+n_totalint)), colIndex = c(1,2),encoding= 'UTF-8',header=TRUE))
     

     
     
  #If the compute bottom is clicked...
  observeEvent(input$compute, {
    
    ####################################
    ##########LOAD INPUTED DATA###########
    ###################################
    #Number of Blocks
    #n_blocks=4
    n_blocks<-input$n_blocks
    
    #Number of Total Interventions
    #n_totalint=53
    n_totalint<-input$n_totalint
    
    #Number of respondants for each block
    #n_resp=c(3,4,3,3)
    #Read the input and separete it by number
    n_resp<-as.numeric(unlist(strsplit(as.character(input$n_resp),"/")))
    
    #Number of Interventions in each block
    n_int=as.numeric(unlist(strsplit(as.character(input$n_int),"/")))
    
    #Number of actions
    n_bin_actions<-input$n_bin_actions
    
    #Number of respondants for actions complexity
    n_cri_resp<-input$n_cri_resp
    
    
    ####################################
    ##########COMPUTE RESULTS###########
    ###################################
    
     #Generate the criteria and actions vector
     criandstr<-FinalWeights(EXCEL,n_bin_actions,n_cri_resp)
     
    
     #Define summary data frame to populate later
     FinalInterventionsResults<-data.frame(Interventions=vector(),Average_Outcomes=vector(),Average_Complexity=vector())
     summary<-list()
     
     
     #Generate each block results
     for (i in 1:n_blocks){#Loop throught the blocks
       for (j in 1:n_resp[i]){#Loop through the respondents
         print(paste("Computing responses for respondat",j, "in block",i))
         #Define the starting and ending rows
         start<-7+(1+n_int[i])*(j-1) #Int start in row 7 in the template+the number of interventions and two extra rows for each new respondant
         end<-7+(1+n_int[i])*(j-1)+n_int[i]-1 #sum the number of interventions to now where to end
         #Sheet
         sheet<-as.character(paste0("IC",i))
         
         #Generate resuls for each respondant
         print(paste("Computing results...",j, "in block",i))
         results<-resultsbyResp(EXCEL,sheet,start:end,criandstr,n_bin_actions)
         assign(paste0("resultsB",i,"_R",j),results)
         
         
         #Summary dataframe
         print(paste("Generating summary dataset...",j, "in block",i))
         if (j==1) {summary[[i]]<-data.frame(results[,c(1,2,3,6)])}#If it is the first respondant it will get the names
         else{summary[[i]]<-cbind(summary[[i]],results[,c(3,6)])}
       }
       print(paste("Generating averages",j, "in block",i))
       summary[[i]]$Average_Outcomes<-round(rowMeans(summary[[i]][which(colnames(summary[[i]])=="OutcomesMark")]),2)
       summary[[i]]$Average_Complexity<-round(rowMeans(summary[[i]][which(colnames(summary[[i]])=="ComplexityMark")]),2)
       
       
       #Generate a table showing the final results for each intervention
       print(paste("Generating FinalInterventions dataframe...",j, "in block",i))
       FinalInterventionsResults<-rbind(FinalInterventionsResults,
                                        data.frame(Internventions=summary[[i]]$Interventions,
                                                   Outcomes=summary[[i]]$Average_Outcomes,
                                                   Complexity=summary[[i]]$Average_Complexity))
     }
  
     
##MAYBE???SAVE SUMMARY AND FINALINTERVENTIONS????##
     
  
     ####################################
     ##########GENERATE OUTPUTS###########
     ###################################
     
     
  #Show results for each block-Table and plot
  output$RankingbyBlock<-renderUI({
    summary_list<-lapply(1:n_blocks,function(i){
      fluidPage(paste("BLOCK",i,"RESULTS"),
                fluidRow(div(style="color:white", renderText("  jkkjh "))),
                fluidRow(div(style="color:white", renderText("  jkkjh "))),
                fluidRow(renderText("   ")),
      fluidRow(
      column(6,renderDataTable({
        data.frame(Int=summary[[i]]$Interventions,
                   Outcomes=summary[[i]]$Average_Outcomes,
                   Complexity=summary[[i]]$Average_Complexity)
      }))
      ,column(6,renderPlot({grafico(summary[[i]])}))
         ),
      fluidRow(div(style="color:white", renderText("  jkkjh "))),
      fluidRow(div(style="color:white", renderText("  jkkjh "))))})
    do.call(tagList, summary_list)
    })

  
  #Show the final matrix
  output$FinalM<-renderDataTable({FinalInterventionsResults})
  
  
  #Show the ranking for actions
  output$ActionsRanking<-renderDataTable({
    Comp<-AccCompl(EXCEL,n_bin_actions=57,n_cri_resp=4)
    Actions<-data.frame(Code=Comp$Code)
    Actions$Interventions<-colSums(PM[,c(13:(12+n_bin_actions))])
    Actions$Complexity<-Comp$FINAL
    rownames(Actions)<-NULL
    Actions
  },caption="Actions")
  
  ##Details by respondant if you want to check some results
  #Show results for each respondant  
  output$DetailedResults<-renderUI({
    summary_list<-lapply(1:n_blocks,function(i){renderDataTable({summary[[i]]},caption=paste0('Block',i))})
    do.call(tagList, summary_list)
  })
  
  output$ActionsSurvey<-renderDataTable({
    Comp<-AccCompl(EXCEL,n_bin_actions=57,n_cri_resp=4)
  },caption="Actions Complexity by Respondant")

 #Maybe show something about the criteria weights and the actions complexity??  
  

  #Save final interventions results to use later
  #saveRDS(FinalInterventionsResults, file="FinalInterventionsResults.rds")
  #})
  
  
  ####################################
  ##########ITERATIVE TOOL###########
  ###################################
  
####################################
##########ITERATIOn 0###########
###################################
#Inizialize and empty strategy

 x<-rep(0,n_totalint)
 y<-rep(0,n_bin_actions)

#Run summary for first situation
Iteration0<-summary(x,y
         ,PM
         ,FinalInterventionsResults$Outcomes
         ,criandstr$weights
         ,n_bin_actions
         ,n_totalint)

#OutputTable in Iteration 0
output$Iteration0table<-DT::renderDataTable({
  datatable(as.data.frame(Iteration0[[5]]),filter = 'top',class = 'white-space: nowrap')
})

#Read what happens on the table
s1<-input$Iteration0table_rows_all #Shows rows after being filtered
s2<-input$Iteration0table_rows_selected

# output$testfilter<-renderDataTable({ExtraInt<-Iteration0[[5]]
# s1<-input$Iteration0table_rows_all
# inp<-ExtraInt[s1,]$Compl
# as.data.frame(inp)})


#OutputGraph in Iteration 0
output$Iteration0graph<-renderPlot({
  ExtraInt<-Iteration0[[5]]
  s1<-input$Iteration0table_rows_all
  ExtraInt<-ExtraInt[s1,]
  inp<-ExtraInt$Compl
  outp<-ExtraInt$Out
  #eff<-dea(inp,outp)
  #DEA<-data.frame(eff=eff$eff)
  dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
  ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
  
  axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
  axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
  
 #if (length(s2)) points(ExtraInt[s2,], pch = 19, cex = 2)
})

output$Add0<-renderUI({actionButton("Add0", "Add selected Interventions to the strategy")})

####################################
##########ITERATIOn 1###########
###################################



observeEvent(input$Add0,{
  
  s2<-input$Iteration0table_rows_selected
  x[s2]<-1
  # y<-rep(0,n_bin_actions)
  Iteration1<-summary(x,y
                      ,PM
                      ,FinalInterventionsResults$Outcomes
                      ,criandstr$weights
                      ,n_bin_actions
                      ,n_totalint)
  
  #SHOW SUMMARY OF STRATEGY
  output$summary1<-renderText({paste("The percentage of Outcomes reached by the strategy is",Iteration1[[1]] ) })
  output$summary2<-renderText({paste("The percentage of Complexity reached by the strategy is",Iteration1[[2]] ) })
  output$summary3<-renderText({
    int<-as.data.frame(Iteration1[[3]])
    count_int<-sum(int$choose)
    paste("The number of selected Interventions is",count_int ) })
  output$summary4<-renderText({
    df_acc<-as.data.frame(Iteration1[[4]])
    count_acc<-sum(df_acc$need[1:n_bin_actions])
    paste("The number of needed Actions is",count_acc ) })
 
  
#SHOW INTERVENTIONS TO ADD 

#OutputTable in Iteration 1
output$Iteration1table<-DT::renderDataTable({
  datatable(as.data.frame(Iteration1[[5]]),filter = 'top',class = 'white-space: nowrap')
})
s11<-input$Iteration1table_rows_all #Shows rows after being filtered
s12<-input$Iteration1table_rows_selected

#OutputGraph in Iteration 1
output$Iteration1graph<-renderPlot({
  ExtraInt<-Iteration1[[5]]
  s11<-input$Iteration1table_rows_all
  ExtraInt<-ExtraInt[s11,]
  inp<-ExtraInt$Compl
  outp<-ExtraInt$Out
  dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                    ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
  
  axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
  axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
  
  #if (length(s2)) points(ExtraInt[s2,], pch = 19, cex = 2)
})

output$Add1<-renderUI({actionButton("Add1", "Add selected Interventions to the strategy")})

})


    

  }) 
})
}






