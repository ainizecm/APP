function(input, output, session) {
 
  runjs(jsCode)
  
  observe ({
  #If the file is null do nothing, otherwise start reading the inputs
  file <-input$file.upload
  if (is.null(file)) 
    return(NULL)
  
  EXCEL<-file$datapath
  
     
     
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
    
    #Names of the interventins and actions
    #Read the actions names from the top of of the Performance Matrix NEED TO CHANCHE IF ACTIONS CHANGE
    Actnames<-t(read.xlsx(EXCEL,"PerformanceMatrix",rowIndex =c(4,5), colIndex = 14:(16+n_bin_actions),encoding='UTF-8',header=FALSE))
    Actnames<-data.frame(Action=Actnames[,1],Code=Actnames[,2])
    
    ###################################
    ##########READ PERFORMANCE MATRIX###
    ####################################
    
   
    
    PM<-read.xlsx2(EXCEL,"PerformanceMatrix" ,startRow=5, endRow = (5+n_totalint),encoding='UTF-8',header=TRUE)
    PM[,c("encoding")]<-NULL
    PM[,c(4:ncol(PM))]<-apply(PM[,c(4:ncol(PM))],c(1,2),as.numeric)
    output$PM<-renderDataTable({PM})
    
    ####################################
    ##########COMPUTE RESULTS###########
    ###################################
    
     #Generate the criteria and actions vector
     criandstr<-FinalWeights(EXCEL,n_bin_actions,n_cri_resp)
     
    
     #Define strategy data frame to populate later
     FinalInterventionsResults<-data.frame(Block=vector(),Description=vector(),Code=vector(),Average_Outcomes=vector(),Average_Complexity=vector())
     strategy<-list()
     blocknames<-vector()
     
     #Generate each block results
     for (i in 1:n_blocks){#Loop throught the blocks
       blocknames[i]<-PM[7,"Code"]
       for (j in 1:n_resp[i]){#Loop through the respondents
         print(paste("Computing responses for respondat",j, "in block",i))
         #Define the starting and ending rows
         start<-7+(1+n_int[i])*(j-1) #Int start in row 7 in the template+the number of interventions and two extra rows for each new respondant
         end<-7+(1+n_int[i])*(j-1)+n_int[i]-1 #sum the number of interventions to now where to end
         #Sheet
         #sheet<-as.character(paste0("IC",i))
         
         #Generate resuls for each respondant
         print(paste("Computing results...",j, "in block",i))
         results<-resultsbyResp(EXCEL,i,start:end,criandstr,n_bin_actions)
         assign(paste0("resultsB",i,"_R",j),results)
         
         
         #strategy dataframe
         print(paste("Generating strategy dataset...",j, "in block",i))
         if (j==1) {strategy[[i]]<-data.frame(results[,c(1,2,3,4,7)])}#If it is the first respondant it will get the names
         else{strategy[[i]]<-cbind(strategy[[i]],results[,c(4,7)])}
       }
       print(paste("Generating averages",j, "in block",i))
       strategy[[i]]$Average_Outcomes<-round(rowMeans(strategy[[i]][which(colnames(strategy[[i]])=="OutcomesMark")]),2)
       strategy[[i]]$Average_Complexity<-round(rowMeans(strategy[[i]][which(colnames(strategy[[i]])=="ComplexityMark")]),2)
       
       
       #Generate a table showing the final results for each intervention
       print(paste("Generating FinalInterventions dataframe...",j, "in block",i))
       FinalInterventionsResults<-rbind(FinalInterventionsResults,
                                        data.frame(Block=strategy[[i]]$Block,
                                                     Descrition=strategy[[i]]$Description,
                                                     Code=strategy[[i]]$Code,
                                                     Outcomes=strategy[[i]]$Average_Outcomes,
                                                    Complexity=strategy[[i]]$Average_Complexity))
     }
  
     
##MAYBE???SAVE strategy AND FINALINTERVENTIONS????##
     
  
     ####################################
     ##########GENERATE OUTPUTS###########
     ###################################
  #Show the final matrix
  output$FinalM<-DT::renderDataTable({datatable(FinalInterventionsResults,
                                               filter = 'top',class = 'white-space: nowrap')
                                      })
     
 output$downloadData1 <- downloadHandler(
       filename = function() { paste('FinalInterventionsResults', '.csv', sep='') },
       content = function(file) {
         write.csv(FinalInterventionsResults, file)
       }
     )
     
     
  #Show results for each block-Table and plot
  output$RankingbyBlock<-renderUI({
    strategy_list<-lapply(1:n_blocks,function(i){
      fluidPage(paste("BLOCK",i,"RESULTS"),
                fluidRow(div(style="color:white", renderText("  jkkjh "))),
                fluidRow(div(style="color:white", renderText("  jkkjh "))),
                fluidRow(renderText("   ")),
      fluidRow(
      column(6,
             DT::renderDataTable({
               datatable(
                # FinalInterventionsResults[which(FinalInterventionsResults$Block==blocknames[i]),],
                     data.frame(
                     Description=sapply(strategy[[i]]$Description,function(t) paste0(strtrim(t,20),'...')),
                     Code=strategy[[i]]$Code,
                     Outcomes=strategy[[i]]$Average_Outcomes,
                     Complexity=strategy[[i]]$Average_Complexity),
                    class = 'white-space: nowrap'
      )}
      
      ))
      ,column(6,renderPlot({grafico(strategy[[i]])}))
         ),
      fluidRow(div(style="color:white", renderText("  jkkjh "))),
      fluidRow(div(style="color:white", renderText("  jkkjh "))))
      })
    do.call(tagList, strategy_list)
    })


  
  #Show the ranking for actions
  Comp<-AccCompl(EXCEL,n_bin_actions,n_cri_resp)
  Actions<-data.frame(Code=Comp$Code,Description=Comp$Spanish)
  Actions$Interventions<-colSums(PM[,c(14:(13+n_bin_actions))])
  Actions$Complexity<-Comp$FINAL
  rownames(Actions)<-NULL
  output$ActionsRanking<-renderDataTable({
    # Comp<-AccCompl(EXCEL,n_bin_actions,n_cri_resp)
    # Actions<-data.frame(Code=Comp$Code,Description=Comp$Spanish)
    # Actions$Interventions<-colSums(PM[,c(14:(13+n_bin_actions))])
    # Actions$Complexity<-Comp$FINAL
    # rownames(Actions)<-NULL
    Actions
  },caption="Actions")
  
  output$downloadData2 <- downloadHandler(
    filename = function() { paste('Actions', '.csv', sep='') },
    content = function(file) {
      write.csv(Actions, file)
    }
  )
  
  ##Details by respondant if you want to check some results
  #Show results for each respondant  
  output$DetailedResults<-renderUI({
    strategy_list<-lapply(1:n_blocks,function(i){renderDataTable({strategy[[i]]},caption=paste0('Block',i))})
    do.call(tagList, strategy_list)
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
 

  ###Iteration0####
number<-0
  x<-rep(0,n_totalint)
  y<-rep(0,n_bin_actions)
   
    It<-strategy(x,y
                 ,PM
                 ,FinalInterventionsResults$Outcomes
                 ,criandstr$weights
                 ,n_bin_actions
                 ,n_totalint)
    
    #SHOW SUMMARY OF STRATEGY
    output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
    output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
    output[[paste0("strategy",number,"3")]]<-renderText({
      int<-as.data.frame(It[[3]])
      count_int<-sum(int$choose)
      paste("The number of selected Interventions is",count_int ) })
    output[[paste0("strategy",number,"4")]]<-renderText({
      df_acc<-as.data.frame(It[[4]])
      count_acc<-sum(df_acc$need[1:n_bin_actions])
      paste("The number of needed Actions is",count_acc ) })
    
    output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({It[[3]][which(It[[3]]$choose==1),c(1,2)]})
    output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
      ac<-data.frame(
        Actnames$Action
        , It[[4]])
      rownames(ac)<-NULL
      ac[which(ac$need>0),]
      
    })
    
    
    #SHOW INTERVENTIONS TO ADD 
    
    #OutputTable 
    output[[paste0("Iteration",number,"table")]]<-renderDataTable({
      datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')
    })
    s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
    s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
    
    #OutputGraph 
    output[[paste0("Iteration",number,"graph")]]<-renderPlot({
      ExtraInt<-It[[5]]
      s11<-input[[paste0("Iteration",number,"table_rows_all")]]
      ExtraInt<-ExtraInt[s11,]
      inp<-ExtraInt$Compl
      outp<-ExtraInt$Out
      dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                        ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
      
      axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
      axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
    })
    

    ############################ITERATION 1####################################  
observeEvent(input[[paste0("Add",0)]],{
  number<-1
  prev<-number-1            
  s2<-input[[paste0("Iteration",prev,"table_rows_selected")]]
  x[s2]<-1
  #y is the needed vector from the previous iteration
  y[which(It[[4]]$need==1)]<-1
  It<-strategy(x,y
               ,PM
               ,FinalInterventionsResults$Outcomes
               ,criandstr$weights
               ,n_bin_actions
               ,n_totalint)
  
  
  #SHOW SUMMARY OF STRATEGY
  output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
  output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
  output[[paste0("strategy",number,"3")]]<-renderText({
    int<-as.data.frame(It[[3]])
    count_int<-sum(int$choose)
    paste("The number of selected Interventions is",count_int ) })
  output[[paste0("strategy",number,"4")]]<-renderText({
    df_acc<-as.data.frame(It[[4]])
    count_acc<-sum(df_acc$need[1:n_bin_actions])
    paste("The number of needed Actions is",count_acc ) })

  #ITERATIONS AND ACTIONS IN THE STRATEGY
  iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
  output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
    })
  output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
    filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
    content = function(file) {
      write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
    }
  )
  
  ac<-data.frame(Actnames$Action, It[[4]])
  ac<-ac[which(ac$need>0),c(1,2,5)]
  #ac$nedd<-round(ac$nedd,2)
  output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
    ac
  })
  output[[paste0('Download',number,'Actions')]] <- downloadHandler(
    filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
    content = function(file) {
      write.csv(ac, file)
    }
  )
  
  #SHOW INTERVENTIONS TO ADD 
  
  #OutputTable in Iteration 1
  output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
  output[[paste0('Download',number,'table')]] <- downloadHandler(
    filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
    content = function(file) {
      write.csv(It[[5]], file)
    }
  )
  s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
  s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
  

  
  #OutputGraph in Iteration 1
  output[[paste0("Iteration",number,"graph")]]<-renderPlot({
    ExtraInt<-It[[5]]
    s11<-input[[paste0("Iteration",number,"table_rows_all")]]
    ExtraInt<-ExtraInt[s11,]
    inp<-ExtraInt$Compl
    outp<-ExtraInt$Out
    dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                      ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
    
    axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
    axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
    
  })
  
  output[[paste0('Download',number,'graph')]] <- downloadHandler(
    filename =  function() {
      paste0('Download',number,'graph.png')
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
        png(file) 
      ExtraInt<-It[[5]]
      s11<-input[[paste0("Iteration",number,"table_rows_all")]]
      ExtraInt<-ExtraInt[s11,]
      inp<-ExtraInt$Compl
      outp<-ExtraInt$Out
      dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                        ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
      
      axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
      axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
      dev.off()  # turn the device off
      
    } 
  )
  
  ############################ITERATION 2####################################
    observeEvent(input[[paste0("Add",1)]],{
      number<-2
      
      prev<-number-1    
      
      #Read from the table
      position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
      index<-rownames(It[[5]])
      selected<-as.numeric(index[position])
      x[selected]<-1
      #y is the needed vector from the previous iteration
      y[which(It[[4]]$need==1)]<-1
      It<-strategy(x,y
                   ,PM
                   ,FinalInterventionsResults$Outcomes
                   ,criandstr$weights
                   ,n_bin_actions
                   ,n_totalint)
      
      
      #SHOW SUMMARY OF STRATEGY
      output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
      output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
      output[[paste0("strategy",number,"3")]]<-renderText({
        int<-as.data.frame(It[[3]])
        count_int<-sum(int$choose)
        paste("The number of selected Interventions is",count_int ) })
      output[[paste0("strategy",number,"4")]]<-renderText({
        df_acc<-as.data.frame(It[[4]])
        count_acc<-sum(df_acc$need[1:n_bin_actions])
        paste("The number of needed Actions is",count_acc ) })
      
      #ITERATIONS AND ACTIONS IN THE STRATEGY
      iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
      output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
      })
      output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
        filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
        content = function(file) {
          write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
        }
      )
      
      ac<-data.frame(Actnames$Action, It[[4]])
      ac<-ac[which(ac$need>0),c(1,2,5)]
      #ac$nedd<-round(ac$nedd,2)
      output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
        ac
      })
      output[[paste0('Download',number,'Actions')]] <- downloadHandler(
        filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
        content = function(file) {
          write.csv(ac, file)
        }
      )
      
      #SHOW INTERVENTIONS TO ADD 
      
      #OutputTable in Iteration 1
      output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
      output[[paste0('Download',number,'table')]] <- downloadHandler(
        filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
        content = function(file) {
          write.csv(It[[5]], file)
        }
      )
      s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
      s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
      
      
      
      #OutputGraph in Iteration 1
      output[[paste0("Iteration",number,"graph")]]<-renderPlot({
        ExtraInt<-It[[5]]
        s11<-input[[paste0("Iteration",number,"table_rows_all")]]
        ExtraInt<-ExtraInt[s11,]
        inp<-ExtraInt$Compl
        outp<-ExtraInt$Out
        dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                          ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
        
        axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
        axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
        
      })
      
      output[[paste0('Download',number,'graph')]] <- downloadHandler(
        filename =  function() {
          paste0('Download',number,'graph.png')
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          png(file) 
          ExtraInt<-It[[5]]
          s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
          ExtraInt<-ExtraInt[s11,]
          inp<-ExtraInt$Compl
          outp<-ExtraInt$Out
          dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                            ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
          
          axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
          axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
          dev.off()  # turn the device off
          
        } 
      )
    ############################ITERATION 3####################################
      observeEvent(input[[paste0("Add",2)]],{
        number<-number+1
        prev<-number-1            
        
        #read from previous table
        position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
        index<-rownames(It[[5]])
        selected<-as.numeric(index[position])
        x[selected]<-1
        #y is the needed vector from the previous iteration
        y[which(It[[4]]$need==1)]<-1
        It<-strategy(x,y
                     ,PM
                     ,FinalInterventionsResults$Outcomes
                     ,criandstr$weights
                     ,n_bin_actions
                     ,n_totalint)
        
        
        #SHOW SUMMARY OF STRATEGY
        output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
        output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
        output[[paste0("strategy",number,"3")]]<-renderText({
          int<-as.data.frame(It[[3]])
          count_int<-sum(int$choose)
          paste("The number of selected Interventions is",count_int ) })
        output[[paste0("strategy",number,"4")]]<-renderText({
          df_acc<-as.data.frame(It[[4]])
          count_acc<-sum(df_acc$need[1:n_bin_actions])
          paste("The number of needed Actions is",count_acc ) })
        
        #ITERATIONS AND ACTIONS IN THE STRATEGY
        iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
        output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
        })
        output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
          filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
          content = function(file) {
            write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
          }
        )
        
        ac<-data.frame(Actnames$Action, It[[4]])
        ac<-ac[which(ac$need>0),c(1,2,5)]
        #ac$nedd<-round(ac$nedd,2)
        output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
          ac
        })
        output[[paste0('Download',number,'Actions')]] <- downloadHandler(
          filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
          content = function(file) {
            write.csv(ac, file)
          }
        )
        
        #SHOW INTERVENTIONS TO ADD 
        
        #OutputTable in Iteration 1
        output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
        output[[paste0('Download',number,'table')]] <- downloadHandler(
          filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
          content = function(file) {
            write.csv(It[[5]], file)
          }
        )
        s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
        s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
        
        
        
        #OutputGraph in Iteration 1
        output[[paste0("Iteration",number,"graph")]]<-renderPlot({
          ExtraInt<-It[[5]]
          s11<-input[[paste0("Iteration",number,"table_rows_all")]]
          ExtraInt<-ExtraInt[s11,]
          inp<-ExtraInt$Compl
          outp<-ExtraInt$Out
          dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                            ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
          
          axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
          axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
          
        })
        
        output[[paste0('Download',number,'graph')]] <- downloadHandler(
          filename =  function() {
            paste0('Download',number,'graph.png')
          },
          # content is a function with argument file. content writes the plot to the device
          content = function(file) {
            png(file) 
            ExtraInt<-It[[5]]
            s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
            ExtraInt<-ExtraInt[s11,]
            inp<-ExtraInt$Compl
            outp<-ExtraInt$Out
            dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                              ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
            
            axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
            axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
            dev.off()  # turn the device off
            
          } 
        )
       
        ############################ITERATION 4####################################
        observeEvent(input[[paste0("Add",3)]],{
          number<-number+1
          prev<-number-1            
          
          #read from previous table
          position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
          index<-rownames(It[[5]])
          selected<-as.numeric(index[position])
          x[selected]<-1
          #y is the needed vector from the previous iteration
          y[which(It[[4]]$need==1)]<-1
          It<-strategy(x,y
                       ,PM
                       ,FinalInterventionsResults$Outcomes
                       ,criandstr$weights
                       ,n_bin_actions
                       ,n_totalint)
          
          
          #SHOW SUMMARY OF STRATEGY
          output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
          output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
          output[[paste0("strategy",number,"3")]]<-renderText({
            int<-as.data.frame(It[[3]])
            count_int<-sum(int$choose)
            paste("The number of selected Interventions is",count_int ) })
          output[[paste0("strategy",number,"4")]]<-renderText({
            df_acc<-as.data.frame(It[[4]])
            count_acc<-sum(df_acc$need[1:n_bin_actions])
            paste("The number of needed Actions is",count_acc ) })
          
          #ITERATIONS AND ACTIONS IN THE STRATEGY
          iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
          output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
          })
          output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
            filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
            content = function(file) {
              write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
            }
          )
          
          ac<-data.frame(Actnames$Action, It[[4]])
          ac<-ac[which(ac$need>0),c(1,2,5)]
          #ac$nedd<-round(ac$nedd,2)
          output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
            ac
          })
          output[[paste0('Download',number,'Actions')]] <- downloadHandler(
            filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
            content = function(file) {
              write.csv(ac, file)
            }
          )
          
          #SHOW INTERVENTIONS TO ADD 
          
          #OutputTable in Iteration 1
          output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
          output[[paste0('Download',number,'table')]] <- downloadHandler(
            filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
            content = function(file) {
              write.csv(It[[5]], file)
            }
          )
          s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
          s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
          
          
          
          #OutputGraph in Iteration 1
          output[[paste0("Iteration",number,"graph")]]<-renderPlot({
            ExtraInt<-It[[5]]
            s11<-input[[paste0("Iteration",number,"table_rows_all")]]
            ExtraInt<-ExtraInt[s11,]
            inp<-ExtraInt$Compl
            outp<-ExtraInt$Out
            dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                              ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
            
            axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
            axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
            
          })
          
          output[[paste0('Download',number,'graph')]] <- downloadHandler(
            filename =  function() {
              paste0('Download',number,'graph.png')
            },
            # content is a function with argument file. content writes the plot to the device
            content = function(file) {
              png(file) 
              ExtraInt<-It[[5]]
              s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
              ExtraInt<-ExtraInt[s11,]
              inp<-ExtraInt$Compl
              outp<-ExtraInt$Out
              dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
              
              axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
              axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
              dev.off()  # turn the device off
              
            } 
          ) 
          
          ############################ITERATION 5####################################
          observeEvent(input[[paste0("Add",4)]],{
            number<-number+1
            prev<-number-1            
            
            #read from previous table
            position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
            index<-rownames(It[[5]])
            selected<-as.numeric(index[position])
            x[selected]<-1
            #y is the needed vector from the previous iteration
            y[which(It[[4]]$need==1)]<-1
            It<-strategy(x,y
                         ,PM
                         ,FinalInterventionsResults$Outcomes
                         ,criandstr$weights
                         ,n_bin_actions
                         ,n_totalint)
            
            
            #SHOW SUMMARY OF STRATEGY
            output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
            output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
            output[[paste0("strategy",number,"3")]]<-renderText({
              int<-as.data.frame(It[[3]])
              count_int<-sum(int$choose)
              paste("The number of selected Interventions is",count_int ) })
            output[[paste0("strategy",number,"4")]]<-renderText({
              df_acc<-as.data.frame(It[[4]])
              count_acc<-sum(df_acc$need[1:n_bin_actions])
              paste("The number of needed Actions is",count_acc ) })
            
            #ITERATIONS AND ACTIONS IN THE STRATEGY
            iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
            output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
            })
            output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
              filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
              content = function(file) {
                write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
              }
            )
            
            ac<-data.frame(Actnames$Action, It[[4]])
            ac<-ac[which(ac$need>0),c(1,2,5)]
            #ac$nedd<-round(ac$nedd,2)
            output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
              ac
            })
            output[[paste0('Download',number,'Actions')]] <- downloadHandler(
              filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
              content = function(file) {
                write.csv(ac, file)
              }
            )
            
            #SHOW INTERVENTIONS TO ADD 
            
            #OutputTable in Iteration 1
            output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
            output[[paste0('Download',number,'table')]] <- downloadHandler(
              filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
              content = function(file) {
                write.csv(It[[5]], file)
              }
            )
            s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
            s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
            
            
            
            #OutputGraph in Iteration 1
            output[[paste0("Iteration",number,"graph")]]<-renderPlot({
              ExtraInt<-It[[5]]
              s11<-input[[paste0("Iteration",number,"table_rows_all")]]
              ExtraInt<-ExtraInt[s11,]
              inp<-ExtraInt$Compl
              outp<-ExtraInt$Out
              dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
              
              axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
              axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
              
            })
            
            output[[paste0('Download',number,'graph')]] <- downloadHandler(
              filename =  function() {
                paste0('Download',number,'graph.png')
              },
              # content is a function with argument file. content writes the plot to the device
              content = function(file) {
                png(file) 
                ExtraInt<-It[[5]]
                s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                ExtraInt<-ExtraInt[s11,]
                inp<-ExtraInt$Compl
                outp<-ExtraInt$Out
                dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                  ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                
                axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                dev.off()  # turn the device off
                
              } 
            ) 
            
            
            
            
            
            
            
            ############################ITERATION 6####################################
           ## observeEvent(input[[paste0("Add",5)]],{
              number<-number+1
              prev<-number-1            
              
              #read from previous table
              position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
              index<-rownames(It[[5]])
              selected<-as.numeric(index[position])
              x[selected]<-1
              #y is the needed vector from the previous iteration
              y[which(It[[4]]$need==1)]<-1
              It<-strategy(x,y
                           ,PM
                           ,FinalInterventionsResults$Outcomes
                           ,criandstr$weights
                           ,n_bin_actions
                           ,n_totalint)
              
              
              #SHOW SUMMARY OF STRATEGY
              output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
              output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
              output[[paste0("strategy",number,"3")]]<-renderText({
                int<-as.data.frame(It[[3]])
                count_int<-sum(int$choose)
                paste("The number of selected Interventions is",count_int ) })
              output[[paste0("strategy",number,"4")]]<-renderText({
                df_acc<-as.data.frame(It[[4]])
                count_acc<-sum(df_acc$need[1:n_bin_actions])
                paste("The number of needed Actions is",count_acc ) })
              
              #ITERATIONS AND ACTIONS IN THE STRATEGY
              iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
              output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
              })
              output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                content = function(file) {
                  write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                }
              )
              
              ac<-data.frame(Actnames$Action, It[[4]])
              ac<-ac[which(ac$need>0),c(1,2,5)]
              #ac$nedd<-round(ac$nedd,2)
              output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                ac
              })
              output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                content = function(file) {
                  write.csv(ac, file)
                }
              )
              
              #SHOW INTERVENTIONS TO ADD 
              
              #OutputTable in Iteration 1
              output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
              output[[paste0('Download',number,'table')]] <- downloadHandler(
                filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                content = function(file) {
                  write.csv(It[[5]], file)
                }
              )
              s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
              s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
              
              
              
              #OutputGraph in Iteration 1
              output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                ExtraInt<-It[[5]]
                s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                ExtraInt<-ExtraInt[s11,]
                inp<-ExtraInt$Compl
                outp<-ExtraInt$Out
                dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                  ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                
                axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                
              })
              
              output[[paste0('Download',number,'graph')]] <- downloadHandler(
                filename =  function() {
                  paste0('Download',number,'graph.png')
                },
                # content is a function with argument file. content writes the plot to the device
                content = function(file) {
                  png(file) 
                  ExtraInt<-It[[5]]
                  s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                  ExtraInt<-ExtraInt[s11,]
                  inp<-ExtraInt$Compl
                  outp<-ExtraInt$Out
                  dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                    ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                  
                  axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                  axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                  dev.off()  # turn the device off
                  
                } 
              )    
            
            
            
              
              ############################ITERATION 7####################################
              observeEvent(input[[paste0("Add",6)]],{
                number<-number+1
                prev<-number-1            
                
                #read from previous table
                position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                index<-rownames(It[[5]])
                selected<-as.numeric(index[position])
                x[selected]<-1
                #y is the needed vector from the previous iteration
                y[which(It[[4]]$need==1)]<-1
                It<-strategy(x,y
                             ,PM
                             ,FinalInterventionsResults$Outcomes
                             ,criandstr$weights
                             ,n_bin_actions
                             ,n_totalint)
                
                
                #SHOW SUMMARY OF STRATEGY
                output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                output[[paste0("strategy",number,"3")]]<-renderText({
                  int<-as.data.frame(It[[3]])
                  count_int<-sum(int$choose)
                  paste("The number of selected Interventions is",count_int ) })
                output[[paste0("strategy",number,"4")]]<-renderText({
                  df_acc<-as.data.frame(It[[4]])
                  count_acc<-sum(df_acc$need[1:n_bin_actions])
                  paste("The number of needed Actions is",count_acc ) })
                
                #ITERATIONS AND ACTIONS IN THE STRATEGY
                iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                })
                output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                  filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                  content = function(file) {
                    write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                  }
                )
                
                ac<-data.frame(Actnames$Action, It[[4]])
                ac<-ac[which(ac$need>0),c(1,2,5)]
                #ac$nedd<-round(ac$nedd,2)
                output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                  ac
                })
                output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                  filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                  content = function(file) {
                    write.csv(ac, file)
                  }
                )
                
                #SHOW INTERVENTIONS TO ADD 
                
                #OutputTable in Iteration 1
                output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                output[[paste0('Download',number,'table')]] <- downloadHandler(
                  filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                  content = function(file) {
                    write.csv(It[[5]], file)
                  }
                )
                s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                
                
                
                #OutputGraph in Iteration 1
                output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                  ExtraInt<-It[[5]]
                  s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                  ExtraInt<-ExtraInt[s11,]
                  inp<-ExtraInt$Compl
                  outp<-ExtraInt$Out
                  dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                    ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                  
                  axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                  axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                  
                })
                
                output[[paste0('Download',number,'graph')]] <- downloadHandler(
                  filename =  function() {
                    paste0('Download',number,'graph.png')
                  },
                  # content is a function with argument file. content writes the plot to the device
                  content = function(file) {
                    png(file) 
                    ExtraInt<-It[[5]]
                    s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                    ExtraInt<-ExtraInt[s11,]
                    inp<-ExtraInt$Compl
                    outp<-ExtraInt$Out
                    dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                      ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                    
                    axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                    axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                    dev.off()  # turn the device off
                    
                  } 
                )
                
        
                ############################ITERATION 8####################################
                observeEvent(input[[paste0("Add",7)]],{
                  number<-number+1
                  prev<-number-1            
                  
                  #read from previous table
                  position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                  index<-rownames(It[[5]])
                  selected<-as.numeric(index[position])
                  x[selected]<-1
                  #y is the needed vector from the previous iteration
                  y[which(It[[4]]$need==1)]<-1
                  It<-strategy(x,y
                               ,PM
                               ,FinalInterventionsResults$Outcomes
                               ,criandstr$weights
                               ,n_bin_actions
                               ,n_totalint)
                  
                  
                  #SHOW SUMMARY OF STRATEGY
                  output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                  output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                  output[[paste0("strategy",number,"3")]]<-renderText({
                    int<-as.data.frame(It[[3]])
                    count_int<-sum(int$choose)
                    paste("The number of selected Interventions is",count_int ) })
                  output[[paste0("strategy",number,"4")]]<-renderText({
                    df_acc<-as.data.frame(It[[4]])
                    count_acc<-sum(df_acc$need[1:n_bin_actions])
                    paste("The number of needed Actions is",count_acc ) })
                  
                  #ITERATIONS AND ACTIONS IN THE STRATEGY
                  iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                  output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                  })
                  output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                    filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                    content = function(file) {
                      write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                    }
                  )
                  
                  ac<-data.frame(Actnames$Action, It[[4]])
                  ac<-ac[which(ac$need>0),c(1,2,5)]
                  #ac$nedd<-round(ac$nedd,2)
                  output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                    ac
                  })
                  output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                    filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                    content = function(file) {
                      write.csv(ac, file)
                    }
                  )
                  
                  #SHOW INTERVENTIONS TO ADD 
                  
                  #OutputTable in Iteration 1
                  output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                  output[[paste0('Download',number,'table')]] <- downloadHandler(
                    filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                    content = function(file) {
                      write.csv(It[[5]], file)
                    }
                  )
                  s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                  s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                  
                  
                  
                  #OutputGraph in Iteration 1
                  output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                    ExtraInt<-It[[5]]
                    s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                    ExtraInt<-ExtraInt[s11,]
                    inp<-ExtraInt$Compl
                    outp<-ExtraInt$Out
                    dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                      ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                    
                    axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                    axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                    
                  })
                  
                  output[[paste0('Download',number,'graph')]] <- downloadHandler(
                    filename =  function() {
                      paste0('Download',number,'graph.png')
                    },
                    # content is a function with argument file. content writes the plot to the device
                    content = function(file) {
                      png(file) 
                      ExtraInt<-It[[5]]
                      s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                      ExtraInt<-ExtraInt[s11,]
                      inp<-ExtraInt$Compl
                      outp<-ExtraInt$Out
                      dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                        ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                      
                      axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                      axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                      dev.off()  # turn the device off
                      
                    } 
                  )
                
                
                
                
                  
                  ############################ITERATION 9####################################
                  observeEvent(input[[paste0("Add",8)]],{
                    number<-number+1
                    prev<-number-1            
                    
                    #read from previous table
                    position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                    index<-rownames(It[[5]])
                    selected<-as.numeric(index[position])
                    x[selected]<-1
                    #y is the needed vector from the previous iteration
                    y[which(It[[4]]$need==1)]<-1
                    It<-strategy(x,y
                                 ,PM
                                 ,FinalInterventionsResults$Outcomes
                                 ,criandstr$weights
                                 ,n_bin_actions
                                 ,n_totalint)
                    
                    
                    #SHOW SUMMARY OF STRATEGY
                    output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                    output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                    output[[paste0("strategy",number,"3")]]<-renderText({
                      int<-as.data.frame(It[[3]])
                      count_int<-sum(int$choose)
                      paste("The number of selected Interventions is",count_int ) })
                    output[[paste0("strategy",number,"4")]]<-renderText({
                      df_acc<-as.data.frame(It[[4]])
                      count_acc<-sum(df_acc$need[1:n_bin_actions])
                      paste("The number of needed Actions is",count_acc ) })
                    
                    #ITERATIONS AND ACTIONS IN THE STRATEGY
                    iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                    output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                    })
                    output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                      filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                      content = function(file) {
                        write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                      }
                    )
                    
                    ac<-data.frame(Actnames$Action, It[[4]])
                    ac<-ac[which(ac$need>0),c(1,2,5)]
                    #ac$nedd<-round(ac$nedd,2)
                    output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                      ac
                    })
                    output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                      filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                      content = function(file) {
                        write.csv(ac, file)
                      }
                    )
                    
                    #SHOW INTERVENTIONS TO ADD 
                    
                    #OutputTable in Iteration 1
                    output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                    output[[paste0('Download',number,'table')]] <- downloadHandler(
                      filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                      content = function(file) {
                        write.csv(It[[5]], file)
                      }
                    )
                    s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                    s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                    
                    
                    
                    #OutputGraph in Iteration 1
                    output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                      ExtraInt<-It[[5]]
                      s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                      ExtraInt<-ExtraInt[s11,]
                      inp<-ExtraInt$Compl
                      outp<-ExtraInt$Out
                      dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                        ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                      
                      axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                      axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                      
                    })
                    
                    output[[paste0('Download',number,'graph')]] <- downloadHandler(
                      filename =  function() {
                        paste0('Download',number,'graph.png')
                      },
                      # content is a function with argument file. content writes the plot to the device
                      content = function(file) {
                        png(file) 
                        ExtraInt<-It[[5]]
                        s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                        ExtraInt<-ExtraInt[s11,]
                        inp<-ExtraInt$Compl
                        outp<-ExtraInt$Out
                        dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                          ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                        
                        axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                        axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                        dev.off()  # turn the device off
                        
                      } 
                    )
                  
                  
                  
                    ############################ITERATION 10####################################
                    #observeEvent(input[[paste0("Add",9)]],{
                      number<-number+1
                      prev<-number-1            
                      
                      #read from previous table
                      position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                      index<-rownames(It[[5]])
                      selected<-as.numeric(index[position])
                      x[selected]<-1
                      #y is the needed vector from the previous iteration
                      y[which(It[[4]]$need==1)]<-1
                      It<-strategy(x,y
                                   ,PM
                                   ,FinalInterventionsResults$Outcomes
                                   ,criandstr$weights
                                   ,n_bin_actions
                                   ,n_totalint)
                      
                      
                      #SHOW SUMMARY OF STRATEGY
                      output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                      output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                      output[[paste0("strategy",number,"3")]]<-renderText({
                        int<-as.data.frame(It[[3]])
                        count_int<-sum(int$choose)
                        paste("The number of selected Interventions is",count_int ) })
                      output[[paste0("strategy",number,"4")]]<-renderText({
                        df_acc<-as.data.frame(It[[4]])
                        count_acc<-sum(df_acc$need[1:n_bin_actions])
                        paste("The number of needed Actions is",count_acc ) })
                      
                      #ITERATIONS AND ACTIONS IN THE STRATEGY
                      iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                      output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                      })
                      output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                        filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                        content = function(file) {
                          write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                        }
                      )
                      
                      ac<-data.frame(Actnames$Action, It[[4]])
                      ac<-ac[which(ac$need>0),c(1,2,5)]
                      #ac$nedd<-round(ac$nedd,2)
                      output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                        ac
                      })
                      output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                        filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                        content = function(file) {
                          write.csv(ac, file)
                        }
                      )
                      
                      #SHOW INTERVENTIONS TO ADD 
                      
                      #OutputTable in Iteration 1
                      output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                      output[[paste0('Download',number,'table')]] <- downloadHandler(
                        filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                        content = function(file) {
                          write.csv(It[[5]], file)
                        }
                      )
                      s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                      s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                      
                      
                      
                      #OutputGraph in Iteration 1
                      output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                        ExtraInt<-It[[5]]
                        s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                        ExtraInt<-ExtraInt[s11,]
                        inp<-ExtraInt$Compl
                        outp<-ExtraInt$Out
                        dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                          ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                        
                        axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                        axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                        
                      })
                      
                      output[[paste0('Download',number,'graph')]] <- downloadHandler(
                        filename =  function() {
                          paste0('Download',number,'graph.png')
                        },
                        # content is a function with argument file. content writes the plot to the device
                        content = function(file) {
                          png(file) 
                          ExtraInt<-It[[5]]
                          s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                          ExtraInt<-ExtraInt[s11,]
                          inp<-ExtraInt$Compl
                          outp<-ExtraInt$Out
                          dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                            ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                          
                          axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                          axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                          dev.off()  # turn the device off
                          
                        } 
                      )
                    
                    
                    
                      
                      ############################ITERATION 11####################################
                      observeEvent(input[[paste0("Add",10)]],{
                        number<-number+1
                        prev<-number-1            
                        
                        #read from previous table
                        position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                        index<-rownames(It[[5]])
                        selected<-as.numeric(index[position])
                        x[selected]<-1
                        #y is the needed vector from the previous iteration
                        y[which(It[[4]]$need==1)]<-1
                        It<-strategy(x,y
                                     ,PM
                                     ,FinalInterventionsResults$Outcomes
                                     ,criandstr$weights
                                     ,n_bin_actions
                                     ,n_totalint)
                        
                        
                        #SHOW SUMMARY OF STRATEGY
                        output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                        output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                        output[[paste0("strategy",number,"3")]]<-renderText({
                          int<-as.data.frame(It[[3]])
                          count_int<-sum(int$choose)
                          paste("The number of selected Interventions is",count_int ) })
                        output[[paste0("strategy",number,"4")]]<-renderText({
                          df_acc<-as.data.frame(It[[4]])
                          count_acc<-sum(df_acc$need[1:n_bin_actions])
                          paste("The number of needed Actions is",count_acc ) })
                        
                        #ITERATIONS AND ACTIONS IN THE STRATEGY
                        iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                        output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                        })
                        output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                          filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                          content = function(file) {
                            write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                          }
                        )
                        
                        ac<-data.frame(Actnames$Action, It[[4]])
                        ac<-ac[which(ac$need>0),c(1,2,5)]
                       # ac$nedd<-round(ac$nedd,2)
                        output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                          ac
                        })
                        output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                          filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                          content = function(file) {
                            write.csv(ac, file)
                          }
                        )
                        
                        #SHOW INTERVENTIONS TO ADD 
                        
                        #OutputTable in Iteration 1
                        output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                        output[[paste0('Download',number,'table')]] <- downloadHandler(
                          filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                          content = function(file) {
                            write.csv(It[[5]], file)
                          }
                        )
                        s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                        s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                        
                        
                        
                        #OutputGraph in Iteration 1
                        output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                          ExtraInt<-It[[5]]
                          s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                          ExtraInt<-ExtraInt[s11,]
                          inp<-ExtraInt$Compl
                          outp<-ExtraInt$Out
                          dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                            ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                          
                          axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                          axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                          
                        })
                        
                        output[[paste0('Download',number,'graph')]] <- downloadHandler(
                          filename =  function() {
                            paste0('Download',number,'graph.png')
                          },
                          # content is a function with argument file. content writes the plot to the device
                          content = function(file) {
                            png(file) 
                            ExtraInt<-It[[5]]
                            s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                            ExtraInt<-ExtraInt[s11,]
                            inp<-ExtraInt$Compl
                            outp<-ExtraInt$Out
                            dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                              ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                            
                            axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                            axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                            dev.off()  # turn the device off
                            
                          } 
                        )
                        
                        ############################ITERATION 12####################################
                        observeEvent(input[[paste0("Add",11)]],{
                          number<-number+1
                          prev<-number-1            
                          
                          #read from previous table
                          position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                          index<-rownames(It[[5]])
                          selected<-as.numeric(index[position])
                          x[selected]<-1
                          #y is the needed vector from the previous iteration
                          y[which(It[[4]]$need==1)]<-1
                          It<-strategy(x,y
                                       ,PM
                                       ,FinalInterventionsResults$Outcomes
                                       ,criandstr$weights
                                       ,n_bin_actions
                                       ,n_totalint)
                          
                          
                          #SHOW SUMMARY OF STRATEGY
                          output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                          output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                          output[[paste0("strategy",number,"3")]]<-renderText({
                            int<-as.data.frame(It[[3]])
                            count_int<-sum(int$choose)
                            paste("The number of selected Interventions is",count_int ) })
                          output[[paste0("strategy",number,"4")]]<-renderText({
                            df_acc<-as.data.frame(It[[4]])
                            count_acc<-sum(df_acc$need[1:n_bin_actions])
                            paste("The number of needed Actions is",count_acc ) })
                          
                          #ITERATIONS AND ACTIONS IN THE STRATEGY
                          iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                          output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                          })
                          output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                            filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                            content = function(file) {
                              write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                            }
                          )
                          
                          ac<-data.frame(Actnames$Action, It[[4]])
                          ac<-ac[which(ac$need>0),c(1,2,5)]
                          #ac$nedd<-round(ac$nedd,2)
                          output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                            ac
                          })
                          output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                            filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                            content = function(file) {
                              write.csv(ac, file)
                            }
                          )
                          
                          #SHOW INTERVENTIONS TO ADD 
                          
                          #OutputTable in Iteration 1
                          output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                          output[[paste0('Download',number,'table')]] <- downloadHandler(
                            filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                            content = function(file) {
                              write.csv(It[[5]], file)
                            }
                          )
                          s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                          s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                          
                          
                          
                          #OutputGraph in Iteration 1
                          output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                            ExtraInt<-It[[5]]
                            s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                            ExtraInt<-ExtraInt[s11,]
                            inp<-ExtraInt$Compl
                            outp<-ExtraInt$Out
                            dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                              ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                            
                            axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                            axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                            
                          })
                          
                          output[[paste0('Download',number,'graph')]] <- downloadHandler(
                            filename =  function() {
                              paste0('Download',number,'graph.png')
                            },
                            # content is a function with argument file. content writes the plot to the device
                            content = function(file) {
                              png(file) 
                              ExtraInt<-It[[5]]
                              s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                              ExtraInt<-ExtraInt[s11,]
                              inp<-ExtraInt$Compl
                              outp<-ExtraInt$Out
                              dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                              
                              axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                              axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                              dev.off()  # turn the device off
                              
                            } 
                          )
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                          
                          ############################ITERATION 13####################################
                          observeEvent(input[[paste0("Add",12)]],{
                            number<-number+1
                            prev<-number-1            
                            
                            #read from previous table
                            position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                            index<-rownames(It[[5]])
                            selected<-as.numeric(index[position])
                            x[selected]<-1
                            #y is the needed vector from the previous iteration
                            y[which(It[[4]]$need==1)]<-1
                            It<-strategy(x,y
                                         ,PM
                                         ,FinalInterventionsResults$Outcomes
                                         ,criandstr$weights
                                         ,n_bin_actions
                                         ,n_totalint)
                            
                            
                            #SHOW SUMMARY OF STRATEGY
                            output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                            output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                            output[[paste0("strategy",number,"3")]]<-renderText({
                              int<-as.data.frame(It[[3]])
                              count_int<-sum(int$choose)
                              paste("The number of selected Interventions is",count_int ) })
                            output[[paste0("strategy",number,"4")]]<-renderText({
                              df_acc<-as.data.frame(It[[4]])
                              count_acc<-sum(df_acc$need[1:n_bin_actions])
                              paste("The number of needed Actions is",count_acc ) })
                            
                            #ITERATIONS AND ACTIONS IN THE STRATEGY
                            iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                            output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                            })
                            output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                              filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                              content = function(file) {
                                write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                              }
                            )
                            
                            ac<-data.frame(Actnames$Action, It[[4]])
                            ac<-ac[which(ac$need>0),c(1,2,5)]
                            #ac$nedd<-round(ac$nedd,2)
                            output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                              ac
                            })
                            output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                              filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                              content = function(file) {
                                write.csv(ac, file)
                              }
                            )
                            
                            #SHOW INTERVENTIONS TO ADD 
                            
                            #OutputTable in Iteration 1
                            output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                            output[[paste0('Download',number,'table')]] <- downloadHandler(
                              filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                              content = function(file) {
                                write.csv(It[[5]], file)
                              }
                            )
                            s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                            s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                            
                            
                            
                            #OutputGraph in Iteration 1
                            output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                              ExtraInt<-It[[5]]
                              s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                              ExtraInt<-ExtraInt[s11,]
                              inp<-ExtraInt$Compl
                              outp<-ExtraInt$Out
                              dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                              
                              axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                              axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                              
                            })
                            
                            output[[paste0('Download',number,'graph')]] <- downloadHandler(
                              filename =  function() {
                                paste0('Download',number,'graph.png')
                              },
                              # content is a function with argument file. content writes the plot to the device
                              content = function(file) {
                                png(file) 
                                ExtraInt<-It[[5]]
                                s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                                ExtraInt<-ExtraInt[s11,]
                                inp<-ExtraInt$Compl
                                outp<-ExtraInt$Out
                                dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                  ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                
                                axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                dev.off()  # turn the device off
                                
                              } 
                            )
                            ############################ITERATION 14####################################
                            observeEvent(input[[paste0("Add",13)]],{
                              number<-number+1
                              prev<-number-1            
                              
                              #read from previous table
                              position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                              index<-rownames(It[[5]])
                              selected<-as.numeric(index[position])
                              x[selected]<-1
                              #y is the needed vector from the previous iteration
                              y[which(It[[4]]$need==1)]<-1
                              It<-strategy(x,y
                                           ,PM
                                           ,FinalInterventionsResults$Outcomes
                                           ,criandstr$weights
                                           ,n_bin_actions
                                           ,n_totalint)
                              
                              
                              #SHOW SUMMARY OF STRATEGY
                              output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                              output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                              output[[paste0("strategy",number,"3")]]<-renderText({
                                int<-as.data.frame(It[[3]])
                                count_int<-sum(int$choose)
                                paste("The number of selected Interventions is",count_int ) })
                              output[[paste0("strategy",number,"4")]]<-renderText({
                                df_acc<-as.data.frame(It[[4]])
                                count_acc<-sum(df_acc$need[1:n_bin_actions])
                                paste("The number of needed Actions is",count_acc ) })
                              
                              #ITERATIONS AND ACTIONS IN THE STRATEGY
                              iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                              output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                              })
                              output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                                filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                                content = function(file) {
                                  write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                                }
                              )
                              
                              ac<-data.frame(Actnames$Action, It[[4]])
                              ac<-ac[which(ac$need>0),c(1,2,5)]
                              #ac$nedd<-round(ac$nedd,2)
                              output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                                ac
                              })
                              output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                                filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                                content = function(file) {
                                  write.csv(ac, file)
                                }
                              )
                              
                              #SHOW INTERVENTIONS TO ADD 
                              
                              #OutputTable in Iteration 1
                              output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                              output[[paste0('Download',number,'table')]] <- downloadHandler(
                                filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                                content = function(file) {
                                  write.csv(It[[5]], file)
                                }
                              )
                              s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                              s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                              
                              
                              
                              #OutputGraph in Iteration 1
                              output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                                ExtraInt<-It[[5]]
                                s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                                ExtraInt<-ExtraInt[s11,]
                                inp<-ExtraInt$Compl
                                outp<-ExtraInt$Out
                                dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                  ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                
                                axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                
                              })
                              
                              output[[paste0('Download',number,'graph')]] <- downloadHandler(
                                filename =  function() {
                                  paste0('Download',number,'graph.png')
                                },
                                # content is a function with argument file. content writes the plot to the device
                                content = function(file) {
                                  png(file) 
                                  ExtraInt<-It[[5]]
                                  s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                                  ExtraInt<-ExtraInt[s11,]
                                  inp<-ExtraInt$Compl
                                  outp<-ExtraInt$Out
                                  dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                    ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                  
                                  axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                  axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                  dev.off()  # turn the device off
                                  
                                } 
                              )
                            
                            
                            
                            
                          
                              ############################ITERATION 15####################################
                              observeEvent(input[[paste0("Add",14)]],{
                                number<-number+1
                                prev<-number-1            
                                
                                #read from previous table
                                position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                                index<-rownames(It[[5]])
                                selected<-as.numeric(index[position])
                                x[selected]<-1
                                #y is the needed vector from the previous iteration
                                y[which(It[[4]]$need==1)]<-1
                                It<-strategy(x,y
                                             ,PM
                                             ,FinalInterventionsResults$Outcomes
                                             ,criandstr$weights
                                             ,n_bin_actions
                                             ,n_totalint)
                                
                                
                                #SHOW SUMMARY OF STRATEGY
                                output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                                output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                                output[[paste0("strategy",number,"3")]]<-renderText({
                                  int<-as.data.frame(It[[3]])
                                  count_int<-sum(int$choose)
                                  paste("The number of selected Interventions is",count_int ) })
                                output[[paste0("strategy",number,"4")]]<-renderText({
                                  df_acc<-as.data.frame(It[[4]])
                                  count_acc<-sum(df_acc$need[1:n_bin_actions])
                                  paste("The number of needed Actions is",count_acc ) })
                                
                                #ITERATIONS AND ACTIONS IN THE STRATEGY
                                iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                                output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                                })
                                output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                                  filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                                  content = function(file) {
                                    write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                                  }
                                )
                                
                                ac<-data.frame(Actnames$Action, It[[4]])
                                ac<-ac[which(ac$need>0),c(1,2,5)]
                                #ac$nedd<-round(ac$nedd,2)
                                output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                                  ac
                                })
                                output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                                  filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                                  content = function(file) {
                                    write.csv(ac, file)
                                  }
                                )
                                
                                #SHOW INTERVENTIONS TO ADD 
                                
                                #OutputTable in Iteration 1
                                output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                                output[[paste0('Download',number,'table')]] <- downloadHandler(
                                  filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                                  content = function(file) {
                                    write.csv(It[[5]], file)
                                  }
                                )
                                s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                                s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                                
                                
                                
                                #OutputGraph in Iteration 1
                                output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                                  ExtraInt<-It[[5]]
                                  s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                                  ExtraInt<-ExtraInt[s11,]
                                  inp<-ExtraInt$Compl
                                  outp<-ExtraInt$Out
                                  dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                    ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                  
                                  axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                  axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                  
                                })
                                
                                output[[paste0('Download',number,'graph')]] <- downloadHandler(
                                  filename =  function() {
                                    paste0('Download',number,'graph.png')
                                  },
                                  # content is a function with argument file. content writes the plot to the device
                                  content = function(file) {
                                    png(file) 
                                    ExtraInt<-It[[5]]
                                    s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                                    ExtraInt<-ExtraInt[s11,]
                                    inp<-ExtraInt$Compl
                                    outp<-ExtraInt$Out
                                    dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                      ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                    
                                    axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                    axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                    dev.off()  # turn the device off
                                    
                                  } 
                                )
                              
                              
                              
                                ############################ITERATION 16####################################
                                observeEvent(input[[paste0("Add",15)]],{
                                  number<-number+1
                                  prev<-number-1            
                                  
                                  #read from previous table
                                  position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                                  index<-rownames(It[[5]])
                                  selected<-as.numeric(index[position])
                                  x[selected]<-1
                                  #y is the needed vector from the previous iteration
                                  y[which(It[[4]]$need==1)]<-1
                                  It<-strategy(x,y
                                               ,PM
                                               ,FinalInterventionsResults$Outcomes
                                               ,criandstr$weights
                                               ,n_bin_actions
                                               ,n_totalint)
                                  
                                  
                                  #SHOW SUMMARY OF STRATEGY
                                  output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                                  output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                                  output[[paste0("strategy",number,"3")]]<-renderText({
                                    int<-as.data.frame(It[[3]])
                                    count_int<-sum(int$choose)
                                    paste("The number of selected Interventions is",count_int ) })
                                  output[[paste0("strategy",number,"4")]]<-renderText({
                                    df_acc<-as.data.frame(It[[4]])
                                    count_acc<-sum(df_acc$need[1:n_bin_actions])
                                    paste("The number of needed Actions is",count_acc ) })
                                  
                                  #ITERATIONS AND ACTIONS IN THE STRATEGY
                                  iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                                  output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                                  })
                                  output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                                    filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                                    content = function(file) {
                                      write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                                    }
                                  )
                                  
                                  ac<-data.frame(Actnames$Action, It[[4]])
                                  ac<-ac[which(ac$need>0),c(1,2,5)]
                                  #ac$nedd<-round(ac$nedd,2)
                                  output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                                    ac
                                  })
                                  output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                                    filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                                    content = function(file) {
                                      write.csv(ac, file)
                                    }
                                  )
                                  
                                  #SHOW INTERVENTIONS TO ADD 
                                  
                                  #OutputTable in Iteration 1
                                  output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                                  output[[paste0('Download',number,'table')]] <- downloadHandler(
                                    filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                                    content = function(file) {
                                      write.csv(It[[5]], file)
                                    }
                                  )
                                  s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                                  s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                                  
                                  
                                  
                                  #OutputGraph in Iteration 1
                                  output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                                    ExtraInt<-It[[5]]
                                    s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                                    ExtraInt<-ExtraInt[s11,]
                                    inp<-ExtraInt$Compl
                                    outp<-ExtraInt$Out
                                    dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                      ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                    
                                    axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                    axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                    
                                  })
                                  
                                  output[[paste0('Download',number,'graph')]] <- downloadHandler(
                                    filename =  function() {
                                      paste0('Download',number,'graph.png')
                                    },
                                    # content is a function with argument file. content writes the plot to the device
                                    content = function(file) {
                                      png(file) 
                                      ExtraInt<-It[[5]]
                                      s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                                      ExtraInt<-ExtraInt[s11,]
                                      inp<-ExtraInt$Compl
                                      outp<-ExtraInt$Out
                                      dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                        ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                      
                                      axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                      axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                      dev.off()  # turn the device off
                                      
                                    } 
                                  )
                                
                                
                                
                                
                                
                                
                                  ############################ITERATION 17####################################
                                  observeEvent(input[[paste0("Add",16)]],{
                                    number<-number+1
                                    prev<-number-1            
                                    
                                    #read from previous table
                                    position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                                    index<-rownames(It[[5]])
                                    selected<-as.numeric(index[position])
                                    x[selected]<-1
                                    #y is the needed vector from the previous iteration
                                    y[which(It[[4]]$need==1)]<-1
                                    It<-strategy(x,y
                                                 ,PM
                                                 ,FinalInterventionsResults$Outcomes
                                                 ,criandstr$weights
                                                 ,n_bin_actions
                                                 ,n_totalint)
                                    
                                    
                                    #SHOW SUMMARY OF STRATEGY
                                    output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                                    output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                                    output[[paste0("strategy",number,"3")]]<-renderText({
                                      int<-as.data.frame(It[[3]])
                                      count_int<-sum(int$choose)
                                      paste("The number of selected Interventions is",count_int ) })
                                    output[[paste0("strategy",number,"4")]]<-renderText({
                                      df_acc<-as.data.frame(It[[4]])
                                      count_acc<-sum(df_acc$need[1:n_bin_actions])
                                      paste("The number of needed Actions is",count_acc ) })
                                    
                                    #ITERATIONS AND ACTIONS IN THE STRATEGY
                                    iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                                    output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                                    })
                                    output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                                      filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                                      content = function(file) {
                                        write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                                      }
                                    )
                                    
                                    ac<-data.frame(Actnames$Action, It[[4]])
                                    ac<-ac[which(ac$need>0),c(1,2,5)]
                                    #ac$nedd<-round(ac$nedd,2)
                                    output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                                      ac
                                    })
                                    output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                                      filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                                      content = function(file) {
                                        write.csv(ac, file)
                                      }
                                    )
                                    
                                    #SHOW INTERVENTIONS TO ADD 
                                    
                                    #OutputTable in Iteration 1
                                    output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                                    output[[paste0('Download',number,'table')]] <- downloadHandler(
                                      filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                                      content = function(file) {
                                        write.csv(It[[5]], file)
                                      }
                                    )
                                    s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                                    s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                                    
                                    
                                    
                                    #OutputGraph in Iteration 1
                                    output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                                      ExtraInt<-It[[5]]
                                      s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                                      ExtraInt<-ExtraInt[s11,]
                                      inp<-ExtraInt$Compl
                                      outp<-ExtraInt$Out
                                      dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                        ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                      
                                      axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                      axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                      
                                    })
                                    
                                    output[[paste0('Download',number,'graph')]] <- downloadHandler(
                                      filename =  function() {
                                        paste0('Download',number,'graph.png')
                                      },
                                      # content is a function with argument file. content writes the plot to the device
                                      content = function(file) {
                                        png(file) 
                                        ExtraInt<-It[[5]]
                                        s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                                        ExtraInt<-ExtraInt[s11,]
                                        inp<-ExtraInt$Compl
                                        outp<-ExtraInt$Out
                                        dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                          ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                        
                                        axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                        axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                        dev.off()  # turn the device off
                                        
                                      } 
                                    )
                                  
                                  
                                  
                                  
                                    ############################ITERATION 18####################################
                                    observeEvent(input[[paste0("Add",17)]],{
                                      number<-number+1
                                      prev<-number-1            
                                      
                                      #read from previous table
                                      position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                                      index<-rownames(It[[5]])
                                      selected<-as.numeric(index[position])
                                      x[selected]<-1
                                      #y is the needed vector from the previous iteration
                                      y[which(It[[4]]$need==1)]<-1
                                      It<-strategy(x,y
                                                   ,PM
                                                   ,FinalInterventionsResults$Outcomes
                                                   ,criandstr$weights
                                                   ,n_bin_actions
                                                   ,n_totalint)
                                      
                                      
                                      #SHOW SUMMARY OF STRATEGY
                                      output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                                      output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                                      output[[paste0("strategy",number,"3")]]<-renderText({
                                        int<-as.data.frame(It[[3]])
                                        count_int<-sum(int$choose)
                                        paste("The number of selected Interventions is",count_int ) })
                                      output[[paste0("strategy",number,"4")]]<-renderText({
                                        df_acc<-as.data.frame(It[[4]])
                                        count_acc<-sum(df_acc$need[1:n_bin_actions])
                                        paste("The number of needed Actions is",count_acc ) })
                                      
                                      #ITERATIONS AND ACTIONS IN THE STRATEGY
                                      iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                                      output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                                      })
                                      output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                                        filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                                        content = function(file) {
                                          write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                                        }
                                      )
                                      
                                      ac<-data.frame(Actnames$Action, It[[4]])
                                      ac<-ac[which(ac$need>0),c(1,2,5)]
                                      #ac$nedd<-round(ac$nedd,2)
                                      output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                                        ac
                                      })
                                      output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                                        filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                                        content = function(file) {
                                          write.csv(ac, file)
                                        }
                                      )
                                      
                                      #SHOW INTERVENTIONS TO ADD 
                                      
                                      #OutputTable in Iteration 1
                                      output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                                      output[[paste0('Download',number,'table')]] <- downloadHandler(
                                        filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                                        content = function(file) {
                                          write.csv(It[[5]], file)
                                        }
                                      )
                                      s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                                      s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                                      
                                      
                                      
                                      #OutputGraph in Iteration 1
                                      output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                                        ExtraInt<-It[[5]]
                                        s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                                        ExtraInt<-ExtraInt[s11,]
                                        inp<-ExtraInt$Compl
                                        outp<-ExtraInt$Out
                                        dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                          ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                        
                                        axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                        axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                        
                                      })
                                      
                                      output[[paste0('Download',number,'graph')]] <- downloadHandler(
                                        filename =  function() {
                                          paste0('Download',number,'graph.png')
                                        },
                                        # content is a function with argument file. content writes the plot to the device
                                        content = function(file) {
                                          png(file) 
                                          ExtraInt<-It[[5]]
                                          s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                                          ExtraInt<-ExtraInt[s11,]
                                          inp<-ExtraInt$Compl
                                          outp<-ExtraInt$Out
                                          dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                            ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                          
                                          axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                          axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                          dev.off()  # turn the device off
                                          
                                        } 
                                      )
                                      ############################ITERATION 19####################################
                                      observeEvent(input[[paste0("Add",18)]],{
                                        number<-number+1
                                        prev<-number-1            
                                        
                                        #read from previous table
                                        position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                                        index<-rownames(It[[5]])
                                        selected<-as.numeric(index[position])
                                        x[selected]<-1
                                        #y is the needed vector from the previous iteration
                                        y[which(It[[4]]$need==1)]<-1
                                        It<-strategy(x,y
                                                     ,PM
                                                     ,FinalInterventionsResults$Outcomes
                                                     ,criandstr$weights
                                                     ,n_bin_actions
                                                     ,n_totalint)
                                        
                                        
                                        #SHOW SUMMARY OF STRATEGY
                                        output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                                        output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                                        output[[paste0("strategy",number,"3")]]<-renderText({
                                          int<-as.data.frame(It[[3]])
                                          count_int<-sum(int$choose)
                                          paste("The number of selected Interventions is",count_int ) })
                                        output[[paste0("strategy",number,"4")]]<-renderText({
                                          df_acc<-as.data.frame(It[[4]])
                                          count_acc<-sum(df_acc$need[1:n_bin_actions])
                                          paste("The number of needed Actions is",count_acc ) })
                                        
                                        #ITERATIONS AND ACTIONS IN THE STRATEGY
                                        iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                                        output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                                        })
                                        output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                                          filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                                          content = function(file) {
                                            write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                                          }
                                        )
                                        
                                        ac<-data.frame(Actnames$Action, It[[4]])
                                        ac<-ac[which(ac$need>0),c(1,2,5)]
                                        #ac$nedd<-round(ac$nedd,2)
                                        output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                                          ac
                                        })
                                        output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                                          filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                                          content = function(file) {
                                            write.csv(ac, file)
                                          }
                                        )
                                        
                                        #SHOW INTERVENTIONS TO ADD 
                                        
                                        #OutputTable in Iteration 1
                                        output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                                        output[[paste0('Download',number,'table')]] <- downloadHandler(
                                          filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                                          content = function(file) {
                                            write.csv(It[[5]], file)
                                          }
                                        )
                                        s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                                        s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                                        
                                        
                                        
                                        #OutputGraph in Iteration 1
                                        output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                                          ExtraInt<-It[[5]]
                                          s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                                          ExtraInt<-ExtraInt[s11,]
                                          inp<-ExtraInt$Compl
                                          outp<-ExtraInt$Out
                                          dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                            ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                          
                                          axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                          axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                          
                                        })
                                        
                                        output[[paste0('Download',number,'graph')]] <- downloadHandler(
                                          filename =  function() {
                                            paste0('Download',number,'graph.png')
                                          },
                                          # content is a function with argument file. content writes the plot to the device
                                          content = function(file) {
                                            png(file) 
                                            ExtraInt<-It[[5]]
                                            s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                                            ExtraInt<-ExtraInt[s11,]
                                            inp<-ExtraInt$Compl
                                            outp<-ExtraInt$Out
                                            dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                              ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                            
                                            axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                            axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                            dev.off()  # turn the device off
                                            
                                          } 
                                        )
                                      
                                      
                                      
                                        ############################ITERATION 20####################################
                                        ##observeEvent(input[[paste0("Add",19)]],{
                                          number<-number+1
                                          prev<-number-1            
                                          
                                          #read from previous table
                                          position<-input[[paste0("Iteration",prev,"table_rows_selected")]]
                                          index<-rownames(It[[5]])
                                          selected<-as.numeric(index[position])
                                          x[selected]<-1
                                          #y is the needed vector from the previous iteration
                                          y[which(It[[4]]$need==1)]<-1
                                          It<-strategy(x,y
                                                       ,PM
                                                       ,FinalInterventionsResults$Outcomes
                                                       ,criandstr$weights
                                                       ,n_bin_actions
                                                       ,n_totalint)
                                          
                                          
                                          #SHOW SUMMARY OF STRATEGY
                                          output[[paste0("strategy",number,"1")]]<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(It[[1]],2),"%" ) })
                                          output[[paste0("strategy",number,"2")]]<-renderText({paste("The percentage of Complexity reached by the strategy is",round(It[[2]],2),"%" ) })
                                          output[[paste0("strategy",number,"3")]]<-renderText({
                                            int<-as.data.frame(It[[3]])
                                            count_int<-sum(int$choose)
                                            paste("The number of selected Interventions is",count_int ) })
                                          output[[paste0("strategy",number,"4")]]<-renderText({
                                            df_acc<-as.data.frame(It[[4]])
                                            count_acc<-sum(df_acc$need[1:n_bin_actions])
                                            paste("The number of needed Actions is",count_acc ) })
                                          
                                          #ITERATIONS AND ACTIONS IN THE STRATEGY
                                          iterations<-It[[3]][which(It[[3]]$choose==1),c(1,2)]
                                          output[[paste0("Iteration",number,"Interventions")]]<-renderDataTable({iterations
                                          })
                                          output[[paste0('Download',number,'Interventions')]] <- downloadHandler(
                                            filename = function() { paste('Iteration',number,'Interventions', '.csv', sep='') },
                                            content = function(file) {
                                              write.csv(It[[3]][which(It[[3]]$choose==1),c(1,2)], file)
                                            }
                                          )
                                          
                                          ac<-data.frame(Actnames$Action, It[[4]])
                                          ac<-ac[which(ac$need>0),c(1,2,5)]
                                          #ac$nedd<-round(ac$nedd,2)
                                          output[[paste0("Iteration",number,"Actions")]]<-renderDataTable({
                                            ac
                                          })
                                          output[[paste0('Download',number,'Actions')]] <- downloadHandler(
                                            filename = function() { paste('Iteration',number,'Actions', '.csv', sep='') },
                                            content = function(file) {
                                              write.csv(ac, file)
                                            }
                                          )
                                          
                                          #SHOW INTERVENTIONS TO ADD 
                                          
                                          #OutputTable in Iteration 1
                                          output[[paste0("Iteration",number,"table")]]<-renderDataTable({datatable(as.data.frame(It[[5]]),filter = 'top',class = 'white-space: nowrap')})
                                          output[[paste0('Download',number,'table')]] <- downloadHandler(
                                            filename = function() { paste('Iteration',number,'table', '.csv', sep='') },
                                            content = function(file) {
                                              write.csv(It[[5]], file)
                                            }
                                          )
                                          s11<-input[[paste0("Iteration",number,"table_rows_all")]] #Shows rows after being filtered
                                          s12<-input[[paste0("Iteration",number,"table_rows_selected")]]
                                          
                                          
                                          
                                          #OutputGraph in Iteration 1
                                          output[[paste0("Iteration",number,"graph")]]<-renderPlot({
                                            ExtraInt<-It[[5]]
                                            s11<-input[[paste0("Iteration",number,"table_rows_all")]]
                                            ExtraInt<-ExtraInt[s11,]
                                            inp<-ExtraInt$Compl
                                            outp<-ExtraInt$Out
                                            dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                              ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                            
                                            axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                            axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                            
                                          })
                                          
                                          output[[paste0('Download',number,'graph')]] <- downloadHandler(
                                            filename =  function() {
                                              paste0('Download',number,'graph.png')
                                            },
                                            # content is a function with argument file. content writes the plot to the device
                                            content = function(file) {
                                              png(file) 
                                              ExtraInt<-It[[5]]
                                              s11<-isolate(input[[paste0("Iteration",number,"table_rows_all")]])
                                              ExtraInt<-ExtraInt[s11,]
                                              inp<-ExtraInt$Compl
                                              outp<-ExtraInt$Out
                                              dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                                                                ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
                                              
                                              axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
                                              axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
                                              dev.off()  # turn the device off
                                              
                                            } 
                                          )
                                        
                                        
                                        })
                                      
                                    
                              })
                              
                              })
                            })
                          
                          
                          
                          
                          })
                      })
                  })
              })
        })
        
      })
                    #})
})
 })
  })
})
    })
    
  })
  })})
  
})
    
  

}



  
  





