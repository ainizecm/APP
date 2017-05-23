####################################
##########ITERATIO Formula###########
###################################

it<-0


outputs_iteration<-function(number){
  
  observeEvent(input$paste0("Add",number-1){
    
    s2<-input$paste0("Iteration",number-1,"table_rows_selected")
    x[s2]<-1
    # y<-rep(0,n_bin_actions)
    Iteration<-strategy(x,y
                        ,PM
                        ,FinalInterventionsResults$Outcomes
                        ,criandstr$weights
                        ,n_bin_actions
                        ,n_totalint)
    
    #SHOW SUMMARY OF STRATEGY
    output$paste0("strategy",number,"1")<-renderText({paste("The percentage of Outcomes reached by the strategy is",round(Iteration[[1]],2),"%" ) })
    output$paste0("strategy",number,"2")<-renderText({paste("The percentage of Complexity reached by the strategy is",round(Iteration[[2]],2),"%" ) })
    output$paste0("strategy",number,"3")<-renderText({
      int<-as.data.frame(Iteration[[3]])
      count_int<-sum(int$choose)
      paste("The number of selected Interventions is",count_int ) })
    output$paste0("strategy",number,"4")<-renderText({
      df_acc<-as.data.frame(Iteration[[4]])
      count_acc<-sum(df_acc$need[1:n_bin_actions])
      paste("The number of needed Actions is",count_acc ) })
    
    output$paste0("Iteration",number,"Interventions")<-renderDataTable({Iteration[[3]]})
    output$paste0("Iteration",number,"Actions")<-renderDataTable({Iteration[[4]]})
    
    
    
    
    #SHOW INTERVENTIONS TO ADD 
    
    #OutputTable in Iteration 1
    output$paste0("Iteration",number,"table")<-renderDataTable({
      datatable(as.data.frame(Iteration[[5]]),filter = 'top',class = 'white-space: nowrap')
    })
    s11<-input$paste0("Iteration",number,"table_rows_all") #Shows rows after being filtered
    s12<-input$paste0("Iteration",number,"table_rows_selected")
    
    #OutputGraph in Iteration 1
    output$paste0("Iteration",number,"graph")<-renderPlot({
      ExtraInt<-Iteration[[5]]
      s11<-input$paste0("Iteration",number,"table_rows_all")
      ExtraInt<-ExtraInt[s11,]
      inp<-ExtraInt$Compl
      outp<-ExtraInt$Out
      dea.plot.frontier(inp,outp,txt=ExtraInt$Code,xlab="Added % Complexity",ylab="Added % Outcomes"
                        ,pch=ifelse(outp>median(outp)&inp<median(inp), 19, 1))
      
      axis(1,labels=FALSE,tick=T,line=F,pos=median(outp),lwd.ticks=0)
      axis(2,labels=FALSE,tick=T,line=F,pos=median(inp),lwd.ticks=0)
      
      #if (length(s2)) points(ExtraInt[s2,], pch = 19, cex = 2)
      
      output$paste0("Add",number)<-renderUI({actionButton(paste0("Add",number), "Add selected Interventions to the strategy")})
      
      
      return(list(output$paste0("strategy",number,"1"),
                  output$paste0("strategy",number,"2"),
                  output$paste0("strategy",number,"3"),
                  output$paste0("strategy",number,"4"),
                  output$paste0("Iteration",number,"Interventions"),
                  output$paste0("Iteration",number,"Actions"),
                  output$paste0("Iteration",number,"table"),
                  
                  
                  
                  
                  
      ))
    })
    
  })
}