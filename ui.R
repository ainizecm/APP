

navbarPage("Xtrategy",
           #choose a theme
           #theme = shinytheme("cerulea"), 
           
           #Define the panels
           tabPanel("Model Description and How To"
                    ,includeHTML("Xtrategy_v2.html")
                          #  Xtrategy())
)
           
           ,
           tabPanel("Data Settings"
           ,
           semanticPage(
             tags$head(tags$style(HTML(css)))
             ,
             useShinyjs(),
             div(style="margin-left: 100px",
                 div(id="Configure your data", 
                     class="ui container",
                     datasettings())
             )
             ))
           ,
           
           tabPanel("Analysis"
                    ,
                    semanticPage(
                      tags$head(tags$style(HTML(css)))
                    ,
                      useShinyjs(),
                    conditionalPanel(condition='input.compute== 1',
                    sidebar1(),
                    div(style="margin-left: 210px",
                          div(
                              InterventionsOutputs(),
                              ActionsOutput()
                              #,
                              #ExtraTables()          
                    ))))),
           
           
           # ####################################
           # ##########ITERATIVE TOOL###########
           # ###################################
           # 
           tabPanel("Strategy Constuction",
                    semanticPage(
                      tags$head(tags$style(HTML(css))),
                      useShinyjs(),
                      div(
                          div(id="Iterations", class="ui container",
                              Iteration(0),
                              conditionalPanel(condition='input.Add0== 1',Iteration(1)),
                              conditionalPanel(condition='input.Add1== 1',Iteration(2)),
                              conditionalPanel(condition='input.Add2== 1',Iteration(3)),
                              conditionalPanel(condition='input.Add3== 1',Iteration(4)),
                              conditionalPanel(condition='input.Add4== 1',Iteration(5)),
                              conditionalPanel(condition='input.Add5== 1',Iteration(6)),
                              conditionalPanel(condition='input.Add6== 1',Iteration(7))
                            )
                      )
                    )
           )             
) 

