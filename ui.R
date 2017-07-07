

navbarPage("Xtrategy",
           #choose a theme
           #theme = shinytheme("cerulea"), 
           
           #Define the panels
           tabPanel("Model Description"
                    ,fluidRow(includeMarkdown("Model_Description.Rmd")))
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
                              ActionsOutput(),
                              ExtraTables()
                          
                    )
                    
           )))),
           
           
           # ####################################
           # ##########ITERATIVE TOOL###########
           # ###################################
           # 
           tabPanel("Strategy Constuction",
                    semanticPage(
                      tags$head(tags$style(HTML(css))),
                      useShinyjs(),
                      sidebar2(),
                      div(style="margin-left: 210px",
                          div(id="Iterations", class="ui container",
                              Iteration(0),
                              conditionalPanel(condition='input.Add0== 1',Iteration(1)),
                              conditionalPanel(condition='input.Add1== 1',Iteration(2)),
                              conditionalPanel(condition='input.Add2== 1',Iteration(3)),
                              conditionalPanel(condition='input.Add3== 1',Iteration(4)),
                              conditionalPanel(condition='input.Add4== 1',Iteration(5)),
                              conditionalPanel(condition='input.Add5== 1',Iteration(6)),
                              conditionalPanel(condition='input.Add6== 1',Iteration(7))
                              #conditionalPanel(condition='input.Add7== 1',Iteration(8))
                             # conditionalPanel(condition='input.Add8== 1',Iteration(9))
                              #conditionalPanel(condition='input.Add9== 1',Iteration(10))
                              # conditionalPanel(condition='input.Add10== 1',Iteration(11))
                              # conditionalPanel(condition='input.Add11== 1',Iteration(12)),
                              # conditionalPanel(condition='input.Add12== 1',Iteration(13)),
                              # conditionalPanel(condition='input.Add13== 1',Iteration(14)),
                              # conditionalPanel(condition='input.Add14== 1',Iteration(15)),
                              # conditionalPanel(condition='input.Add15== 1',Iteration(16)),
                              # conditionalPanel(condition='input.Add16== 1',Iteration(17)),
                              # conditionalPanel(condition='input.Add17== 1',Iteration(18)),
                              # conditionalPanel(condition='input.Add18== 1',Iteration(19)),
                              # conditionalPanel(condition='input.Add19== 1',Iteration(20))
                              
                              
                              
                          )
                      )
                    )
           
           
           
           
           )
           #,
           
           
           
          #navbarMenu("More",
          #           tabPanel("About",
          #                     fluidRow(includeMarkdown("about.Rmd")
          #                    )
          #            )
          # )
           
) 

