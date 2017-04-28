

navbarPage("MY APPLICATION TITLE",
           #choose a theme
           #theme = shinytheme("cerulea"), 
           
           #Define the panels
           
           tabPanel("Model Description"),
           
           tabPanel("Analysis1",
                    semanticPage(
                      tags$head(tags$style(HTML(css))),
                      useShinyjs(),
                      sidebar(),
                      div(style="margin-left: 210px",
                          div(id="Configure your data", class="ui container",
                              datasettings(),
                              InterventionsOutputs(),
                              ActionsOutput()
                              #ExtraTables()
                              

                          )
                      )
                    )

           ),
           
           
           ####################################
           ##########ITERATIVE TOOL###########
           ###################################
           
           # tabPanel("Strategy Constuction",
           #          #ITERATION 0
           #            fluidPage(
           #              fluidRow(
           #                column(6,div(style = 'overflow-x: scroll',dataTableOutput(outputId = "Iteration0table"))),
           #                column(6,
           #                       #dataTableOutput('testfilter'))
           #                       plotOutput("Iteration0graph"))
           #            ),
           #            fluidRow(uiOutput("Add0"))
           #            
           #          
           #           #ITERATION 1
           #           ,fluidRow(textOutput("summary1"))
           #           ,fluidRow(textOutput("summary2"))
           #           ,fluidRow(textOutput("summary3"))
           #           ,fluidRow(textOutput("summary4"))
           #         
           #            ,fluidRow(
           #              column(6,div(style = 'overflow-x: scroll',dataTableOutput(outputId = "Iteration1table"))),
           #              column(6,plotOutput("Iteration1graph"))
           #            ),
           #            fluidRow(uiOutput("Add1"))
           # 
           #            )
           #            
           #          
           #              #uiOutput("Iterations")
           #              #Output list
           #              #Output graph
           #              
           #          ),
           
           
           
           
           
           
           navbarMenu("More",
                      tabPanel("About",
                               fluidRow(includeMarkdown("about.Rmd")
                               )
                      )
           )
) 

