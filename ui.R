

navbarPage("MY APPLICATION TITLE",
           #choose a theme
           #theme = shinytheme("cerulea"), 
           
           #Define the panels
           
           tabPanel("Model Description"),
           
           tabPanel("Analysis",
                    
                    #dashboardPage(#skin = "black",
                      #dashboardHeader(title = "Load Data", titleWidth = 250),
                      sidebarLayout(#width = 250,
                        sidebarPanel(fileInput('file.upload',"Upload XLSX file"),
                       
                                               numericInput('n_blocks', 
                                                              "Number of Blocks", 
                                                              value=4, min = NA, max = NA, step = NA,
                                                                width = NULL),
                                                   numericInput('n_totalint', 
                                                                "Number of Total Interventions",
                                                                value=54, min = NA, max = NA, step = NA,
                                                                 width = NULL),
                                                   textInput('n_resp', 
                                                               "Number of Respondants in each block",
                                                               value='3/4/3/3',
                                                              width = NULL),
                                                  textInput('n_int', 
                                                             "Number of Interventions in each block",
                                                             value='19/14/11/10', 
                                                               width = NULL),
                                     numericInput('n_bin_actions', 
                                                  "Number of Actions",
                                                  value=57, min = NA, max = NA, step = NA,
                                                  width = NULL),
                                     numericInput('n_cri_resp', 
                                                  "Number of Respondants for actions complexity",
                                                  value=4, min = NA, max = NA, step = NA,
                                                  width = NULL),
                                     
                                     actionButton("compute", "Compute results"),
                                     width = 3),
                   
                    mainPanel(
                      tabBox(
                        width = 10000,
                        #id = "tabset.result",
                        tabPanel("Performance Matrix",
                                 div(style = 'overflow-x: scroll',dataTableOutput(outputId = "PM"))
                        ),
                        tabPanel("Ranking by Block", #CHANGE DESIGN
                                 fluidPage(
                                  fluidRow(uiOutput("RankingbyBlock")
                                 ))),
                        
                        tabPanel("Interventions Ranking",
                                 div(style = 'overflow-x: scroll',dataTableOutput(outputId = "FinalM"))
                                
                        ),
                        tabPanel("Actions Ranking",
                                 div(style = 'overflow-x: scroll',dataTableOutput(outputId = "ActionsRanking"))
                                 
                        ),
                        
                        tabPanel("Extra Tables for Detail"
                                 ,uiOutput("DetailedResults")
                                 ,div(style = 'overflow-x: scroll',dataTableOutput(outputId = "ActionsSurvey"))
                                 )
                        ))
                    )),
                                         
           
           ####################################
           ##########ITERATIVE TOOL###########
           ###################################
           
           tabPanel("Strategy Constuction",
                    #ITERATION 0
                      fluidPage(
                        fluidRow(
                          column(6,div(style = 'overflow-x: scroll',dataTableOutput(outputId = "Iteration0table"))),
                          column(6,
                                 #dataTableOutput('testfilter'))
                                 plotOutput("Iteration0graph"))
                      ),
                      fluidRow(uiOutput("Add0"))
                      
                    
                     #ITERATION 1
                     ,fluidRow(textOutput("summary1"))
                     ,fluidRow(textOutput("summary2"))
                     ,fluidRow(textOutput("summary3"))
                     ,fluidRow(textOutput("summary4"))
                   
                      ,fluidRow(
                        column(6,div(style = 'overflow-x: scroll',dataTableOutput(outputId = "Iteration1table"))),
                        column(6,plotOutput("Iteration1graph"))
                      ),
                      fluidRow(uiOutput("Add1"))

                      )
                      
                    
                        #uiOutput("Iterations")
                        #Output list
                        #Output graph
                        
                    ),
           
           
           
           
           
           
           navbarMenu("More",
                      tabPanel("About",
                               fluidRow(includeMarkdown("about.Rmd")
                               )
                      )
           )
) 

