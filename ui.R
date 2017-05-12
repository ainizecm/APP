

navbarPage("MY APPLICATION TITLE",
           #choose a theme
           #theme = shinytheme("cerulea"), 
           
           #Define the panels
           
           tabPanel("Model Description"),
           
           tabPanel("Analysis1",
                    semanticPage(
                      tags$head(tags$style(HTML(css))),
                      useShinyjs(),
                      sidebar1(),
                      div(style="margin-left: 210px",
                          div(id="Configure your data", class="ui container",
                              datasettings(),
                              InterventionsOutputs(),
                              ActionsOutput(),
                              ExtraTables()
                              

                          )
                      )
                    )

           ),
           
           
           ####################################
           ##########ITERATIVE TOOL###########
           ###################################
           
           tabPanel("Strategy Constuction",
                    semanticPage(
                      tags$head(tags$style(HTML(css))),
                      useShinyjs(),
                      sidebar2(),
                      div(style="margin-left: 210px",
                          div(id="Iterations", class="ui container",
                              Iteration(0)
                              #,
                              #Iteration(1)
                          )
                      )
                    )
           ),
          
           
           
           
           
           
           
           navbarMenu("More",
                      tabPanel("About",
                               fluidRow(includeMarkdown("about.Rmd")
                               )
                      )
           )
) 

