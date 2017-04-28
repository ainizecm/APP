

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
                    Iteration0()
           ),
          
           
           
           
           
           
           
           navbarMenu("More",
                      tabPanel("About",
                               fluidRow(includeMarkdown("about.Rmd")
                               )
                      )
           )
) 

