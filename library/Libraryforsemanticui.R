
css <- "
#examples > div > .header {
margin-top: 1em;
}"

jsCode <- "
$('.accordion').accordion({selector: {trigger: '.title .icon'}}).accordion('close');
$('.ui.dropdown').dropdown({});
$('.rating').rating('setting', 'clearable', true);
"



####################################
##########ANALYSIS################
##################################


sidebar1 <- function() {
  div(class="ui sidebar inverted vertical visible menu",
      div(style = "width: 100%; height:100px"),
      div(class="item",
          div(class="active header", "Data Settings")),
      div(class="item",
          div(class="active header", "Interventions"),
          div(class="menu",
              a(class="item", href="#Ranking by Block", "Ranking by Block"),
              a(class="item", href="#All Interventions Ranking", "All Interventions Ranking"))),
      div(class="item",
          div(class="active header", "Actions"),
          div(class="menu",
              a(class="item", href="#Actions Ranking", "Actions Ranking"))),
      div(class="item",
          div(class="active header", "Extra Tables"),
          div(class="menu",
              a(class="item", href="#Detailed Results", "Detailed Results"),
              a(class="item", href="#Actions Survey", "Actions Survey")
          )))
}

datasettings <- function() {
  ###TITLE###
  div(class = "ui raised segment",
      h2(class="ui icon header",
         id="Data Settings",
         uiicon("settings"),
         div(class="content", "Data Settings",
             div(class="sub header", "Input your data and variables"))),
      div(style = "width: 100%; height:50px"),
      ###INPUT FILE###
      div(class = "ui horizontal divider", uiicon("upload"), "Upload file"),
      fileInput('file.upload',label=' '),
      
      ###INPUT NUMBERS for Interventions###
      div(class = "ui horizontal divider","Interventions"),
      
      div(class = "ui stackable two column grid", 
          div(class = "column", 
              numericInput('n_totalint', 
                           "Number of Total Interventions",
                           value=85, min = NA, max = NA, step = NA,
                           width = NULL)), 
          div(class = "column", 
              numericInput('n_blocks', 
                           "Number of Blocks", 
                           value=7, min = NA, max = NA, step = NA,
                           width = NULL)) 
      ),
      div(class = "ui stackable two column grid", 
          div(class = "column", 
              textInput('n_int', 
                        "Number of Interventions in each block  (write them separate by /,i.e.,if 3 blocks 19/10/15)",
                        value='11/11/15/12/15/12/9', 
                        width = NULL)), 
          div(class = "column", 
              textInput('n_resp', 
                        "Number of Respondants in each block  (write them separate by /,i.e.,if 3 blocks 3/5/3)",
                        value='3/4/3/3/3/3/4',
                        width = NULL)) 
      ),
      
      ###INPUT NUMBERS for Actions###
      div(class = "ui horizontal divider", "Actions"),
      div(class = "ui stackable two column grid", div(class = "column", 
                                                      numericInput('n_bin_actions', 
                                                                   "Number of Actions",
                                                                   value=57, min = NA, max = NA, step = NA,
                                                                   width = NULL)), 
          div(class = "column", 
              numericInput('n_cri_resp', 
                           "Number of Respondants for actions complexity",
                           value=4, min = NA, max = NA, step = NA,
                           width = NULL)) 
      ),
      
      actionButton("compute", "Compute results")
      
      
  )}

################INTERNVENTIONS, ACTIONS AND EXTRA TABLE RANKINGS OUTPUTS##########
InterventionsOutputs<-function(){
  #TITLE
  div(class = "ui raised segment",
      h1(class="ui icon header",
         id="Interventions Analysis",
         uiicon("Doctor"),
         div(class="content", "Interventions Outcomes",
             div(class="sub header", "Analysis and Ranking of Interventions"))),
  
      #All interventions Ranking
      h2(class='ui header',id="All Interventions Ranking","All Interventions Ranking"),
      div(style = 'overflow-x: scroll',dataTableOutput(outputId = "FinalM")),
      div(downloadButton(outputId="downloadData1",'Download')),
      #All inteventions graph
      plotOutput("totalgraph"),
      div(downloadButton(outputId='downloadtotalgraph','Download')),
      
      #Ranking by block
      h2(class='ui header',id="Ranking by Block","Ranking by Block"),
      uiOutput("RankingbyBlock")
  )
}

ActionsOutput<-function(){
  #TITLE
  div(class = "ui raised segment",
      h1(class="ui header",
         id="Actions Analysis",
         div(class="content", "Actions Analysis")),
      
      #Ranking by block
      h2(class='ui header',id="Actions Ranking","Actions Ranking"),
      div(style = 'overflow-x: scroll',dataTableOutput(outputId = "ActionsRanking")),
      div(downloadButton(outputId="downloadData2",'Download'))
      #Maybe actions nice show in each block?
  )
}

ExtraTables<-function(){
  #TITLE
  div(class = "ui raised segment",
      h1(class="ui icon header",
         id="Extra Tables",
         uiicon("Doctor"),
         div(class="content", "Extra Tables",
             div(class="sub header", "Extra tables for deeper analysis and checks"))),
      
      #Ranking by block
      h2(class='ui header',id="Detailed Results","Detailed Results"),
      uiOutput("DetailedResults"),
      
      #All interventions Ranking
      h2(class='ui header',id="Actions Survey","Actions Survey"),
      div(style = 'overflow-x: scroll',dataTableOutput(outputId = "ActionsSurvey"))
  )
}


####################################
##########ITERATIVE TOOL################
##################################

sidebar2 <- function() {
  div(class="ui sidebar inverted vertical visible menu",
      div(style = "width: 100%; height:100px"),
      div(class="item",
          div(class="active header", "Iterations")),
          div(class="menu",
              a(class="item", href="#Iteration0", "Iteration0"),
              a(class="item", href="#Iteration1", "Iteration1")))
          
}



Iteration<-function(n){
  #TITLE
  div(class = "ui raised segment",
      h1(class="ui icon header",
         id="Iteration",
         uiicon("Doctor"),
         div(class="content", paste0("Iteration",n)),
         
         #Show summary
         div(class = "ui horizontal divider","Summary"),
         div(class = "ui stackable two column grid",
             div(class = "column",
                 div(style='font-size: 75%;',textOutput(outputId =paste0("strategy",n,"1") )),
                 div(style='font-size: 75%;',textOutput(outputId =paste0("strategy",n,"3") ))
                 ),
             div(class = "column",
                 #h2(class='ui header',id=paste0("graph", n),"Graph"),
                 div(style='font-size: 75%;',textOutput(outputId =paste0("strategy",n,"2") )),
                 div(style='font-size: 75%;',textOutput(outputId =paste0("strategy",n,"4") ))
             ))
         ),
      

    
      #Included Interventions and actions
      div(class = "ui horizontal divider","Interventions and Actions in the Strategy"),
      div(class = "ui stackable two column grid", 
          div(class = "column", 
              h2(class='ui header',id=paste0("inerventionsins",n),"Interventions"),
              div(style = 'overflow-x: scroll;font-size: 75%; width: 100%',dataTableOutput(outputId =paste0("Iteration",n,"Interventions") )),
          div(downloadButton(outputId=paste0("Download",n,"Interventions"),'Download'))),
          div(class = "column", 
              h2(class='ui header',id=paste0("actionsins", n),"Actions"),
              div(style = 'overflow-x: scroll;font-size: 75%; width: 100%',dataTableOutput(outputId =paste0("Iteration",n,"Actions") )),
              div(downloadButton(outputId=paste0("Download",n,"Actions"),'Download')))),
          
  
         
         #Show table and graph
         div(class = "ui horizontal divider","Possible Interventions To Add"),
      div(class = "ui stackable two column grid", 
          div(class = "column", 
              #h2(class='ui header',id=paste0("inerventions",n),"Interventions"),
              div(style = 'overflow-x: scroll;font-size: 75%; width: 100%',DT::dataTableOutput(outputId =paste0("Iteration",n,"table") )),
              div(downloadButton(outputId=paste0("Download",n,"Table"),'Download'))), 
          div(class = "column", 
              #h2(class='ui header',id=paste0("graph", n),"Graph"),
              plotOutput(paste0("Iteration",n,"graph")),
              downloadButton(outputId=paste0("Download",n,"graph"),'Download')
      )),
      
      actionButton(paste0("Add",n), "Add interventions to the strategy")
      #uiOutput(paste0("Add",n))
      
  )
}
