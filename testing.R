options(java.parameters = "-Xmx8000m")
#install and load packages
packages <- c("shiny", "shinydashboard","Benchmarking","DT","markdown","shinythemes","xlsx" ,"TFDEA", "ggplot2", "dplyr", "tidyr")
if (length(setdiff(packages, installed.packages())) > 0)
  install.packages(setdiff(packages, installed.packages()))

library(shiny)
library(shinydashboard)
library(DT)
library(markdown)
library(shinythemes)

library(xlsx)
library(Benchmarking)
library(TFDEA)
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("~/git/AINIZE/Master/APP")
EXCEL<-'data/DATA2_v2.xlsx'
#EXCEL<-'data/cat_test2.xlsx'

source("library/LibraryforRankingAnalysis.R")
source("library/LibraryforIterativeTool.R")
#Number of Blocks
n_blocks=4#3#4

#Number of Total Interventions
n_totalint=54#85 #54

#Number of respondants for each block
n_resp=c(3,4,3,3)#c(2,2,2) #c(3,4,3,3)

#Number of Interventions in each block
n_int=as.numeric(unlist(strsplit(as.character('19/14/11/10'),"/")))
#n_int=as.numeric(unlist(strsplit(as.character('55/12/18'),"/")))
#Number of actions
n_bin_actions<-57

#Number of respondants for actions complexity
n_cri_resp<-4

lastrow<-6+n_totalint

###1.Read PM
PM<-read.xlsx2(EXCEL,startRow = 5,"PerformanceMatrix",encoding='UTF-8',header=TRUE)
PM[,c("encoding")]<-NULL
###2.read criteria weights and compine with action compl
criandstr<-FinalWeights(EXCEL,n_bin_actions,n_cri_resp)


###3.compute results
FinalInterventionsResults<-data.frame(Interventions=vector(),Average_Outcomes=vector(),Average_Complexity=vector())
strategy<-list()

for (i in 1:n_blocks){#Loop throught the blocks
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
    if (j==1) {strategy[[i]]<-data.frame(results[,c(1,2,3,6)])}#If it is the first respondant it will get the names
    else{strategy[[i]]<-cbind(strategy[[i]],results[,c(3,6)])}
  }
  print(paste("Generating averages",j, "in block",i))
  strategy[[i]]$Average_Outcomes<-round(rowMeans(strategy[[i]][which(colnames(strategy[[i]])=="OutcomesMark")]),2)
  strategy[[i]]$Average_Complexity<-round(rowMeans(strategy[[i]][which(colnames(strategy[[i]])=="ComplexityMark")]),2)
  
  
  
  #Generate a table showing the final results for each intervention
  print(paste("Generating FinalInterventions dataframe...",j, "in block",i))
  FinalInterventionsResults<-rbind(FinalInterventionsResults,
                                   data.frame(Internventions=strategy[[i]]$Interventions,
                                              Outcomes=strategy[[i]]$Average_Outcomes,
                                              Complexity=strategy[[i]]$Average_Complexity))
}

weights<-criandstr
i=1
j=1
start<-7+(1+n_int[i])*(j-1) #Int start in row 7 in the template+the number of interventions and two extra rows for each new respondant
end<-7+(1+n_int[i])*(j-1)+n_int[i]-1 #sum the number of interventions to now where to end
#Sheet
sheet<-as.character(paste0("IC",1))
rowindex=start:end
PERM<-read.xlsx(EXCEL,sheet,
                rowIndex =c(5,rowindex), 
                ##colIndex = 1:lastcolum,
                header=TRUE)
#Generate resuls for each respondant
results<-resultsbyResp(EXCEL,sheet,start:end,criandstr,n_bin_actions)

#####generate the first dataset when the  strategy is emty
x<-rep(0,n_totalint)
y<-rep(0,n_bin_actions)

t<-needed_actions_complexity(x,y,weights=criandstr$weights,PM,n_bin_actions)
  
  
extra_intervention(x,y,PM=PM,
                             outmark=FinalInterventionsResults$Outcomes,weights_vector=criandstr$weights,n_bin_actions)
  

x[1]<-1
x[8]<-1


iteration<-strategy(x=x,y=y
        ,PM
        ,outmark=FinalInterventionsResults$Outcomes
        ,weights_vector=criandstr$weights,
        n_bin_actions,n_totalint)

iteration[[5]]
grafico_it(iteration[[5]])


strategy(x=rep(0,n_totalint),y=rep(0,n_bin_actions),PM,outmark,weights_vector,n_bin_actions,n_totalint)

###TESTING strategy FUNCTION

x[which(x>1)]<-1 

#the total outcomes
outpercen<-sum(x*outmark)/sum(outmark)
print('total outcomes ok')


#compute the actions that are needed for the current srategy
df_actions<-needed_actions_complexity(x,y,weights_vector,PM,n_bin_actions)
y<-df_actions$need[1:n_bin_actions] #see which are the needed actions
print('actions ok')

#the total complexity
complepercen<-sum(df_actions$weight)
print('total compls ok')

#data frame for the interventions
#Compute the outcomes and complexity of interventions not in the strategy
extra_interventions<-extra_intervention(x=x,y=y,PM,outmark,weights_vector,n_bin_actions) 
print('extra int ok')

df_actions$extraint<-rep(0,nrow(df_actions))
#Compute the new actions complexity and how many interventins they support
for (i in 1:n_bin_actions){
  ynew<-y
  ynew[i]<-1
  
  #compute how many interventions are possible with the new action
  x_pos_new<-rep(0,n_totalint)
  for (j in 1:n_totalint) {if (all(PM[j,14:(14+n_bin_actions-1)]<=y)){x_pos_new[j]<-1}}
  
  print('new')
  #compute how many interventions were already possible with the new action
  x_pos_old<-rep(0,n_totalint)
  for (k in 1:n_totalint) {if (all(PM[k,14:(14+n_bin_actions-1)]<=y)){x_pos_old[k]<-1}}
  print(paste('old',i))
  
  df_actions$extraint[i]<-sum(x_pos_new-x_pos_old) #the difference between them is the new possible int
  df_actions$extraint[c(n_bin_actions+1,n_bin_actions+2)]<-""}


