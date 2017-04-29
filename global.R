# File:        global.R
# Description: calls libraries and defines global contants used throughout code
# Author:      Ainize Cidoncha

#set directory

#setwd("~/git/AINIZE/Master/APP")
options(java.parameters = "-Xmx8000m")
#install and load packages
#install.packages("devtools")
#devtools::install_github("Appsilon/shiny.semantic")
#devtools::install_github("Appsilon/highlighter")
packages <- c("shiny", "shinydashboard","Benchmarking","DT","markdown","shinythemes","shinyjs","xlsx" ,"TFDEA", "ggplot2", "dplyr", "tidyr","magrittr","formatR")
if (length(setdiff(packages, installed.packages())) > 0)
  install.packages(setdiff(packages, installed.packages()))
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
library(shiny.semantic)
library(shinyjs)
library(magrittr)
library(highlighter) 
library(formatR)
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

#load my scripts
source("library/LibraryforRankingAnalysis.R")
source("library/LibraryforIterativeTool.R")
source("library/Libraryforsemanticui.R")


