library(shinydashboard)
require(shiny)
library(xlsx)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(DT)
library(readxl)
Sys.setlocale('LC_ALL','C')
# Define server logic required to draw a histogram


load("fb.rda")
load("sw.rda")
load("pop.rda")
load("res_ss.rda")