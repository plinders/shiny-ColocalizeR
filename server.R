#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tools)
library(devtools)
library(EBImage)
library(rJava)
library(RBioFormats)
library(parallel)
library(data.table)
#library(log4r)
options(shiny.maxRequestSize = 1024 * 1024 ^ 2)

#loggerDebug <- create.logger()
#logfile(loggerDebug) <- 'debugData.log'
#level(loggerDebug) <- 'INFO'

#loggerServer <- create.logger()
#logfile(loggerServer) <- 'serverData.log'
#level(loggerServer) <- 'INFO'

# Define server logic required for ColocalizeR
shinyServer(function(input, output, session) source("src/app.R", local = TRUE))
