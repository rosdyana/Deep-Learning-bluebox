

# Rosdyana Kusuma @ 2017
#
# http://rosdyanakusuma.com/
# https://github.com/rosdyana/Deep-Learning-bluebox

library(shiny)

navbarPage(
  "DARCH-greybox",
  tabPanel("darch",
           sidebarLayout(
             sidebarPanel(
               fileInput("darchTrn", 
                         "Upload training File", 
                         accept = c("text/csv", 
                                    "text/comma-separated-values,text/plain", 
                                    ".csv", 
                                    ".libsvm", 
                                    ".arff")),
               uiOutput("uploadTestingDarch"),
               uiOutput("submitDarchBtn")
              ),
           mainPanel(
             uiOutput("darchPlotTmp"),
             verbatimTextOutput("darchResult")
             ))),
  tabPanel("darch+caret",
           sidebarLayout(
             sidebarPanel(
               fileInput("darchTrn2", 
                         "Upload data :", 
                         accept = c("text/csv", 
                                    "text/comma-separated-values,text/plain", 
                                    ".csv", 
                                    ".libsvm", 
                                    ".arff")),
               uiOutput("submitDNBtn")
             ),
             mainPanel(
               uiOutput("dcPlotTmp"),
               verbatimTextOutput("dcResult")
             )))
)
