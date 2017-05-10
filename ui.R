

# Rosdyana Kusuma @ 2017
#
# http://rosdyanakusuma.com/
#

library(shiny)

navbarPage(
  "Deep-Learning bluebox",
  tabPanel("Darch",
           sidebarLayout(
             sidebarPanel(
               fileInput("darchTrn", 
                         "Choose training File", 
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
  tabPanel("Deepnet",
           sidebarLayout(
             sidebarPanel(
               fileInput("deepnetTrn", 
                         "Choose training File", 
                         accept = c("text/csv", 
                                    "text/comma-separated-values,text/plain", 
                                    ".csv", 
                                    ".libsvm", 
                                    ".arff")),
               uiOutput("uploadTestingDN"),
               uiOutput("submitDNBtn")
             ),
             mainPanel(
               uiOutput("deepnetPlotTmp"),
               verbatimTextOutput("deepnetResult")
             )))
)
