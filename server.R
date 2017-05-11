

# Rosdyana Kusuma @ 2017
#
# http://rosdyanakusuma.com/
#

library(shiny)
library(darch)
library(caret)

shinyServer(function(input, output) {
  
  darchFileTrn <- reactive({
    infile <- input$darchTrn
    if (is.null(infile)) {
      return(NULL)
    }
    infile$datapath
  })
  
  output$uploadTestingDarch <- renderUI({
    df = darchFileTrn()
    if(is.null(df))
      return(NULL)
    fileInput("darchTst", 
              "Upload training File", 
              accept = c("text/csv", 
                         "text/comma-separated-values,text/plain", 
                         ".csv", 
                         ".libsvm", 
                         ".arff"))
  })
  
  darchFileTst <- reactive({
    infile <- input$darchTst
    if (is.null(infile)) {
      return(NULL)
    }
    infile$datapath
  })
  
  output$submitDarchBtn <- renderUI({
    df = darchFileTst()
    if(is.null(df))
      return(NULL)
    trainData = data.frame(read.csv(darchFileTrn(), sep = ",", header = T))
    tagList(
      selectInput("classX","Select Class :",choices = colnames(trainData) ),
      textInput("numOfEpoch", "Number of Epoch :", value = 250),
      textInput("batchSize", "Number of Batch Size :", value = 5),
      textInput("rateWeights", "Rate of Weight :", value = .8),
      textInput("rateBiases", "Rate of Biases :", value = .8),
      actionButton("Submit","Submit"),
      downloadButton("download1", "Save chart")
    )})
  
  darchFunc <-
    function(trainData,
             testingData,
             numOfEpoch,
             batchSize,
             rateWeights,
             rateBiases) {
      trainData = data.frame(read.csv(darchFileTrn(), sep = ",", header = T))
      testingData = data.frame(read.csv(darchFileTst(), sep = ",", header = T))
      numOfEpoch = as.numeric(input$numOfEpoch)
      batchSize = as.numeric(input$batchSize)
      rateWeights = as.numeric(input$rateWeights)
      rateBiases = as.numeric(input$rateBiases)
      
      withProgress(message = 'Calculating . . .', value = 0, {
        n <- 1
        for (i in 1:n){
          # See XOR example #1 for more details on the parameter values.
          modelDarch <- darch(
            class ~ .,
            trainData,
            # We'll scale all data, useful for faster convergence when data
            # is not already relatively close to 0 (or, say, within -1..1)
            scale = T,
            # number of epochs for pre-training
            rbm.numEpochs = 0,
            # vector containing one integer for the number of neurons
            # in each layer ( including input and output layers )
            layers = c(4, 20, 10, 3),
            # batch size equals the number of classes, which is usually a
            # sensible choice
            darch.batchSize = batchSize,
            # higher for sigmoid activation
            darch.learnRateWeights = rateWeights,
            darch.learnRateBiases = rateBiases,
            # binary classification
            darch.isBin = T,
            # We'll stop when either all training examples are correctly
            # classified or the validation error drops below 1%...
            darch.stopClassErr = 0,
            darch.stopValidClassErr = 1,
            # ... or when training has been going on for n epochs.
            darch.numEpochs = numOfEpoch,
            # change to DEBUG if needed
            darch.logLevel = futile.logger::INFO
          )
          incProgress(1/n, detail = paste("Doing part", i))
          Sys.sleep(0.1)
        }
      })
      # print(modelDarch)
      
      # the predict function can be used to get the network output for a new set of
      # data, it will even convert the output back to the original character labels
      predictions <- predict(modelDarch, newdata = testingData, type = "class")
      
      # And these labels can then easily be compared to the correct ones
      numIncorrect <-
        sum(predictions != trainData[, length(trainData)])
      output$darchResult <- renderPrint({
        cat(
          paste0(
            "Incorrect classifications on all examples: ",
            numIncorrect,
            " (",
            round(numIncorrect / nrow(trainData) * 100, 2),
            "%)\n"
          )
        )
      })
      output$darchPlot <- renderPlot({
        plot(modelDarch)
      })
      
      # Download chart
      output$download1 <- downloadHandler(
        filename = function() { paste(input$darchTrn, '.png', sep='') },
        content = function(file) {
          ggsave(file,plot(modelDarch))
        }
      )
      outputOptions(output, "download2", suspendWhenHidden=FALSE)
    }
  
  output$darchPlotTmp <- renderUI({
    plotOutput("darchPlot")
  })
  
  observeEvent(input$Submit, {
    darchFunc()
  })
  
  ## darch + caret
  
  darchFileTrn2 <- reactive({
    infile <- input$darchTrn2
    if (is.null(infile)) {
      return(NULL)
    }
    infile$datapath
  })
  
  output$submitDNBtn <- renderUI({
    df = darchFileTrn2()
    if(is.null(df))
      return(NULL)
    datas = data.frame(read.csv(darchFileTrn2(), sep = ",", header = T))
    tagList(
      selectInput("class","Select Class :",choices = colnames(datas) ),
      textInput("numOfEpoch2", "Number of Epoch :", value = 10),
      textInput("batchSize2", "Number of Batch Size :", value = 5),
      actionButton("SubmitDC","Submit"),
      downloadButton("download2", "Save chart")
    )})
  
  darchCaretFunc <- function(){
    
    datas = data.frame(read.csv(darchFileTrn2(), sep = ",", header = T))
    tuneLenght = as.numeric(input$tuneLenght)
    batchSize = as.numeric(input$batchSize2)
    numOfEpoch = as.numeric(input$numOfEpoch2)
    
    
    pp <- preProcess(datas, method=c("scale"))
    catsScaled <- predict(pp, newdata = datas)
    withProgress(message = 'Calculating . . .', value = 0, {
      n <- 1
      for (i in 1:n){
    darchCaret <- darch(class ~.,
                        datas,
                        preProc.params = list(method = c("center", "scale")),
                        preProc.targets = T,
                        layers = c(1,20,50,20,1),
                        darch.batchSize =  batchSize,
                        bp.learnRate = .01,
                        darch.isClass = F,
                        darch.numEpochs = numOfEpoch,
                        darch.unitFunction = linearUnit
    )
    incProgress(1/n, detail = paste("Doing part", i))
    Sys.sleep(0.1)
      }
    })
    #print(darchTest(darchCaret, newdata = datas))
    
    output$dcResult <- renderPrint(darchCaret)
    
    
    output$dcPlot <- renderPlot({
      plot(darchCaret)
    })
    
    # Download chart
    output$download2 <- downloadHandler(
      filename = function() { paste(input$darchTrn2, '.png', sep='') },
      content = function(file) {
        ggsave(file,plot(darchCaret))
      }
    )
    outputOptions(output, "download2", suspendWhenHidden=FALSE)
  }
  
  output$dcPlotTmp <- renderUI({
    plotOutput("dcPlot")
  })
  
  observeEvent(input$SubmitDC, {
    darchCaretFunc()
  })
  
})
