library(shiny)
library(ggplot2)
library(shinydashboard)
library(markdown)
require(mosaic)
library(leaps)
library(DT)

server <- function(input, output) {
    

 output$modeloptions <- DT::renderDataTable({
     
     inFile <- input$file1
     
     if (is.null(inFile))
         return(NULL)
     
     df <- read.csv(inFile$datapath, header = TRUE)
     forward <- regsubsets(y ~ ., data=df, nbest = 1, nvmax = 10, method = "forward")
     models <- with(summary(forward), data.frame(cp, outmat))
 }, selection = 'single')

 output$dataviz <- renderPlot({
     
     inFile <- input$file1
     
     if (is.null(inFile))
         return(NULL)
     
     df <- read.csv(inFile$datapath, header = TRUE)
     plot(df)
 })

 output$regression <- renderPlot({
     inFile <- input$file1
     
     if (is.null(inFile))
         return(NULL)
     
     df <- read.csv(inFile$datapath, header = TRUE)
     forward <- regsubsets(y ~ ., data=df, nbest = 1, nvmax = 10, method = "forward")
     models <- with(summary(forward), data.frame(cp, outmat))
     
     rowId <- input$modeloptions_rows_selected[length(input$modeloptions_rows_selected)]
     modelsRow <- models[as.numeric(rowId),-1]
     cls <- colnames(modelsRow)[sapply(modelsRow, function(modelsRow) {
         !is.numeric(modelsRow) & '*' == modelsRow
         })]
     clsformat <- paste("y ~ ",paste(cls, collapse="+"),sep = "")
     lmr <- lm(clsformat, data=df)
     plot(lmr)
     #ggplot(lmr)
     })
 
 output$qqplot <- renderPlot({
     
     inFile <- input$file1
     
     if (is.null(inFile))
         return(NULL)
     
     df <- read.csv(inFile$datapath, header = TRUE)
     forward <- regsubsets(y ~ ., data=df, nbest = 1, nvmax = 10, method = "forward")
     models <- with(summary(forward), data.frame(cp, outmat))
     
     rowId <- input$modeloptions_rows_selected[length(input$modeloptions_rows_selected)]
     modelsRow <- models[as.numeric(rowId),-1]
     cols <- colnames(modelsRow)[apply(modelsRow,1,which.max)]
     cls <- colnames(modelsRow)[sapply(modelsRow, function(modelsRow) {
         !is.numeric(modelsRow) & '*' == modelsRow
     })]
     clsformat <- paste("y ~ ",paste(cls, collapse="+"),sep = "")
     lmr <- lm(clsformat, data=df)
     
     qqnorm(lmr$residuals)
     qqline(lmr$residuals)
 })
 
 output$residuals <- renderPlot({
     
     inFile <- input$file1
     
     if (is.null(inFile))
         return(NULL)
     
     df <- read.csv(inFile$datapath, header = TRUE)
     forward <- regsubsets(y ~ ., data=df, nbest = 1, nvmax = 10, method = "forward")
     models <- with(summary(forward), data.frame(cp, outmat))
     
     rowId <- input$modeloptions_rows_selected[length(input$modeloptions_rows_selected)]
     modelsRow <- models[as.numeric(rowId),-1]
     cols <- colnames(modelsRow)[apply(modelsRow,1,which.max)]
     cls <- colnames(modelsRow)[sapply(modelsRow, function(modelsRow) {
         !is.numeric(modelsRow) & '*' == modelsRow
     })]
     clsformat <- paste("y ~ ",paste(cls, collapse="+"),sep = "")
     lmr <- lm(clsformat, data=df)
     
     plot(lmr$fitted.values, lmr$residuals,
          main = "Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Residuals" )
 })
 
 output$summary <- renderPrint({
     
     inFile <- input$file1
     
     if (is.null(inFile))
         return(NULL)
     
     df <- read.csv(inFile$datapath, header = TRUE)
     forward <- regsubsets(y ~ ., data=df, nbest = 1, nvmax = 10, method = "forward")
     models <- with(summary(forward), data.frame(cp, outmat))
     
     rowSel = length(input$modeloptions_rows_selected)
     if (0 < rowSel)
     {
         rowId <- input$modeloptions_rows_selected[length(input$modeloptions_rows_selected)]
         modelsRow <- models[as.numeric(rowId),-1]
         cls <- colnames(modelsRow)[sapply(modelsRow, function(modelsRow) {
             !is.numeric(modelsRow) & '*' == modelsRow
         })]
         clsformat <- paste("y ~ ",paste(cls, collapse="+"),sep = "")
         lmr <- lm(clsformat, data=df)
         summary(lmr)
     }
     else
     {
         "Click on a row of table 'Model Options'"
     }
     
     
 })
 
}
