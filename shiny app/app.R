myDF <- list("Introduction" = c("aids data set", "pbc2 data set"), "Linear Mixed-Effects Models" = "", "Relative Risk Models" = "",
             "The Basic Joint Model" = "", "Extensions of Joint Models" = c("Parameterizations", "Multivariate Joint Models", 
                                                                            "Time-Varying Effects"), 
             "Dynamic Predictions" = "",
             "Practical 2 - extra" = c("Task 1-5", "Task 6", "Task 7", "Task 8-9", "Task 10"))

library(shiny)
library(shinyWidgets)
library(JMbayes)

load("multJMFit.RData")
load("jointFit.RData")
load("multJMFitPractical.RData")

# Define UI for application that draws a histogram
ui <-  fluidPage(   
  
  h3("Joint Modeling Workshop"),
  
  sidebarLayout(
    
    sidebarPanel(
      wellPanel(
        selectInput("Chapter", "Chapter:", 
                    choices = c("Introduction", "Linear Mixed-Effects Models", "Relative Risk Models", 
                                "The Basic Joint Model", "Extensions of Joint Models", "Dynamic Predictions",
                                "Practical 2 - extra"), selected = "Introduction"),
        
        uiOutput("secondSelection"),
        uiOutput("MMSelection"),
        uiOutput("JMparamSelection"),
        uiOutput("ShrinkSelection"),
        uiOutput("TimeVarSelection"),
        uiOutput("dynPredSelection"),
        uiOutput("obsChoose"),
        uiOutput("obsChoosePractical")
      )
    ),
    
    
    mainPanel(    
      tabsetPanel(
        tabPanel("R code", uiOutput("codeIntr"), uiOutput("codeMM"), uiOutput("codeRR"), uiOutput("codeJM"), 
                 uiOutput("codeJMparam"), uiOutput("codeJMshrink"), uiOutput("codeJM_TVeffect"), uiOutput("codePred"),
                 uiOutput("codeT12345"), uiOutput("codeT6"), uiOutput("codeT7"), uiOutput("codeT89"),
                 uiOutput("codeT10")), 
        tabPanel("Output", uiOutput("outputIntr"), uiOutput("outputMM"), uiOutput("outputRR"), uiOutput("outputJM"), 
                 uiOutput("outputJMparam"), uiOutput("outputJMshrink"), uiOutput("outputJM_TVeffect"), 
                 uiOutput("outputT12345"), uiOutput("outputT6"), uiOutput("outputT7"), uiOutput("outputT89"), 
                 #plotOutput("MvDynPredPract"),
                 conditionalPanel(condition = 'input.Chapter == "Practical 2 - extra" && input.Subselection == "Task 10"', plotOutput("MvDynPredPract")),
                 conditionalPanel(condition = 'input.dynPred == "univariate"', plotOutput("DynPred")),
                 conditionalPanel(condition = 'input.dynPred == "multivariate"', plotOutput("MvDynPred"))
                 )
        )
      )
      
    )
    
    
)  
  
  
  



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  output$secondSelection <- renderUI({
    if (input$Chapter %in% c("Introduction", "Extensions of Joint Models", "Practical 2 - extra")) {
      selectInput("Subselection", "", choices = myDF[[input$Chapter]])
    }
  })
  
  output$MMSelection <- renderUI({
    if (input$Chapter == "Linear Mixed-Effects Models") {
      radioButtons("RE", "Random effects", choices = c("Random intercept", "Random intercept and slope", 
                                                       "Diagonal variance matrix, random intercept and slope",
                                                       "Diagonal variance matrix, random intercept and nonlinear slope"))
    }
  })
  
  output$JMparamSelection <- renderUI({
    if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Parameterizations") {
      radioButtons("extensionsJMparam", "Association parameter", choices = c("lag effect", "value and slope"))
    }
  })
  
  output$ShrinkSelection <- renderUI({
    if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Multivariate Joint Models") {
      materialSwitch("extensionsJMshrink", "Apply shrinkage")
    }
  })
  
  output$TimeVarSelection <- renderUI({
    if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Time-Varying Effects") {
      materialSwitch("extensionsJM_TVeffect", "Show plot")
    }
  })
  
  output$obsChoose <- renderUI({
    if (input$Chapter == "Dynamic Predictions") {
      sliderInput("obs", "Number of observations to use in prediction:", min = 2, max = 7, value = 2, step = 1, animate = T)
    }
  })
  
  
  output$dynPredSelection <- renderUI({
    if (input$Chapter == "Dynamic Predictions") {
      radioButtons("dynPred", "Biomarkers", choices = c("univariate", "multivariate"), selected = "univariate")
    }
  })
  
  ## Observations to be used for predictions
  # output$obsChoose <- renderUI({
  #   if (input$Chapter == "Dynamic Predictions") {
  #     load("newPat.RData")
  #     
  #     nr <- nrow(newPat)
  #     #if (!is.null(nr) && nr > 1) {
  #       sliderInput("obs", "Number of observations to use in prediction:",
  #                   min = 2, max = nr - 1, value = 2, step = 1, animate = TRUE)
  #     #}
  #   }
  # })
  

  
  output$obsChoosePractical <- renderUI({
    if (input$Chapter == "Practical 2 - extra" && input$Subselection == "Task 10") {
      sliderInput("obsPractical", "Number of observations to use in prediction:", min = 2, max = 10, value = 2, step = 1, animate = T)
    }
  })
  
  
  
  
  ###########################################################################################  
  output$codeIntr <- reactive({ 
    if (length(input$Subselection) > 0) {
      if (input$Chapter == "Introduction" && input$Subselection == "aids data set"){
        includeMarkdown("Intro_code_aids.Rmd")
      } else if (input$Chapter == "Introduction" && input$Subselection == "pbc2 data set"){
        includeMarkdown("Intro_code_pbc2.Rmd")
      }
    }
  })
  
  output$codeMM <- reactive({ 
    if (length(input$RE) > 0) {
      if (input$Chapter == "Linear Mixed-Effects Models" && input$RE == "Random intercept"){
        includeMarkdown("Mixed_models_RI_code.Rmd")
      } else if (input$Chapter == "Linear Mixed-Effects Models" && input$RE == "Random intercept and slope"){
        includeMarkdown("Mixed_models_RIS_code.Rmd")
      } else if (input$Chapter == "Linear Mixed-Effects Models" && input$RE == "Diagonal variance matrix, random intercept and slope"){
        includeMarkdown("Mixed_models_DRIS_code.Rmd")
      } else if (input$Chapter == "Linear Mixed-Effects Models" && input$RE == "Diagonal variance matrix, random intercept and nonlinear slope"){
        includeMarkdown("Mixed_models_DRINS_code.Rmd")
      }
    }
  })
  
  
  output$codeRR <- reactive({ 
      if (input$Chapter == "Relative Risk Models"){
        includeMarkdown("Survival_code.Rmd")
      }
  })
  
  output$codeJM <- reactive({ 
      if (input$Chapter == "The Basic Joint Model"){
        includeMarkdown("Joint_models_code.Rmd")
      }
  })
  
  output$codeJMparam <- reactive({ 
    if (length(input$Subselection) > 0 && length(input$extensionsJMparam) > 0) {
      if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Parameterizations" &&
          input$extensionsJMparam == "lag effect" ){
        includeMarkdown("Joint_models_lag_code.Rmd")
      } else if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Parameterizations" &&
                 input$extensionsJMparam == "value and slope" ){
        includeMarkdown("Joint_models_VS_code.Rmd")
      }
    }
  })
  
  output$codeJMshrink <- reactive({ 
    if (length(input$Subselection) > 0 && length(input$extensionsJMshrink) > 0) {
      if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Multivariate Joint Models" &&
          input$extensionsJMshrink == 0){
        includeMarkdown("Joint_models_MV_code.Rmd")
      } else if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Multivariate Joint Models" &&
                 input$extensionsJMshrink > 0){
        includeMarkdown("Joint_models_MVshrink_code.Rmd")
      }
    }
  })
  
  output$codeJM_TVeffect <- reactive({ 
    if (length(input$Subselection) > 0 && length(input$extensionsJM_TVeffect) > 0) {
      if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Time-Varying Effects" &&
          input$extensionsJM_TVeffect == 0){
        includeMarkdown("Joint_models_TVeffect_code.Rmd")
      } else if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Time-Varying Effects" &&
                 input$extensionsJM_TVeffect > 0){
        includeMarkdown("Joint_models_TVeffectPlot_code.Rmd")
      }
    }
  })
  
  output$codePred <- reactive({
      if (input$Chapter == "Dynamic Predictions" && input$dynPred == "univariate") {
        includeMarkdown("Joint_models_dynPred_code.Rmd")
      } else if (input$Chapter == "Dynamic Predictions" && input$dynPred == "multivariate"){
        includeMarkdown("Joint_models_MVdynPred_code.Rmd")
      }
    
  })
  
  output$codeT12345 <- reactive({ 
    if (input$Chapter == "Practical 2 - extra" && input$Subselection == "Task 1-5"){
      includeMarkdown("T12345_code.Rmd")
    }
  })
  output$codeT6 <- reactive({ 
    if (input$Chapter == "Practical 2 - extra" && input$Subselection == "Task 6"){
      includeMarkdown("T6_code.Rmd")
    }
  })
  output$codeT7 <- reactive({ 
    if (input$Chapter == "Practical 2 - extra" && input$Subselection == "Task 7"){
      includeMarkdown("T7_code.Rmd")
    }
  })
  output$codeT89 <- reactive({ 
    if (input$Chapter == "Practical 2 - extra" && input$Subselection == "Task 8-9"){
      includeMarkdown("T89_code.Rmd")
    }
  })
  output$codeT10 <- reactive({ 
    if (input$Chapter == "Practical 2 - extra" && input$Subselection == "Task 10"){
      includeMarkdown("T10_code.Rmd")
    }
  })
    
  ###########################################################################################  
  output$outputIntr <- reactive({
     if (length(input$Subselection) > 0) {
      if (input$Chapter == "Introduction" && input$Subselection == "aids data set"){
        includeHTML("Intro_output_aids.html")
      } else if (input$Chapter == "Introduction" && input$Subselection == "pbc2 data set"){
        includeHTML("Intro_output_pbc2.html")
      }
     }
  })

  output$outputMM <- reactive({
     if (length(input$RE) > 0) {
      if (input$Chapter == "Linear Mixed-Effects Models" && input$RE == "Random intercept"){
        includeHTML("Mixed_models_RI_output.html")
      } else if (input$Chapter == "Linear Mixed-Effects Models" && input$RE == "Random intercept and slope"){
        includeHTML("Mixed_models_RIS_output.html")
      } else if (input$Chapter == "Linear Mixed-Effects Models" && input$RE == "Diagonal variance matrix, random intercept and slope"){
        includeHTML("Mixed_models_DRIS_output.html")
      } else if (input$Chapter == "Linear Mixed-Effects Models" && input$RE == "Diagonal variance matrix, random intercept and nonlinear slope"){
        includeHTML("Mixed_models_DRINS_output.html")
      }

     }

  })


  output$outputRR <- reactive({
      if (input$Chapter == "Relative Risk Models"){
        includeHTML("Survival_output.html")
      }
  })

  output$outputJM <- reactive({
      if (input$Chapter == "The Basic Joint Model"){
        includeHTML("Joint_models_output.html")
      }
  })

  output$outputJMparam <- reactive({
     if (length(input$Subselection) > 0 && length(input$extensionsJMparam) > 0) {
      if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Parameterizations" &&
          input$extensionsJMparam == "lag effect" ){
        includeHTML("Joint_models_lag_output.html")
      } else if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Parameterizations" &&
                 input$extensionsJMparam == "value and slope" ){
        includeHTML("Joint_models_VS_output.html")
      }
  
     }
  })

  output$outputJMshrink <- reactive({
     if (length(input$Subselection) > 0 && length(input$extensionsJMshrink) > 0) {
      if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Multivariate Joint Models" &&
          input$extensionsJMshrink == 0){
        includeHTML("Joint_models_MV_output.html")
      } else if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Multivariate Joint Models" &&
                 input$extensionsJMshrink > 0){
        includeHTML("Joint_models_MVshrink_output.html")
      }
     }
  })

  output$outputJM_TVeffect <- reactive({ 
    if (length(input$Chapter) > 0 & length(input$Subselection) > 0 & length(input$extensionsJM_TVeffect) > 0) {
      if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Time-Varying Effects" &&
          input$extensionsJM_TVeffect == 0){
        includeHTML("Joint_models_TVeffect_output.html")
      } else if (input$Chapter == "Extensions of Joint Models" && input$Subselection == "Time-Varying Effects" &&
                 input$extensionsJM_TVeffect > 0){
        includeHTML("Joint_models_TVeffectPlot_output.html")
      }
    }
  })
  
  
  ##################################################################  
  ### Dynamic prediction plots
 
  output$DynPred <- renderPlot({
    if (length(input$dynPred) > 0){
      #if (length(input$Chapter) > 0 && length(input$obs) > 0){
        if (input$Chapter == "Dynamic Predictions"){
         newPat <- pbc2[pbc2$id %in% c(8), ]
         newPatPred <- newPat[1:input$obs,]
         sfit <- survfitJM(jointFit, newdata = newPatPred)
         dynPred <- plot(sfit, include.y = TRUE, estimator = "mean", conf.int = TRUE, fill.area = TRUE, lwd = 3, pch = 16,
              col.abline = "black", col.area = "grey", col.points = "black", cex.axis.z = 1, cex.lab.z = 1)
         dynPred
        }
     # }
    }
  })
  
  
  ### Multivariate dynamic prediction plots
  
  output$MvDynPred <- renderPlot({
    if (length(input$dynPred) > 0){
    #if (length(input$Chapter) > 0 && length(input$obs) > 0){
      if (input$Chapter == "Dynamic Predictions"){
        newPat <- pbc2[pbc2$id %in% c(8), ]
          newPatPred <- newPat[1:input$obs,]
          sfit <- survfitJM(multJMFit, newdata = newPatPred)
          MVdynPred <- plot(sfit, split = c(2, 1), include.y = TRUE, estimator = "mean", conf.int = TRUE, fill.area = TRUE, lwd = 3, pch = 16, 
               col.abline = "black", col.area = "grey", col.points = "black", cex.axis.z = 1, cex.lab.z = 1)
        MVdynPred
      }
    }
  })
  
  #############################################
  
  output$outputT12345 <- reactive({ 
    if (input$Chapter == "Practical 2 - extra" && input$Subselection == "Task 1-5"){
      includeHTML("T12345_output.html")
    }
  })
  output$outputT6 <- reactive({ 
    if (input$Chapter == "Practical 2 - extra" && input$Subselection == "Task 6"){
      includeHTML("T6_output.html")
    }
  })
  output$outputT7 <- reactive({ 
    if (input$Chapter == "Practical 2 - extra" && input$Subselection == "Task 7"){
      includeHTML("T7_output.html")
    }
  })
  output$outputT89 <- reactive({ 
    if (input$Chapter == "Practical 2 - extra" && input$Subselection == "Task 8-9"){
      includeHTML("T89_output.html")
    }
  })
  
  ### Dynamic prediction plots Practical
  
  output$MvDynPredPract <- renderPlot({
    if (length(input$Chapter) > 0 & length(input$obsPractical) > 0){
      if (input$Chapter == "Practical 2 - extra" && input$Subselection == "Task 10"){
        newPat <- pbc2[pbc2$id %in% c(81), ]
        newPatPred <- newPat[1:input$obsPractical,]
        sfit <- survfitJM(multJMFit, newdata = newPatPred)
        dynPred <- plot(sfit, include.y = TRUE, split = c(2, 1), estimator = "mean", conf.int = TRUE, fill.area = TRUE, lwd = 3, pch = 16, 
                        col.abline = "black", col.area = "grey", col.points = "black", cex.axis.z = 1, cex.lab.z = 1)
        dynPred
      }
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

