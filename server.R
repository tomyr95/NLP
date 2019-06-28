library(shiny)
library(DT)

set.seed(123)

shinyServer <- function(session, input, output) {
  
    output$NextWord <- renderDT({
        
        Check <- input$Ngram
        if(Check == ""){
          Data <<- Base
          Result <- Base
        }
        if(Check != ""){
          Data <<- as.character(GuessWord(input$Ngram)[, 1])
          Result <- GuessWord(input$Ngram)
        }
        
          Result
      
                       }, server = FALSE,
                       selection=list(mode="single", target="row"),
                       options = list(dom = 't', ordering = FALSE,
                       columnDefs = list(list(className = 'dt-center', targets = "_all")))
    )
    
    observeEvent(input$Clear, {
      WordChoice <- ""
      updateTextInput(session, "Ngram", value = WordChoice)
    })
  
    observeEvent(input$RndGram, {
      Row <- sample(1:5000, 1)
      WordChoice <- paste(dt5[Row, 1], dt5[Row, 2], collapse = " ")
      updateTextInput(session, "Ngram", value = WordChoice)
    })
    
    observeEvent(input$Add1, {
      WordChoice <- paste(input$Ngram, Data[1], collapse = " ")
      updateTextInput(session, "Ngram", value = WordChoice)
    })
    
    observeEvent(input$Add2, {
      WordChoice <- paste(input$Ngram, Data[2], collapse = " ")
      updateTextInput(session, "Ngram", value = WordChoice)
    })
    
    observeEvent(input$Add3, {
      WordChoice <- paste(input$Ngram, Data[3], collapse = " ")
      updateTextInput(session, "Ngram", value = WordChoice)
    })
    
    observeEvent(input$Add4, {
      WordChoice <- paste(input$Ngram, Data[4], collapse = " ")
      updateTextInput(session, "Ngram", value = WordChoice)
    })
    
    observeEvent(input$Add5, {
      WordChoice <- paste(input$Ngram, Data[5], collapse = " ")
      updateTextInput(session, "Ngram", value = WordChoice)
    })
    
}