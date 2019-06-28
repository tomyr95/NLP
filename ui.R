library(shiny)
library(DT)

shinyUI <- fluidPage( 

    navbarPage(title = h2("WordPredict"),
        
        tabsetPanel(
            
            tabPanel("Main",
                     
                fluidRow(
                    
                    hr(),
                         
                    column(12, wellPanel(
                 
                        textAreaInput("Ngram", label = h3("Enter text to predict next word:"),
                                      value = "at the end of the", rows = 3),
                        
                        actionButton("Clear", "Clear"),
                        actionButton("RndGram", "Random Ngram"),
                        actionButton("Add1", "Add 1"),
                        actionButton("Add2", "Add 2"),
                        actionButton("Add3", "Add 3"),
                        actionButton("Add4", "Add 4"),
                        actionButton("Add5", "Add 5")
                        
                    )),
                
  
                    column(6,
                           
                           DTOutput("NextWord")
                           
                    )
                    
                )
            
            ),
        
            tabPanel("Description", hr(), h3("What is the algorythm behind this app?"),
                     includeHTML("description.html")),
            
            tabPanel("Help", hr(), h3("Instructions:"),
                     includeHTML("instructions.html"))
        
        )
    
    )

)
