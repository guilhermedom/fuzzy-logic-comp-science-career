library(shiny)
library(shinythemes)
source("util.R")

system = util$setFuzzySystem()

# Define UI for application
ui = fluidPage(
    theme = shinytheme("readable"),

    # Application title
    titlePanel("Finding out if computer science is a good career for you!"),
    helpText("Set the sliders and click on \'Find Out!\'."),
    
    fluidRow(
        column(4, align = "center",
               sliderInput("sliMathID",
                           "How much do you like math?",
                           min = 0,
                           max = 100,
                           step = 5,
                           value = 90)
        ),
        column(4, align = "center",
               sliderInput("sliInterID",
                           "How would you rate your interpersonal skills?",
                           value = 30,
                           min = 0,
                           max = 100,
                           step = 5)
        ),
        column(4, align = "center",
               sliderInput("sliCodeID",
                           "How much do you like writing code?",
                           min = 0,
                           max = 100,
                           step = 5,
                           value = 95)
        ),
    ),
    fluidRow(
        column(4, align = "center",
               sliderInput("sliLeadID",
                           "How strong do you think your leadership skills are?",
                           min = 0,
                           max = 100,
                           step = 5,
                           value = 35)
        ),
        column(4, align = "center",
               sliderInput("sliStudyID",
                           "How much do you like to study?",
                           min = 0,
                           max = 100,
                           step = 5,
                           value = 90)
        ),
        column(4, align = "center",
               sliderInput("sliCommID",
                           "How would you rate your communication skills?",
                           min = 0,
                           max = 100,
                           step = 5,
                           value = 45)
        )
    ),
    fluidRow(
        column(12, align = "center",
               actionButton("findOutButtonID", "Find Out!"),
               helpText("The red line shows your compatibility degree with the
                        computer science career."),
               plotOutput("resultGraphID")
        )
    ),
    fluidRow(
        column(12, align = "center",
               h2(textOutput("systemTextID"))),
    ),
    fluidRow(
        column(6, plotOutput("systemGraph1ID")),
        column(6, plotOutput("systemGraph2ID"))
    ),
    fluidRow(
        column(6, plotOutput("systemGraph3ID")),
        column(6, plotOutput("systemGraph4ID"))
    ),
    fluidRow(
        column(6, plotOutput("systemGraph5ID")),
        column(6, plotOutput("systemGraph6ID"))
    ),
    fluidRow(
        column(6, plotOutput("systemGraph7ID"))
    )
)

# Define server logic
server = function(input, output) {
    observeEvent(input$findOutButtonID, {
        inference = fuzzy_inference(
            system, list(
                mathAffinity = input$sliMathID,
                interpersonalAffinity = input$sliInterID,
                codeAffinity = input$sliCodeID,
                leadAffinity = input$sliLeadID,
                studyAffinity = input$sliStudyID,
                commAffinity = input$sliCommID
            )
        )
    
        output$resultGraphID = renderPlot({
            plot(system$variables$class)
            lines(inference, col = "red", lwd=4)
        })
        
        output$systemTextID = renderText({
            "Fuzzy System"
        })
        
        output$systemGraph1ID = renderPlot({
            plot(system[[1]]$mathAffinity)
        })
        output$systemGraph2ID = renderPlot({
            plot(system[[1]]$interpersonalAffinity)
        })
        output$systemGraph3ID = renderPlot({
            plot(system[[1]]$codeAffinity)
        })
        output$systemGraph4ID = renderPlot({
            plot(system[[1]]$leadAffinity)
        })
        output$systemGraph5ID = renderPlot({
            plot(system[[1]]$studyAffinity)
        })
        output$systemGraph6ID = renderPlot({
            plot(system[[1]]$commAffinity)
        })
        output$systemGraph7ID = renderPlot({
            plot(system[[1]]$class)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
