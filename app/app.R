library(shiny)
library(shinythemes)
source("util.R")

# Load fuzzy system script.
system = util$setFuzzySystem()

# Define UI for application.
ui = fluidPage(
    # Set theme for UI.
    theme = shinytheme("readable"),

    # Application title.
    titlePanel(h1(align="center", "Find out if computer science is a good
                  career for you!"),
               windowTitle = "Find out if computer science is a good
                  career for you!"),
    h5(align="center", helpText("Set the sliders and click on \'Find Out!\'.")),
    
    fluidRow(
        column(4, align = "center",
               sliderInput("sliMathID",
                           "How much do you like math?",
                           value = 90,
                           min = 0,
                           max = 100,
                           step = 5,
                           width = '80%')
        ),
        column(4, align = "center",
               sliderInput("sliInterID",
                           "How would you rate your interpersonal skills?",
                           value = 30,
                           min = 0,
                           max = 100,
                           step = 5,
                           width = '80%')
        ),
        column(4, align = "center",
               sliderInput("sliCodeID",
                           "How much do you like writing code?",
                           value = 95,
                           min = 0,
                           max = 100,
                           step = 5,
                           width = '80%')
        ),
    ),
    fluidRow(
        column(4, align = "center",
               sliderInput("sliLeadID",
                           "How strong do you think your leadership skills are?",
                           value = 35,
                           min = 0,
                           max = 100,
                           step = 5,
                           width = '80%')
        ),
        column(4, align = "center",
               sliderInput("sliStudyID",
                           "How much do you like to study?",
                           value = 90,
                           min = 0,
                           max = 100,
                           step = 5,
                           width = '80%')
        ),
        column(4, align = "center",
               sliderInput("sliCommID",
                           "How would you rate your communication skills?",
                           value = 45,
                           min = 0,
                           max = 100,
                           step = 5,
                           width = '80%')
        )
    ),
    fluidRow(
        column(12, align = "center",
               actionButton("findOutButtonID", "Find Out!"),
               
               hr(),
               
               plotOutput("resultGraphID"),
               textOutput("plotHintID")
        )
    ),
    
    br(),
    
    # Plot fuzzy system and membership functions.
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

# Define server logic.
server = function(input, output) {
    # All back end processing is done once the "Generate!" button is pressed.
    observeEvent(input$findOutButtonID, {
        # Pass slider inputs to the fuzzy system set by the util script.
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
            plot(system$variables$class,
                 main = "Affinity with the Computer Science Career")
            lines(inference, col = "red", lwd=4)
        })
        output$plotHintID = renderText({
            "The red line shows your compatibility degree with the computer
            science career."
        })
        
        # Plot membership functions.
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

# Run the application.
shinyApp(ui = ui, server = server)
