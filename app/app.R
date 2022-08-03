library(shiny)
library(shinythemes)
library(sets)

sets_options("universe", seq(0, 100, 1))

variables = set(
    mathAffinity = fuzzy_partition(
        varnames = c(mathMin = 5, mathMinor = 15, mathMedian = 50,
                     mathMajor = 75, mathMax = 90), sd = 10
    ),
    interpersonalAffinity = fuzzy_partition(
        varnames = c(interMin = 30, interMinor = 35, interMedian = 55,
                     interMajor = 75, interMax = 85), sd = 10
    ),
    codeAffinity = fuzzy_partition(
        varnames = c(codeMin = 10, codeMinor = 25, codeMedian = 50,
                     codeMajor = 75, codeMax = 95), sd = 10
    ),
    leaderAffinity = fuzzy_partition(
        varnames = c(leaderMin = 30, leaderMinor = 50, leaderMedian = 70,
                     leaderMajor = 90, leaderMax = 95), sd = 10
    ),
    studyAffinity = fuzzy_partition(
        varnames = c(studyMin = 20, studyMinor = 40, studyMedian = 60,
                     studyMajor = 80, studyMax = 90), sd = 10
    ),
    communicationAffinity = fuzzy_partition(
        varnames = c(communicationMin = 40, communicationMinor = 50,
                     communicationMedian = 60, communicationMajor = 70,
                     communicationMax = 75), sd = 10
    ),
    class = fuzzy_partition(
        varnames = c(poor = 10, average = 50, good = 75,
                     excellent = 95), sd = 10
    )
)

rules = set(
    fuzzy_rule(mathAffinity %is% mathMax && interpersonalAffinity %is% interMin
               && codeAffinity %is% codeMax && leaderAffinity %is% leaderMin
               && studyAffinity %is% studyMax
               && communicationAffinity %is% communicationMin, class %is% excellent
    ),
    fuzzy_rule(mathAffinity %is% mathMax && interpersonalAffinity %is% interMinor
               && codeAffinity %is% codeMajor && leaderAffinity %is% leaderMin
               && studyAffinity %is% studyMax
               && communicationAffinity %is% communicationMinor, class %is% excellent
    ),
    fuzzy_rule(mathAffinity %is% mathMajor && interpersonalAffinity %is% interMinor
               && codeAffinity %is% codeMajor && leaderAffinity %is% leaderMin
               && studyAffinity %is% studyMax
               && communicationAffinity %is% communicationMedian, class %is% excellent
    ),
    fuzzy_rule(mathAffinity %is% mathMajor && interpersonalAffinity %is% interMinor
               && codeAffinity %is% codeMajor && leaderAffinity %is% leaderMinor
               && studyAffinity %is% studyMajor
               && communicationAffinity %is% communicationMedian, class %is% good
    ),
    fuzzy_rule(mathAffinity %is% mathMajor && interpersonalAffinity %is% interMedian
               && codeAffinity %is% codeMedian && leaderAffinity %is% leaderMedian
               && studyAffinity %is% studyMajor
               && communicationAffinity %is% communicationMajor, class %is% good
    ),
    fuzzy_rule(mathAffinity %is% mathMajor && interpersonalAffinity %is% interMedian
               && codeAffinity %is% codeMedian && leaderAffinity %is% leaderMajor
               && studyAffinity %is% studyMajor
               && communicationAffinity %is% communicationMajor, class %is% good
    ),
    fuzzy_rule(mathAffinity %is% mathMedian && interpersonalAffinity %is% interMajor
               && codeAffinity %is% codeMinor && leaderAffinity %is% leaderMajor
               && studyAffinity %is% studyMedian
               && communicationAffinity %is% communicationMajor, class %is% average
    ),
    fuzzy_rule(mathAffinity %is% mathMedian && interpersonalAffinity %is% interMajor
               && codeAffinity %is% codeMinor && leaderAffinity %is% leaderMajor
               && studyAffinity %is% studyMedian
               && communicationAffinity %is% communicationMax, class %is% average
    ),
    fuzzy_rule(mathAffinity %is% mathMinor && interpersonalAffinity %is% interMax
               && codeAffinity %is% codeMin && leaderAffinity %is% leaderMax
               && studyAffinity %is% studyMedian
               && communicationAffinity %is% communicationMax, class %is% average
    ),
    fuzzy_rule(mathAffinity %is% mathMinor && interpersonalAffinity %is% interMax
               && codeAffinity %is% codeMin && leaderAffinity %is% leaderMax
               && studyAffinity %is% studyMinor
               && communicationAffinity %is% communicationMax, class %is% poor
    ),
    fuzzy_rule(mathAffinity %is% mathMin && interpersonalAffinity %is% interMax
               && codeAffinity %is% codeMin && leaderAffinity %is% leaderMax
               && studyAffinity %is% studyMinor
               && communicationAffinity %is% communicationMax, class %is% poor
    ),
    fuzzy_rule(mathAffinity %is% mathMin && interpersonalAffinity %is% interMax
               && codeAffinity %is% codeMin && leaderAffinity %is% leaderMax
               && studyAffinity %is% studyMin
               && communicationAffinity %is% communicationMax, class %is% poor
    )
)

system = fuzzy_system(variables, rules)

# Define UI for application that draws a histogram
ui = fluidPage(
    theme = shinytheme("readable"),

    # Application title
    titlePanel("Finding out if computer science is a good career for you!"),
    helpText("Set the sliders and click on \'Find Out!\'."),
    
    fluidRow(
        column(4, sliderInput("sliMathID", "How much do you like math?", min = 0, max = 100, step = 5, value = 90)),
        column(4, sliderInput("sliInterID", "How would you rate your interpersonal skills?", min = 0, max = 100, step = 5, value = 30)),
        column(4, sliderInput("sliCodeID", "How much do you like writing code?", min = 0, max = 100, step = 5, value = 95)),
    ),
    fluidRow(
        column(4, sliderInput("sliLeaderID", "How strong do you think your leadership skills are?", min = 0, max = 100, step = 5, value = 35)),
        column(4, sliderInput("sliStudyID", "How much do you like to study?", min = 0, max = 100, step = 5, value = 90)),
        column(4, sliderInput("sliCommunicationID", "How would you rate your communication skills?", min = 0, max = 100, step = 5, value = 45))
    ),
    fluidRow(
        column(6, h2("Fuzzy system: "), plotOutput("systemGraphID")),
        column(6, actionButton("findOutButtonID", "Find Out!"),
               helpText("The red line shows your compatibility degree with the computer science career."),
               plotOutput("resultGraphID")
        )
    )
)

# Define server logic required to draw a histogram
server = function(input, output) {
    output$systemGraphID = renderPlot({
        plot(system)
    })
    
    observeEvent(input$findOutButtonID, {
        inference = fuzzy_inference(
            system, list(
                mathAffinity = input$sliMathID, interpersonalAffinity = input$sliInterID,
                codeAffinity = input$sliCodeID, leaderAffinity = input$sliLeaderID,
                studyAffinity = input$sliStudyID, communicationAffinity = input$sliCommunicationID
            )
        )
    
        output$resultGraphID = renderPlot({
            plot(system$variables$class)
            lines(inference, col = "red", lwd=4)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
