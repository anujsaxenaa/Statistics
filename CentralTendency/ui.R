library(shiny)
shinyUI(fluidPage(theme = "http://bootswatch.com/2/readable/bootstrap.min.css","Central Tendencies",
                  title = "Central Tendencies",
                  fluidRow(column(11.5,offset=0,plotOutput('densityPlot')),
                           column(7,offset=2,
                                  sliderInput(
                                    "start", 
                                    "Iteration",
                                    min = 1, 
                                    max = 21,
                                    value = 1,
                                    step = 1,
                                    round = FALSE, 
                                    ticks = FALSE,
                                    format = "####.##",
                                    #playButton
                                    animate = animationOptions(
                                      interval = 1300, 
                                      loop = FALSE
                                    )
                                  ))
                            )
                  )
        )