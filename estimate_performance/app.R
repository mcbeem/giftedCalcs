# Estimate gifted identification sensitivity

#library(devtools)
#install_github("mcbeem/giftedCalcs")

library(shiny)
library(knitr)
library(giftedCalcs)
library(scdensity)

# Define UI
ui <- shinyUI(fluidPage(

  # Application title
  titlePanel("Estimate Gifted Identification System Sensitivity (Beta)"),

  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Select data',
                accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
      tags$hr(),

      sliderInput("id.rate",
                  "Gifted identification rate",
                  min = 0,
                  max = .3,
                  value = .05,
                  step=.001),

      sliderInput("nom.rate",
                  HTML("Nomination rate"),
                  min = .00,
                  max = .5,
                  value = .10,
                  step=.001),

      # conditionalPanel(
      #   condition = "input.know_relyt == true",
      #   sliderInput("relyt",
      #               HTML("Confirmatory test reliability"),
      #               min = .00,
      #               max = 1,
      #               value = 1,
      #               step=.01)),

      sliderInput("reps",
                  HTML("Bootstrap repetitions"),
                  min = 10,
                  max = 800,
                  value =10,
                  step=10),

      # checkboxInput("know_relyt",
      #               "Specify confirmatory test reliability?",
      #               value=F),

      actionButton("goButton", "Go")
    ),

      mainPanel(
        plotOutput("plot1"),
        tableOutput('summary')
      )
  )
))


# Define server logic required to draw a histogram
server <- function(input, output, session) {

    observe({
      file = input$file
      if (is.null(file)) {
        return(NULL)
      }

      # Take a dependency on input$goButton
      data = as.matrix(isolate(read.csv(file$datapath)))

      output$plot1 <- renderPlot({

        hist(data,
             main="Histogram of observed scores for identified students",
             xlab="Score", ylab="", yaxt='n', freq=F, right=F,
             breaks=seq(min(data), max(data),
             length.out=round(length(data)/20,0)),
             xlim=c(min(data)-sd(data)/4, max(data)+sd(data)/3))

         points(scdensity(data, constraint="unimodal", adjust=.65),
                col="blue", type="l")

         abline(v=min(data), col="red", lwd=2.5, lty="dashed")
      })


      # observeEvent(input$know_relyt, {
      #   if (input$know_relyt==FALSE) {updateSliderInput(session, "relyt", value =1)}
      # })
      #
      observeEvent(input$goButton, {

        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())

        progress$set(message = "Performing computations...", value = 0)

        id.rate <- isolate(input$id.rate)
        nom.rate <- isolate(input$nom.rate)
        reps <- isolate(input$reps)

        #relyt <- ifelse(input$know_relyt == true, isolate(input$relyt), 1)
        #relyt <- isolate(input$relyt)

        #data.std <- (data[,1] - 100) / 15

        a <- estimate_performance(x=data, id.rate=id.rate,
                                    nom.rate=nom.rate, reps=reps,
                                  pop.mean=100, pop.sd=15)

        # if (input$know_relyt==TRUE) {
        #
        #   result <- data.frame(
        #     matrix(
        #       c("Sensitivity", a$summary[2, 1:4],
        #         "Nomination validity", a$summary[1, 1:4],
        #         "Incorrect identification rate", a$summary[3, 1:4],
        #         "Nomination pass rate", a$summary[4, 1:4],
        #         "Test cutoff percentile", a$summary[5, 1:4]),
        #       byrow=T, ncol=5))
        # }
        #
        # if (input$know_relyt==FALSE) {

          result <- data.frame(
            matrix(
              c("Sensitivity upper bound", a$summary[2, 1:4],
                "Nomination validity", a$summary[1, 1:4],
                "Nomination pass rate", a$summary[3, 1:4],
                "Test cutoff percentile", a$summary[4, 1:4]),
              byrow=T, ncol=5))
        # }

       names(result) <- c("Parameter", "Value", "Std Err", "95% CI lower", "95% CI upper")

        output$summary <- renderTable({
          result
        }, digits=3, striped=T)


      })

  })
}

# Run the application
shinyApp(ui = ui, server = server)

