library(shiny)
library(shinyalert)
library(tidyverse)

source("helpers.R")

#Define UI for application that draws a scatterplot of correlation between 2 values
ui <- fluidPage(

  titlePanel("Correlation Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Find Correlation:"),
      selectizeInput("corr_x", "x Variable", selected = NULL, choices = numeric_vars),
      selectizeInput("corr_y", "y Variable", selected = NULL, choices = numeric_vars),

      h2("Select a Sample Size"),
      radioButtons("hhl_corr", "Household Language", c("English Only" = "english",
                                                       "Spanish" = "spanish")),
      radioButtons("fs_corr", "Household Language", c("Yes" = "yes",
                                                       "No" = "no",
                                                       "All" = "all")),

      radioButtons("schl_corr", "Educational attainment", c("All" = "all",
                                                      "High School Not Completed" = "no_hs",
                                                      "High School or GED" = "hs",
                                                      "College Degree" = ""
                                                      )),
      sliderInput("corr_n", label = NULL, min = 20, max = 500, value = c(20,500)),
      actionButton("corr_sample", "Get a Sample!")
    ),

    mainPanel(
      plotOutput("corr_plot"),
      conditionalPanel("input.corr_sample",
                       h2("Guess the correlation!"),
                       column(6,
                              numericInput("corr_guess",
                                           "",
                                           value = 0,
                                           min = -1,
                                           max = 1
                              )
                       ),
                       column(6,
                              actionButton("corr_submit", "Check Your Guess!"))
      )
    )
  )
)

my_sample <- readRDS("my_sample_temp.rds")

#Define server logic required for scatterplot
server <- function(input, output, session){

  #Create a reactiveValues object called sample_corr
  sample_corr <- reactiveValues(corr_data = NULL, corr_truth = NULL)

  # Update input boxes so they can't choose the same variable
  observeEvent(c(input$corr_x, input$corr_y), {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x == corr_y){
      choices <- choices[-which(choices == corr_x)]
      updateSelectizeInput(session, "corr_y", choices = choices)
    }
  })

  # Use an observeEvent() to look for the action button (corr_sample)
  observeEvent(input$corr_sample, {

    # Subset logic for hhl, fs, and schl
    if(input$hhl_corr == "all"){
      hhl_sub <- HHLvals
    } else if(input$hhl_corr == "english"){
      hhl_sub <- HHLvals["1"]
    } else if(input$hhl_corr == "spanish"){
      hhl_sub <- HHLvals["2"]
    } else {
      hhl_sub <- HHLvals[c("0", "3", "4", "5")]
    }

    if(input$fs_corr == "all"){
      fs_sub <- FSvals
    } else if(input$fs_corr == "yes"){
      fs_sub <- FSvals["1"]
    } else {
      fs_sub <- FSvals["2"]
    }

    if(input$schl_corr == "all"){
      schl_sub <- SCHLvals
    } else if(input$schl_corr == "no_hs"){
      schl_sub <- SCHLvals[as.character(0:15)]
    } else if(input$schl_corr == "hs"){
      schl_sub <- SCHLvals[as.character(16:19)]
    } else {
      schl_sub <- SCHLvals[as.character(20:24)]
    }

    corr_vars <- c(input$corr_x, input$corr_y)

    # Subset the data
    subsetted_data <- my_sample %>%
      filter(
        HHLfac %in% hhl_sub,
        FSfac %in% fs_sub,
        SCHLfac %in% schl_sub
      ) %>%
      {if("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
      {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
      {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
      {if("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
      {if("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
      {if("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
      {if("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
      {if("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
      {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .}

    # Debugging output
    cat("Number of rows in subsetted_data: ", nrow(subsetted_data), "\n")

    # Validate the number of rows before sampling
    if(nrow(subsetted_data) == 0){
      shinyalert(title = "No data", "No data available for the selected filters.")
      return()
    }

    # Ensure input$corr_n is within valid range
    corr_n <- input$corr_n
    if(corr_n > nrow(subsetted_data)){
      shinyalert(title = "Sample Size Error", "Sample size exceeds available data. Please adjust sample size.")
      return()
    }

    # Ensure there's enough data to sample
    index <- sample(1:nrow(subsetted_data), size = corr_n, replace = TRUE,
                    prob = subsetted_data$PWGTP / sum(subsetted_data$PWGTP))

    sampled_data <- subsetted_data[index, ]
    cat("Number of rows in sampled_data: ", nrow(sampled_data), "\n")

    # Store the sampled data and calculated correlation
    sample_corr$corr_data <- sampled_data
    sample_corr$corr_truth <- cor(sampled_data[[input$corr_x]], sampled_data[[input$corr_y]], use = "complete.obs")

    cat("Number of rows in sample_corr$corr_data: ", nrow(sample_corr$corr_data), "\n")
  })

  # Create the scatter plot using ggplot2
  output$corr_plot <- renderPlot({
    validate(
      need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button."),
      need(!is.null(input$corr_x), "Please select the X variable."),
      need(!is.null(input$corr_y), "Please select the Y variable.")
    )

    ggplot(sample_corr$corr_data, aes_string(x = input$corr_x, y = input$corr_y)) +
      geom_point() +
      labs(x = input$corr_x, y = input$corr_y, title = "Scatter Plot of Selected Variables")
  })

  # Correlation guessing game
  observeEvent(input$corr_submit, {
    close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
    if(close){
      shinyalert(title = "Nicely done!",
                 paste0("The sample correlation is ", round(sample_corr$corr_truth, 4), "."),
                 type = "success"
      )
    } else {
      if(input$corr_guess > sample_corr$corr_truth){
        shinyalert(title = "Try again!", "Try guessing a lower value.")
      } else {
        shinyalert(title = "Try again!", "Try guessing a higher value.")
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
