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
      radioButtons("hhl_corr", "Household Language", c("All" = "all",
                                                       "English Only" = "english",
                                                       "Spanish" = "spanish",
                                                       "Other" = "other")),
      radioButtons("fs_corr", "SNAP Recepient", c("All" = "all",
                                                      "Yes" = "yes",
                                                       "No" = "no"
                                                       )),
      radioButtons("schl_corr", "Educational attainment", c("All" = "all",
                                                      "High School Not Completed" = "no_hs",
                                                      "High School or GED" = "hs",
                                                      "College Degree" = "college"
                                                      )),
      sliderInput("corr_n", label = NULL, min = 20, max = 500, value = 20),
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
  #Two elements (corr_data = NULL and corr_truth = NULL)
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
    } else if(input$hhl_corr == "other"){
      hhl_sub <- HHLvals[c("0", "3", "4", "5")]
    }

    if(input$fs_corr == "all"){
      fs_sub <- FSvals
    } else if(input$fs_corr == "yes"){
      fs_sub <- FSvals["1"]
    } else if(input$fs_corr == "no"){
      fs_sub <- FSvals["2"]
    }

    if(input$schl_corr == "all"){
      schl_sub <- SCHLvals
    } else if(input$schl_corr == "no_hs"){
      schl_sub <- SCHLvals[as.character(0:15)]
    } else if(input$schl_corr == "hs"){
      schl_sub <- SCHLvals[as.character(16:19)]
    } else if(input$schl_corr == "college"){
      schl_sub <- SCHLvals[as.character(20:24)]
    }

    corr_vars <- c(input$corr_x, input$corr_y)

    # Subset the data
    subsetted_data <- my_sample |>
      filter(
        HHLfac %in% hhl_sub,
        FSfac %in% fs_sub,
        SCHLfac %in% schl_sub
      ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
      {if("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
      {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
      {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
      {if("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
      {if("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
      {if("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
      {if("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
      {if("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
      {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .}


    corr_n <- input$corr_n

    # Ensure there's enough data to sample
    index <- sample(1:nrow(subsetted_data),
                    size = corr_n,
                    replace = TRUE,
                    prob = subsetted_data$PWGTP / sum(subsetted_data$PWGTP))

    # sampled_data <- subsetted_data[index, ]
    # Store the sampled data and calculated correlation
    sample_corr$corr_data <- subsetted_data[index, ]
    sample_corr$corr_truth <- cor(sample_corr$corr_data |> select(all_of(corr_vars)))[1,2]

  })
  # Create the scatter plot using ggplot2
  output$corr_plot <- renderPlot({
    validate(
      need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
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
