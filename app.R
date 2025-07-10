library(shiny)
library(colourpicker)
library(colorspace)
library(readr)
library(dplyr)

# Load the color data
color_data <- readr::read_csv("data/color_ranks.csv")

# Function to calculate CIE Delta E distance between two colors
calculate_color_distance <- function(color1_hex, color2_hex) {
  # Convert hex colors to Lab color space (perceptually uniform)
  lab1 <- as(hex2RGB(color1_hex), "LAB")@coords
  lab2 <- as(hex2RGB(color2_hex), "LAB")@coords

  # Calculate Delta E (CIE76 formula)
  delta_L <- lab1[1] - lab2[1]
  delta_a <- lab1[2] - lab2[2]
  delta_b <- lab1[3] - lab2[3]

  sqrt(delta_L^2 + delta_a^2 + delta_b^2)
}

# Function to calculate score (lower distance = higher score)
calculate_score <- function(distance) {
  # Max possible distance in Lab space is roughly 100
  # Convert to 0-1000 point scale
  max(0, round(1000 - (distance * 10)))
}

# Define UI
ui <- fluidPage(
  titlePanel("🎨 Color Guessing Game"),

  fluidRow(
    column(
      6,
      wellPanel(
        h3("Current Challenge"),
        h4("You need to guess: "),
        h5(textOutput("color_name")),
        h4("Your Guess:"),
        colourInput("guess_color", "Pick a color:", "#FF0000"),
        div(
          style = "height: 50px; border: 2px solid #ccc; border-radius: 5px; margin: 10px 0;",
          div(
            id = "guess_display",
            style = "height: 100%; border-radius: 3px; background-color: #FF0000;"
          )
        ),
        br(),
        actionButton("submit_guess", "Submit Guess", class = "btn-primary"),
        actionButton("new_color", "New Color", class = "btn-success")
      )
    ),

    column(
      6,
      wellPanel(
        h3("Game Stats"),
        h4("Current Round:"),
        verbatimTextOutput("round_info"),
        h4("Score History:"),
        verbatimTextOutput("score_history")
      )
    )
  ),

  # Add custom CSS and JavaScript
  tags$head(
    tags$style(HTML(
      "
      .well { background-color: #f8f9fa; }
      .btn { margin: 5px; }
      #target_color { transition: background-color 0.3s; }
      #guess_display { transition: background-color 0.3s; }
    "
    )),
    tags$script(HTML(
      "
      $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'guess_color') {
          $('#guess_display').css('background-color', event.value);
        }
      });
      
      Shiny.addCustomMessageHandler('runjs', function(message) {
        eval(message);
      });
    "
    ))
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store game state
  values <- reactiveValues(
    current_color = NULL,
    scores = numeric(0),
    round_number = 0,
    last_distance = NULL,
    last_score = NULL
  )

  # Function to start a new round
  start_new_round <- function() {
    values$current_color <- sample_n(color_data, 1)
    values$round_number <- values$round_number + 1
    values$last_distance <- NULL
    values$last_score <- NULL

    # Update the target color display
    session$sendCustomMessage(
      type = "runjs",
      message = paste0(
        "$('#target_color').css('background-color', '",
        values$current_color$hex,
        "');"
      )
    )
  }

  # Initialize with first color
  observe({
    if (is.null(values$current_color)) {
      start_new_round()
    }
  })

  # Display current color name
  output$color_name <- renderText({
    if (!is.null(values$current_color)) {
      values$current_color$color
    } else {
      "Loading..."
    }
  })

  # Handle guess submission
  observeEvent(input$submit_guess, {
    if (!is.null(values$current_color)) {
      # Calculate distance and score
      distance <- calculate_color_distance(
        values$current_color$hex,
        input$guess_color
      )
      score <- calculate_score(distance)

      # Store results
      values$last_distance <- distance
      values$last_score <- score
      values$scores <- c(values$scores, score)

      # Show feedback
      showModal(modalDialog(
        title = "Round Results",
        div(
          h4("Color Comparison:"),
          div(
            style = "display: flex; gap: 20px; margin: 20px 0;",
            div(
              style = "flex: 1;",
              h5("Target Color"),
              div(
                style = paste0(
                  "height: 60px; background-color: ",
                  values$current_color$hex,
                  "; border: 2px solid #ccc; border-radius: 5px;"
                )
              ),
              p(values$current_color$hex)
            ),
            div(
              style = "flex: 1;",
              h5("Your Guess"),
              div(
                style = paste0(
                  "height: 60px; background-color: ",
                  input$guess_color,
                  "; border: 2px solid #ccc; border-radius: 5px;"
                )
              ),
              p(input$guess_color)
            )
          ),
          h4(paste("Score:", score, "/ 1000")),
          p(paste("Color Distance:", round(distance, 2))),
          p(paste("Average Score:", round(mean(values$scores), 1)))
        ),
        easyClose = TRUE,
        footer = tagList(
          actionButton("continue_game", "Continue", class = "btn-primary")
        )
      ))
    }
  })

  # Continue to next round
  observeEvent(input$continue_game, {
    removeModal()
    start_new_round()
  })

  # Start new color manually
  observeEvent(input$new_color, {
    start_new_round()
  })

  # Display round information
  output$round_info <- renderText({
    if (!is.null(values$current_color)) {
      paste(
        "Round:",
        values$round_number,
        "\nCurrent Color:",
        values$current_color$color,
        "\nTarget Hex:",
        values$current_color$hex,
        if (!is.null(values$last_score)) {
          paste("\nLast Score:", values$last_score, "/ 1000")
        } else {
          "\nMake your guess!"
        }
      )
    }
  })

  # Display score history
  output$score_history <- renderText({
    if (length(values$scores) > 0) {
      paste(
        "Games Played:",
        length(values$scores),
        "\nAverage Score:",
        round(mean(values$scores), 1),
        "\nBest Score:",
        max(values$scores),
        "\nLast 5 Scores:",
        paste(tail(values$scores, 5), collapse = ", ")
      )
    } else {
      "No games played yet."
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
