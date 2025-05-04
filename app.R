library(shiny)
library(shinythemes)

# Load necessary libraries
library(xml2)
library(dplyr)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),

  # Include the custom CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=1")
  ),
  
  # Top-level input section
  fluidRow(
    column(12,
      titlePanel("Quiz Interaction App"),
      fluidRow(
        column(4,
          # Select Quiz XML File
          selectInput("quiz_file", "Select Quiz File:", choices = list.files("quizXMLs", pattern = "\\.xml$", full.names = TRUE))
        ),
        column(4,
          # Global Settings
          numericInput("seed", "Set Seed:", value = 123, min = 1),
          numericInput("num_versions", "Number of Versions:", value = 1, min = 1),
          numericInput("num_letter_versions", "Number of Letter Versions:", value = 1, min = 1)
        ),
        column(4,
          # Save Options
          radioButtons("output_format", "Save As:", choices = c("HTML", "Word"), selected = "HTML"),
          actionButton("generate_files", "Generate Output Files")
        )
      )
    )
  ),
  
  # Tabs for different views
  fluidRow(
    column(12,
      tabsetPanel(
        tabPanel("View All",
          fluidRow(
            column(12,
              # Top-level controls for View All
              fluidRow(
                column(4,
                  selectInput("section_filter", "Select Section:", choices = c("All"), selected = "All"),
                  radioButtons("view_format", "View Format:", choices = c("Answer boxes" = "div", "Question list" = "list"), selected = "div")
                ),
                column(4,
                  radioButtons("show_answers", "Show Answers:", choices = c("Yes", "No"), selected= "Yes"),
                  radioButtons("shuffle_answers", "Shuffle Answers:", choices = c("Yes", "No"), selected = "Yes")
                ),
                column(4,
                  actionButton("save_all", "Save All Questions"),
                  downloadButton("download_questions", "Download Questions as HTML")
                )
              ),
              # Full-width display for questions
              htmlOutput("questions_output")
            )
          )
        ),
        tabPanel("View Quiz",
          fluidRow(
            column(12,
              # Full-width display for quiz
              fluidRow(
                column(4,
                  selectInput("quiz_version", "Select Version:", choices = NULL),
                  selectInput("quiz_letter", "Select Letter:", choices = NULL)
                ),
                column(4,
                  checkboxInput("highlight_answers", "Highlight Correct Answers", value = FALSE)
                )
              ),
              htmlOutput("quiz_output"),
              tableOutput("answer_key_table")
            )
          )
        ),
        tabPanel("Save",
          fluidRow(
            column(12,
              h4("Use the Generate Output Files button in the top section to save all versions.")
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
    # Load custom functions
    files <- list.files("R/02-functions", pattern = "\\.R$", full.names = TRUE)
    sapply(files, source)

    # Reactive values for quiz data and HTML content
    quiz_data <- reactiveVal(NULL)
    questions_html <- reactiveVal(NULL)  # Store the HTML content

    # Load and parse the selected quiz file
    observeEvent(input$quiz_file, {
      req(input$quiz_file)
      parsed_data <- parse_d2l_xml(input$quiz_file)
      quiz_data(parsed_data)
    })
    
    # Update section filter choices when a new quiz file is loaded
    observeEvent(input$quiz_file, {
      req(input$quiz_file)
      quiz_data(parse_d2l_xml(input$quiz_file))
      
      # Extract section titles
      sections <- c("All", sapply(quiz_data(), function(section) section$section_title))
      sections <- setNames(sections, sections)  # Ensure all elements are named
      
      # Update the section filter dropdown
      updateSelectInput(session, "section_filter", choices = sections)
    })

    # Render questions in the "View All" tab
    output$questions_output <- renderUI({
      req(quiz_data())
      
      # Get the selected section
      selected_section <- input$section_filter
      
      # Filter questions based on the selected section
      if (selected_section == "All") {
        questions_to_display <- quiz_data()
      } else {
        # Filter the section based on section_title
        questions_to_display <- list(
          quiz_data()[[which(sapply(quiz_data(), function(section) section$section_title == selected_section))]]
        )
      }

      # Generate HTML for all questions
      html_content <- generate_questions_html(
        questions_to_display, 
        dispFormat = input$view_format, 
        showAnswers = ifelse(input$show_answers == "Yes", TRUE, FALSE), 
        shuffleAnswers = ifelse(input$shuffle_answers == "Yes", TRUE, FALSE), 
        thisSeed = input$seed
      )
      
      # Store the HTML content in the reactive value
      questions_html(html_content)
      
      # Render the HTML
      HTML(html_content)
    })

    # Download handler for saving questions as HTML
    output$download_questions <- downloadHandler(
      filename = function() {
        paste0("questions_output_", Sys.Date(), ".html")
      },
      content = function(file) {
        # Get the stored HTML content
        html_content <- questions_html()
        req(html_content)  # Ensure the content is available
        
        # Add a link to the style.css file in the header
        # Read the CSS content from the styles.css file
        css_content <- readLines("www/styles.css")
        
        # Combine the CSS content with the HTML content
        styled_html <- paste0(
          '<!DOCTYPE html><html><head><style>',
          paste(css_content, collapse = "\n"),
          '</style></head><body>',
          html_content,
          '</body></html>'
        )
        
        # Write the styled HTML content to the file
        writeLines(styled_html, file)
      }
    )
}

# Run the application
shinyApp(ui = ui, server = server)