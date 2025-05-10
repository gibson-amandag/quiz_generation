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
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=4")
  ),

  # Top-level input section
  fluidRow(
    column(
      12,
      titlePanel("Quiz Interaction App"),
      fluidRow(
        column(
          4,
          # Select Quiz XML File
          selectInput(
            "quiz_file",
            "Select Quiz File:",
            choices = list.files("quizXMLs",
              pattern = "\\.xml$",
              full.names = TRUE
            )
          )
          # Quiz Title
          , textInput("quiz_title", "Quiz Title:", value = "")
          # File Title
          , textInput("file_title", "File Title:", value = "Quiz")
        ),
        column(
          4,
          # Global Settings
          numericInput("seed", "Set Seed:", value = 123, min = 1)
        )
      )
    )
  ),

  # Tabs for different views
  fluidRow(
    column(
      12,
      tabsetPanel(
        tabPanel(
          "View All",
          fluidRow(
            column(
              12,
              # Top-level controls for View All
              fluidRow(
                column(
                  4,
                  selectInput(
                    "section_filter", "Select Section:"
                    , choices = c("All")
                    , selected = "All"
                  ),
                  radioButtons(
                    "view_format"
                    , "View Format:"
                    , choices = c("Question list" = "list", "Table" = "table")
                    , selected = "table"
                  )
                ),
                column(
                  4,
                  radioButtons(
                    "show_answers"
                    , "Show Answers:"
                    , choices = c("Yes", "No")
                    , selected = "No"
                  ),
                  radioButtons("shuffle_answers"
                  , "Shuffle Answers:"
                  , choices = c("Yes", "No")
                  , selected = "Yes"
                )
                ),
                column(
                  4,
                  downloadButton(
                    "download_questions"
                    , "Download Questions as HTML"
                  )
                )
              ),
              # Full-width display for questions
              htmlOutput("questions_output")
            )
          )
        ),
        tabPanel(
          "View Quiz",
          fluidRow(
            column(
              12,
              # Full-width display for quiz
              fluidRow(
                column(
                  4,
                  numericInput(
                    "num_versions"
                    , "Number of Versions:"
                    , value = 1
                    , min = 1
                  ),
                  numericInput(
                    "num_letter_versions"
                    , "Number of Letter Versions:"
                    , value = 1
                    , min = 1
                  ),
                  # shuffle if all questions used
                  radioButtons(
                    "shuffle_questions"
                    , "Shuffle Questions:"
                    , choices = c("Yes", "No")
                    , selected = "No"
                  )
                ),
                column(
                  4,
                  selectInput(
                    "quiz_version"
                    , "Select Version:"
                    , choices = NULL
                  ),
                  selectInput(
                    "quiz_letter"
                    , "Select Letter:"
                    , choices = NULL
                  ),
                  checkboxInput(
                    "highlight_answers"
                    , "Show Correct Answers"
                    , value = FALSE
                  ),
                  radioButtons(
                    "quiz_display_option"
                    , "Display Option:"
                    , choices = c(
                      "All as Table" = "table"
                      , "All as List" = "list"
                      , "First as Table, Rest as List" = "mixed"
                    )
                    , selected = "table"
                  ), 
                ),
                column(
                  4,
                  # Save Options
                  # radioButtons(
                  #   "output_format"
                  #   , "Save As:"
                  #   , choices = c("HTML", "Word")
                  #   , selected = "HTML"
                  # ),
                  # actionButton("generate_files", "Generate Output Files"),

                  # template selection
                  radioButtons(
                    "template_selection"
                    , "Select Template:"
                    , choices = c("Basic", "MC - table", "MC - list")
                    , selected = "Basic"
                  ),
                  # Add biorender note
                  radioButtons(
                    "biorender_note"
                    , "Add Biorender Footnote:"
                    , choices = c("Yes", "No")
                    , selected = "Yes"
                  ),
                  downloadButton(
                    "download_quiz"
                    , "Download Quiz as HTML"
                  ),
                  downloadButton(
                    "download_answer_key"
                    , "Download Answer Key as CSV"
                  )
                )
              ),
                fluidRow(
                column(
                  12,
                  class = "col-lg-8",
                  htmlOutput("quiz_output")
                ),
                column(
                  12,
                  class = "col-lg-4",
                  tableOutput("answer_key_table")
                )
                )
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
  questions_html <- reactiveVal(NULL) # Store the HTML content
  quiz_versions_html <- reactiveVal(NULL) # Store the quiz versions HTML content
  quiz_versions_with_answers_html <- reactiveVal(NULL) # Store the quiz versions HTML content
  quiz_versions_answers <- reactiveVal(NULL) # Store the quiz answer data frames

  # Load and parse the selected quiz file
  observeEvent(input$quiz_file, {
    req(input$quiz_file)
    
    # Parse the XML file
    parsed_data <- parse_d2l_xml(input$quiz_file)
    quiz_data(parsed_data)
    
    quiz_title <- quiz_data()$title
    
    # Update the quiz title input
    updateTextInput(session, "quiz_title", value = quiz_title)
  })

  # Update section filter choices when a new quiz file is loaded
  observeEvent(input$quiz_file, {
    req(input$quiz_file)
    quiz_data(parse_d2l_xml(input$quiz_file))

    # Extract section titles
    sections <- c(
      "All"
      , sapply(
        quiz_data()$sections
        , function(section) section$section_title)
    )
    sections <- setNames(sections, sections) # Ensure all elements are named

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
      questions_to_display <- quiz_data()$sections
    } else {
      # Filter the section based on section_title
      questions_to_display <- list(
        # May 6, 8:56a
        # After making the quiz_data output a list, this doesn't work any more.
        # Store quiz data and ensure quiz_data() is treated as a list.
        quiz_data()$sections[[which(
          sapply(quiz_data()$sections, 
                 function(section) section$section_title == selected_section)
        )]]
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
    questions_html(html_content$questions)

    # Render the HTML
    HTML(html_content$questions)
  })

  # Download handler for saving questions as HTML
  output$download_questions <- downloadHandler(
    filename = function() {
      quiz_title <- input$file_title
      paste0(quiz_title, "_allQuestions", ".html")
    },
    content = function(file) {
      # Get the stored HTML content
      html_content <- questions_html()
      req(html_content) # Ensure the content is available

      # Add a link to the style.css file in the header
      # Read the CSS content from the styles.css file
      css_content <- readLines("www/styles.css")

      # Combine the CSS content with the HTML content
      styled_html <- paste0(
        "<!DOCTYPE html><html><head><style>",
        paste(css_content, collapse = "\n"),
        "</style></head><body>",
        html_content,
        "</body></html>"
      )

      # Write the styled HTML content to the file
      writeLines(styled_html, file)
    }
  )

    # Download handler for saving the selected quiz version as HTML
  output$download_quiz <- downloadHandler(
    filename = function() {
      # Get the quiz title, version, and letter
      quiz_title <- input$file_title
      selected_version <- input$quiz_version
      selected_letter <- input$quiz_letter
      
      # Construct the filename
      paste0(quiz_title, "_", selected_version, selected_letter, ".html")
    },
    content = function(file) {
      # Get the selected version and letter
      selected_version <- input$quiz_version
      selected_letter <- input$quiz_letter
      show_answers <- input$highlight_answers
      
      # Construct the key for the selected version/letter
      version_key <- paste0(selected_version, "_L", selected_letter)
      
      # Get the HTML for the selected version/letter
      if (show_answers) {
        version_html <- quiz_versions_with_answers_html()[[version_key]]
      } else {
        version_html <- quiz_versions_html()[[version_key]]
      }
      
      req(version_html) # Ensure the HTML exists
      
      # Add a link to the style.css file in the header
      css_content <- readLines("www/styles.css")

      if (input$biorender_note == "No") {
        # Replace the footer text with an empty string
        css_content <- gsub("Figures created with biorender.com", "", css_content)
      }

      # Get the template quiz
      template_file_name <- "basicTemplate"
      if(input$template_selection == "MC - table") {
        template_file_name <- "MC-table-template"
      } else if(input$template_selection == "MC - list") {
        template_file_name <- "MC-list-template"
      }

      intro_content <- readLines(paste0("www/", template_file_name, ".html"))

      # Extract the body content from the template
      body_start <- grep("<body>", intro_content) + 1
      body_end <- grep("</body>", intro_content) - 1
      body_content <- intro_content[body_start:body_end]

      # Replace {{quiz_title}} with the actual quiz title
      print_title <- paste0(input$quiz_title, " - ", selected_version, selected_letter)
      body_content <- gsub("\\{\\{quiz_title\\}\\}", print_title, body_content)

      # Combine the CSS content with the HTML content
      styled_html <- paste0(
        "<!DOCTYPE html><html><head><style>",
        paste(css_content, collapse = "\n"),
        "</style></head><body>",
        # "<div>Test</div>",
        paste(body_content, collapse = "\n"),
        version_html,
        "</body></html>"
      )
      
      # Write the styled HTML content to the file
      writeLines(styled_html, file)
    }
  )

  # Download handler for saving the answer key as CSV
  output$download_answer_key <- downloadHandler(
    filename = function() {
      # Get the quiz title, version, and letter
      quiz_title <- input$file_title
      
      # Construct the filename
      paste0(quiz_title, "_answerKeys.csv")
    },
    content = function(file) {
      # get all answer keys
      answer_keys <- quiz_versions_answers()

      # new row for each version and letter, add column for version and letter
      answer_key <- do.call(rbind, lapply(names(answer_keys), function(key) {
        version_letter <- strsplit(key, "_L")[[1]]
        version <- version_letter[1]
        # remove the "V" from the version
        version <- gsub("V", "", version)
        letter <- version_letter[2]
        data <- answer_keys[[key]]
        data$version <- version
        data$letter <- letter
        return(data)
      }))
      
      req(answer_key)  # Ensure the answer key exists
      
      # Write the answer key to a CSV file
      write.csv(answer_key, file, row.names = FALSE)
    }
  )

  observeEvent({
    input$quiz_display_option
    input$num_versions
    input$num_letter_versions
    input$quiz_file
    input$shuffle_questions
  }, {
    req(quiz_data())  # Ensure quiz data is available
  
    # Initialize a list to store HTML for all versions
    versions_html <- list()
    # Initialize a list to store HTML for all versions
    versions_with_answers_html <- list()

    # Initialize a list to store answer keys
    answer_keys <- list()
  
    # Loop through each version
    for (version in seq_len(input$num_versions)) {
      # Select questions for this version
      shuffleQuestionsIfAll <- if(input$shuffle_questions == "Yes") TRUE else FALSE
      selected_questions <- select_questions(quiz_data()$sections, seed = input$seed + version, shuffleWithinSection = shuffleQuestionsIfAll)
  
      # Loop through each letter version
      for (letterNum in seq_len(input$num_letter_versions)) {
        # Determine the display format for this version/letterNum
        if (input$quiz_display_option == "table") {
          dispFormat <- "table"
        } else if (input$quiz_display_option == "list") {
          dispFormat <- "list"
        } else if (input$quiz_display_option == "mixed") {
          dispFormat <- if (version == 1 && letterNum == 1) "table" else "list"
        }
  
        # Generate HTML for this version/letterNum
        version_html <- generate_questions_html(
          selected_questions,
          dispFormat = dispFormat,
          showAnswers = FALSE,
          shuffleAnswers = TRUE,  # Shuffle answers for each letterNum version
          thisSeed = as.integer(paste0(input$seed, version, letterNum)),
          showSectionTitles = FALSE  # Hide section titles
          , sampledQuestions = TRUE
        )

        # Generate HTML for this version/letterNum
        version_with_answers_html <- generate_questions_html(
          selected_questions,
          dispFormat = dispFormat,
          showAnswers = TRUE,
          shuffleAnswers = TRUE,  # Shuffle answers for each letterNum version
          thisSeed = as.integer(paste0(input$seed, version, letterNum)),
          showSectionTitles = FALSE  # Hide section titles
          , sampledQuestions = TRUE
        )
  
        # Store the HTML in the list
        versionName <- paste0("V", version, "_L", LETTERS[letterNum])
        versions_html[[versionName]] <- version_html$questions
        versions_with_answers_html[[versionName]] <- version_with_answers_html$questions
        # Store the answer key in the list
        answer_keys[[versionName]] <- version_html$answers
      }
    }
  
    # Update the reactive value
    quiz_versions_html(versions_html)
    quiz_versions_with_answers_html(versions_with_answers_html)
    quiz_versions_answers(answer_keys)
  })

  output$quiz_output <- renderUI({
    req(quiz_versions_html())  # Ensure versions HTML is available
  
    # Get the selected version and letter
    selected_version <- input$quiz_version
    selected_letter <- input$quiz_letter
    selected_output <- input$highlight_answers
  
    # Construct the key for the selected version/letter
    version_key <- paste0(selected_version, "_L", selected_letter)
    
    # Get the HTML for the selected version/letter
    if(selected_output) {
      version_html <- quiz_versions_with_answers_html()[[version_key]]
    } else {
      version_html <- quiz_versions_html()[[version_key]]
    }
    
    req(version_html)  # Ensure the HTML exists

  
    # Render the HTML
    HTML(version_html)
  })
  
  # Update the version and letter dropdowns when quiz versions are recalculated
  observeEvent(quiz_versions_html(), {
    req(quiz_versions_html())
  
    # Extract version and letter keys
    version_keys <- names(quiz_versions_html())
    versions <- unique(sub("_L.*", "", version_keys))
    letters <- unique(sub("V.*_L", "", version_keys))
  
    # Update the dropdowns
    updateSelectInput(session, "quiz_version", choices = versions, selected = versions[1])
    updateSelectInput(session, "quiz_letter", choices = letters, selected = letters[1])
  })

  # Render the answer key table
  output$answer_key_table <- renderTable({
    req(quiz_versions_answers())  # Ensure answer keys are available
  
    # Get the selected version and letter
    selected_version <- input$quiz_version
    selected_letter <- input$quiz_letter
  
    # Construct the key for the selected version/letter
    version_key <- paste0(selected_version, "_L", selected_letter)
    
    # Get the answer key for the selected version/letter
    answer_key <- quiz_versions_answers()[[version_key]]
    
    req(answer_key)  # Ensure the answer key exists
  
    answer_key %>%
    mutate(
      questionNum = as.integer(questionNum)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
