library(shiny)
library(shinythemes)

# Load necessary libraries
library(xml2)
library(dplyr)

library(officer)
library(stringr)

# Define UI
ui <- navbarPage(
  title = "Quiz Tool",
  theme = shinytheme("cerulean"),

  # Page 1: D2L Export
  tabPanel(
    "D2L export",
    fluidPage(
      # Include the custom CSS file
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=4")
      ),

      # Top-level input section
      fluidRow(
        column(
          12,
          titlePanel("D2L Quiz Reading"),
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
              ),
              # Add help text to inform the user
              helpText("You can either select a quiz XML file from the list above (files in the 'quizXMLs' folder) or upload your own XML file below."),
              # Add an upload button option
              fileInput("quiz_file_upload", "Upload Quiz XML File:", accept = c(".xml"))
            ),
            column(
              4,
              # Quiz Title
              textInput("quiz_title", "Quiz Title:", value = ""),
              # File Title
              textInput("file_title", "File Title:", value = "Quiz")
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
                        "section_filter", "Select Section:",
                        choices = c("All"),
                        selected = "All"
                      ),
                      radioButtons(
                        "view_format",
                        "View Format:",
                        choices = c("Question list" = "list", "Table" = "table"),
                        selected = "table"
                      )
                    ),
                    column(
                      4,
                      radioButtons(
                        "show_answers",
                        "Show Answers:",
                        choices = c("Yes", "No"),
                        selected = "No"
                      ),
                      radioButtons(
                        "shuffle_answers",
                        "Shuffle Answers:",
                        choices = c("Yes", "No"),
                        selected = "Yes"
                      )
                    ),
                    column(
                      4,
                      downloadButton(
                        "download_questions",
                        "Download Questions as HTML"
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
                        "num_versions",
                        "Number of Versions:",
                        value = 3,
                        min = 1
                      ),
                      numericInput(
                        "num_letter_versions",
                        "Number of Letter Versions:",
                        value = 2,
                        min = 1
                      ),
                      # shuffle if all questions used
                      radioButtons(
                        "shuffle_questions",
                        "Shuffle Questions:",
                        choices = c("Yes", "No"),
                        selected = "No"
                      )
                    ),
                    column(
                      4,
                      selectInput(
                        "quiz_version",
                        "Select Version:",
                        choices = NULL
                      ),
                      selectInput(
                        "quiz_letter",
                        "Select Letter:",
                        choices = NULL
                      ),
                      checkboxInput(
                        "highlight_answers",
                        "Show Correct Answers",
                        value = FALSE
                      ),
                      radioButtons(
                        "quiz_display_option",
                        "Display Option:",
                        choices = c(
                          "All as Table" = "table",
                          "All as List" = "list",
                          "First as Table, Rest as List" = "mixed"
                        ),
                        selected = "table"
                      )
                    ),
                    column(
                      4,
                      # Save Options
                      radioButtons(
                        "template_selection",
                        "Select Template:",
                        choices = c("Basic", "MC - table", "MC - list"),
                        selected = "Basic"
                      ),
                      # Add biorender note
                      radioButtons(
                        "biorender_note",
                        "Add Biorender Footnote:",
                        choices = c("Yes", "No"),
                        selected = "Yes"
                      ),
                      downloadButton(
                        "download_quiz",
                        "Download Quiz as HTML"
                      ),
                      downloadButton(
                        "download_quiz_word",
                        "Download Quiz as Word"
                      ),
                      downloadButton(
                        "download_answer_key",
                        "Download Answer Key as CSV"
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
  ),

  # Page 2: Word to D2L Import
  tabPanel(
    "Word to D2L import",
    fluidPage(
      titlePanel("Word to D2L Import"),
      fluidRow(
        column(
          12,
          fileInput("word_file_upload", "Upload Word File:", accept = c(".docx")),
          helpText("Upload a Word document containing quiz questions to convert it into a D2L-compatible format."),
          # get file name
          textInput("word_quiz_file_name", "File Name:", value = "Quiz")
        )
      ),
      fluidRow(
        column(
          12,
          tableOutput("processed_questions_table"),
          actionButton("save_d2l_csvs", "Save D2L CSVs to output folder"),
          downloadButton("download_d2l_file", "Specify D2L CSV save location")
        )
      ),
      fluidRow(
        column(
          12,
          # Display the quiz as HTML
          htmlOutput("quiz_html_output")
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
  quiz_versions_word <- reactiveVal(NULL) # Store the quiz versions word content
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
      "All",
      sapply(
        quiz_data()$sections,
        function(section) section$section_title
      )
    )
    sections <- setNames(sections, sections) # Ensure all elements are named

    # Update the section filter dropdown
    updateSelectInput(session, "section_filter", choices = sections)
  })

  # Observe event for uploaded quiz file
  observeEvent(input$quiz_file_upload, {
    req(input$quiz_file_upload)

    # Parse the uploaded XML file
    file_path <- input$quiz_file_upload$datapath
    parsed_data <- parse_d2l_xml(file_path)
    quiz_data(parsed_data)

    # Extract the quiz title
    quiz_title <- quiz_data()$title

    # Update the quiz title input
    updateTextInput(session, "quiz_title", value = quiz_title)

    # Extract section titles
    sections <- c(
      "All",
      sapply(
        quiz_data()$sections,
        function(section) section$section_title
      )
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
        quiz_data()$sections[[which(
          sapply(
            quiz_data()$sections,
            function(section) section$section_title == selected_section
          )
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
      if (input$template_selection == "MC - table") {
        template_file_name <- "MC-table-template"
      } else if (input$template_selection == "MC - list") {
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

  output$download_quiz_word <- downloadHandler(
    filename = function() {
      # Get the quiz title, version, and letter
      quiz_title <- input$file_title
      selected_version <- input$quiz_version
      selected_letter <- input$quiz_letter

      # Construct the filename
      paste0(quiz_title, "_", selected_version, selected_letter, ".docx")
    },
    content = function(file) {
      # Require the word documents
      word_docs <- quiz_versions_word()
      req(word_docs) # Ensure the word documents are available

      # get the selected version and letter
      selected_version <- input$quiz_version
      selected_letter <- input$quiz_letter

      # Get the word document for the selected version/letter
      version_key <- paste0(selected_version, "_L", selected_letter)

      word_doc <- word_docs[[version_key]]
      req(word_doc) # Ensure the word document exists

      # Save the Word document to the specified file
      print(word_doc, target = file)
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

      req(answer_key) # Ensure the answer key exists

      # Write the answer key to a CSV file
      write.csv(answer_key, file, row.names = FALSE)
    }
  )

  observeEvent(
    {
      input$quiz_display_option
      input$num_versions
      input$num_letter_versions
      input$quiz_file
      input$shuffle_questions
      input$quiz_file_upload
      input$seed
      input$quiz_title
    },
    {
      req(quiz_data()) # Ensure quiz data is available

      # Initialize a list to store HTML for all versions
      versions_html <- list()
      # Initialize a list to store HTML for all versions
      versions_with_answers_html <- list()

      # Initialize a list to store answer keys
      answer_keys <- list()

      # Initialize a list to store the word doc versions
      word_docs <- list()

      # Loop through each version
      for (version in seq_len(input$num_versions)) {
        # Select questions for this version
        shuffleQuestionsIfAll <- if (input$shuffle_questions == "Yes") TRUE else FALSE
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
            shuffleAnswers = TRUE, # Shuffle answers for each letterNum version
            thisSeed = as.integer(paste0(input$seed, version, letterNum)),
            showSectionTitles = FALSE # Hide section titles
            , sampledQuestions = TRUE
          )

          # Generate HTML for this version/letterNum
          version_with_answers_html <- generate_questions_html(
            selected_questions,
            dispFormat = dispFormat,
            showAnswers = TRUE,
            shuffleAnswers = TRUE, # Shuffle answers for each letterNum version
            thisSeed = as.integer(paste0(input$seed, version, letterNum)),
            showSectionTitles = FALSE # Hide section titles
            , sampledQuestions = TRUE
          )

          numQuestions <- sum(sapply(selected_questions, function(section) length(section$sampled_questions)))

          title <- input$quiz_title
          if (is.null(title) || title == "") {
            title <- quiz_title <- quiz_data()$title
          }

          # # Generate the word document for this version/letterNum
          word_doc <- generate_quiz_wordDoc(
            selected_questions = selected_questions,
            shuffleLetter = LETTERS[letterNum],
            quizTitle = title,
            versionNum = version,
            totalQs = numQuestions,
            seed = as.integer(paste0(input$seed, version, letterNum)),
            shuffleAnswers = TRUE
          )

          # Store the HTML in the list
          versionName <- paste0("V", version, "_L", LETTERS[letterNum])
          versions_html[[versionName]] <- version_html$questions
          versions_with_answers_html[[versionName]] <- version_with_answers_html$questions
          # Store the answer key in the list
          answer_keys[[versionName]] <- version_html$answers
          # Store the word doc in the list
          word_docs[[versionName]] <- word_doc
        }
      }

      # Update the reactive value
      quiz_versions_html(versions_html)
      quiz_versions_word(word_docs)
      quiz_versions_with_answers_html(versions_with_answers_html)
      quiz_versions_answers(answer_keys)
    }
  )

  output$quiz_output <- renderUI({
    req(quiz_versions_html()) # Ensure versions HTML is available

    # Get the selected version and letter
    selected_version <- input$quiz_version
    selected_letter <- input$quiz_letter
    selected_output <- input$highlight_answers

    # Construct the key for the selected version/letter
    version_key <- paste0(selected_version, "_L", selected_letter)

    # Get the HTML for the selected version/letter
    if (selected_output) {
      version_html <- quiz_versions_with_answers_html()[[version_key]]
    } else {
      version_html <- quiz_versions_html()[[version_key]]
    }

    req(version_html) # Ensure the HTML exists


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
    req(quiz_versions_answers()) # Ensure answer keys are available

    # Get the selected version and letter
    selected_version <- input$quiz_version
    selected_letter <- input$quiz_letter

    # Construct the key for the selected version/letter
    version_key <- paste0(selected_version, "_L", selected_letter)

    # Get the answer key for the selected version/letter
    answer_key <- quiz_versions_answers()[[version_key]]

    req(answer_key) # Ensure the answer key exists

    answer_key %>%
      mutate(
        questionNum = as.integer(questionNum)
      )
  })

  ## Word to D2L Import ------

  # reactive value to store quiz list data from word
  quiz_list_data <- reactiveVal(NULL)

  # reactive value to store CSVs
  csv_list_data <- reactiveVal(NULL)

  # load the word file
  observeEvent(input$word_file_upload, {
    req(input$word_file_upload)
    file_path <- input$word_file_upload$datapath

    # Get the quiz title from the uploaded word doc
    quiz_title <- tools::file_path_sans_ext(input$word_file_upload$name)
    print(quiz_title)
    if (is.null(quiz_title) || quiz_title == "") {
      quiz_title <- "Quiz"
    }
    # Update the quiz title input
    updateTextInput(session, "word_quiz_file_name", value = quiz_title)

    # Read the quiz from the Word document
    quiz_data <- read_quiz_from_word(file_path)
    # print(quiz_data)
    quiz_list_data(quiz_data)

    # Generate HTML for the quiz
    quiz_html <- generate_questions_html(
      quiz_data,
      dispFormat = "list",
      showAnswers = FALSE,
      shuffleAnswers = FALSE,
      showSectionTitles = TRUE
    )
    # print(quiz_html)

    # display the quiz as HTML
    output$quiz_html_output <- renderUI({
      HTML(quiz_html$questions)
    })


    # Create CSV list
    csv_list <- write_quiz_csvs(quiz_data)
    # print(csv_list)
    csv_list_data(csv_list)
  })

  # Download handler for saving the quiz as a CSV
  output$download_d2l_file <- downloadHandler(
    filename = function() {
      # Get the quiz title from the uploaded Word doc
      quiz_title <- input$word_quiz_file_name
      if (is.null(quiz_title) || quiz_title == "") {
        quiz_title <- "Quiz"
      }
      # Construct the filename for the zip file
      paste0(quiz_title, "_sections.zip")
    },
    content = function(file) {
      # Get the CSV list data
      csv_list <- csv_list_data()
      req(csv_list) # Ensure the CSV list exists

      # Create a temporary directory to store the section files
      temp_dir <- tempdir()
      section_files <- c()

      # Write each section's CSV content to a separate file
      for (i in seq_along(csv_list)) {
        section_csv <- csv_list[[i]]
        section_filename <- paste0(input$word_quiz_file_name, "_Section", i, ".csv")
        section_filepath <- file.path(temp_dir, section_filename)
        write.table(section_csv, section_filepath, row.names = FALSE, col.names = FALSE, quote = TRUE, sep = ",", fileEncoding = "UTF-8")
        section_files <- c(section_files, section_filename)
      }

      # Create a zip file containing all the section files
      old_wd <- setwd(temp_dir) # Change working directory to the temp directory
      on.exit(setwd(old_wd)) # Ensure the working directory is reset after zipping
      zip(file, section_files) # Use relative paths for the zip file
    },
    contentType = "application/zip"
  )

  # Action button to save the CSVs to the output folder
  observeEvent(input$save_d2l_csvs, {
    req(csv_list_data()) # Ensure quiz data is available

    # Get the quiz data
    csv_content_list <- csv_list_data()

    # File name for the quiz
    quizTitle <- input$word_quiz_file_name
    if (is.null(quizTitle) || quizTitle == "") {
      quizTitle <- "Quiz"
    }

    output_dir <- "./output/D2L_CSVs/" # Specify the output directory

    # Write each section's CSV content to a separate file
    for (i in seq_along(csv_content_list)) {
      section_csv <- csv_content_list[[i]]
      section_filename <- paste0(quizTitle, "_Section", i, ".csv")
      write.table(section_csv, file.path(output_dir, section_filename), row.names = FALSE, col.names = FALSE, quote = TRUE, sep = ",", fileEncoding = "UTF-8")
      # add confirmation message after saving
      showNotification(paste("Saved", section_filename, "to", output_dir), type = "message")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
