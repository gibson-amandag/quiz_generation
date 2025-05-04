library(shiny)
library(shinythemes)

# Load necessary libraries
library(xml2)
library(dplyr)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # App title
  titlePanel("Quiz Interaction App"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Select Quiz XML File
      selectInput("quiz_file", "Select Quiz File:", choices = list.files("quizXMLs", pattern = "\\.xml$", full.names = TRUE)),
      
      # Global Settings
      checkboxInput("show_answer_boxes", "Show Answer Boxes (Div Version)", value = TRUE),
      numericInput("seed", "Set Seed:", value = 123, min = 1),
      numericInput("num_versions", "Number of Versions:", value = 1, min = 1),
      numericInput("num_letter_versions", "Number of Letter Versions:", value = 1, min = 1),
      
      # Save Options
      radioButtons("output_format", "Save As:", choices = c("HTML", "Word"), selected = "HTML"),
      actionButton("generate_files", "Generate Output Files")
    ),
    
    mainPanel(
      # Tabs
      tabsetPanel(
        tabPanel("View All",
          sidebarLayout(
            sidebarPanel(
              selectInput("section_filter", "Select Section:", choices = c("All"), selected = "All"),
              radioButtons("view_format", "View Format:", choices = c("Div Format", "Regular Format"), selected = "Div Format"),
              actionButton("save_all", "Save All Questions")
            ),
            mainPanel(
              htmlOutput("questions_output")
            )
          )
        ),
        tabPanel("View Quiz",
          sidebarLayout(
            sidebarPanel(
              selectInput("quiz_version", "Select Version:", choices = NULL),
              selectInput("quiz_letter", "Select Letter:", choices = NULL),
              checkboxInput("highlight_answers", "Highlight Correct Answers", value = FALSE)
            ),
            mainPanel(
              htmlOutput("quiz_output"),
              tableOutput("answer_key_table")
            )
          )
        ),
        tabPanel("Save",
          mainPanel(
            h4("Use the Generate Output Files button in the sidebar to save all versions.")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
    # Load custom functions
        # Source all R scripts in the R/ directory
    files <- list.files("R/02-functions", pattern = "\\.R$", full.names = TRUE)
    sapply(files, source)

    # Reactive values for quiz data
    quiz_data <- reactiveVal(NULL)

    # Load and parse the selected quiz file
    observeEvent(input$quiz_file, {
      req(input$quiz_file)
      parsed_data <- parse_d2l_xml(input$quiz_file)
    #   print(parsed_data)  # Debugging: Print the parsed data
      quiz_data(parsed_data)
    })
    
    # Update section filter choices when a new quiz file is loaded
    observeEvent(input$quiz_file, {
      req(input$quiz_file)
      quiz_data(parse_d2l_xml(input$quiz_file))
      
      # Extract section titles
      sections <- c("All", sapply(quiz_data(), function(section) section$section_title))
    #   print(sections)  # Debugging: Print the sections
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

      # debugging: Print the questions to display
    #   print(questions_to_display) 
      
      # Generate HTML for all questions
      questions_html <- generate_questions_html(questions_to_display)

        # debugging: Print the generated HTML
        # print(questions_html)
      
      # Render the HTML
      HTML(questions_html)
    })

    # Save all questions
    observeEvent(input$save_all, {
    req(quiz_data())
    format <- input$view_format
    save_all_questions(quiz_data(), format = format)
    })

   # Generate quiz versions and save output files
    observeEvent(input$generate_files, {
        req(quiz_data())
        
        # Define paths
        xml_folder <- "quizXMLs"
        output_folder <- "output/HTML_quizzes"
        template_file <- file.path("www", "templateQuiz.html")
        retake_template_file <- file.path("www", "retakeTemplate.html")
        
        # Ensure the output folder exists
        if (!dir.exists(output_folder)) {
            dir.create(output_folder, recursive = TRUE)
        }
        
        # Parameters for quiz generation
        num_versions <- input$num_versions
        num_letters <- input$num_letter_versions
        base_seed <- input$seed
        
        # Generate the first version of the quiz
        if (num_versions >= 1) {
            selected_questions <- select_questions(quiz_data(), seed = base_seed)
            
            for (num_letter in 1:num_letters) {
                letter <- LETTERS[num_letter]
                generate_quiz_html_div(
                    selected_questions,
                    template_file,
                    output_folder,
                    quiz_number = 1,
                    version_number = 1,
                    quiz_letter = letter,
                    seed = sample(1:1000, 1)
                )
            }
        }

    # Generate additional versions of the quiz
        if (num_versions < 2) return()  # Skip if only one version is requested
        for (version in 2:num_versions) {
            selected_questions <- select_questions(quiz_data(), seed = base_seed + version)
            
            for (num_letter in 1:num_letters) {
                letter <- LETTERS[num_letter]
                generate_quiz_html(
                    selected_questions,
                    retake_template_file,
                    output_folder,
                    quiz_number = 1,
                    version_number = version,
                    quiz_letter = letter,
                    seed = sample(1:1000, 1)
                )
            }
        }
        
        # Notify the user
        showNotification("Quiz files have been generated and saved to the output folder.", type = "message")
    })

    

    # Commented out: Render quiz in View Quiz tab
    # output$quiz_output <- renderUI({
    #   req(quiz_data())
    #   version <- input$quiz_version
    #   letter <- input$quiz_letter
    #   highlight <- input$highlight_answers
    #   
    #   quiz_html <- generate_quiz_html_div(
    #     quiz_data(),
    #     version = version,
    #     letter = letter,
    #     highlight_answers = highlight
    #   )
    #   
    #   HTML(quiz_html)
    # })
  
  # Render answer key table
#   output$answer_key_table <- renderTable({
#     req(quiz_data())
#     version <- input$quiz_version
#     letter <- input$quiz_letter
    
#     generate_answer_key(quiz_data(), version = version, letter = letter)
#   })
}

# Run the application
shinyApp(ui = ui, server = server)