# Function to read a quiz from a Word document
read_quiz_from_word <- function(file_path) {
  # Read the Word document
  doc <- read_docx(file_path)

  # Extract text from the Word document
  text <- docx_summary(doc)

  # Convert the extracted text to a data frame
  text_df <- as.data.frame(text)

  # if no level, check style
  text_df <- text_df %>%
    mutate(
      level = ifelse(
        is.na(level),
        ifelse(style_name == "Level 1 list", 1, 
               ifelse(style_name == "Level 2 list", 2, NA)), 
        level
      )
    )

  # Replace apostrophes in the text column
  text_df$text <- gsub("’", "'", text_df$text)

  # Filter section headers (if any)
  section_headers <- text_df %>%
    filter(style_name == "heading 1")
  
  # If no section headers exist, create a default section
  if (nrow(section_headers) == 0) {
    section_headers <- data.frame(
      doc_index = 1,                # Default doc_index
      content_type = "paragraph",   # Mimic content_type
      style_name = "heading 1",     # Mimic style_name
      text = "Default Section",     # Default section name
      level = NA,                   # Default level
      num_id = NA,                  # Default num_id
      sectionNum = 1,               # Default section number
      stringsAsFactors = FALSE
    )
    
    # Assign all rows in text_df to the default section
    text_df$sectionNum <- 1
  } else {
    # Assign section numbers based on the order of headings
    section_headers <- section_headers %>%
      mutate(sectionNum = seq(1, nrow(.)))
  }

  # Filter question stems and add section numbers
  question_stems <- text_df %>%
    filter(level == 1) %>%
    select(-style_name) %>%
    rowwise() %>%
    mutate(sectionNum = find_section_num(section_headers, doc_index)) %>%
    ungroup() %>%
    mutate(questionNum = row_number())

  # Filter answer options and add section/question numbers
  answers <- text_df %>%
    filter(level == 2)

  # If no answers exist, create an empty data frame
  if (nrow(answers) == 0) {
    answers <- data.frame(
      style_name = character(),
      text = character(),
      sectionNum = integer(),
      questionNum = integer(),
      stringsAsFactors = FALSE
    )
  } else {
    answers <- answers %>%
      select(-style_name) %>%
      rowwise() %>%
      mutate(
        sectionNum = find_section_num(section_headers, doc_index),
        questionNum = find_question_num(question_stems, doc_index)
      ) %>%
      select(-doc_index, -content_type, -level, -num_id) %>%
      ungroup()
  }

  # Clean up section headers and question stems
  section_headers <- section_headers %>%
    select(-c(doc_index, content_type, style_name, level, num_id), everything())

  question_stems <- question_stems %>%
    select(-c(doc_index, content_type, level, num_id))

  # Initialize the quiz list
  quiz <- list()

  # Loop through each section
  for (i in 1:nrow(section_headers)) {
    section <- section_headers[i, ]

    # Create a list for the section
    section_list <- list(
      section_title = section$text,
      questions = list()
    )

    # Filter questions for the current section
    section_questions <- question_stems %>% filter(sectionNum == section$sectionNum)

    # if no rows in section_questions, go to next section
    if (nrow(section_questions) == 0) {
      section_list$questions <- list()
      section_list$num_items <- 0
      quiz[[i]] <- section_list
      next
    }

    # Loop through each question in the section
    for (j in 1:nrow(section_questions)) {
      question <- section_questions[j, ]

      # Create a list for the question
      question_list <- list(
        question_text = question$text,
        answers = list()
      )

      # Filter answer options for the current question
      question_answers <- answers %>% filter(sectionNum == question$sectionNum, questionNum == question$questionNum)

      # Append answer options to the question list
      question_list$answers <- question_answers$text

      # Determine the question type based on the presence of answer options
      if (nrow(question_answers) > 0) {
        question_list$question_type <- "Multiple Choice"
      } else {
        question_list$question_type <- "Long Answer"
      }

      # Append the question list to the section list
      section_list$questions[[j]] <- question_list
      # Append the number of questions to the section list
      section_list$num_items <- nrow(section_questions)
    }

    # Append the section list to the quiz list
    quiz[[i]] <- section_list
  }

  return(quiz)
}

# Helper function to find the largest doc_index less than a given number
find_section_num <- function(df, number) {
  filtered_df <- df %>% filter(doc_index < number)
  if (nrow(filtered_df) == 0) {
    return(1)
  } # Default to section 1 if no match
  max_doc_index_row <- filtered_df %>% filter(doc_index == max(doc_index))
  return(max_doc_index_row$sectionNum)
}

# Helper function to find the question number
find_question_num <- function(df, number) {
  filtered_df <- df %>% filter(doc_index < number)
  if (nrow(filtered_df) == 0) {
    return(NA)
  }
  max_doc_index_row <- filtered_df %>% filter(doc_index == max(doc_index))
  return(max_doc_index_row$questionNum)
}

# Function to write CSV content for each section of the quiz
write_quiz_csvs <- function(
  quiz
  # , quizNum, output_dir = "./R/quizzes/forD2L/"
) {
  # Initialize an empty list to store the CSV content for each section
  csv_content_list <- list()
  
  # Iterate through each section in the quiz list
  for (section_index in seq_along(quiz)) {
    section <- quiz[[section_index]]
    
    # Initialize an empty data frame for the section
    csv_content <- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(csv_content) <- c("A", "B", "C", "D", "E")
    
    # Iterate through each question in the section
    for (question in section$questions) {
      if (question$question_type == "Multiple Choice") {
        # Add the question type comment
        csv_content <- rbind(csv_content, data.frame(A = paste0("//", question$question_text), B = "", C = "", D = "", E = ""))
        
        # Add the NewQuestion row
        csv_content <- rbind(csv_content, data.frame(A = "NewQuestion", B = "MC", C = "", D = "", E = ""))
        
        # Add the QuestionText row
        csv_content <- rbind(csv_content, data.frame(A = "QuestionText", B = question$question_text, C = "", D = "", E = ""))
        
        # Add the options
        for (i in seq_along(question$answers)) {
          option_text <- question$answers[i]
          option_value <- ifelse(i == 1, 100, 0)  # First option is correct
          csv_content <- rbind(csv_content, data.frame(A = "Option", B = option_value, C = option_text, D = "", E = ""))
        }
      } else if (question$question_type == "Long Answer") {
        # Add the NewQuestion row
        csv_content <- rbind(csv_content, data.frame(A = "NewQuestion", B = "WR", C = "", D = "", E = ""))
        
        # Add the QuestionText row
        csv_content <- rbind(csv_content, data.frame(A = "QuestionText", B = question$question_text, C = "", D = "", E = ""))
        
        # Add the Points row (default to 1 point)
        csv_content <- rbind(csv_content, data.frame(A = "Points", B = "1", C = "", D = "", E = ""))
      }
    }
    
    # Append the section's CSV content to the list
    csv_content_list[[section_index]] <- csv_content
  }
  
  return(csv_content_list)
}

# Function to view the contents of the Word document as HTML
view_word_as_html <- function(file_path) {
  # Read the Word document
  doc <- read_docx(file_path)
  
  # Convert the Word document to HTML
  html_content <- to_html(doc)
  
  # Return the HTML content
  return(html_content)
}
