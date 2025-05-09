# Function to parse the XML file and extract sections, questions, and the quiz title
parse_d2l_xml <- function(xml_file) {
  # Check if the file exists and has contents
  if (!file.exists(xml_file)) {
    return(NULL)
  }

  file_info <- file.info(xml_file)
  if (file_info$size == 0) {
    return(NULL)
  }

  # Read the XML file
  xml <- read_xml(xml_file)

  # Extract the quiz title from the <assessment> tag
  quiz_title <- xml_attr(xml_find_first(xml, "//assessment"), "title")
  if (is.null(quiz_title) || is.na(quiz_title)) {
    quiz_title <- "Quiz" # Default title if none is found
  }

  # Define the namespace
  ns <- xml_ns(xml)

  # Extract the main container section
  container_section <- xml_find_first(xml, ".//section[@ident='CONTAINER_SECTION']", ns)
  if (is.null(container_section)) {
    stop("No container section found. Check the XML structure.")
  }

  # Extract nested sections within the container section
  nested_sections <- xml_find_all(container_section, ".//section", ns)
  if (length(nested_sections) == 0) {
    stop("No nested sections found within the container section.")
  }

  # Parse each nested section
  sections <- lapply(nested_sections, function(section) {
    section_id <- xml_attr(section, "ident")
    section_title <- xml_attr(section, "title")
    num_items <- as.integer(xml_text(xml_find_first(section, ".//fieldentry[../fieldlabel='qmd_numberofitems']", ns)))

    # Extract questions within the section
    items <- xml_find_all(section, ".//item", ns)
    questions <- lapply(items, function(item) {
      question_id <- xml_attr(item, "ident")
      question_text <- xml_text(xml_find_first(item, ".//mattext", ns))

      # Extract answer options
      answers <- xml_find_all(item, ".//response_label", ns)
      answer_texts <- sapply(answers, function(answer) {
        xml_text(xml_find_first(answer, ".//mattext", ns))
      })
      answer_idents <- sapply(answers, function(answer) {
        xml_attr(answer, "ident")
      })

      # Determine correct answers
      respconditions <- xml_find_all(item, ".//respcondition", ns)
      correct_answers <- sapply(respconditions, function(respcondition) {
        varequal <- xml_text(xml_find_first(respcondition, ".//varequal", ns))
        setvar <- xml_text(xml_find_first(respcondition, ".//setvar", ns))
        if (setvar == "100.000000000") {
          return(varequal)
        } else {
          return(NULL)
        }
      })
      correct_answers <- correct_answers[!is.na(correct_answers)] # Remove NULL values

      # Match correct answer idents to their text
      correct_answer_texts <- answer_texts[answer_idents %in% correct_answers]

      list(
        question_id = question_id,
        question_text = question_text,
        answers = answer_texts,
        correct_answers = correct_answer_texts
      )
    })

    # Return section data
    list(
      section_id = section_id,
      section_title = section_title,
      num_items = num_items,
      questions = questions
    )
  })

  # Return the parsed data, including the quiz title
  list(
    title = quiz_title,
    sections = sections
  )
}


# Function to randomly select questions from each section
select_questions <- function(sections, seed = 123) {
  set.seed(seed)
  selected_questions <- lapply(sections, function(section) {
    # Extract the number of questions to select
    num_items <- section$num_items

    # Extract all questions from the section
    questions <- section$questions

    # Randomly sample the required number of questions
    if (length(questions) >= num_items) {
      sampled_questions <- sample(questions, num_items)
    } else {
      warning(paste(
        "Not enough questions in section:", section$section_id,
        "Requested:", num_items, "Available:", length(questions)
      ))
      sampled_questions <- questions # Return all questions if not enough available
    }

    # Return the sampled questions along with section metadata
    list(
      section_id = section$section_id,
      section_title = section$section_title,
      sampled_questions = sampled_questions
    )
  })

  return(selected_questions)
}

generate_quiz_html_div <- function(selected_questions, template_file, output_folder, quiz_number, version_number, quiz_letter, seed = 123) {
  set.seed(seed)
  # Read the template
  template <- readLines(template_file, warn = FALSE)

  # Replace the quiz title placeholder
  quiz_title <- paste("Quiz", quiz_number, paste0("V", version_number, quiz_letter))
  template <- gsub("\\{\\{quiz_title\\}\\}", quiz_title, template)

  # Generate the questions and answers HTML
  answer_key_html <- "<h2>Answer Key</h2><ol>"
  excel_key <- c() # Initialize a vector to store correct answers for Excel
  question_number <- 1

  questions_html <- paste0(
    "<div class='question-container'>",
    "<div class='question-blank'>Answer</div>", # Blank column
    "<div class='question-content'>Question</div></div>"
  )

  for (section in selected_questions) {
    for (question in section$sampled_questions) {
      # Clean up question text: Remove the first <p> and </p> if they exist
      question_text <- question$question_text
      question_text <- sub("<p>", "", question_text) # Remove the first <p>
      question_text <- sub("</p>", "", question_text) # Remove the first </p>

      # Start the question container
      questions_html <- paste0(
        questions_html,
        "<div class='question-container'>",
        "<div class='question-blank'></div>", # Blank column
        "<div class='question-content'>",
        "<strong>", question_number, ".</strong> ", question_text,
        "<ol type='A'>"
      )

      # Shuffle and add answers
      shuffled_answers <- sample(question$answers)
      for (answer in shuffled_answers) {
        questions_html <- paste0(questions_html, "<li>", answer, "</li>")
      }

      # Add the correct answer to the answer key
      correct_answer <- question$correct_answers[1] # Use the first correct answer from the parsed data
      correct_letter <- LETTERS[which(shuffled_answers == correct_answer)]
      answer_key_html <- paste0(answer_key_html, "<li>", correct_letter, "</li>")
      excel_key <- c(excel_key, correct_letter) # Append the correct letter to the Excel key

      # Close the question content and container
      questions_html <- paste0(questions_html, "</ol></div></div>")

      question_number <- question_number + 1
    }
  }
  answer_key_html <- paste0(answer_key_html, "</ol>")

  # Replace the questions placeholder
  template <- gsub("\\{\\{questions\\}\\}", questions_html, template)

  output_file <- file.path(output_folder, paste0("Quiz", quiz_number, "_V", version_number, quiz_letter, ".html"))
  answer_key_file <- file.path(output_folder, paste0("Quiz", quiz_number, "_V", version_number, quiz_letter, "_ans.html"))
  excel_key_file <- file.path(output_folder, paste0("Quiz", quiz_number, "_V", version_number, quiz_letter, "_KEY.html"))
  # Write the final HTML to the output file
  writeLines(template, output_file)
  message("Quiz has been saved to ", output_file)

  # Write the answer key to a separate file
  writeLines(answer_key_html, answer_key_file)
  message("Answer key has been saved to ", answer_key_file)

  # Write the Excel-friendly key to a plain text file
  writeLines(paste(excel_key, collapse = "\t"), excel_key_file)
  message("Excel-friendly answer key has been saved to ", excel_key_file)
}

# Function to generate the quiz HTML using a template
generate_quiz_html <- function(selected_questions, template_file, output_folder, quiz_number, version_number, quiz_letter, seed = 123) {
  set.seed(seed)
  # Read the template
  template <- readLines(template_file, warn = FALSE)

  # Replace the quiz title placeholder
  quiz_title <- paste("Quiz", quiz_number)
  template <- gsub("\\{\\{quiz_title\\}\\}", quiz_title, template)

  # Generate the questions and answers HTML
  questions_html <- ""
  answer_key_html <- "<h2>Answer Key</h2><ol>"
  excel_key <- c() # Initialize a vector to store correct answers for Excel
  question_number <- 1

  # writeLines(template, )
  for (section in selected_questions) {
    for (question in section$sampled_questions) {
      # Question stem
      # Clean up question text if it starts with <p> and ends with </p>
      question_text <- question$question_text
      # print(question_text)
      if (grepl("<p>.*</p>", question_text)) {
        question_text <- sub("<p>", "", question_text) # Remove the opening <p>
        question_text <- sub("</p>", "", question_text) # Remove the closing </p>
      }
      questions_html <- paste0(
        questions_html,
        "<li>", question_text, "<ol type='A'>"
      )

      # Shuffle and add answers
      shuffled_answers <- sample(question$answers)
      for (answer in shuffled_answers) {
        questions_html <- paste0(questions_html, "<li>", answer, "</li>")
      }

      # Add the correct answer to the answer key
      correct_answer <- question$correct_answers[1] # Use the first correct answer from the parsed data
      correct_letter <- LETTERS[which(shuffled_answers == correct_answer)]
      answer_key_html <- paste0(answer_key_html, "<li>", correct_letter, "</li>")
      excel_key <- c(excel_key, correct_letter) # Append the correct letter to the Excel key

      questions_html <- paste0(questions_html, "</ol></li>")

      question_number <- question_number + 1
    }
  }

  # Replace the questions placeholder
  template <- gsub("\\{\\{questions\\}\\}", paste0("<ol>", questions_html, "</ol>"), template)

  output_file <- file.path(output_folder, paste0("Quiz", quiz_number, "_V", version_number, quiz_letter, ".html"))
  answer_key_file <- file.path(output_folder, paste0("Quiz", quiz_number, "_V", version_number, quiz_letter, "_ans.html"))
  excel_key_file <- file.path(output_folder, paste0("Quiz", quiz_number, "_V", version_number, quiz_letter, "_KEY.html"))
  # Write the final HTML to the output file
  writeLines(template, output_file)
  message("Quiz has been saved to ", output_file)

  # Write the answer key to a separate file
  writeLines(answer_key_html, answer_key_file)
  message("Answer key has been saved to ", answer_key_file)

  # Write the Excel-friendly key to a plain text file
  writeLines(paste(excel_key, collapse = "\t"), excel_key_file)
  message("Excel-friendly answer key has been saved to ", excel_key_file)
}



generate_questions_html <- function(sections, dispFormat = "list", showAnswers = FALSE, shuffleAnswers = TRUE, thisSeed = 123, showSectionTitles = TRUE, sampledQuestions = FALSE) {
  set.seed(thisSeed)
  # Initialize HTML for questions
  questions_html <- ""


  if (!showSectionTitles) {
    question_number <- 1
    # initalize answer key table
    answer_key_df <- data.frame(
      questionNum = integer(),
      correctAnswer = character(),
      stringsAsFactors = FALSE
    )
  } else {
    answer_key_df <- data.frame(
      questionNum = integer(),
      correctAnswer = character(),
      section = character(),
      stringsAsFactors = FALSE
    )
    sectionNum <- 1
  }

  firstSection <- TRUE

  for (section in sections) {
    # Add section title
    if (showSectionTitles) {
      questions_html <- paste0(
        questions_html,
        "<h3>", section$section_title, "</h3>"
      )
      question_number <- 1
    }

    if (dispFormat == "list" && (showSectionTitles || firstSection == TRUE)) {
      # List format
      questions_html <- paste0(questions_html, "<ol>")
    } else if (dispFormat == "table" && (showSectionTitles || firstSection == TRUE)) {
      # Start the table for the section
      questions_html <- paste0(
        questions_html,
        "<table class='question-table' style='width: 7in; border-collapse: collapse;'>",
        "<tr>",
        "<td style='width: 0.7in;'>Answer</th>",
        "<td>Question</th></tr>"
      )
    }

    if (sampledQuestions) {
      sectionQuestions <- section$sampled_questions
    } else {
      sectionQuestions <- section$questions
    }

    for (question in sectionQuestions) {
      # Clean up question text
      question_text <- question$question_text
      question_text <- sub("<p>", "", question_text) # Remove the first <p>
      question_text <- sub("</p>", "", question_text) # Remove the first </p>

      if (dispFormat == "list") {
        # Start the question block for list format
        questions_html <- paste0(
          questions_html,
          "<li>", question_text, "<ol type='A'>"
        )
      } else if (dispFormat == "div") {
        # Start the question container for div format
        questions_html <- paste0(
          questions_html,
          "<div class='question-container'>",
          "<div class='question-blank'></div>", # Blank column
          "<div class='question-content'>",
          "<strong>", question_number, ".</strong> ", question_text,
          "<ol type='A'>"
        )
      } else if (dispFormat == "table") {
        # Add a row to the table for the question
        questions_html <- paste0(
          questions_html,
          "<tr>",
          "<td>&nbsp;</td>",
          "<td>",
          "<strong>", question_number, ".</strong> ", question_text,
          "<ol type='A'>"
        )
      }

      # Shuffle answers if the option is enabled
      answer_options <- if (shuffleAnswers) sample(question$answers) else question$answers

      correct_letter <- LETTERS[which(answer_options %in% question$correct_answers)]
      if (dispFormat == "table" && showAnswers && any(answer_options %in% question$correct_answers)) {
        questions_html <- sub("(.*)<td>&nbsp;</td>(.*)$", paste0("\\1<td class='correct-letter'>", correct_letter, "</td>\\2"), questions_html)
      }

      if (showSectionTitles) {
        # Add the section number to the answer key
        answer_key_df <- rbind(
          answer_key_df,
          data.frame(
            questionNum = question_number,
            correctAnswer = correct_letter,
            section = sectionNum,
            stringsAsFactors = FALSE
          )
        )
      } else {
        # Add the question number to the answer key
        answer_key_df <- rbind(
          answer_key_df,
          data.frame(
            questionNum = question_number,
            correctAnswer = correct_letter,
            stringsAsFactors = FALSE
          )
        )
      }
      
      for (answer in answer_options) {
        # Check if the answer is correct
        is_correct <- answer %in% question$correct_answers
        answer_class <- if (showAnswers && is_correct) "class='correct-answer'" else ""



        questions_html <- paste0(
          questions_html,
          "<li ", answer_class, ">", answer, "</li>"
        )
      }

      if (dispFormat == "list") {
        # Close the question block for list format
        questions_html <- paste0(questions_html, "</ol></li>")
      } else if (dispFormat == "div") {
        # Close the question container for div format
        questions_html <- paste0(questions_html, "</ol></div></div>")
      } else if (dispFormat == "table") {
        # Close the table row for the question
        questions_html <- paste0(questions_html, "</ol></td></tr>")
      }

      question_number <- question_number + 1
    }

    if (dispFormat == "list" && showSectionTitles) {
      # Close the section block for list format
      questions_html <- paste0(questions_html, "</ol>")
    } else if (dispFormat == "table" && showSectionTitles) {
      # Close the table for the section
      questions_html <- paste0(questions_html, "</table>")
    }
    firstSection <- FALSE
    if(showSectionTitles){
      sectionNum <- sectionNum + 1
    }
  }

  if (dispFormat == "table" && !showSectionTitles) {
    # Close the table for the section
    questions_html <- paste0(questions_html, "</table>")
  } else if (dispFormat == "list" && !showSectionTitles) {
    # Close the section block for list format
    questions_html <- paste0(questions_html, "</ol>")
  }

  return(list(
    questions = questions_html,
    answers = answer_key_df
  ))
}
