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
    # Initialize answer key table
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
      if ((section$section_title %in% c("Uncategorized", "Uncategorized Questions"))) {
        title <- " "
      } else {
        title <- section$section_title
      }
      questions_html <- paste0(
        questions_html,
        "<h3>", title, "</h3>"
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
      # Render the question HTML
      rendered_question <- render_question_html(
        question = question,
        question_number = question_number,
        dispFormat = dispFormat,
        showAnswers = showAnswers,
        shuffleAnswers = shuffleAnswers
      )

      questions_html <- paste0(questions_html, rendered_question$html)

      # Add the correct answer to the answer key
      correct_letter <- rendered_question$correct_letter
      if (!is.null(correct_letter) && length(correct_letter) > 0) {
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
    if (showSectionTitles) {
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

render_question_html <- function(question, question_number, dispFormat, showAnswers, shuffleAnswers) {
  # Clean up question text
  question_text <- question$question_text
  question_text <- sub("<p>", "", question_text) # Remove the first <p>
  question_text <- sub("</p>", "", question_text) # Remove the first </p>
  question$question_text <- question_text

  # Initialize the HTML for the question
  questionInfo <- render_internalQuestion_html(
    question = question,
    question_number = question_number,
    dispFormat = dispFormat,
    showAnswers = showAnswers,
    shuffleAnswers = shuffleAnswers
  )

  question_html <- questionInfo$html

  if(dispFormat == "list"){
    question_html <- paste0(
      "<li class='question-list'>", question_html, "</li>"
    )
  } else if(dispFormat == "div"){
    question_html <- paste0(
      "<div class='question-container'>",
      "<div class='question-blank'></div>", # Blank column
      "<div class='question-content'>",
      "<strong>", question_number, ".</strong> ", question_html,
      "</div></div>"
    )
  } else if(dispFormat == "table"){
    question_html <- paste0(
      "<tr>",
      if(showAnswers && !(question$question_type %in% c("Matching", "Ordering"))) paste0("<td class='correct-letter'>", questionInfo$correct_answer,"</td>") else "<td>&nbsp;</td>",
      "<td>",
      "<strong>", question_number, ".</strong> ", question_html,
      "</td></tr>"
    )
  }
  return(list(html = question_html, correct_letter = questionInfo$correct_answer))
}

render_internalQuestion_html <- function(question, question_number, dispFormat, showAnswers, shuffleAnswers) {
  # Initialize question_html to an empty string to avoid NULL issues
  question_html <- ""
  question_html <- paste0(
    question_html, question$question_text
  )

  # Determine the question type and render accordingly
  result <- switch(
    question$question_type,
    "Multiple Choice" = ,
    "True/False" = ,
    "Multi-Select" = {
      # Shuffle answers if the option is enabled
      answer_options <- if (shuffleAnswers) sample(question$answers) else question$answers

      # Determine the correct answer letters
      correct_letter <- LETTERS[which(answer_options %in% question$correct_answers)]

      # Answer text
      correct_answer_text <- paste(correct_letter, collapse = ", ")

      if (question$question_type == "Multi-Select") {
        question_html <- paste0(
          question_html, " <em>(Select all that apply)</em>"
        )
      }
      question_html <- paste0(
        question_html, "<ol type='A'>"
      )

      # Add the answer options
      for (answer in answer_options) {
        # Check if the answer is correct
        is_correct <- answer %in% question$correct_answers
        answer_class <- if (showAnswers && is_correct) "class='correct-answer'" else ""

        # Remove <p> and </p> tags from the answer
        answer <- sub("<p>", "", answer) # Remove the first <p>
        answer <- sub("</p>", "", answer) # Remove the first </p>

        question_html <- paste0(
          question_html,
          "<li ", answer_class, ">", answer, "</li>"
        )
      }
      question_html <- paste0(
        question_html,
        "</ol>"
      )
      list(html = question_html, correct_answer = correct_answer_text)
    },
    "Fill in the Blanks" = {
      # Extract the correct answers
      correct_answers <- question$correct_answers

      # Add the correct answers to the HTML
      correct_answer_text <- paste(correct_answers, collapse = ", ")
      
      list(html = question_html, correct_answer = correct_answer_text)
    },
    "Matching" = {
      # Extract the choices and prompts
      choices <- question$choices
      prompts <- question$prompts

      # Remove <p> and </p> tags from choices and prompts
      choices <- lapply(choices, function(choice) {
        choice$choice <- sub("<p>", "", choice$choice)
        choice$choice <- sub("</p>", "", choice$choice)
        choice
      })
      prompts <- lapply(prompts, function(prompt) {
        prompt$prompt <- sub("<p>", "", prompt$prompt)
        prompt$prompt <- sub("</p>", "", prompt$prompt)
        prompt
      })

      # Shuffle choices and prompts if shuffleAnswers is enabled
      if (shuffleAnswers) {
        choices <- sample(choices)
        prompts <- sample(prompts)
      }

      # Generate the HTML for the matching question
      question_html <- paste0(
        question_html,
        "<div style='display: flex; width: 100%;'>",
        "<div style='width: 50%; padding-right: 10px;'>"
      )

      # Add choices to the left column
      for (choice in choices) {
        correctAnswer <- LETTERS[which(sapply(prompts, function(prompt) prompt$prompt_id) == choice$correct_prompt_id)]
        if(showAnswers && !is.null(choice$correct_prompt)) {
          entryLine <- paste0("<u class='correct-letter'>", correctAnswer, "</u>")
        } else {
          entryLine <- "_____"
        }
        question_html <- paste0(
          question_html,
          "<div>", entryLine, " ", choice$choice,
          "</div>"
        )
      }

      question_html <- paste0(
        question_html,
         "</div>",
        "<div style='width: 50%; padding-left: 10px;'>"
      )

      # Add prompts to the right column, lettered A, B, C, etc.
      letters <- LETTERS[1:length(prompts)]
      for (i in seq_along(prompts)) {
        question_html <- paste0(
          question_html,
          "<div>", letters[i], ". ", prompts[[i]]$prompt, "</div>"
        )
      }

      question_html <- paste0(
        question_html,
        "</div>",
        "</div>"
      )

      # Generate the correct answer key (letters in the correct order)
      correct_order <- sapply(choices, function(choice) {
        matching_prompt <- Filter(function(prompt) prompt$prompt_id == choice$correct_prompt_id, prompts)
        if (length(matching_prompt) > 0) {
          letters[which(sapply(prompts, function(prompt) prompt$prompt_id) == matching_prompt[[1]]$prompt_id)]
        } else {
          NULL
        }
      })
      correct_answer_text <- paste(correct_order, collapse = ", ")

      list(html = question_html, correct_answer = correct_answer_text)
    },
    "Ordering" = {
      # Shuffle answers if the option is enabled
      answer_options <- if (shuffleAnswers) sample(question$answers) else question$answers

      # Find the numerical order of the shuffled answer options that matches the correct order
      correct_order_indices <- match(question$correct_order, answer_options)
      correct_answer_text <- paste(correct_order_indices, collapse = ",")

      # Add the answer options below the question text
      question_html <- paste0(
        question_html,
        "<div style='margin-left: 20px;'>"
      )
      for (answer in answer_options) {
        # get the correct order index
        correct_order_index <- which(question$correct_order == answer)
        answer <- sub("<p>", "", answer) # Remove the first <p>
        answer <- sub("</p>", "", answer) # Remove the first </p>

        if(showAnswers && !is.na(correct_order_index)) {
          entryLine <- paste0("<u class='correct-letter'>", correct_order_index, "</u>")
        } else {
          entryLine <- "_____"
        }

        question_html <- paste0(
          question_html,
          "<div>", entryLine, " ", answer, "</div>"
        )
      }
      question_html <- paste0(
        question_html,
        "</div>"
      )

      list(html = question_html, correct_answer = correct_answer_text)
    },
    "Short Answer" = {
      # Extract the correct answers
      correct_answers <- question$correct_answers

      # Add the correct answers to the HTML
      correct_answer_text <- paste(correct_answers, collapse = ", ")
      
      list(html = question_html, correct_answer = correct_answer_text)
    },
    "Arithmetic" = {
      # Extract the formula and variables
      formula <- question$formula
      variables <- question$variables

      # Add the formula and variables to the HTML
      question_html <- paste0(
        question_html, "<p>Formula: ", formula, "</p>"
      )
      for (var in variables) {
        question_html <- paste0(
          question_html,
          "<p>Variable: ", var$name, " (", var$minvalue, " to ", var$maxvalue, ")</p>"
        )
      }
      list(html = question_html, correct_answer = " ")
    },
    "Significant Figures" = {
      # Extract the formula and precision
      formula <- question$formula
      precision <- question$precision

      # Add the formula and precision to the HTML
      question_html <- paste0(
        question_html, "<p>Formula: ", formula, "</p>"
      )
      question_html <- paste0(
        question_html,
        "<p>Precision: ", precision, "</p>"
      )
      list(html = question_html, correct_answer = " ")
    },
    "Multi-Short Answer" = {
      # Extract the correct answers
      correct_answers <- question$correct_answers

      # Add the correct answers to the HTML
      correct_answer_text <- paste(correct_answers, collapse = ", ")
      
      list(html = question_html, correct_answer = correct_answer_text)
    },
    "Long Answer" = {
      # Long answer questions typically do not have a correct answer
      correct_answer_text <- " "

      question_html <- paste0(
        question_html,
        "<p>&nbsp;</p><p>&nbsp;</p><p>&nbsp;</p>"
      )
      
      list(html = question_html, correct_answer = correct_answer_text)
    },
    {
      # Handle unsupported question types
      question_html <- paste0(
        question_html,
        "<p>Unsupported question type</p>"
      )
      list(html = question_html, correct_answer = "Unsupported question type")
    }
  )

  # Return the result
  return(result)
}