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
    feedback_meta_df <- data.frame(
      questionNum = integer(),
      questionType = character(),
      numOptions = integer(),
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
    feedback_meta_df <- data.frame(
      questionNum = integer(),
      questionType = character(),
      numOptions = integer(),
      correctAnswer = character(),
      stringsAsFactors = FALSE,
      section = character()
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

      correct_letter_clean <- gsub("<br/>", "\n", correct_letter)
      correct_letter_clean <- gsub("&#160;", " ", correct_letter_clean)
      correct_letter_clean <- gsub("&#39;", "'", correct_letter_clean)

      # Clean up correct_letter: remove HTML tags
      correct_letter_clean <- gsub("<[^>]+>", "", correct_letter_clean)
    
      if (!is.null(correct_letter_clean) && length(correct_letter_clean) > 0) {
        if (showSectionTitles) {
          answer_key_df <- rbind(
            answer_key_df,
            data.frame(
              questionNum = question_number,
              correctAnswer = correct_letter_clean,
              section = sectionNum,
              stringsAsFactors = FALSE
            )
          )
          if (question$question_type %in% c("Multiple Choice", "True/False", "Multi-Select")) {
            num_options <- length(question$answers)
            feedback_meta_df <- rbind(
              feedback_meta_df,
              data.frame(
                questionNum = question_number,
                questionType = question$question_type,
                numOptions = num_options,
                correctAnswer = correct_letter_clean,
                stringsAsFactors = FALSE,
                section = sectionNum
              )
            )
          }
        } else {
          answer_key_df <- rbind(
            answer_key_df,
            data.frame(
              questionNum = question_number,
              correctAnswer = correct_letter_clean,
              stringsAsFactors = FALSE
            )
          )
          if (question$question_type %in% c("Multiple Choice", "True/False", "Multi-Select")) {
            num_options <- length(question$answers)
            feedback_meta_df <- rbind(
              feedback_meta_df,
              data.frame(
                questionNum = question_number,
                questionType = question$question_type,
                numOptions = num_options,
                correctAnswer = correct_letter_clean,
                stringsAsFactors = FALSE
              )
            )
          }
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
    answers = answer_key_df,
    feedback_meta = feedback_meta_df
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

  if (dispFormat == "list") {
    question_html <- paste0(
      "<li class='question-list'>", question_html, "</li>"
    )
  } else if (dispFormat == "div") {
    question_html <- paste0(
      "<div class='question-container'>",
      "<div class='question-blank'></div>", # Blank column
      "<div class='question-content'>",
      "<strong>", question_number, ".</strong> ", question_html,
      "</div></div>"
    )
  } else if (dispFormat == "table") {
    question_html <- paste0(
      "<tr>",
      if (showAnswers && !(question$question_type %in% c("Matching", "Ordering", "Long Answer"))) paste0("<td class='correct-letter'>", questionInfo$correct_answer, "</td>") else "<td>&nbsp;</td>",
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
  result <- switch(question$question_type,
    "Multiple Choice" = ,
    "True/False" = ,
    "Multi-Select" = {
      if (shuffleAnswers) {
        if (question$question_type == "True/False") {
          shuffleAnswers <- FALSE
        }
        # Check if any answer includes "A and B," "B and C," or "A and C"
        contains_combined_answers <- any(grepl("\\b(A and B|B and C|A and C)\\b", question$answers, ignore.case = TRUE))

        if (!contains_combined_answers) {
          contains_combined_answers <- any(grepl("\\b(A\\) and B\\)|B\\) and C\\)|A\\) and C\\))\\b", question$answers, ignore.case = TRUE))
        }

        # check if options are letters in order
        # Check if the answers are sequential letters (A, B, C, D, ...)
        is_sequential_letters <- all(
          question$answers %in% LETTERS[1:length(question$answers)]
        )

        # Disable shuffling if the answers are sequential letters
        if (is_sequential_letters) {
          shuffleAnswers <- FALSE
        }

        # Disable shuffling if such answers exist
        if (contains_combined_answers) {
          shuffleAnswers <- FALSE
        }
      }

      # Shuffle answers if the option is enabled
      answer_options <- if (shuffleAnswers) sample(question$answers) else question$answers

      # Ensure "All of the above" or "all of the above" is at the end
      if (any(tolower(answer_options) == "all of the above")) {
        answer_options <- c(
          setdiff(answer_options, answer_options[tolower(answer_options) == "all of the above"]),
          answer_options[tolower(answer_options) == "all of the above"]
        )
      }
      
      # Ensure "None of the above" or "none of the above" is at the end
      if (any(tolower(answer_options) == "none of the above")) {
        answer_options <- c(
          setdiff(answer_options, answer_options[tolower(answer_options) == "none of the above"]),
          answer_options[tolower(answer_options) == "none of the above"]
        )
      }

      # Determine the correct answer letters
      correct_letter <- LETTERS[which(answer_options %in% question$correct_answers)]

      # Answer text
      correct_answer_text <- paste(correct_letter, collapse = ", ")

      if (question$question_type == "Multi-Select") {
        # check if already includes "Select all that apply"
        if (!grepl("Select all that apply", question_html) && !grepl("\\(Select all\\)", question_html)) {
          # Add "Select all that apply" to the question text
          question_html <- paste0(
            question_html, " <em>(Select all that apply)</em>"
          )
        }
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
      # Extract the choices, prompts, and image
      choices <- question$choices
      prompts <- question$prompts
      image <- question$image

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

      is_sequential_letters <- all(
        sapply(prompts, function(prompt) prompt$prompt) %in% LETTERS[1:length(prompts)]
      )

      # Shuffle choices and prompts if shuffleAnswers is enabled
      if (shuffleAnswers) {
        # check if a, b, c, d, sequentially
        if (!is_sequential_letters) {
          prompts <- sample(prompts)
        }
        choices <- sample(choices)
      }

      # Generate the HTML for the matching question
      question_html <- paste0(
        question_html,
        "<div style='display: flex; flex-direction: column; width: 100%;'>"
      )

      # Add the image if it exists
      if (!is.null(image) && !is.na(image) && image != "") {
        question_html <- paste0(
          question_html,
          "<div style='text-align: center; margin-bottom: 10px;'>",
          "<img src='", image, "' alt='Matching Question Image' style='max-height: 400px;'>",
          "</div>"
        )
      }

      question_html <- paste0(
        question_html,
        "<div style='display: flex; width: 100%;'>",
        "<div style='width: 50%; padding-right: 10px;'>"
      )

      # Add choices to the left column
      for (choice in choices) {
        correctAnswer <- LETTERS[which(sapply(prompts, function(prompt) prompt$prompt_id) == choice$correct_prompt_id)]
        if (showAnswers && !is.null(choice$correct_prompt)) {
          entryLine <- paste0("<u class='correct-letter'>", correctAnswer, "</u>")
        } else {
          entryLine <- "_____"
        }
        question_html <- paste0(
          question_html,
          "<div style='margin-top: 10px;'>", entryLine, " ", choice$choice,
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
        if (!is_sequential_letters) {
          question_html <- paste0(
            question_html,
            "<div>", letters[i], ". ", prompts[[i]]$prompt, "</div>"
          )
        } else {
          question_html <- paste0(
            question_html,
            "<div>", prompts[[i]]$prompt, "</div>"
          )
        }
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

        if (showAnswers && !is.na(correct_order_index)) {
          entryLine <- paste0("<u class='correct-letter'>", correct_order_index, "</u>")
        } else {
          entryLine <- "_____"
        }

        question_html <- paste0(
          question_html,
          "<div style='margin-top: 10px;'>", entryLine, " ", answer, "</div>"
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
      # Use the answer key if present, otherwise blank
      answer_key <- question$answer_key
      if (!is.null(answer_key) && !is.na(answer_key) && nzchar(answer_key)) {
        correct_answer_text <- answer_key
        # Clean up correct answer text
        correct_answer_text <- sub("<p>", "", correct_answer_text)
        correct_answer_text <- sub("</p>", "", correct_answer_text)
      } else {
        correct_answer_text <- " "
      }

      # If showing answers, display the answer key below the question
      if (showAnswers && nzchar(correct_answer_text) && correct_answer_text != " ") {
        question_html <- paste0(
          question_html,
          "<div class='correct-answer'>",
          correct_answer_text,
          "</div>"
        )
      } else {
        question_html <- paste0(
          question_html,
          "<p>&nbsp;</p><p>&nbsp;</p><p>&nbsp;</p>"
        )
      }

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

generate_styled_html <- function(version_html, css_file, template_file, quiz_title, version = NULL, letter, add_biorender_note = TRUE) {
  # Read the CSS content
  css_content <- readLines(css_file)

  # Replace {{version}} with the letter
  if (is.null(letter)) {
    css_content <- gsub("\\{\\{version\\}\\}", "", css_content)
  } else {
    css_content <- gsub("\\{\\{version\\}\\}", letter, css_content)
  }

  # Optionally remove the biorender note
  if (!add_biorender_note) {
    css_content <- gsub("Figures created with biorender.com", "", css_content)
  }

  # Read the template file
  intro_content <- readLines(template_file)

  # Extract the body content from the template
  body_start <- grep("<body>", intro_content) + 1
  body_end <- grep("</body>", intro_content) - 1
  body_content <- intro_content[body_start:body_end]

  # Replace {{quiz_title}} with the actual quiz title
  if (!is.null(version)) {
    print_title <- paste0(quiz_title, " - ", version, letter)
  } else if (!is.null(letter)) {
    print_title <- paste0(quiz_title, " - ", letter)
  } else {
    print_title <- quiz_title
  }
  body_content <- gsub("\\{\\{quiz_title\\}\\}", print_title, body_content)

  # Combine the CSS content, body content, and footer with the HTML content
  styled_html <- paste0(
    "<!DOCTYPE html><html><head><style>",
    paste(css_content, collapse = "\n"),
    "</style></head><body>",
    paste(body_content, collapse = "\n"),
    version_html,
    "</body></html>"
  )

  return(styled_html)
}


generate_feedback_table <- function(feedback) {
  # Find the max number of options across all questions
  max_options <- max(feedback$numOptions, na.rm = TRUE)

  # Prepare the table
  table_rows <- lapply(seq_len(nrow(feedback)), function(i) {
    correct_letters <- unlist(strsplit(feedback$correctAnswer[i], ",\\s*"))
    row <- rep("", max_options)
    for (j in seq_len(max_options)) {
      letter <- LETTERS[j]
      if (j <= feedback$numOptions[i]) {
        if (letter %in% correct_letters) {
          row[j] <- "*"
        }
      } else {
        row[j] <- "extra"
      }
    }
    c(feedback$questionNum[i], row)
  })

  # Build the final data frame
  col_names <- c("Q#", LETTERS[1:max_options])
  table_df <- as.data.frame(do.call(rbind, table_rows), stringsAsFactors = FALSE)
  colnames(table_df) <- col_names

  table_df
}

generate_feedback_table_html <- function(feedback) {
  table_df <- generate_feedback_table(feedback)
  max_options <- ncol(table_df) - 1
  col_letters <- colnames(table_df)[-1]
  
  html <- "<table class='feedback-table'>"
  # Header
  html <- paste0(html, "<tr><th class='feedback-qnum-header'>Q#</th>")
  for (letter in col_letters) {
    html <- paste0(html, "<th class='feedback-option-header'>", letter, "</th>")
  }
  html <- paste0(html, "</tr>")
  # Rows
  for (i in seq_len(nrow(table_df))) {
    html <- paste0(html, "<tr>")
    html <- paste0(html, "<td class='feedback-qnum'>", table_df[i, 1], "</td>")
    for (j in 2:ncol(table_df)) {
      cell_content <- table_df[i, j]
      cell_class <- "feedback-cell"
      box_class <- "feedback-box"
      # If cell_content is "*", randomize its position
      if (cell_content == "*") {
        n_spaces <- sample(0:3, 1) # Adjust range for more/less randomness
        if (runif(1) < 0.5) {
          cell_content <- paste0(strrep("&nbsp;", n_spaces), "*")
        } else {
          cell_content <- paste0("*", strrep("&nbsp;", n_spaces))
        }
      }
      if (cell_content == "extra") {
        cell_content <- ""
        box_class <- paste(box_class, "feedback-box-extra")
      }
      html <- paste0(html, "<td class='", cell_class, "'><div class='", box_class, "'>", cell_content, "</div></td>")
    }
    html <- paste0(html, "</tr>")
  }
  html <- paste0(html, "</table>")
  return(html)
}

generate_styled_html_cards <- function(
  cards_list,         # named list: version_key -> card HTML content
  quiz_title,         # character, quiz title
  css_file = "www/styles_cardPrint.css", # path to CSS file
  card_width = "3.5in",
  card_height = "3in",
  includeVersions = c("V1_LC", "V1_LD", "V1_LE", "V1_LF", "V1_LG")
) {
  # Read CSS
  css_content <- readLines(css_file, warn = FALSE)

  # Start HTML
  html <- paste0(
    "<!DOCTYPE html><html><head><meta charset='UTF-8'><title>", quiz_title, "</title>",
    "<style>",
    paste(css_content, collapse = "\n"),
    "
    .card-container {
      min-width: ", card_width, ";
      min-height: ", card_height, ";
      display: inline-block;
      vertical-align: top;
      margin: 0.05in;
      padding: 0.05in;
      box-sizing: border-box;
      border: 1px solid #ccc;
      background: #fff;
      overflow: hidden;
    }
    ",
    "</style></head><body>",
    "<div class='cards-wrapper'>"
  )
  
  # Add each card
  for (version_key in names(cards_list)) {
    version <- gsub("_L.*", "", version_key)
    versionNum <- sub("^V", "", version)
    if(version_key %in% includeVersions){
      letter <- gsub(".*_L", "", version_key)

      html <- paste0(
        html,
        "<div class='card-container'>",
        "<div class='card-header'>", quiz_title, 
        " ", version, letter, " Team # _____", "</div>",
        cards_list[[version_key]],
        "</div>"
      )
    }
  }
  
  # Close wrapper and HTML
  html <- paste0(html, "</div></body></html>")
  return(html)
}