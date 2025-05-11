library(officer)
library(stringr)

generate_quiz_wordDoc <- function(selected_questions, shuffleLetter, quizTitle, versionNum, totalQs = 20, seed = 123, shuffleAnswers = FALSE, includeMCinfo = FALSE) {
  # Set the seed for reproducibility
  set.seed(seed)

  # Read the base Word document template
  base_doc <- read_docx(paste0("./WordDocs/Quiz_style_template_", shuffleLetter, ".docx"))
  
  # Helper function to remove all paragraphs from the document
  remove_all_paragraphs <- function(doc) {
    while (length(docx_summary(doc)$content) > 0) {
      doc <- body_remove(doc)
    }
    return(doc)
  }
  
  # Remove all paragraphs from base_doc
  base_doc <- remove_all_paragraphs(base_doc)
  
  # Add title: "Quiz # (Version #)"
  title <- paste0(quizTitle, " (Version ", versionNum, shuffleLetter, ")")
  base_doc <- body_add_par(base_doc, value = title, style = "Title")
  
  if(includeMCinfo){
    base_doc <- body_add_fpar(
      base_doc
      , fpar(
        ftext(
          "For each of the following multiple-choice questions, please choose the BEST answer and write your answer for each question in the first column. "
          )
        , ftext(
          "If you change your answer, make your final answer clear. If there are two letters in the column, I will grade the first"
          , prop = italic_text
        )
      )
      , style = "Normal"
    )
    
    # Add the italic text to the document
    base_doc <- body_add_fpar(
      base_doc
      , fpar(ftext(
        paste0("To earn a “success” on this assessment, you must correctly answer ", ceiling(totalQs*.8), " of ", totalQs, " questions")
        , prop = italic_bold_text)
        )
        , style = "Normal"
      )
  }

  for (section in selected_questions) {
    for (question in section$sampled_questions) {
      if (question$question_type %in% c("Multiple Choice", "True/False", "Multi-Select")) {
        base_doc <- handle_mc_tf_multi(question, base_doc, shuffleAnswers, italic_text, bold_text)
      } else if (question$question_type == "Fill in the Blanks") {
        base_doc <- handle_fill_in_the_blanks(question, base_doc, italic_text)
      } else if (question$question_type == "Matching") {
        base_doc <- handle_matching(question, base_doc, shuffleAnswers, italic_text)
      } else if (question$question_type == "Ordering") {
        base_doc <- handle_ordering(question, base_doc, shuffleAnswers, italic_text)
      } else if (question$question_type == "Short Answer") {
        base_doc <- handle_short_answer(question, base_doc, italic_text)
      } else if (question$question_type == "Arithmetic") {
        base_doc <- handle_arithmetic(question, base_doc, italic_text)
      } else if (question$question_type == "Significant Figures") {
        base_doc <- handle_significant_figures(question, base_doc, italic_text)
      } else if (question$question_type == "Multi-Short Answer") {
        base_doc <- handle_multi_short_answer(question, base_doc, italic_text)
      } else if (question$question_type == "Long Answer") {
        base_doc <- handle_long_answer(question, base_doc, italic_text)
      }
    }
  }

  return(base_doc)
}

# Create a text formatting property for italics
italic_text <- fp_text_lite(italic = TRUE)
bold_text <- fp_text_lite(bold = TRUE)
italic_bold_text <- fp_text_lite(italic = TRUE, bold = TRUE)

# Helper function to clean HTML tags
clean_html_tags <- function(text) {
  text <- str_replace_all(text, "<p>", "\n")
  text <- str_replace_all(text, "</p>", "")
  text <- str_replace_all(text, "<em>(.*?)</em>", "{italics}\\1{italics}") # Italics
  text <- str_replace_all(text, "<strong>(.*?)</strong>", "**\\1**") # Bold
  text <- str_replace_all(text, "<.*?>", "") # Remove other HTML tags
  text <- str_replace_all(text, "&#39;", "'")
  text <- str_replace_all(text, "&amp;", "&") # Replace &amp; with &
  text <- str_replace_all(text, "&lt;", "<") # Replace &lt; with <
  text <- str_replace_all(text, "&gt;", ">") # Replace &gt; with >
  text <- str_replace_all(text, "&#237;", "í") # Replace &#237; with í
  text <- str_replace_all(text, "&#233;", "é") # Replace &#233; with é
  text <- str_replace_all(text, "&#243;", "ó") # Replace &#243; with ó
  text <- str_replace_all(text, "&#250;", "ú") # Replace &#250; with ú
  text <- str_replace_all(text, "&#191;", "¿") # Replace &#191; with ¿
  text <- str_replace_all(text, "&#161;", "¡") # Replace &#161; with ¡
  text <- str_replace_all(text, "&#8211;", "-") # Replace &#8211; with -
  return(text)
}

# Helper function to parse and format text with fp_text
format_text_with_fp <- function(text) {
  # Match all text and formatting tags
  matches <- str_match_all(text, "(\\{italics\\}.*?\\{italics\\}|\\*\\*.*?\\*\\*|[^\\{\\}\\*]+)")[[1]][, 1]
  
  # Initialize a list to hold ftext objects
  ftext_list <- list()
  
  # Iterate through matches and apply formatting
  for (i in seq_along(matches)) {
    match <- matches[i]
    if (str_detect(match, "^\\{italics\\}.*\\{italics\\}$")) {
      # Remove the {italics} tags and apply italic formatting
      clean_text <- str_remove_all(match, "\\{italics\\}")
      if (str_trim(clean_text) != "") {
        ftext_list[[i]] <- ftext(clean_text, prop = italic_text)
      }
    } else if (str_detect(match, "^\\*\\*.*\\*\\*$")) {
      # Remove the ** tags and apply bold formatting
      clean_text <- str_remove_all(match, "\\*\\*")
      if (str_trim(clean_text) != "") {
        ftext_list[[i]] <- ftext(clean_text, prop = bold_text)
      }
    } else {
      # Add plain text
      if (match != "" && match != "NULL") {
        ftext_list[[i]] <- ftext(match)
      }
    }
  }
  
  # Combine all ftext objects into a single fpar
  formatted_fpar <- do.call(fpar, ftext_list)
  
  return(formatted_fpar)
}

# Function to handle Multiple Choice, True/False, and Multi-Select questions
handle_mc_tf_multi <- function(question, base_doc, shuffleAnswers, italic_text, bold_text) {
  # Shuffle answers if enabled
  answer_options <- if (shuffleAnswers && question$question_type != "True/False") sample(question$answers) else question$answers
  
  # Add question text
  question_text <- clean_html_tags(question$question_text)
  question_fpar <- format_text_with_fp(question_text)
  base_doc <- body_add_fpar(base_doc, question_fpar, style = "Level 1 list")
  
  # Add answer options
  for (i in seq_along(answer_options)) {
    answer_text <- clean_html_tags(answer_options[i])
    answer_fpar <- format_text_with_fp(answer_text)
    base_doc <- body_add_fpar(base_doc, answer_fpar, style = "Level 2 list")
  }
  
  return(base_doc)
}

# Function to handle Fill in the Blanks questions
handle_fill_in_the_blanks <- function(question, base_doc, italic_text) {
  # Add question text
  question_text <- clean_html_tags(question$question_text)
  question_fpar <- format_text_with_fp(question_text)
  base_doc <- body_add_fpar(base_doc, question_fpar, style = "Level 1 list")

  base_doc <- body_add_par(base_doc, value = "", style = "Normal")

  
  return(base_doc)
}

# Function to handle Matching questions
handle_matching <- function(question, base_doc, shuffleAnswers, italic_text) {
  # Shuffle choices and prompts if enabled
  choices <- if (shuffleAnswers) sample(question$choices) else question$choices
  prompts <- if (shuffleAnswers) sample(question$prompts) else question$prompts
  
  # Add question text
  question_text <- clean_html_tags(question$question_text)
  question_fpar <- format_text_with_fp(question_text)
  base_doc <- body_add_fpar(base_doc, question_fpar, style = "Level 1 list")

  # create a table by row binding
  max_length <- max(length(choices), length(prompts))

  # make a data frame
  table_data <- data.frame(
    Choices = character(max_length),
    Prompts = character(max_length),
    stringsAsFactors = FALSE
  )

  for(i in seq_len(max_length)) {
    choice_text <- if (i <= length(choices)) clean_html_tags(choices[[i]]$choice) else ""
    prompt_text <- if (i <= length(prompts)) clean_html_tags(prompts[[i]]$prompt) else ""

    # fill the data frame
    if(choice_text == "") {
      choice_text <- ""
    } else {
      if (str_starts(choice_text, "\n")) {
        choice_text <- str_replace(choice_text, "^\n", "\n_____")
      } else {
        choice_text <- paste0("\n_____", choice_text)
      }
    }
    
    if(prompt_text == "") {
      prompt_text <- ""
    } else {
      if (str_starts(prompt_text, "\n")) {
        prompt_text <- str_replace(prompt_text, "^\n", paste0("\n", LETTERS[i], ". "))
      } else {
        prompt_text <- paste0("\n", LETTERS[i], ". ", prompt_text)
      }
    }

    table_data[i, ] <- c(choice_text, prompt_text)
  }
  
  # Add the table to the Word document
  base_doc <- body_add_table(base_doc, value = table_data, header = FALSE)
  
  return(base_doc)
}

# Function to handle Ordering questions
handle_ordering <- function(question, base_doc, shuffleAnswers, italic_text) {
  # Shuffle answers if enabled
  answer_options <- if (shuffleAnswers) sample(question$answers) else question$answers
  
  # Add question text
  question_text <- clean_html_tags(question$question_text)
  question_fpar <- format_text_with_fp(question_text)
  base_doc <- body_add_fpar(base_doc, question_fpar, style = "Level 1 list")
  
  # Add answer options
  for (answer in answer_options) {
    answer_text <- clean_html_tags(answer)

    if(answer_text == "") {
      answer_text <- ""
    } else {
      if (str_starts(answer_text, "\n")) {
        answer_text <- str_replace(answer_text, "^\n", "\n_____")
      } else {
        answer_text <- paste0("\n_____", answer_text)
      }
    }
    base_doc <- body_add_par(base_doc, value = answer_text, style = "Normal")
  }
  
  return(base_doc)
}

# Function to handle Short Answer questions
handle_short_answer <- function(question, base_doc, italic_text) {
  # Add question text
  question_text <- clean_html_tags(question$question_text)
  question_fpar <- format_text_with_fp(question_text)
  base_doc <- body_add_fpar(base_doc, question_fpar, style = "Level 1 list")

  base_doc <- body_add_par(base_doc, value = "", style = "Normal")
  
  return(base_doc)
}

# Function to handle Arithmetic questions
handle_arithmetic <- function(question, base_doc, italic_text) {
  # Add question text
  question_text <- clean_html_tags(question$question_text)
  question_fpar <- format_text_with_fp(question_text)
  base_doc <- body_add_fpar(base_doc, question_fpar, style = "Level 1 list")
  
  # Add formula and variables
  formula <- question$formula
  base_doc <- body_add_par(base_doc, value = paste0("Formula: ", formula), style = "Normal")
  for (var in question$variables) {
    base_doc <- body_add_par(
      base_doc,
      value = paste0("Variable: ", var$name, " (", var$minvalue, " to ", var$maxvalue, ")"),
      style = "Normal"
    )
  }
  
  return(base_doc)
}

# Function to handle Significant Figures questions
handle_significant_figures <- function(question, base_doc, italic_text) {
  # Add question text
  question_text <- clean_html_tags(question$question_text)
  question_fpar <- format_text_with_fp(question_text)
  base_doc <- body_add_fpar(base_doc, question_fpar, style = "Level 1 list")
  
  # Add formula and precision
  formula <- question$formula
  precision <- question$precision
  base_doc <- body_add_par(base_doc, value = paste0("Formula: ", formula), style = "Normal")
  base_doc <- body_add_par(base_doc, value = paste0("Precision: ", precision), style = "Normal")
  
  return(base_doc)
}

# Function to handle Multi-Short Answer questions
handle_multi_short_answer <- function(question, base_doc, italic_text) {
  # Add question text
  question_text <- clean_html_tags(question$question_text)
  question_fpar <- format_text_with_fp(question_text)
  base_doc <- body_add_fpar(base_doc, question_fpar, style = "Level 1 list")

  base_doc <- body_add_par(base_doc, value = "", style = "Normal")
  
  return(base_doc)
}

# Function to handle Long Answer questions
handle_long_answer <- function(question, base_doc, italic_text) {
  # Add question text
  question_text <- clean_html_tags(question$question_text)
  question_fpar <- format_text_with_fp(question_text)
  base_doc <- body_add_fpar(base_doc, question_fpar, style = "Level 1 list")
  
  # Add space for a long answer
  base_doc <- body_add_par(base_doc, value = "", style = "Normal")
  base_doc <- body_add_par(base_doc, value = "", style = "Normal")
  base_doc <- body_add_par(base_doc, value = "", style = "Normal")
  
  return(base_doc)
}