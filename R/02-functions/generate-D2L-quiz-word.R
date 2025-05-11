library(officer)
library(stringr)

generate_quiz_wordDoc <- function(selected_questions, shuffleLetter, quizTitle, versionNum, totalQs = 20) {
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

  # Create a text formatting property for italics
  italic_text <- fp_text_lite(italic = TRUE)
  bold_text <- fp_text_lite(bold = TRUE)
  italic_bold_text <- fp_text_lite(italic = TRUE, bold = TRUE)
  
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
  
  # Helper function to clean HTML tags
  clean_html_tags <- function(text) {
    text <- str_replace_all(text, "<p>", "\n")
    text <- str_replace_all(text, "</p>", "")
    text <- str_replace_all(text, "<em>(.*?)</em>", "{italics}\\1{italics}") # Italics
    text <- str_replace_all(text, "<strong>(.*?)</strong>", "**\\1**") # Bold
    text <- str_replace_all(text, "<.*?>", "") # Remove other HTML tags
    text <- str_replace_all(text, "&#39;", "'")
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

  for (section in selected_questions) {
    for (question in section$sampled_questions) {
      # Clean and format question text
      question_text <- clean_html_tags(question$question_text)
      question_fpar <- format_text_with_fp(question_text)
  
      # Add question text
      base_doc <- body_add_fpar(base_doc, question_fpar, style = "Level 1 list")
  
      # Add answer options
      if (!is.null(question$answers)) {
        for (i in seq_along(question$answers)) {
          answer_text <- clean_html_tags(question$answers[i])
          answer_fpar <- format_text_with_fp(answer_text)
  
          base_doc <- body_add_fpar(base_doc, answer_fpar, style = "Level 2 list")
        }
      }
    }
  }

  return(base_doc)
}