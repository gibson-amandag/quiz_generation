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

  # Check if the file is a database file by looking for the <objectbank> element
  if (inherits(xml_find_first(xml, "//assessment"), "xml_missing")) {
    # Delegate parsing to parse_questiondb
    message("Couldn't find assessment. Parsing with parse_questiondb...")
    db_list <- parse_questiondb(xml_file)
    if (is.null(db_list)) {
      return(NULL)
    }
    return(db_list)
  }

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

  # Extract all direct children of the container section (both <item> and <section>)
  container_children <- xml_find_all(container_section, "./*", ns)

  # Initialize a list to store sections and uncategorized questions in order
  sections <- list()

  # Process each child in the order it appears
  for (child in container_children) {
    if (xml_name(child) == "item") {
      # If the child is an <item>, treat it as an uncategorized question
      question <- parse_question(child, ns)

      # Check if the last section in the list is uncategorized
      if (length(sections) > 0 && sections[[length(sections)]]$section_id == "UNCATEGORIZED") {
        # Add the question to the existing uncategorized section
        sections[[length(sections)]]$questions <- append(sections[[length(sections)]]$questions, list(question))
        sections[[length(sections)]]$num_items <- sections[[length(sections)]]$num_items + 1
      } else {
        # Create a new uncategorized section
        sections <- append(sections, list(list(
          section_id = "UNCATEGORIZED",
          section_title = "Uncategorized",
          num_items = 1,
          questions = list(question)
        )))
      }
    } else if (xml_name(child) == "section") {
      # If the child is a <section>, parse it as a section
      section <- parse_section(child, ns)
      sections <- append(sections, list(section))
    }
  }

  # Return the parsed data, including the quiz title
  list(
    title = quiz_title,
    sections = sections
  )
}

parse_section <- function(section, ns) {
  section_id <- xml_attr(section, "ident")
  section_title <- xml_attr(section, "title")
  num_items <- as.integer(xml_text(xml_find_first(section, ".//fieldentry[../fieldlabel='qmd_numberofitems']", ns)))

  if (is.na(num_items) || is.null(num_items)) {
    # Default to number of questions in the section
    num_items <- length(xml_find_all(section, ".//item", ns))
  }

  # Extract questions within the section
  items <- xml_find_all(section, ".//item", ns)
  questions <- lapply(items, function(item) {
    parse_question(item, ns)
  })

  # Return section data
  list(
    section_id = section_id,
    section_title = section_title,
    num_items = num_items,
    questions = questions
  )
}

parse_question <- function(item, ns) {
  question_id <- xml_attr(item, "ident")
  question_text <- xml_text(xml_find_first(item, ".//mattext", ns))

  # Extract the question type
  question_type <- xml_text(xml_find_first(item, ".//qti_metadatafield[fieldlabel='qmd_questiontype']/fieldentry"))

  # Call the appropriate parsing function based on the question type
  if (question_type == "Multiple Choice") {
    parse_multiple_choice_question(item, ns)
  } else if (question_type == "True/False") {
    parse_true_false_question(item, ns)
  } else if (question_type == "Fill in the Blanks") {
    parse_fill_in_the_blanks_question(item, ns)
  } else if (question_type == "Multi-Select") {
    parse_multi_select_question(item, ns)
  } else if (question_type == "Matching") {
    parse_matching_question(item, ns)
  } else if (question_type == "Ordering") {
    parse_ordering_question(item, ns)
  } else if (question_type == "Short Answer") {
    parse_short_answer_question(item, ns)
  } else if (question_type == "Arithmetic") {
    parse_arithmetic_question(item, ns)
  } else if (question_type == "Significant Figures") {
    parse_significant_figures_question(item, ns)
  } else if (question_type == "Multi-Short Answer") {
    parse_multi_short_answer_question(item, ns)
  } else if (question_type == "Long Answer") {
    parse_long_answer_question(item, ns)
  } else {
    warning(paste("Unsupported question type:", question_type))
    NULL
  }
}

parse_multiple_choice_question <- function(item, ns) {
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
    question_type = "Multiple Choice",
    answers = answer_texts,
    correct_answers = correct_answer_texts
  )
}

parse_matching_question <- function(item, ns) {
  question_id <- xml_attr(item, "ident")
  question_text <- xml_text(xml_find_first(item, ".//mattext", ns))

  # Extract the image (if present)
  matimage_node <- xml_find_first(item, ".//matimage", ns)
  image_uri <- if (!is.null(matimage_node)) xml_attr(matimage_node, "uri") else NULL

  # Extract prompts (right column)
  response_groups <- xml_find_all(item, ".//response_grp", ns)
  prompts <- lapply(response_groups, function(group) {
    prompt_text <- xml_text(xml_find_first(group, ".//mattext", ns))
    prompt_ident <- xml_attr(group, "respident")
    list(prompt = prompt_text, prompt_id = prompt_ident)
  })

  # Extract unique choices (left column)
  response_labels <- xml_find_all(item, ".//response_label", ns)
  choices <- unique(lapply(response_labels, function(label) {
    choice_text <- xml_text(xml_find_first(label, ".//mattext", ns))
    choice_ident <- xml_attr(label, "ident")
    list(choice = choice_text, choice_id = choice_ident)
  }))

  # Extract correct answers from <respcondition> where varname="D2L_Correct"
  respconditions <- xml_find_all(item, ".//respcondition", ns)
  correct_answers <- lapply(respconditions, function(respcondition) {
    setvar <- xml_attr(xml_find_first(respcondition, ".//setvar", ns), "varname")
    if (!is.null(setvar) && setvar == "D2L_Correct") {
      varequal <- xml_text(xml_find_first(respcondition, ".//varequal", ns))
      respident <- xml_attr(xml_find_first(respcondition, ".//varequal", ns), "respident")
      list(choice_id = varequal, prompt_id = respident)
    } else {
      NULL
    }
  })
  correct_answers <- Filter(Negate(is.null), correct_answers) # Remove NULL values

  # Associate correct answers with prompts
  choices <- lapply(choices, function(choice) {
    # Find the correct answer that matches this choice
    matching_answer <- Filter(function(answer) answer$choice_id == choice$choice_id, correct_answers)
    if (length(matching_answer) > 0) {
      # Find the corresponding prompt for the correct answer
      correct_prompt <- Filter(function(prompt) prompt$prompt_id == matching_answer[[1]]$prompt_id, prompts)
      if (length(correct_prompt) > 0) {
        choice$correct_prompt <- correct_prompt[[1]]$prompt
        choice$correct_prompt_id <- correct_prompt[[1]]$prompt_id
      } else {
        choice$correct_prompt <- NULL
        choice$correct_prompt_id <- NULL
      }
    } else {
      choice$correct_prompt <- NULL
      choice$correct_prompt_id <- NULL
    }

    choice
  })

  # Return the parsed data
  list(
    question_id = question_id,
    question_text = question_text,
    question_type = "Matching",
    prompts = prompts,
    choices = choices,
    image = image_uri # Include the image URI
  )
}

parse_long_answer_question <- function(item, ns) {
  question_id <- xml_attr(item, "ident")
  question_text <- xml_text(xml_find_first(item, ".//mattext", ns))

  # Extract answer key (if present)
  answer_key_node <- xml_find_first(item, ".//answer_key//mattext", ns)
  answer_key <- if (!is.null(answer_key_node)) xml_text(answer_key_node) else NA

  list(
    question_id = question_id,
    question_text = question_text,
    question_type = "Long Answer",
    answer_key = answer_key
  )
}

parse_true_false_question <- function(item, ns) {
  question_id <- xml_attr(item, "ident")
  question_text <- xml_text(xml_find_first(item, ".//mattext", ns))

  # Extract answer options (True/False)
  answers <- xml_find_all(item, ".//response_label", ns)
  answer_texts <- sapply(answers, function(answer) {
    xml_text(xml_find_first(answer, ".//mattext", ns))
  })

  # Extract the correct answer identifier from <respcondition>
  correct_answer_ident <- xml_text(xml_find_first(item, ".//respcondition/conditionvar/varequal", ns))

  # Match the correct answer identifier to its text
  correct_answer <- answer_texts[which(sapply(answers, function(answer) {
    xml_attr(answer, "ident")
  }) == correct_answer_ident)]

  list(
    question_id = question_id,
    question_text = question_text,
    question_type = "True/False",
    answers = answer_texts,
    correct_answers = correct_answer
  )
}

parse_fill_in_the_blanks_question <- function(item, ns) {
  question_id <- xml_attr(item, "ident")
  question_text <- xml_text(xml_find_first(item, ".//mattext", ns))

  # Extract correct answers
  correct_answers <- xml_find_all(item, ".//varequal", ns)
  correct_answers_text <- sapply(correct_answers, xml_text)

  list(
    question_id = question_id,
    question_text = question_text,
    question_type = "Fill in the Blanks",
    correct_answers = correct_answers_text
  )
}

parse_multi_select_question <- function(item, ns) {
  question_id <- xml_attr(item, "ident")
  question_text <- xml_text(xml_find_first(item, ".//mattext", ns))

  # Extract all answer options
  answers <- xml_find_all(item, ".//response_label", ns)
  answer_texts <- sapply(answers, function(answer) {
    xml_text(xml_find_first(answer, ".//mattext", ns))
  })
  answer_idents <- sapply(answers, function(answer) {
    xml_attr(answer, "ident")
  })

  # Determine grading type
  grading_type <- as.integer(xml_text(xml_find_first(item, ".//d2l_2p0:grading_type", ns)))

  # Initialize correct and incorrect answer identifiers
  correct_answer_idents <- c()
  incorrect_answer_idents <- c()

  # Extract <respcondition> blocks
  respconditions <- xml_find_all(item, ".//respcondition", ns)

  if (grading_type == 0) {
    # All-or-Nothing Grading (Reverted to original logic)
    correct_answers <- xml_find_all(item, ".//respcondition/conditionvar/varequal", ns)
    correct_answer_idents <- sapply(correct_answers, xml_text)
  } else if (grading_type == 1 || grading_type == 2 || grading_type == 3) {
    # Right-Minus-Wrong, Correct Selection, or Limited Selection Grading
    for (respcondition in respconditions) {
      setvar <- xml_attr(xml_find_first(respcondition, ".//setvar", ns), "varname")
      varequal <- xml_text(xml_find_first(respcondition, ".//varequal", ns))

      if (!is.null(setvar) && setvar == "D2L_Correct") {
        correct_answer_idents <- c(correct_answer_idents, varequal)
      } else if (!is.null(setvar) && setvar == "D2L_Incorrect") {
        incorrect_answer_idents <- c(incorrect_answer_idents, varequal)
      }
    }
  }

  # Deduplicate identifiers
  correct_answer_idents <- unique(correct_answer_idents)
  incorrect_answer_idents <- unique(incorrect_answer_idents)

  # Match correct answer identifiers to their texts
  correct_answer_texts <- answer_texts[answer_idents %in% correct_answer_idents]

  list(
    question_id = question_id,
    question_text = question_text,
    question_type = "Multi-Select",
    answers = answer_texts,
    correct_answers = correct_answer_texts,
    grading_type = grading_type
  )
}

parse_ordering_question <- function(item, ns) {
  question_id <- xml_attr(item, "ident")
  question_text <- xml_text(xml_find_first(item, ".//mattext", ns))

  # Extract ordering options
  answers <- xml_find_all(item, ".//response_label", ns)
  answer_texts <- sapply(answers, function(answer) {
    xml_text(xml_find_first(answer, ".//mattext", ns))
  })

  # Extract correct order
  correct_order <- xml_find_all(item, ".//response_label", ns)
  correct_order_idents <- sapply(correct_order, function(answer) {
    xml_attr(answer, "ident")
  })

  # Match the correct order to the answer texts
  correct_order_texts <- answer_texts[match(correct_order_idents, xml_attr(answers, "ident"))]

  list(
    question_id = question_id,
    question_text = question_text,
    question_type = "Ordering",
    answers = answer_texts,
    correct_order = correct_order_texts
  )
}

parse_short_answer_question <- function(item, ns) {
  question_id <- xml_attr(item, "ident")
  question_text <- xml_text(xml_find_first(item, ".//mattext", ns))

  # Extract correct answers
  correct_answers <- xml_find_all(item, ".//varequal", ns)
  correct_answers_text <- sapply(correct_answers, xml_text)

  list(
    question_id = question_id,
    question_text = question_text,
    question_type = "Short Answer",
    correct_answers = correct_answers_text
  )
}

parse_arithmetic_question <- function(item, ns) {
  question_id <- xml_attr(item, "ident")
  question_text <- xml_text(xml_find_first(item, ".//mattext", ns))

  # Extract formula and variables
  formula <- xml_text(xml_find_first(item, ".//formula", ns))
  variables <- xml_find_all(item, ".//variable", ns)
  variable_details <- lapply(variables, function(var) {
    list(
      name = xml_attr(var, "name"),
      minvalue = xml_text(xml_find_first(var, ".//minvalue", ns)),
      maxvalue = xml_text(xml_find_first(var, ".//maxvalue", ns))
    )
  })

  list(
    question_id = question_id,
    question_text = question_text,
    question_type = "Arithmetic",
    formula = formula,
    variables = variable_details
  )
}

parse_significant_figures_question <- function(item, ns) {
  question_id <- xml_attr(item, "ident")
  question_text <- xml_text(xml_find_first(item, ".//mattext", ns))

  # Extract formula and precision
  formula <- xml_text(xml_find_first(item, ".//formula", ns))
  precision <- xml_text(xml_find_first(item, ".//precision", ns))

  list(
    question_id = question_id,
    question_text = question_text,
    question_type = "Significant Figures",
    formula = formula,
    precision = precision
  )
}

parse_multi_short_answer_question <- function(item, ns) {
  question_id <- xml_attr(item, "ident")
  question_text <- xml_text(xml_find_first(item, ".//mattext", ns))

  # Extract correct answers
  correct_answers <- xml_find_all(item, ".//varequal", ns)
  correct_answers_text <- sapply(correct_answers, xml_text)

  list(
    question_id = question_id,
    question_text = question_text,
    question_type = "Multi-Short Answer",
    correct_answers = correct_answers_text
  )
}
