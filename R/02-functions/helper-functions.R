# Flatten quiz section nodes for renderers that still accept a section list.
remove_biorender_attribution <- function(text) {
  text <- gsub(
    "(?is)<p\\b[^>]*>\\s*(?:<em\\b[^>]*>\\s*)?(?:Image|Figure)\\s+created\\s+with\\s+BioRender(?:\\.com)?\\s*(?:</em>)?\\s*</p>",
    "",
    text,
    perl = TRUE
  )
  text <- gsub(
    "(?is)(?:<em\\b[^>]*>\\s*)?(?:Image|Figure)\\s+created\\s+with\\s+BioRender(?:\\.com)?\\s*(?:</em>)?",
    "",
    text,
    perl = TRUE
  )
  text
}

flatten_quiz_sections <- function(sections, parent_path = character(), parent_key = character()) {
  flattened <- list()

  for (section_index in seq_along(sections)) {
    section <- sections[[section_index]]
    section_path <- c(parent_path, section$section_title)
    section_key <- c(parent_key, paste0(section_index, ":", section$section_id))
    has_nested_sections <- !is.null(section$nested_sections) && length(section$nested_sections) > 0
    question_segment <- list()
    segment_number <- 0L

    if (has_nested_sections && isTRUE(section$display_section_name)) {
      title_section <- section
      title_section$section_title <- paste(section_path, collapse = " / ")
      title_section$section_key <- paste(c(section_key, "title"), collapse = "/")
      title_section$section_group_key <- paste(section_key, collapse = "/")
      title_section$questions <- list()
      title_section$nested_sections <- NULL
      title_section$contents <- NULL
      flattened <- append(flattened, list(title_section))
    }

    append_question_segment <- function() {
      if (length(question_segment) == 0) {
        return(invisible(NULL))
      }

      segment_number <<- segment_number + 1L
      flat_section <- section
      flat_section$section_title <- paste(section_path, collapse = " / ")
      flat_section$section_key <- paste(c(section_key, paste0("segment:", segment_number)), collapse = "/")
      flat_section$section_group_key <- paste(section_key, collapse = "/")
      flat_section$questions <- question_segment
      if (has_nested_sections) {
        flat_section$display_section_name <- FALSE
      }
      if (!isTRUE(section$is_pool)) {
        flat_section$num_items <- length(question_segment)
      }
      flat_section$nested_sections <- NULL
      flat_section$contents <- NULL
      flattened <<- append(flattened, list(flat_section))
      question_segment <<- list()
      invisible(NULL)
    }

    contents <- section$contents
    if (is.null(contents)) {
      contents <- lapply(seq_along(section$questions), function(index) {
        list(type = "question", index = index)
      })
      contents <- c(contents, lapply(seq_along(section$nested_sections), function(index) {
        list(type = "nested_section", index = index)
      }))
    }

    for (content in contents) {
      if (content$type == "question") {
        question_segment <- append(question_segment, list(section$questions[[content$index]]))
      } else if (content$type == "nested_section") {
        append_question_segment()
        nested <- section$nested_sections[[content$index]]
        flattened <- c(flattened, flatten_quiz_sections(list(nested), section_path, section_key))
      }
    }

    append_question_segment()

    if (length(section$questions) == 0 && !isTRUE(section$is_pool) && !has_nested_sections) {
      flat_section <- section
      flat_section$section_title <- paste(section_path, collapse = " / ")
      flat_section$section_key <- paste(c(section_key, "segment:1"), collapse = "/")
      flat_section$section_group_key <- paste(section_key, collapse = "/")
      flat_section$nested_sections <- NULL
      flat_section$contents <- NULL
      flattened <- append(flattened, list(flat_section))
    }
  }

  flattened
}

flatten_quiz_sections_for_view <- function(sections, depth = 1L, parent_key = character(), parent_path = character()) {
  flattened <- list()

  for (section_index in seq_along(sections)) {
    section <- sections[[section_index]]
    section_key <- c(parent_key, paste0(section_index, ":", section$section_id))
    section_key <- paste(section_key, collapse = "/")
    section_path <- c(parent_path, section$section_title)
    section_path_label <- paste(section_path, collapse = " / ")

    title_entry <- section
    title_entry$section_key <- section_key
    title_entry$section_node_key <- section_key
    title_entry$section_path_label <- section_path_label
    title_entry$section_title <- section$section_title
    title_entry$questions <- list()
    title_entry$sampled_questions <- list()
    title_entry$force_section_title <- TRUE
    title_entry$heading_level <- min(depth + 1L, 6L)
    title_entry$nested_sections <- NULL
    title_entry$contents <- NULL
    flattened <- append(flattened, list(title_entry))

    question_segment <- list()
    segment_number <- 0L
    append_question_segment <- function() {
      if (length(question_segment) == 0) {
        return(invisible(NULL))
      }

      segment_number <<- segment_number + 1L
      question_entry <- section
      question_entry$section_key <- paste0(section_key, "/segment:", segment_number)
      question_entry$section_node_key <- section_key
      question_entry$section_path_label <- section_path_label
      question_entry$section_title <- section$section_title
      question_entry$questions <- question_segment
      question_entry$sampled_questions <- list()
      question_entry$force_section_title <- FALSE
      question_entry$nested_sections <- NULL
      question_entry$contents <- NULL
      flattened <<- append(flattened, list(question_entry))
      question_segment <<- list()
      invisible(NULL)
    }

    contents <- section$contents
    if (is.null(contents)) {
      contents <- lapply(seq_along(section$questions), function(index) {
        list(type = "question", index = index)
      })
      contents <- c(contents, lapply(seq_along(section$nested_sections), function(index) {
        list(type = "nested_section", index = index)
      }))
    }

    for (content in contents) {
      if (content$type == "question") {
        question_segment <- append(question_segment, list(section$questions[[content$index]]))
      } else if (content$type == "nested_section") {
        append_question_segment()
        nested_section <- section$nested_sections[[content$index]]
        flattened <- c(
          flattened,
          flatten_quiz_sections_for_view(list(nested_section), depth + 1L, section_key, section_path)
        )
      }
    }

    append_question_segment()
  }

  flattened
}

# Function to randomly select questions from each section
select_questions <- function(sections, seed = 123, shuffleWithinSection = FALSE, shuffleSections = FALSE) {
  set.seed(seed)

  flattened_sections <- flatten_quiz_sections(sections)
  has_hierarchy <- any(vapply(sections, function(section) {
    !is.null(section$nested_sections) && length(section$nested_sections) > 0
  }, logical(1)))
  if (has_hierarchy) {
    flattened_sections <- Filter(function(section) {
      isTRUE(section$is_pool) || isTRUE(section$display_section_name) || length(section$questions) > 0
    }, flattened_sections)
  }

  pool_indices <- which(vapply(flattened_sections, function(section) isTRUE(section$is_pool), logical(1)))
  pool_groups <- split(
    pool_indices,
    vapply(flattened_sections[pool_indices], function(section) section$section_group_key, character(1))
  )
  pool_groups <- Filter(function(indices) length(indices) > 1, pool_groups)
  for (indices in pool_groups) {
    pool_questions <- unlist(lapply(flattened_sections[indices], function(section) section$questions), recursive = FALSE)
    flattened_sections[[indices[[1]]]]$questions <- pool_questions
    for (index in indices[-1]) {
      flattened_sections[[index]]$questions <- list()
      flattened_sections[[index]]$num_items <- 0L
      flattened_sections[[index]]$is_pool <- FALSE
    }
  }

  selected_questions <- lapply(flattened_sections, function(section) {
    # Extract the number of questions to select
    num_items <- if (!is.null(section$num_items)) {
      section$num_items
    } else {
      length(section$questions)
    }

    # Extract all questions from the section
    questions <- section$questions

    # Randomly sample the required number of questions
    if (length(questions) > num_items || (shuffleWithinSection && length(questions) == num_items)) {
      sampled_questions <- sample(questions, num_items)
    } else if (length(questions) == num_items) {
      sampled_questions <- questions # Keep the original order if the count matches
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
      display_section_name = isTRUE(section$display_section_name),
      sampled_questions = sampled_questions
    )
  })

  if(shuffleSections) {
    selected_questions <- sample(selected_questions)
  }

  selected_questions <- Filter(function(section) {
    length(section$sampled_questions) > 0 || isTRUE(section$display_section_name)
  }, selected_questions)

  return(selected_questions)
}