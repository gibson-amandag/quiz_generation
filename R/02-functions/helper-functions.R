# Function to randomly select questions from each section
select_questions <- function(sections, seed = 123, shuffleWithinSection = FALSE) {
  set.seed(seed)
  # print(sections)

  selected_questions <- lapply(sections, function(section) {
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
      sampled_questions = sampled_questions
    )
  })

  return(selected_questions)
}