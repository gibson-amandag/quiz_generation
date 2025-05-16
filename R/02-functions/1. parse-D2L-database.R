# Main function to parse the question database
parse_questiondb <- function(xml_file) {
  # Check if the file exists and has contents
  if (!file.exists(xml_file)) {
    stop("File does not exist.")
  }
  
  file_info <- file.info(xml_file)
  if (file_info$size == 0) {
    stop("File is empty.")
  }
  
  # Read the XML file
  xml <- read_xml(xml_file)
  
  # Extract the root <objectbank> element
  objectbank <- xml_find_first(xml, "//objectbank")
  if (is.null(objectbank)) {
    stop("No <objectbank> element found in the XML.")
  }
  
  # Define the namespace (if applicable)
  ns <- xml_ns(xml)
  
  # Parse all top-level sections
  sections <- xml_find_all(objectbank, "./section")
  parsed_sections <- lapply(sections, function(section) parse_section_recursive(section, ns))

  # Identify unsectioned questions
  unsectioned_items <- xml_find_all(objectbank, "./item")
  unsectioned_questions <- lapply(unsectioned_items, function(item) parse_question(item, ns))

  # Add unsectioned questions as a separate "Uncategorized" section
  if (length(unsectioned_questions) > 0) {
    uncategorized_section <- list(
      section_id = "UNCATEGORIZED",
      section_title = "Uncategorized",
      questions = unsectioned_questions,
      nested_sections = list()
    )
    parsed_sections <- append(parsed_sections, list(uncategorized_section))
  }

  # Return the parsed data as a list of sections
  list(
    sections = parsed_sections
  )
}

# Recursive function to parse sections and their nested subsections
parse_section_recursive <- function(section, ns) {
  section_id <- xml_attr(section, "ident")
  section_title <- xml_attr(section, "title")

  # Parse nested sections
  nested_sections <- xml_find_all(section, "./section")
  parsed_nested_sections <- lapply(nested_sections, function(nested_section) parse_section_recursive(nested_section, ns))

  # Parse items within the section
  items <- xml_find_all(section, "./item")
  parsed_items <- lapply(items, function(item) parse_question(item, ns))

  # Return the parsed section data
  list(
    section_id = section_id,
    section_title = section_title,
    questions = parsed_items,
    nested_sections = parsed_nested_sections
  )
}

flatten_sections <- function(section) {
  # Initialize an empty list to store flattened sections
  flattened <- list()
  
  # Include the current section only if it has questions
  if (!is.null(section$questions) && length(section$questions) > 0) {
    flattened <- list(
      list(
        section_title = section$section_title,
        questions = section$questions
      )
    )
  }
  
  # Recursively add nested sections
  if (!is.null(section$nested_sections) && length(section$nested_sections) > 0) {
    for (nested_section in section$nested_sections) {
      flattened <- c(flattened, flatten_sections(nested_section))
    }
  }
  
  return(flattened)
}