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
  
  # Return the parsed data as a list of sections
  list(
    sections = parsed_sections
  )
}

# Recursive function to parse a section and its nested sections/items
parse_section_recursive <- function(section, ns) {
  section_id <- xml_attr(section, "ident")
  section_title <- xml_attr(section, "title")
  
  # Parse nested sections
  nested_sections <- xml_find_all(section, "./section")
  parsed_nested_sections <- lapply(nested_sections, function(nested_section) parse_section_recursive(nested_section, ns))
  
  # Parse items within the section using the existing parse_question function
  items <- xml_find_all(section, "./item")
  parsed_items <- lapply(items, function(item) parse_question(item, ns))
  
  # Combine nested sections and items into a single list
  questions <- c(parsed_items, unlist(lapply(parsed_nested_sections, function(ns) ns$questions), recursive = FALSE))
  
  # Return the parsed section data
  list(
    section_id = section_id,
    section_title = section_title,
    num_items = length(questions),
    questions = questions
  )
}