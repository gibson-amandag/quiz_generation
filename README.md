# Quiz Shiny App

This repository contains a Shiny application designed to help users interact with quizzes. The application allows users to load quizzes from XML or Word files, shuffle questions, generate multiple versions, and export quizzes in various formats (HTML, Word, and CSV).

## Project Structure

The project is organized as follows:

```
quiz-shiny-app
├── R
│   ├── 02-functions
│   │   ├── 1. generate-D2L-quiz-html.R  # Functions for generating HTML quizzes.
│   │   ├── 1. generate-D2L-quiz-word.R  # Functions for generating Word quizzes.
│   │   ├── 2. read-word-quiz.R          # Functions for reading Word quiz files.
│   │   └── helper-functions.R           # Additional helper functions for data manipulation and formatting.
│   ├── quizXMLs
│   │   └── sample_quiz.xml              # Sample XML quiz file for input data.
│   └── shiny-server.R                   # Sets up the Shiny server logic for the application.
├── www
│   ├── basicTemplate.html               # Basic HTML template for quizzes.
│   └── styles.css                       # CSS styles for the application.
├── output
│   ├── HTML_quizzes                     # Folder for generated HTML quizzes.
│   ├── D2L_CSVs                         # Folder for generated CSV files for D2L import.
│   └── AnswerKeys                       # Folder for generated answer keys.
├── app.R                                # Main entry point for the Shiny application, integrating UI and server components.
├── README.md                            # Documentation for the project.
```

## Features

- **Quiz Import**: Load quizzes from XML or Word files.
- **Shuffle Questions**: Shuffle questions and answers to create multiple versions.
- **Export Options**: Export quizzes as HTML, Word documents, or CSV files for D2L Brightspace.
- **Answer Keys**: Automatically generate answer keys for each quiz version.
- **Customizable Templates**: Use predefined HTML and Word doc templates for quiz formatting.

## Installation

To run the application, ensure you have R and the necessary packages installed. You can install the required packages using the following command:

```R
install.packages(c("shiny", "xml2", "dplyr", "officer", "stringr"))
```

## Usage

1. Clone the repository to your local machine.
2. Navigate to the project directory.
3. Open the `app.R` file in RStudio or your preferred R environment.
4. Run the application using the command:

   ```R
   shiny::runApp("app.R")
   ```

5. Follow the on-screen instructions to:
   - Shuffle questions and generate multiple versions from a Word doc
   - Upload XML from D2L BrightSpace to view quizzes and generate paper versions
   - Export quizzes in your desired format (HTML or CSV)
   - Generate CSV files to upload to D2L from multiple-choice or short answers questions in Word

## License

This project is licensed under the MIT License. See the LICENSE file for details.