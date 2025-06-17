# Quiz Shiny App

This repository contains a Shiny application designed to help users interact with quizzes. The application allows users to load quizzes from XML or Word files, shuffle questions, generate multiple versions, and export quizzes in various formats (HTML, Word, and CSV).

You can clone the repo to make your own version and edit templates, or use the standing application here: [Quiz Generation App](https://0196c215-c833-2a0b-bdba-5bf91f7322eb.share.connect.posit.cloud/)

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


## Usage

### Web application

Use the application as it exists on Posit Connect Cloud: [https://0196c215-c833-2a0b-bdba-5bf91f7322eb.share.connect.posit.cloud/](https://0196c215-c833-2a0b-bdba-5bf91f7322eb.share.connect.posit.cloud/)

### Install, edit, and use a local version

To run the application, ensure you have R and the necessary packages installed. You can install the required packages using the following command:

```R
install.packages(c("shiny", "xml2", "dplyr", "officer", "stringr"))
```


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

This project is licensed under the MIT License.

# Quiz Tool Application

Welcome to the Quiz Tool! This application helps you manage, shuffle, and export quizzes for educational use. Below you'll find an overview of the main features and links to detailed instructions for each section.

## Main Features

- **Shuffle Word Doc**: Shuffle questions and answers in a Word document and export new versions.
- **D2L Export to Paper**: Convert D2L quiz XML files into printable formats and generate answer keys.
- **Word to D2L Import**: Convert Word quizzes into D2L-compatible CSV files for import.
- **Quiz Library**: Browse and export questions from a question database.

## How to Use

1. Launch the app in RStudio or your browser.
2. Use the navigation bar at the top to switch between features.
3. Follow the on-screen instructions or see the docs below for each section:
   - [Shuffle Word Doc](docs/shuffle_word_doc.md)
   - [D2L Export to Paper](docs/d2l_export_to_paper.md)
   - [Word to D2L Import](docs/word_to_d2l_import.md)
   - [Quiz Library](docs/quiz_library.md)

For troubleshooting or more help, see the documentation in the `docs/` folder.