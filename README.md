# Quiz Shiny App

This repository contains a Shiny application designed to help users interact with quizzes. The application allows users to load quizzes from XML files, view questions, and manage quiz data effectively.

## Project Structure

The project is organized as follows:

```
quiz-shiny-app
├── R
│   ├── 02-functions
│   │   ├── parse-print-D2L-quiz.R  # Functions for parsing D2L quiz XML files and printing quiz content.
│   │   └── helper-functions.R        # Additional helper functions for data manipulation and formatting.
│   ├── quizXMLs
│   │   └── sample_quiz.xml          # Sample XML quiz file for input data.
│   └── shiny-server.R                # Sets up the Shiny server logic for the application.
├── www
│   └── styles.css                    # CSS styles for the application.
├── app.R                             # Main entry point for the Shiny application, integrating UI and server components.
└── README.md                         # Documentation for the project.
```

## Installation

To run the application, ensure you have R and the necessary packages installed. You can install the required packages using the following command:

```R
install.packages(c("shiny", "xml2", "dplyr"))
```

## Usage

1. Clone the repository to your local machine.
2. Navigate to the project directory.
3. Open the `app.R` file in RStudio or your preferred R environment.
4. Run the application using the command:

```R
shiny::runApp("app.R")
```

5. Follow the on-screen instructions to interact with the quizzes.

## Contributing

Contributions are welcome! If you have suggestions for improvements or new features, please open an issue or submit a pull request.

## License

This project is licensed under the MIT License. See the LICENSE file for details.