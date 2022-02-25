#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(wesanderson)

# Load data
data <- readxl::read_excel("data/funding_overview_all.xlsx")

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Wo fördert RAM die Wissenschaft?"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(sidebarPanel(
    checkboxGroupInput(
      # Gebt dieser Auswahl einen Namen (wir werden den Namen später
      # nutzen, um auf dieses Auswahlmenü zuzugreifen)
      "majorSelection",
      "Wähle ein Studienfach",
      # Der nächste Schritt ist wichtig - hier definieren wir, was der
      # Nutzer auswählen kann. Die Logik ist, dass wir eine Liste (`list`)
      # an Möglichkeiten in `choiches` speichern.
      choices = list(
        "CDSS" = "CDSS",
        "Erziehungswissenschaften" = "Erziehungswissenschaften",
        "Politikwissenschaften" = "Politikwissenschaften",
        "Psychologie" = "Psychologie",
        "Sozialwissenschaften" = "Sozialwissenschaften",
        "Soziologie" = "Soziologie"
      ),
      selected = "Sozialwissenschaften"
    )
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(plotOutput("barplot")))
)

# Define server logic
server <- function(input, output) {
  output$barplot <- renderPlot({
    data %>%
      dplyr::filter(major %in% input$majorSelection) %>%
      dplyr::filter(language == "de" & !is.na(major)) %>%
      dplyr::group_by(year, major) %>%
      dplyr::count() %>%
      ggplot2::ggplot(aes(x = year, y = n, fill = major)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual("Studienfach", values = wes_palette("IsleofDogs1")) +
      theme_classic() +
      ylab("Anzahl der Förderungen") +
      xlab("Jahr")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
