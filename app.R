
# TODO Add download information in ui and server sections


# Load libraries and data -------------------------------------------------

library(shiny)
library(shinythemes)
library(tidyverse)

pao1Data <- readRDS("data/Pseudomonas_aeruginosa_PAO1_107.Rds")
pa14Data <- readRDS("data/Pseudomonas_aeruginosa_UCBPP-PA14_109.Rds")
lesb58Data <- readRDS("data/Pseudomonas_aeruginosa_LESB58_125.Rds")



# Define the UI elements --------------------------------------------------

ui <- fluidPage(

    theme = shinytheme("flatly"),

    # Application title
    titlePanel(div(HTML("Retreive <em>P. aeruginosa</em> Sequences"))),

    tags$br(),

    sidebarLayout(

        sidebarPanel(

            # Dropdown to pick strain
            selectInput(
                "strainChoice",
                "Please select your strain:",
                c("PAO1" = "PAO1",
                  "PA14" = "PA14",
                  "LESB58" = "LESB58")
                ),

            # Place to paste your genes of interest
            textAreaInput("pastedInput",
                          "Paste your list of locus tags, one per line:",
                          height = "300px"),

            # Button which triggers results to display
            actionButton("search", "Search")
        ),

        mainPanel(

            h3("Output:"),
            dataTableOutput("displayTable")

            # Hidden display for user-input genes
            # tags$br(),
            # tags$br(),
            # tags$br(),
            #
            # h3("Input Genes:"),
            # dataTableOutput("usersGenes")
        )
    )
)




# Define the server logic -------------------------------------------------

server <- function(input, output) {


# Extract the genes to be mapped, dependent on chosen strain
    myGenes <- reactive({

        req(input$pastedInput)

        if (input$strainChoice == "PAO1") {
            str_extract_all(input$pastedInput, pattern = "PA[0-9]{4}")

        } else if (input$strainChoice == "PA14") {
            str_extract_all(input$pastedInput, pattern = "PA14_[0-9]{5}")

        } else if (input$strainChoice == "LESB58") {
            str_extract_all(input$pastedInput, pattern = "PALES_[0-9]{5}")

        } else {
            return(NULL)
        }
    })


    # Convert to a data frame, and fix column name
    myGenesTable <- reactive({

        req(myGenes())

        part1 <- data.frame(Genes = myGenes(), stringsAsFactors = FALSE)
        colnames(part1) <- "Locus_Tag"

        return(part1)
    })


    # Map the input genes, dependent on strain
    filteredTable <- reactive({

        if (input$strainChoice == "PAO1") {
            inner_join(myGenesTable(), pao1Data, by = "Locus_Tag")

        } else if (input$strainChoice == "PA14") {
            inner_join(myGenesTable(), pa14Data, by = "Locus_Tag")

        } else if (input$strainChoice == "LESB58") {
            inner_join(myGenesTable(), lesb58Data,  by = "Locus_Tag")

        } else {
            return(NULL)
        }
    })


    # Create table without sequence to facilitate display
    displayTable <- reactive({

        select(filteredTable(), -c(Nucleotide_Sequence, Amino_Acid_Sequence)) %>%
            arrange(Locus_Tag)
    })


    # Wait until the "Search" button is clicked before rendering output
    observeEvent(

        input$search, {

        output$displayTable <- renderDataTable({
            isolate(displayTable())
        }, options = list(searching = FALSE))
    })
}




# Run the app -------------------------------------------------------------

shinyApp(ui, server)
