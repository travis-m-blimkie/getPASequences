
# Load libraries and data -------------------------------------------------

library(shiny)
library(shinythemes)
library(tidyverse)

pa14Data <- readRDS("data/PA14_geneInfo_withAASeqs.Rds")



# Define the UI elements --------------------------------------------------

ui <- fluidPage(

    theme = shinytheme("flatly"),

    # Application title
    titlePanel("Retreive P. aeruginosa Sequences"),


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

        # TODO Add download information
        # downloadButton("")


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


    # Extract the genes to be mapped
    myGenes <- reactive({

        req(input$pastedInput)

        if (input$strainChoice == "PA14") {
            str_extract_all(input$pastedInput, pattern = "PA14_[0-9]{5}")
        }
    })


    # Convert to a data frame, and fix column name
    myGenesTable <- reactive({

        part1 <- data.frame(Genes = myGenes(), stringsAsFactors = FALSE)
        colnames(part1) <- "Locus_Tag"

        return(part1)
    })


    # Map the input genes
    filteredTable <- reactive({
        left_join(myGenesTable(), pa14Data, by = "Locus_Tag")
    })


    # Create table without sequence to facilitate display
    displayTable <- reactive({
        select(filteredTable(), -c(Amino_Acid_Sequence))
    })


    observeEvent(input$search, {

        output$displayTable <- renderDataTable({
            displayTable()
        }, options = list(searching = FALSE))

    })


    # TODO Add download information

    # Hidden display of user-input genes
    # output$usersGenes <- renderDataTable({
    #     myGenesTable()
    # }, options = list(searching = FALSE, pageLength = 10))


}


# Run the application
shinyApp(ui = ui, server = server)
