
# TODO Add message if strain and IDs don't match (i.e. 0 rows in table)


# Load libraries and data -------------------------------------------------

library(shiny)
library(shinythemes)
library(shinyjs)
library(seqinr)
library(tidyverse)

pao1Data <- readRDS("data/Pseudomonas_aeruginosa_PAO1_107.Rds")
pa14Data <- readRDS("data/Pseudomonas_aeruginosa_UCBPP-PA14_109.Rds")
lesb58Data <- readRDS("data/Pseudomonas_aeruginosa_LESB58_125.Rds")



# Define the UI elements --------------------------------------------------

ui <- fluidPage(

    theme = shinytheme("flatly"),

    # Enable shinyjs usage
    shinyjs::useShinyjs(),

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
            textAreaInput(
                "pastedInput",
                "Paste your list of locus tags, one per line:",
                height = "300px"
            ),


            # Button which triggers results to display. "background-color"
            # defines the colour of the button (default #337ab7)
            actionButton(
                inputId = "search",
                label   = "Search",
                icon    = icon("search"),
                style   = "color: #fff; background-color: #18bc9c; border-color: #18bc9c"
            ),

            tags$br(),
            tags$br(),

            # Download button for annotation table
            disabled(downloadButton(
                "resultTable",
                "Download Results Table",
                style = "color: #fff; background-color: #337ab7; border-color: #337ab7"
            )),

            tags$br(),
            tags$br(),

            # Download button for nucleotide sequences
            disabled(downloadButton(
                "ntSeqs",
                "Nucleotide Sequences"
            )),

            # Download button for amino acid sequences
            disabled(downloadButton(
                "aaSeqs",
                "Protein Sequences"
            ))
        ),

        mainPanel(

            dataTableOutput("displayTable")
        )
    )
)




# Define the server logic -------------------------------------------------

server <- function(input, output) {


    # Extract the genes to be mapped, dependent on chosen strain. Delay all code
    # until the search button is pressed
    observeEvent(input$search, {

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

            req(myGenesTable(), input$strainChoice)

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


        # Render the table of results
        output$displayTable <- renderDataTable({
            isolate(displayTable())
        }, options = list(searching = FALSE))


        # Download button for displayTable, enabled and then populated
        enable("resultTable")
        output$resultTable <- downloadHandler(
            filename = function() {
                paste0(input$strainChoice, "_annotations.csv")
            },
            content = function(file) {
                write.csv(displayTable(), file, row.names = FALSE, quote = FALSE)
            })


        # Download data for nucleotide sequences i.e. ntSeqs
        enable("ntSeqs")
        output$ntSeqs <- downloadHandler(
            filename = function() {
                paste0(input$strainChoice, "_nucleotideSequences.fasta")
            },
            content = function(file) {
                write.fasta(
                    as.list(filteredTable()$Nucleotide_Sequence),
                    names = paste0(
                        filteredTable()$Locus_Tag,
                        " | ",
                        filteredTable()$Name,
                        "; ",
                        filteredTable()$Product_Name
                        ),
                    file.out = file
                    )
            }
        )


        # Download data for amino acid sequences i.e. aaSeqs
        enable("aaSeqs")
        output$aaSeqs <- downloadHandler(
            filename = function() {
                paste0(input$strainChoice, "_proteinSequences.fasta")
            },
            content = function(file) {
                write.fasta(
                    as.list(filteredTable()$Amino_Acid_Sequence),
                    names = paste0(
                        filteredTable()$Locus_Tag,
                        " | ",
                        filteredTable()$Name,
                        "; ",
                        filteredTable()$Product_Name
                        ),
                    file.out = file
                    )
            }
        )


    })
}




# Run the app -------------------------------------------------------------

shinyApp(ui, server)
