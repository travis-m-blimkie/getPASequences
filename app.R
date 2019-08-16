
# TODO Add message/notification if str_extract_all() yeilds no IDs aka input IDs
# are not in the proper format

# TODO How should missing input IDs be handled?

# TODO change showNotification() on gene paste to renderUI (like in metabridge)


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
    titlePanel(div(HTML("Retreive <em>P. aeruginosa</em> Annotations and Sequences")),
               windowTitle = "getPASequences"),

    tags$br(),

    sidebarLayout(

        sidebarPanel(

            tags$p(div(HTML(
                "This app is designed to retreive annotations, nucleotide, and ",
                "amino acid sequences for three strains of <em>P. aeruginosa</em>,",
                "namely PAO1, PA14, and LESB58."
                ))),

            tags$p(div(HTML(
                "<b>NOTE:</b> Currently non-matching genes are NOT returned to the user, ",
                "and there is no warning when this occurs. Please check the ",
                "number of results compared to your number of inputs to ensure ",
                "no genes went missing."
            ))),

            tags$br(),

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
                style   = "color: #fff; background-color: #18bc9c; border-color: #18bc9c; width: 200px"
            ),

            tags$hr(),

            # Download button for annotation table
            disabled(downloadButton(
                "resultTable",
                "Download Annotations",
                style = "color: #fff; background-color: #337ab7; border-color: #337ab7; width: 200px"
            )),

            tags$br(),
            tags$br(),

            # Download button for nucleotide sequences, disabled until data is
            # available
            disabled(downloadButton(
                "ntSeqs",
                "Nucleotide Sequences",
                style = "width: 200px; background-color: #2c3e50; border-color: #2c3e50"
            )),

            div(style = "display: inline-block; vertical-align: top; width: 10px;", HTML("<br>")),

            # Download button for amino acid sequences
            disabled(downloadButton(
                "aaSeqs",
                "Protein Sequences",
                style = "width: 200px; background-color: #2c3e50; border-color: #2c3e50"
            ))
        ),

        mainPanel(

            h3("Your results will be displayed below:"),
            tags$br(),
            dataTableOutput("displayTable")
        )
    )
)




# Define the server logic -------------------------------------------------

server <- function(input, output) {

    # Display notification bubble when users pastes IDs. ignoreInit = TRUE
    # prevents the dialog from displaying when app is started
    observeEvent(input$pastedInput, {
        showNotification("Click the Search button to continue.",
                         type = "message")
    }, ignoreInit = TRUE)


    # Delay all code until the search button is pressed
    observeEvent(input$search, {


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


        # Convert to a data frame, and fix column name for easy joining later
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
                arrange(Locus_Tag) %>%
                dplyr::rename("Locus Tag" = Locus_Tag,
                              "Product" = Product_Name)
        })


        # Render the table of results; prevent updating the results table when
        # input IDs are changed until the search button is pressed again
        output$displayTable <- renderDataTable({
            isolate(displayTable())
        }, options = list(searching = FALSE,
                          # pageLength = 10,
                          # lengthMenu = c(5, 10, 15, 20),
                          scrollX = "100%",
                          scrollY = "600px",
                          scrollCollapse = TRUE,
                          paging = FALSE)
        )


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




# Run the app! ------------------------------------------------------------

shinyApp(ui, server)
