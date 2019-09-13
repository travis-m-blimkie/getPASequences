
# TODO Add ortholog mapping? Would require reformatting the whole app based
# around a header+tab style


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

    # Enable shinyjs usage - NEED THIS LINE!!
    shinyjs::useShinyjs(),

    # Using the flatly theme
    theme = shinytheme("flatly"),

    # Application title
    titlePanel(
        div(HTML("Retrieve <em>P. aeruginosa</em> Annotations and Sequences")),
        windowTitle = "getPASequences"
    ),

    tags$br(),

    sidebarLayout(

        #################
        # SIDEBAR PANEL #
        #################
        sidebarPanel(

            tags$p(div(HTML(
                "This app is designed to retreive annotations, nucleotide, ",
                "and amino acid sequences for three strains of ",
                "<em>P. aeruginosa</em>, namely PAO1, PA14, and LESB58."
            ))),

            tags$p(div(HTML(
                "<b>NOTE:</b> Non-matching IDs are returned in a separate ",
                "table to the user. IDs must still be in the proper format, ",
                "(e.g. PA0000 for strain PAO1) to be recognized and parsed ",
                "correctly."
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
                placeholder = "Your genes here...",
                height = "300px"
            ),


            # Button which triggers results to display. Most code depends on
            # this input state changing before running
            actionButton(
                "search",
                "Search",
                icon = icon("search"),
                style = "color: #fff; background-color: #18bc9c; border-color: #18bc9c; width: 200px"
            ),


            # Download button for annotation table, to be created with
            # renderUI()
            uiOutput("resultTable_dl"),


            # Download button for nucleotide and amino acid sequences, hidden
            # until data is available
            uiOutput("seqs_dl"),

            tags$hr(),

            tags$p("This app was developed by Travis Blimkie. Source code for ",
                   "this app is available at the ",
                   shiny::tags$a(
                       href = "https://github.com/travis-m-blimkie/getPASequences",
                       "Github page."
                   )

            ),
            tags$p(div(HTML("<b>Coming soon:</b> Ortholog mapping!")))
        ),

        ##############
        # MAIN PANEL #
        ##############
        mainPanel(

            # Render panel for the matched results/annotations (showing
            # displayTable())
            h3("Your results will be displayed below:"),
            tags$br(),
            dataTableOutput("displayTable"),

            # Output for the non-matching genes. Using uiOutput() here so that
            # it only displays if there are non-matching genes (i.e. the table
            # which holds said genes has more than 0 rows)
            uiOutput("missingGenesPanel")
        )
    )
)




# Define the server logic -------------------------------------------------

server <- function(input, output) {

    # Display notification bubble when users pastes IDs. The option "ignoreInit
    # = TRUE" prevents the dialog from displaying when app is started.
    # observeEvent(input$pastedInput, {
    #     showNotification("Click the Search button to continue.",
    #                      type = "message",
    #                      duration = 1)
    # }, ignoreInit = TRUE)


    # Extract the genes to be mapped, using a single regex to match locus tags
    # from any of the three supported strains
    myGenes <- reactive({
        req(input$pastedInput)
        str_extract_all(input$pastedInput, pattern = "PA(14|LES)?_?[0-9]{4,5}")
    })


    # Delay all code until the search button is pressed. End of this is noted
    # with a comment.
    observeEvent(input$search, {


        # Convert to a data frame, and fix column name for easy joining later
        myGenesTable <- reactive({
            req(myGenes())

            part1 <- data.frame(Genes = myGenes(), stringsAsFactors = FALSE)
            colnames(part1) <- "Locus_Tag"

            return(part1)
        })


        # Map the input genes, dependent on strain. Notice we use inner_join()
        # here, which means genes with no hits must be handled separately
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


        # Create table without sequence to facilitate display. This is also the
        # annotation table the user downloads with the "Download Annotations"
        # button
        displayTable <- reactive({

            select(filteredTable(), -c(Nucleotide_Sequence, Amino_Acid_Sequence)) %>%
                arrange(Locus_Tag) %>%
                dplyr::rename("Locus Tag" = Locus_Tag,
                              "Product" = Product_Name)
        })


        # Now we deal with any genes submitted that didn't have a match
        noMatchGenes <- reactive({
            anti_join(myGenesTable(), filteredTable(), by = "Locus_Tag")
        })


        # Create and render the table of results; prevent updating the results
        # table when input IDs are changed until the search button is pressed
        # again
        output$displayTable <- renderDataTable({
            isolate(displayTable())
        }, options = list(searching = FALSE,
                          scrollX = "100%",
                          scrollY = "500px",
                          scrollCollapse = TRUE,
                          paging = FALSE)
        )


        # Create the output for non-matching genes, if present. This first chunk
        # creates the table which will be rendered conditionally in the next
        # block
        output$missingGenesTable <- renderDataTable({
            isolate(noMatchGenes())
        }, options = list(searching = FALSE,
                          scrollX = "100%",
                          scrollY = "250px",
                          scrollCollapse = TRUE,
                          paging = FALSE)
        )

        # This chunk renders the results only if there are non-matching genes
        # (see above chunk)
        output$missingGenesPanel <- renderUI({
            isolate(noMatchGenes())

            if (nrow(noMatchGenes()) == 0) {
                return(NULL)
            } else {
                return(tagList(
                    tags$hr(),
                    tags$h3("The following submitted genes had no matches:"),
                    dataTableOutput("missingGenesTable"),
                    tags$br(),
                    tags$br()
                ))
            }
        })


        # Download file for displayTable (annotations) to be shown with next
        # chunk (renderUI())
        output$resultTable <- downloadHandler(
            filename = function() {
                paste0(input$strainChoice, "_annotations.csv")
            },
            content = function(file) {
                write.csv(displayTable(), file, row.names = FALSE, sep = "\t")
            }
        )

        # Rendering the download button once displayTable() is populated with
        # data (see above chunk)
        output$resultTable_dl <- renderUI({
            if(nrow(displayTable()) != 0) {
                tagList(
                    tags$hr(),
                    downloadButton(
                        "resultTable",
                        "Download Annotations",
                        style = "color: #fff; background-color: #337ab7; border-color: #337ab7; width: 200px"
                    ),
                    tags$br(),
                    tags$br()
                )
            }
        })


        # Here we set up the handlers for downloading sequence information.
        # Tables for nt and aa sequences are created independently, but the
        # buttons to download them are rendered at the same time.

        # First the downloadHandler() for nucleotide sequences i.e. ntSeqs
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


        # The the downloadHandler() for amino acid sequences i.e. aaSeqs
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


        # Now render both sequence download buttons simultaneously, along with
        # some specific styling via tagList()
        output$seqs_dl <- renderUI({
            if (nrow(displayTable()) != 0) {
                tagList(
                    downloadButton(
                        "ntSeqs",
                        "Nucleotide Sequences",
                        style = "width: 200px; background-color: #2c3e50; border-color: #2c3e50"
                    ),

                    # Divider so both sequence download buttons render on the
                    # same line, with a small separation
                    div(
                        style = "display: inline-block; vertical-align: top; width: 10px;",
                        HTML("<br>")
                    ),

                    downloadButton(
                        "aaSeqs",
                        "Protein Sequences",
                        style = "width: 200px; background-color: #2c3e50; border-color: #2c3e50"
                    )
                )
            }
        })

    }) # Closes the observation based on search button input
}




# Run the app -------------------------------------------------------------

shinyApp(ui, server)
