
# Load libraries and data -------------------------------------------------

library(shiny)
library(shinythemes)
library(shinyjs)
library(seqinr)
library(DT)
library(tidyverse)

# Annotation files
annosPAO1 <- readRDS("data/annotations_PAO1_107.rds")
annosPA14 <- readRDS("data/annotations_UCBPP-PA14_109.rds")
annosLESB58 <- readRDS("data/annotations_LESB58_125.rds")

# Ortholog relations
orthos_PAO1toPA14 <- readRDS("data/orthologs_pao1_pa14.rds")
orthos_PAO1toLESB58 <- readRDS("data/orthologs_pao1_lesb58.rds")
orthos_PA14toLESB58 <- readRDS("data/orthologs_pa14_lesb58.rds")


# Define the UI elements --------------------------------------------------

ui <- fluidPage(
    theme = shinytheme("flatly"),

    navbarPage(
        id = "navBarLayout",

        ####################################
        ## Settings for the NavBar layout ##
        ####################################

        # Blank title, as we want the first tab to be our title. Maybe place an
        # image/logo here in the future...
        title = HTML(""),

        # Title that's shown in the browser window
        windowTitle = "getPASequences",

        # Make the navbar collapsible
        collapsible = TRUE,

        # Enable shinyjs usage - NEED THIS LINE!!
        header = tagList(useShinyjs()),

        #################
        ## Welcome Tab ##
        #################
        tabPanel(
            value = "main",

            # TODO Come up with a better title/name!
            div(HTML("PATool")),

            tags$div(
                class = "jumbotron",
                h1("Welcome"),

                tags$hr(),

                tags$div(

                    tags$p(HTML(paste0(
                        "Welcome to PATool, a Shiny app designed to facilitate ",
                        "analyses with <em>Pseudomonas aeruginosa</em>. Here ",
                        "you can upload a list of locus tags and retrieve ",
                        "gene annotations, nucleotide or amino acid ",
                        "sequences, as well as map between strains. For more ",
                        "information, see the <b>",
                        actionLink("linkAbout", "About"), "</b> page."
                    ))),

                    tags$p(HTML(paste0(
                        "We currently support the following strains of ",
                        "<em>P. aeruginosa</em>: PAO1, PA14, & LESB58."
                    ))),

                    tags$p("To get started, select one of the options below:"),

                    br(),

                    div(
                        actionButton(
                            "anno",
                            "Get Annotations & Sequences",
                            class = "btn btn-primary btn-lg",
                            style = "color: #2c3e50; background-color: #fff; border-color: #2c3e50;"
                        ),

                        HTML("&nbsp;&nbsp;&nbsp;"),

                        actionButton(
                            "ortho",
                            "Perform Ortholog Mapping",
                            class = "btn btn-primary btn-lg",
                            style = "color: #fff; background-color: #2c3e50; border-color: #2c3e50;"
                        )
                    )
                )
            )
        ),

        #################################
        ## Annotation and Sequence Tab ##
        #################################
        tabPanel(
            value = "annos",
            "Annotations and Sequences",

            sidebarLayout(

                ### SIDEBAR PANEL ###
                sidebarPanel(

                    tags$p(div(HTML(
                        "<b>NOTE:</b> Non-matching IDs are returned in a ",
                        "separate table. IDs must still be in the proper ",
                        "format (e.g. PA0000 for strain PAO1) to be recognized ",
                        "and parsed correctly."
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
                    # this input state changing before running (sort of?).
                    actionButton(
                        "search",
                        "Search",
                        icon = icon("search"),
                        style = "color: #fff; background-color: #18bc9c; border-color: #18bc9c; width: 150px"
                    ),


                    # Download button for annotation table, to be created with
                    # `renderUI()`.
                    uiOutput("resultTable_dl"),


                    # Download button for nucleotide and amino acid sequences, hidden
                    # until data is available.
                    uiOutput("seqs_dl"),

                    tags$hr()
                ),

                ### MAIN PANEL ###
                mainPanel(

                    # Render panel for the matched results/annotations (showing
                    # `displayTable()`). NOTE that we want use the DT functions
                    # when rendering output tables. There are `shiny` versions
                    # of the same functions, and although DT is loaded later, we
                    # want to be explicit just in case.
                    h3("Your results will be displayed below:"),
                    tags$br(),
                    DT::dataTableOutput(outputId = "displayTable"),

                    uiOutput(outputId = "resultSummary"),

                    # Output for the non-matching genes. Using `uiOutput()` here
                    # so that it only displays if there are non-matching genes
                    # (i.e. the table which holds said genes has more than 0
                    # rows).
                    uiOutput(outputId = "missingGenesPanel")
                )
            )
        ),

        ##########################
        ## Ortholog Mapping Tab ##
        ##########################
        tabPanel(
            value = "orthos",
            "Ortholog Mapping",

            sidebarLayout(

                sidebarPanel(
                    tags$p(
                        "Please select the strains for which you wish to ",
                        "retreive orthologs:"
                    ),


                    # Choose strain 1
                    div(style = "display: inline-block;vertical-align:top; width: 150px;",
                        selectInput(
                            inputId = "strain1",
                            label = "First Strain:",
                            choices = c("PAO1" = "PAO1",
                                        "PA14" = "PA14",
                                        "LESB58" = "LESB58"),
                            selected = "PAO1"
                        )
                    ),

                    # Separator since we have both dropdowns on one "line"
                    div(
                        style = "display: inline-block;vertical-align:top; width: 100px;",
                        HTML("<br>")
                    ),


                    # Choose strain 2
                    div(style = "display: inline-block;vertical-align:top; width: 150px;",
                        selectInput(
                            inputId = "strain2",
                            label = "Second Strain:",
                            choices = c("PAO1" = "PAO1",
                                        "PA14" = "PA14",
                                        "LESB58" = "LESB58"),
                            selected = "PA14"
                        )
                    ),

                    # Input area for genes
                    textAreaInput(
                        "orthoPastedInput",
                        "Paste your list of locus tags, one per line:",
                        placeholder = "Your genes here...",
                        height = "300px"
                    ),

                    actionButton(
                        "orthoSearch",
                        "Map",
                        style = "color: #fff; background-color: #2c3e50; border-color: #2c3e50; width: 100px"
                    ),
                ),

                mainPanel(
                    tags$h3("Results here:")
                )

            )
        ),

        ###############
        ## About Tab ##
        ###############
        tabPanel(
            value = "about",
            "About",

            tags$div(
                class = "jumbotron",

                tags$p("Source code for this app is available at the ",
                       tags$a(href = "https://github.com/travis-m-blimkie/getPASequences", "Github page"),
                       " under the MIT license."
                ),

                tags$p(
                    "The data used by this app for annotations and sequences comes from the ",
                    tags$a(href = "https://pseudomonas.com", "Pseudomonas Genome Database"),
                    ", version 18.1."
                ),

                tags$p(
                    "Ortholog information was obtained from ",
                    tags$a(href = "http://pseudoluge.pseudomonas.com/", "OrtholugeDB"),
                    ", version 1.0."
                ),

                tags$hr(),

                tags$p("This app uses the following R packages:"),
                tags$p(
                    tags$dt(tags$a(href = "https://shiny.rstudio.com/", "Shiny")),
                    tags$dd("Framework for app construction."),

                    tags$br(),

                    tags$dt(tags$a(href = "https://deanattali.com/shinyjs/", "ShinyJS")),
                    tags$dd("Additional app functionality."),

                    tags$br(),

                    tags$dt(tags$a(href = "https://www.tidyverse.org/", "The Tidyverse")),
                    tags$dd("Data manipulation functions, as well as reading and writing data."),

                    tags$br(),

                    tags$dt(tags$a(href = "https://cran.r-project.org/package=seqinr", "seqinr")),
                    tags$dd("Writing output fasta files.")
                )
            )
        )
    ))




# Define the server logic -------------------------------------------------

server <- function(input, output, session) {

    # Switch to the anno tab panel via the button
    observeEvent(input$anno, {
        updateNavbarPage(session, inputId = "navBarLayout", selected = "annos")
    }, ignoreInit = TRUE)

    # Switch to the ortholog tab panel via the button
    observeEvent(input$ortho, {
        updateNavbarPage(session, inputId = "navBarLayout", selected = "orthos")
    }, ignoreInit = TRUE)

    # Switch to the About panel via link in the intro text
    observeEvent(input$linkAbout, {
        updateNavbarPage(session, inputId = "navBarLayout", selected = "about")
    }, ignoreInit = TRUE)


    # Extract the genes to be mapped, using a single regex to match locus tags
    # from any of the three supported strains
    myGenes <- reactive({
        req(input$pastedInput)
        str_extract_all(input$pastedInput, pattern = "PA(14|LES)?_?[0-9]{4,5}")
    })


    ### Delay all code until the search button is pressed. End of this is noted
    ### with a comment.
    observeEvent(input$search, {


        # Convert to a data frame, and fix column name for easy joining later.
        myGenesTable <- reactive({
            req(myGenes())

            part1 <- data.frame(Genes = myGenes(), stringsAsFactors = FALSE)
            colnames(part1) <- "Locus_Tag"

            return(part1)
        })


        # Map the input genes, dependent on strain. Notice we use `inner_join()`
        # here, which means genes with no hits must be handled separately.
        filteredTable <- reactive({
            req(myGenesTable(), input$strainChoice)

            if (input$strainChoice == "PAO1") {
                inner_join(myGenesTable(), annosPAO1, by = "Locus_Tag")

            } else if (input$strainChoice == "PA14") {
                inner_join(myGenesTable(), annosPA14, by = "Locus_Tag")

            } else if (input$strainChoice == "LESB58") {
                inner_join(myGenesTable(), annosLESB58,  by = "Locus_Tag")

            } else {
                return(NULL)
            }
        })


        # Create table without sequence to facilitate display. This is also the
        # annotation table the user downloads with the "Download Annotations"
        # button.
        displayTable <- reactive({
            filteredTable() %>%
                select(-c(Nucleotide_Sequence, Amino_Acid_Sequence)) %>%
                arrange(Locus_Tag) %>%
                dplyr::rename("Locus Tag" = Locus_Tag, "Product" = Product_Name)
        })

        # Get numbers of genes to be included as a summary below the results table
        numGenes <- reactive({
            list(
                submitted = nrow(myGenesTable()),
                matched = nrow(displayTable())
            )
        })


        # Now we deal with any genes submitted that didn't have a match.
        noMatchGenes <- reactive({
            anti_join(myGenesTable(), filteredTable(), by = "Locus_Tag")
        })


        # Create and render the table of results; prevent updating the results
        # table when input IDs are changed until the search button is pressed
        # again. As before, we are being explicit with our use of DT functions.
        output$displayTable <- DT::renderDataTable({
            isolate(displayTable())
        }, options = list(searching = FALSE,
                          scrollX = "100%",
                          scrollY = "500px",
                          scrollCollapse = TRUE,
                          paging = FALSE,
                          dom = "t"),
        rownames = FALSE,
        selection = "none"
        )

        # Print some text describing the number of matched/submitted genes.
        output$resultSummary <- renderUI({
            isolate(displayTable())

            tagList(
                tags$br(),

                tags$p(paste0(
                    "Matched ", numGenes()$matched, " out of ",
                    numGenes()$submitted, " genes submitted."
                ))
            )
        })


        # Create the output for non-matching genes, if present. This first chunk
        # creates the table which will be rendered conditionally in the next
        # block. As before, we are being explicit with our use of DT functions.
        output$missingGenesTable <- DT::renderDataTable({
            isolate(noMatchGenes())
        }, options = list(searching = FALSE,
                          scrollX = "100%",
                          scrollY = "250px",
                          scrollCollapse = TRUE,
                          paging = FALSE,
                          dom = "t"),
        rownames = FALSE,
        selection = "none"
        )

        # This chunk renders the results only if there are non-matching genes
        # (see above chunk). As before, we are being explicit with our use of DT
        # functions beacuse of potential overlap with `shiny` functions.
        output$missingGenesPanel <- renderUI({
            isolate(noMatchGenes())

            if (nrow(noMatchGenes()) == 0) {
                return(NULL)
            } else {
                return(tagList(
                    tags$hr(),
                    tags$h3("The following submitted genes had no matches:"),
                    DT::dataTableOutput(outputId = "missingGenesTable"),
                    tags$br(),
                    tags$br()
                ))
            }
        })


        # Download file for displayTable (annotations) to be shown with next
        # chunk (`renderUI()`).
        output$resultTable <- downloadHandler(
            filename = function() {
                paste0(input$strainChoice, "_annotations.txt")
            },
            content = function(file) {
                write_delim(displayTable(), file, delim = "\t")
            }
        )

        # Rendering the download button once displayTable() is populated with
        # data (see above chunk).
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

        # First the downloadHandler() for nucleotide sequences i.e. ntSeqs.
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


        # The the downloadHandler() for amino acid sequences i.e. aaSeqs.
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
        # some specific styling via `tagList()`.
        output$seqs_dl <- renderUI({
            if (nrow(displayTable()) != 0) {
                tagList(
                    downloadButton(
                        "ntSeqs",
                        "Nucleotide Sequences",
                        style = "width: 200px; background-color: #2c3e50; border-color: #2c3e50"
                    ),

                    # Divider so both sequence download buttons render on the
                    # same line, with a small separation between them.
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

    }) ### Closes the observation based on search button input
}




# Run the app -------------------------------------------------------------

shinyApp(ui, server)
