
# Load libraries and data -------------------------------------------------

invisible(lapply(
    c("shiny", "shinythemes", "shinyjs", "seqinr", "DT", "tidyverse"),
    library,
    character.only = TRUE
))

# This file contains code to read data files and defines the annotation and
# ortholog mapping functions used in the app.
source("global.R")


# Define the UI elements --------------------------------------------------

ui <- fluidPage(
    theme = shinytheme("flatly"),

    navbarPage(
        ####################################
        ## Settings for the NavBar layout ##
        ####################################

        id = "navBarLayout",

        # Blank title, as we want the "Welcome" tab to be our title. Maybe place
        # an image/logo here in the future...
        title = HTML(""),

        # Title that's shown in the browser window
        windowTitle = "PATool",

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
                        actionLink("aboutTabLink", "About"), "</b> page."
                    ))),

                    tags$p(HTML(
                        "We currently support the following strains of ",
                        "<em>P. aeruginosa</em>: PAO1, PA14, & LESB58."
                    )),

                    tags$p("To get started, select one of the options below:"),

                    br(),

                    div(
                        actionButton(
                            "annoTabBtn",
                            "Get Annotations & Sequences",
                            class = "btn btn-primary btn-lg",
                            style = "color: #2c3e50; background-color: #fff; border-color: #2c3e50;"
                        ),

                        HTML("&nbsp;&nbsp;&nbsp;"),

                        actionButton(
                            "orthoTabBtn",
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
            value = "annoTab",
            "Annotations and Sequences",

            sidebarLayout(

                ### SIDEBAR PANEL ###
                sidebarPanel(

                    tags$p(HTML(
                        "<b>NOTE:</b> Non-matching IDs are returned in a ",
                        "separate table. IDs must still be in the proper ",
                        "format (e.g. PA0000 for strain PAO1) to be recognized ",
                        "and parsed correctly."
                    )),

                    tags$br(),

                    # Dropdown to pick strain
                    selectInput(
                        inputId = "annoStrainChoice",
                        label = "Please select a strain:",
                        choices = c("PAO1" = "PAO1",
                                    "PA14" = "PA14",
                                    "LESB58" = "LESB58"),
                        width = "50%"
                    ),


                    # Place to paste your genes of interest
                    textAreaInput(
                        inputId = "annoPastedInput",
                        label = "Paste your list of locus tags, one per line:",
                        placeholder = "Your genes here...",
                        height = "300px"
                    ),


                    # Button which triggers results to display. Most code depends on
                    # this input state changing before running (sort of?).
                    actionButton(
                        inputId = "annoSearch",
                        label = tags$b("Search"),
                        icon = icon("search"),
                        style = "color: #fff; background-color: #2c3e50; border-color: #2c3e50; width: 100px"
                    ),


                    # Download button for annotation table, to be created with
                    # `renderUI()`.
                    uiOutput("annoResultTableBtn"),


                    # Download button for nucleotide and amino acid sequences, hidden
                    # until data is available.
                    uiOutput("annoSeqsBtn"),

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
                    DT::dataTableOutput("annoDisplayTable"),

                    uiOutput(outputId = "annoResultSummary"),

                    # Output for the non-matching genes. Using `uiOutput()` here
                    # so that it only displays if there are non-matching genes
                    # (i.e. the table which holds said genes has more than 0
                    # rows).
                    uiOutput("annoMissingGenesPanel")
                )
            )
        ),

        ##########################
        ## Ortholog Mapping Tab ##
        ##########################
        tabPanel(
            value = "orthoTab",
            title = "Ortholog Mapping",

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
                            label = "Mapping from:",
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
                            label = "Mappping to:",
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
                        tags$b("Map"),
                        icon = icon("search"),
                        style = "color: #fff; background-color: #2c3e50; border-color: #2c3e50; width: 100px"
                    ),

                    uiOutput("mappedOrtho_btn")
                ),

                ### Main Panel ###
                mainPanel(
                    tags$h3("Your results will be displayed below:"),
                    tags$br(),
                    DT::dataTableOutput("orthoResultPanel"),

                    # Summary of number of mapped genes
                    uiOutput("orthoResultSummary"),

                    # Show missing genes if present
                    uiOutput("orthoMissingGenesPanel")
                )
            )
        ),

        ###############
        ## About Tab ##
        ###############
        tabPanel(
            value = "aboutTab",
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

                tags$dl(
                    tags$dt(tags$a(href = "https://shiny.rstudio.com/", "Shiny")),
                    tags$dd("Framework for app construction."),

                    tags$dt(tags$a(href = "https://deanattali.com/shinyjs/", "ShinyJS")),
                    tags$dd("Additional app functionality."),

                    tags$dt(tags$a(href = "https://www.tidyverse.org/", "The Tidyverse")),
                    tags$dd("Data manipulation functions, as well as reading and writing data."),

                    tags$dt(tags$a(href = "https://cran.r-project.org/package=seqinr", "seqinr")),
                    tags$dd("Writing output fasta files.")
                )
            )
        )
    )
)




# Define the server logic -------------------------------------------------

server <- function(input, output, session) {

    #################
    ## Welcome Tab ##
    #################

    # Switch to the anno tab panel via the button
    observeEvent(input$annoTabBtn, {
        updateNavbarPage(session, inputId = "navBarLayout", selected = "annoTab")
    }, ignoreInit = TRUE)

    # Switch to the ortholog tab panel via the button
    observeEvent(input$orthoTabBtn, {
        updateNavbarPage(session, inputId = "navBarLayout", selected = "orthoTab")
    }, ignoreInit = TRUE)

    # Switch to the About panel via link in the intro text
    observeEvent(input$aboutTabLink, {
        updateNavbarPage(session, inputId = "navBarLayout", selected = "aboutTab")
    }, ignoreInit = TRUE)


    ####################
    ## Annotation Tab ##
    ####################

    # Extract the genes to be mapped, using a single regex to match locus tags
    # from any of the three supported strains.
    annoInputGenes <- reactive({
        req(input$annoPastedInput)
        str_extract_all(input$annoPastedInput, pattern = "PA(14|LES)?_?[0-9]{4,5}") %>%
            map(~str_trim(.))
    })


    ### Delay all code until the search button is pressed. End of this is noted
    ### with a comment. Might need to change this...
    observeEvent(input$annoSearch, {


        # Convert to a data frame, and fix column name for easy joining later.
        annoInputGenesTable <- reactive({
            req(annoInputGenes())
            part1 <- data.frame(Genes = annoInputGenes(), stringsAsFactors = FALSE)
            colnames(part1) <- "Locus Tag"
            return(part1)
        })


        # Map the input genes, dependent on strain. Notice this function uses
        # `inner_join()`, which means genes with no hits must be handled
        # separately.
        annoFilteredTable <- reactive({
            req(annoInputGenesTable(), input$annoStrainChoice)
            retrieveAnnotations(annoInputGenesTable(), strain = input$annoStrainChoice)
        })


        # Create table without sequence to facilitate display. This is also the
        # annotation table the user downloads with the "Download Annotations"
        # button.
        annoDisplayTable <- reactive({
            annoFilteredTable() %>%
                select(-c(`Nucleotide Sequence`, `Amino Acid Sequence`)) %>%
                arrange(`Locus Tag`)
        })

        # Get numbers of genes to be included as a summary below the results table
        annoNumGenes <- reactive({
            list(
                submitted = nrow(annoInputGenesTable()),
                matched = nrow(annoDisplayTable())
            )
        })


        # Now we deal with any genes submitted that didn't have a match.
        annoNoMatchGenes <- reactive({
            anti_join(annoInputGenesTable(), annoFilteredTable(), by = "Locus Tag")
        })


        # Create and render the table of results; prevent updating the results
        # table when input IDs are changed until the search button is pressed
        # again. As before, we are being explicit with our use of DT functions.
        output$annoDisplayTable <- DT::renderDataTable({
            isolate(annoDisplayTable())
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
        output$annoResultSummary <- renderUI({
            isolate(annoDisplayTable())

            tagList(
                tags$br(),
                tags$p(paste0(
                    "Matched ", annoNumGenes()$matched, " out of ",
                    annoNumGenes()$submitted, " genes submitted."
                ))
            )
        })


        # Create the output for non-matching genes, if present. This first chunk
        # creates the table which will be rendered conditionally in the next
        # block. As before, we are being explicit with our use of DT functions.
        output$annoMissingGenesTable <- DT::renderDataTable({
            isolate(annoNoMatchGenes())
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
        output$annoMissingGenesPanel <- renderUI({
            isolate(annoNoMatchGenes())

            if (nrow(annoNoMatchGenes()) == 0) {
                return(NULL)
            } else {
                return(tagList(
                    tags$hr(),
                    tags$h3("The following submitted genes had no matches:"),
                    DT::dataTableOutput("annoMissingGenesTable"),
                    tags$br(),
                    tags$br()
                ))
            }
        })


        # Download file for displayTable (annotations) to be shown with next
        # chunk via `renderUI()`.
        output$annoResultTable_dl <- downloadHandler(
            filename = function() {
                paste0(input$annoStrainChoice, "_annotations.txt")
            },
            content = function(file) {
                write_delim(annoDisplayTable(), file, delim = "\t")
            }
        )

        # Rendering the download button once displayTable() is populated with
        # data (see above chunk).
        output$annoResultTableBtn <- renderUI({
            if(nrow(annoDisplayTable()) != 0) {
                tagList(
                    tags$hr(),
                    tags$p(
                        "Download the annotation table as a tab delimited-file, ",
                        "or the nucleotide or amino acid sequences in multi-",
                        "fasta format."
                    ),
                    downloadButton(
                        "annoResultTable_dl",
                        tags$b("Annotations"),
                        style = "color: #fff; background-color: #18bc9c; border-color: #18bc9c; width: 200px" # #337ab7
                    ),
                    tags$br(),
                    tags$br()
                )
            }
        })


        # Here we set up the handlers for downloading sequence information.
        # Tables for nt and aa sequences are created independently, but the
        # buttons to download them are rendered at the same time.

        # First the `downloadHandler()` for nucleotide sequences i.e.
        # annoNTSeqs_dl.
        output$annoNTSeqs_dl <- downloadHandler(
            filename = function() {
                paste0(input$annoStrainChoice, "_nucleotideSequences.fasta")
            },
            content = function(file) {
                write.fasta(
                    as.list(annoFilteredTable()$`Nucleotide Sequence`),
                    names = paste0(
                        annoFilteredTable()$`Locus Tag`,
                        " | ",
                        annoFilteredTable()$`Name`,
                        "; ",
                        annoFilteredTable()$`Description`
                    ),
                    file.out = file
                )
            }
        )


        # The the `downloadHandler()` for amino acid sequences i.e.
        # annoAASeqs_dl
        output$annoAASeqs_dl <- downloadHandler(
            filename = function() {
                paste0(input$annoStrainChoice, "_proteinSequences.fasta")
            },
            content = function(file) {
                write.fasta(
                    as.list(annoFilteredTable()$`Amino Acid Sequence`),
                    names = paste0(
                        annoFilteredTable()$`Locus Tag`,
                        " | ",
                        annoFilteredTable()$Name,
                        "; ",
                        annoFilteredTable()$Description
                    ),
                    file.out = file
                )
            }
        )


        # Now render both sequence download buttons simultaneously, along with
        # some specific styling via `tagList()`.
        output$annoSeqsBtn <- renderUI({
            if (nrow(annoDisplayTable()) != 0) {
                tagList(
                    downloadButton(
                        "annoNTSeqs_dl",
                        tags$b("Nucleotide Sequences"),
                        style = "width: 200px; background-color: #337ab7; border-color: #337ab7"
                    ),

                    # Divider so both sequence download buttons render on the
                    # same line, with a small separation between them.
                    div(
                        style = "display: inline-block; vertical-align: top; width: 10px;",
                        HTML("<br>")
                    ),

                    downloadButton(
                        "annoAASeqs_dl",
                        tags$b("Protein Sequences"),
                        style = "width: 200px; background-color: #337ab7; border-color: #337ab7"
                    )
                )
            }
        })

    }) ### Closes the observation based on search button input


    ##################
    ## Ortholog Tab ##
    ##################

    # Extract and clean input genes
    orthoInputGenes <- reactive({
        req(input$orthoPastedInput)
        str_extract_all(input$orthoPastedInput, pattern = "PA(14|LES)?_?[0-9]{4,5}") %>%
            map(~str_trim(.))
    })

    # Starting the mapping stuff
    observeEvent(input$orthoSearch, {

        isolate(input$orthoSearch)

        # Convert to a data frame, and fix column name for easy joining later.
        orthoGenesTable <- reactive({
            req(orthoInputGenes(), input$strain1, input$strain2)

            part1 <- data.frame(Genes = orthoInputGenes(), stringsAsFactors = FALSE)
            colnames(part1) <- paste0(input$strain1, " Locus Tag")

            return(part1)
        })

        mappedOrthoGenes <- reactive({
            req(orthoGenesTable())
            isolate(input$orthoSearch)

            mapOrthosGenerally(
                orthoGenesTable(),
                strain1 = input$strain1,
                strain2 = input$strain2
            )
        })

        # Handle genes without orthologs
        missingOrthoGenes <- reactive({
            req(orthoGenesTable(), input$strain1, input$strain2)

            anti_join(orthoGenesTable(), mappedOrthoGenes())
        })

        # Summarize mapped genes
        orthoNumGenes <- reactive({
            list(
                submitted = nrow(orthoGenesTable()),
                matched = nrow(mappedOrthoGenes())
            )
        })

        output$orthoResultPanel <- DT::renderDataTable({
            isolate(mappedOrthoGenes())
        }, options = list(searching = FALSE,
                          scrollX = "100%",
                          scrollY = "500px",
                          scrollCollapse = TRUE,
                          paging = FALSE,
                          dom = "t"),
        rownames = FALSE,
        selection = "none")

        output$orthoResultSummary <- renderUI({
            isolate(mappedOrthoGenes())

            tagList(
                tags$br(),
                tags$p(paste0(
                    "Matched ", orthoNumGenes()$matched, " out of ",
                    orthoNumGenes()$submitted, " genes submitted."
                ))
            )
        })

        output$orthoMissingGenesTable <- DT::renderDataTable({
            isolate(missingOrthoGenes())
        }, options = list(searching = FALSE,
                          scrollX = "100%",
                          scrollY = "500px",
                          scrollCollapse = TRUE,
                          paging = FALSE,
                          dom = "t"),
        rownames = FALSE,
        selection = "none")

        output$orthoMissingGenesPanel <- renderUI({
            isolate(missingOrthoGenes())

            if (nrow(missingOrthoGenes()) == 0) {
                return(NULL)
            } else {
                return(tagList(
                    tags$hr(),
                    tags$h3("The following submitted genes had no orthologs:"),
                    DT::dataTableOutput("orthoMissingGenesTable"),
                    tags$br(),
                    tags$br()
                ))
            }
        })

        # Download button for mapped orthologs
        output$mappedOrtho_dl <- downloadHandler(
            filename = function() {
                paste0(input$strain1, "to", input$strain2, "_orthologs.txt")
            },
            content = function(file) {
                write_delim(mappedOrthoGenes(), file, delim = "\t")
            }
        )

        # Render the download button when there's something to save
        output$mappedOrtho_btn <- renderUI({
            isolate(mappedOrthoGenes())

            if (nrow(mappedOrthoGenes()) != 0) {
                tagList(
                    tags$hr(),
                    tags$p("Download your orthologs as a tab-delimted file:"),
                    downloadButton(
                        "mappedOrtho_dl",
                        tags$b("Download Orthologs"),
                        style = "color: #fff; background-color: #18bc9c; border-color: #18bc9c"
                    ),
                    tags$br()
                )
            }
        })
    }) # Closing the `observeEvent()` for Ortholog Mapping
}




# Run the app -------------------------------------------------------------

shinyApp(ui, server)
