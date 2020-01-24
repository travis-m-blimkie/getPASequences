
# Load libraries and data -------------------------------------------------

library(shiny)

# This file contains code to read data files and defines the annotation and
# ortholog mapping functions used in the app. It also loads a bunch of
# libraries, to keep this clean.
source("global.R")

# Useful colours which match the flatly theme:
# Dark blue     #2c3e50
# Light grey    #ecf0f1
# Grey          #75818c
# Turquoise     #18bc9c
# Light blue    #3498db
# DT blue       #0075b0
# White         #ffffff


# Define the UI elements --------------------------------------------------

ui <- fluidPage(

    # This CSS file is equivalent to using `shinytheme("flatly")`, with some
    # tweaks made. By using a local version of the same theme, we can more
    # easily make global tweaks to the app, without inserting as much CSS into
    # this file.
    theme = "shinyflatlybootstrap.css",

    # Head linking to custom CSS tweaks
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/user.css")
    ),

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
            title = div(HTML("Home")),
            tags$div(
                class = "jumbotron",

                h1("Welcome"),

                tags$hr(),

                tags$div(

                    # Note that we only need `paste0()` here because of the link
                    # to the about page within the body of the text.
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

                    tags$br(),

                    tags$div(
                        actionButton(
                            "annoTabBtn",
                            "Get Annotations & Sequences",
                            class = "btn btn-default btn-lg",
                            style = "color: #fff; background-color: #3498db; border-color: #3498db;"
                        ),

                        HTML("&nbsp;&nbsp;&nbsp;"),

                        actionButton(
                            "orthoTabBtn",
                            "Perform Ortholog Mapping",
                            class = "btn btn-default btn-lg",
                            style = "color: #fff; background-color: #18bc9c; border-color: #18bc9c;"
                        )
                    )
                )
            ),

            # Separate div to include the lab logo below the main section. Also made
            # into a clickable link!
            tags$div(
                style = "position:fixed; bottom:0px; padding-bottom: 10px",
                # style = "padding-top: 5vw; padding-bottom: 10px; padding-right: 10px",
                htmltools::HTML(
                    "<a href='http://cmdr.ubc.ca/bobh/'> <img src = 'hancock-lab-logo.svg'> </a>"
                )
            )
        ),

        #################################
        ## Annotation and Sequence Tab ##
        #################################
        tabPanel(
            value = "annoTab",
            "Annotations and Sequences",

            # Custome sidebarLayout. We are building this as a div so it can be
            # assigned an ID, which is needed for the alert rendered when the
            # example data is loaded.
            tags$div(
                class = "col-sm-4 manual-sidebar",
                id = "annoPanelSidebar",

                ### SIDEBAR PANEL ###
                tags$form(
                    class = "well",
                    style = "padding-top: 0",

                    tags$h3("Upload Your Genes"),

                    tags$p(
                        "Here you can upload your genes as locus tags to ",
                        "search for annotations. You may also use the link ",
                        "below to try our example data."
                    ),

                    tags$p(
                        "Note that IDs must be in the proper format (e.g. ",
                        "PA0000 for strain PAO1) to be recognized and parsed ",
                        "correctly. Non-matching IDs are returned in a ",
                        "separate table. "
                    ),

                    tags$br(),

                    # Dropdown to pick strain
                    selectInput(
                        inputId = "annoStrainChoice",
                        label = "Please select a strain:",
                        choices = c(PAO1 = "PAO1", PA14 = "PA14", LESB58 = "LESB58"),
                        width = "50%"
                    ),


                    # Place to paste your genes of interest
                    textAreaInput(
                        inputId = "annoPastedInput",
                        label = "Paste your list of locus tags, one per line:",
                        placeholder = "Your genes here...",
                        height = "200px"
                    ),


                    # Link to load the example data
                    actionLink(
                        inputId = "annoTryExample",
                        label = tags$b("Try Example Data"),
                        style = "font-size: 110%;"
                    ),


                    # Button which triggers results to display. Most code depends on
                    # this input state changing before running (sort of?).
                    actionButton(
                        inputId = "annoSearch",
                        label = tags$b("Search"),
                        icon = icon("search"),
                        style = "color: #fff; background-color: #2c3e50; border-color: #2c3e50; width: 100px; float: right;"
                    ),

                    tags$br(),
                    tags$br()
                ),

                # Output for all the download buttons (annos, protein, and
                # nucleotide sequences. It goes inside the main div for the
                # sidebar, but outside the well which contains inputs (above),
                # so is rendered as a separate well/box.
                uiOutput("annoBothBtns")
            ),

            ### MAIN PANEL ###
            tags$div(
                class = "col-sm-8",

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
        ),

        ##########################
        ## Ortholog Mapping Tab ##
        ##########################
        tabPanel(
            value = "orthoTab",
            title = "Ortholog Mapping",

            tags$div(
                class = "col-sm-4 manual-sidebar",
                id = "orthoPanelSidebar",

                tags$form(
                    class = "well",
                    style = "padding-top: 0",

                    tags$h3("Upload Your Genes"),

                    tags$p(
                      "Paste your genes (one per line) into the box below, then ",
                      "select the strains for which you wish to retrieve ",
                      "orthologs. Additionally you may click the link below ",
                      "to load some example genes."
                    ),

                    tags$p(
                        "Please note that IDs must be in the proper format ",
                        "(e.g. PA14_12345 for strain PA14) to be recognized ",
                        "and parsed correctly."
                    ),

                    tags$br(),

                    # Choose strain 1
                    tags$div(
                        style = "display: inline-block; vertical-align: top; width: 150px;",
                        selectInput(
                            inputId = "strain1",
                            label = "Mapping from:",
                            choices = c(PAO1 = "PAO1", PA14 = "PA14", LESB58 = "LESB58"),
                            selected = "PAO1"
                        )
                    ),

                    # Separator since we have both dropdowns on one "line"
                    tags$div(
                        style = "display: inline-block; vertical-align: top; width: 100px;",
                        HTML("<br>")
                    ),


                    # Choose strain 2
                    tags$div(
                        style = "display: inline-block; vertical-align: top; width: 150px;",
                        selectInput(
                            inputId = "strain2",
                            label = "Mappping to:",
                            choices = c(PAO1 = "PAO1", PA14 = "PA14", LESB58 = "LESB58"),
                            selected = "PA14"
                        )
                    ),

                    # Input area for genes
                    textAreaInput(
                        "orthoPastedInput",
                        "Paste your list of locus tags, one per line:",
                        placeholder = "Your genes here...",
                        height = "200px"
                    ),


                    # Link to load the example data
                    actionLink(
                        inputId = "orthoTryExample",
                        label = tags$b("Try Example Data"),
                        style = "font-size: 110%"
                    ),

                    # Main search button which triggers display of results and
                    # potential alerts
                    actionButton(
                        "orthoSearch",
                        tags$b("Map"),
                        icon = icon("search"),
                        style = "color: #fff; background-color: #2c3e50; border-color: #2c3e50; width: 100px; float: right;"
                    ),

                    tags$br(),
                    tags$br()
                ),

                # Download button for ortholog results. Same method applied here
                # as for the annotation tab.
                uiOutput("mappedOrtho_btn")
            ),

            ### Main Panel ###
            tags$div(
                class = "col-sm-8",

                tags$h3("Your results will be displayed below:"),
                tags$br(),
                DT::dataTableOutput("orthoResultPanel"),

                # Summary of number of mapped genes
                uiOutput("orthoResultSummary"),

                # Show missing genes if present
                uiOutput("orthoMissingGenesPanel")
            )
        ),

        ###############
        ## About Tab ##
        ###############
        tabPanel(
            value = "aboutTab",
            title = "About",

            tags$div(
                class = "jumbotron",

                tags$h1("About"),

                tags$hr(),

                tags$p("Source code for this app is available at the ",
                       tags$a(href = "https://github.com/travis-m-blimkie/getPASequences", "Github page"),
                       " under the MIT license.",
                       "The data used by this app for annotations and sequences comes from the ",
                       tags$a(href = "https://pseudomonas.com", "Pseudomonas Genome Database"),
                       ", version 18.1.",
                       "Ortholog information was obtained from ",
                       tags$a(href = "http://pseudoluge.pseudomonas.com/", "OrtholugeDB"),
                       ", version 1.0."
                ),

                tags$br(),

                tags$p("This app uses the following R packages:"),

                # List of requisite packages, and scaling the font size slightly.
                tags$dl(
                    style = "font-size: 1.25em",
                    tags$dt(tags$a(href = "https://shiny.rstudio.com/", "Shiny")),
                    tags$dd("Framework for app construction."),
                    # ShinyJS
                    tags$dt(tags$a(href = "https://deanattali.com/shinyjs/", "ShinyJS")),
                    tags$dd("Additional app functionality."),
                    # tidyverse
                    tags$dt(tags$a(href = "https://www.tidyverse.org/", "The tidyverse")),
                    tags$dd("Data manipulation functions, as well as reading and writing data."),
                    # seqinr
                    tags$dt(tags$a(href = "https://cran.r-project.org/package=seqinr", "seqinr")),
                    tags$dd("Writing output fasta files.")
                )
            ),

            # Separate div to include the lab logo below the main section. Also made
            # into a clickable link!
            tags$div(
                style = "position:fixed; bottom:0px; padding-bottom: 10px",
                # style = "padding-top: 5vw; padding-bottom: 10px; padding-right: 10px",
                htmltools::HTML(
                    "<a href='http://cmdr.ubc.ca/bobh/'> <img src = 'hancock-lab-logo.svg'> </a>"
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

    # Reactive value which will hold either user input data or example data
    annoInputGenes <- reactiveVal()


    # Load example data if the link is clicked and provide a message
    observeEvent(input$annoTryExample, {
        annoInputGenes(exampleData)

        # Success alert to inform the user the example data has been loaded
        insertUI(
            selector = "#annoPanelSidebar",
            where = "beforeEnd",
            ui = tags$div(
                id = "annoExampleAlert",
                class = "alert alert-dissmissible alert-success",
                tags$button(
                    HTML("&times;"),
                    type = "button",
                    class = "close",
                    `data-dismiss` = "alert"
                ),
                tags$b("Example data successfully loaded. Click the 'Search' button to continue.")
            )
        )
    }, ignoreInit = TRUE, ignoreNULL = TRUE)


    # Extract the user's genes to be mapped, using a single regex to match locus
    # tags from any of the three supported strains.
    observeEvent(input$annoPastedInput, {
        str_extract_all(input$annoPastedInput, pattern = "PA(14|LES)?_?[0-9]{4,5}") %>%
            map(~str_trim(.)) %>%
            annoInputGenes()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)


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


    # Provide an error message if no genes were extracted from the users input
    observeEvent(input$annoSearch, {
        if (nrow(annoDisplayTable()) == 0 & nrow(annoNoMatchGenes()) == 0) {
            insertUI(
                selector = "#annoPanelSidebar",
                where = "beforeEnd",
                ui = tags$div(
                    id = "annoFailAlert",
                    class = "alert alert-dissmissible alert-danger",
                    tags$button(
                        HTML("&times;"),
                        type = "button",
                        class = "close",
                        `data-dismiss` = "alert"
                    ),
                    tags$b("ERROR: No matches were found for your genes. ",
                    "Please check your inputs and ensure they are in the ",
                    "correct format.")
                )
            )
        }
    })


    # Create and render the table of results; prevent updating the results
    # table when input IDs are changed until the search button is pressed
    # again. As before, we are being explicit with our use of DT functions.
    observeEvent(input$annoSearch, {
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

        # Remove example alert, if present
        removeUI("#annoExampleAlert")
    })

    # Print some text describing the number of matched/submitted genes.
    output$annoResultSummary <- renderUI({
        input$annoSearch
        isolate({
            tagList(
                tags$br(),
                tags$p(paste0(
                    "Matched ", annoNumGenes()$matched, " out of ",
                    annoNumGenes()$submitted, " genes submitted."
                ))
            )
        })
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
    # We also have a warning for the user if some genes did not have a match
    output$annoMissingGenesPanel <- renderUI({
        input$annoSearch
        isolate({
            if (nrow(annoNoMatchGenes()) == 0) {
                return(NULL)
            } else {
                insertUI(
                    selector = "#annoPanelSidebar",
                    where = "beforeEnd",
                    ui = tags$div(
                        id = "annoNomatchWarning",
                        class = "alert alert-dissmissible alert-warning",
                        tags$button(
                            HTML("&times;"),
                            type = "button",
                            class = "close",
                            `data-dismiss` = "alert"
                        ),
                        tags$b("WARNING: Some genes did not have a match. ",
                        "Check the table to see which genes did not have ",
                        "annotations.")
                    )
                )
                return(
                    tagList(
                        tags$hr(),
                        tags$div(tags$h3("The following submitted genes had no matches:")),
                        tags$div(
                            class = "col-sm-4 manual-sidebar",
                            DT::dataTableOutput("annoMissingGenesTable"),
                            tags$br(),
                            tags$br()
                        )
                    )
                )
            }
        })
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


    # This chunk renders the UI for all three download buttons. This way, all
    # the buttons are in a single form/well, which only displays when data is
    # ready to download.
    output$annoBothBtns <- renderUI({
        input$annoSearch
        isolate({
            if (nrow(annoDisplayTable()) != 0) {
                tagList(
                    tags$form(
                        class = "well",
                        tags$p(
                            "Download the annotation table as a tab delimited-file, ",
                            "or the nucleotide or amino acid sequences in multi-",
                            "fasta format."
                        ),
                        downloadButton(
                            "annoResultTable_dl",
                            tags$b("Annotations"),
                            style = "color: #fff; background-color: #3498db; border-color: #3498db; width: 200px"
                        ),
                        tags$br(),
                        tags$br(),

                        downloadButton(
                            "annoNTSeqs_dl",
                            tags$b("Nucleotide Sequences"),
                            style = "width: 200px; background-color: #75818c; border-color: #75818c"
                        ),

                        # Divider so both sequence download buttons render on the
                        # same line, with a small separation between them.
                        tags$div(
                            style = "display: inline-block; vertical-align: top; width: 10px;",
                            HTML("<br>")
                        ),

                        downloadButton(
                            "annoAASeqs_dl",
                            tags$b("Protein Sequences"),
                            style = "width: 200px; background-color: #75818c; border-color: #75818c"
                        )
                    )
                )
            }
        })
    })


    ##################
    ## Ortholog Tab ##
    ##################

    # Set up the initial reactive value
    orthoInputGenes <- reactiveVal()


    # Load in example data when the link is clicked, and show a message.
    observeEvent(input$orthoTryExample, {
        orthoInputGenes(exampleData)

        insertUI(
            selector = "#orthoPanelSidebar",
            where = "beforeEnd",
            ui = tags$div(
                id = "orthoExampleAlert",
                class = "alert alert-dissmissible alert-success",
                tags$button(
                    HTML("&times;"),
                    type = "button",
                    class = "close",
                    `data-dismiss` = "alert"
                ),
                tags$b("Example data successfully loaded. Click the 'Map' button to continue.")
            )
        )
    }, ignoreInit = TRUE, ignoreNULL = TRUE)


    # Extract and clean input genes from the user
    observeEvent(input$orthoPastedInput, {
        str_extract_all(input$orthoPastedInput, pattern = "PA(14|LES)?_?[0-9]{4,5}") %>%
            map(~str_trim(.)) %>%
            orthoInputGenes()
    })


    # Convert to a data frame, and fix column name for easy joining later.
    orthoGenesTable <- reactive({
        req(orthoInputGenes(), input$strain1, input$strain2, input$orthoSearch)

        part1 <- data.frame(Genes = orthoInputGenes(), stringsAsFactors = FALSE)
        colnames(part1) <- paste0(input$strain1, " Locus Tag")

        return(part1)
    })

    # Map the genes between strains using previously defined function.
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

    # Output main result table
    observeEvent(input$orthoSearch, {
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
        removeUI("#orthoExampleAlert")
    })

    # Render summary of how many input genes mapped successfully.
    output$orthoResultSummary <- renderUI({
        input$orthoSearch
        isolate({
            tagList(
                tags$br(),
                tags$p(paste0(
                    "Matched ", orthoNumGenes()$matched, " out of ",
                    orthoNumGenes()$submitted, " genes submitted."
                ))
            )
        })
    })

    # Create the table containing the missing genes (those without orthologs),
    # if present.
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


    # Provide an error message if no genes were extracted from the users input
    observeEvent(input$orthoSearch, {
        if (nrow(mappedOrthoGenes()) == 0 & nrow(missingOrthoGenes()) == 0) {
            insertUI(
                selector = "#orthoPanelSidebar",
                where = "beforeEnd",
                ui = tags$div(
                    id = "orthoFailAlert",
                    class = "alert alert-dissmissible alert-danger",
                    tags$button(
                        HTML("&times;"),
                        type = "button",
                        class = "close",
                        `data-dismiss` = "alert"
                    ),
                    tags$b("ERROR: No matches were found for your genes. ",
                           "Please check your inputs and ensure they are in the ",
                           "correct format.")
                )
            )
        }
    })


    # Render the table for genes without orthologs, warning if some genes did
    # not have ortholgos
    output$orthoMissingGenesPanel <- renderUI({
        input$orthoSearch
        isolate({
            if (nrow(missingOrthoGenes()) == 0) {
                return(NULL)
            } else {
                insertUI(
                    selector = "#orthoPanelSidebar",
                    where = "beforeEnd",
                    ui = tags$div(
                        id = "orthoMissingAlert",
                        class = "alert alert-dissmissible alert-warning",
                        tags$button(
                            HTML("&times;"),
                            type = "button",
                            class = "close",
                            `data-dismiss` = "alert"
                        ),
                        tags$b("WARNING: No orthologs were found for some of ",
                               "your genes. Please check the table to see ",
                               "which genes did not have a match.")
                    )
                )
                return(tagList(
                    tags$hr(),
                    tags$div(
                        tags$h3("The following submitted genes had no orthologs:")
                    ),
                    tags$div(
                        class = "col-sm-4 manual-sidebar",
                        DT::dataTableOutput("orthoMissingGenesTable"),
                        tags$br(),
                        tags$br()
                    )
                ))
            }
        })
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
        input$orthoSearch
        isolate({
            if (nrow(mappedOrthoGenes()) != 0) {
                tags$form(
                    class = "well",
                    tags$p("Download your orthologs as a tab-delimted file:"),
                    downloadButton(
                        "mappedOrtho_dl",
                        tags$b("Download Orthologs"),
                        style = "color: #fff; background-color: #3498db; border-color: #3498db"
                    ),
                    tags$br()
                )
            }
        })
    })
}




# Run the app -------------------------------------------------------------

shinyApp(ui, server)
