
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

            textAreaInput("pastedInput",
                          "Paste your list of locus tags, one per line:",
                          height = "300px")
        ),

        # TODO Add download information
        # downloadButton("")


        mainPanel(

            h3("Output:"),
            dataTableOutput("genesAndSeqs")

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

# Define server logic required to draw a histogram
server <- function(input, output) {

    myGenes <- reactive({
        req(input$pastedInput)
        str_extract_all(input$pastedInput, pattern = "PA14_[0-9]{5}")
    })

    myGenesTable <- reactive({
        part1 <- data.frame(Genes = myGenes())
        colnames(part1) <- "Locus_Tag"
        return(part1)
    })

    filteredTable <- reactive({
        left_join(myGenesTable(), pa14Data, by = "Locus_Tag")
    })



    displayTable <- reactive({
        select(filteredTable(), -c(Amino_Acid_Sequence))
    })

    output$genesAndSeqs <- renderDataTable({
        displayTable()
    }, options = list(searching = FALSE, pageLength = 10))


    # TODO Add download information

    # Hidden display of user-input genes
    # output$usersGenes <- renderDataTable({
    #     myGenesTable()
    # }, options = list(searching = FALSE, pageLength = 10))


}


# Run the application
shinyApp(ui = ui, server = server)
