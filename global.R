
# Load all required libraries ---------------------------------------------

library(shinythemes)
library(shinyjs)
library(seqinr)
library(DT)
library(tidyverse)


# Load all the data files -------------------------------------------------

# Annotation files
annosPAO1 <- readRDS("data/annotations_PAO1_107.rds")
annosPA14 <- readRDS("data/annotations_UCBPP-PA14_109.rds")
annosLESB58 <- readRDS("data/annotations_LESB58_125.rds")

# Ortholog relations
orthologs_PAO1_PA14 <- readRDS("data/orthologs_PAO1_PA14.rds")
orthologs_PAO1_LESB58 <- readRDS("data/orthologs_PAO1_LESB58.rds")
orthologs_PA14_LESB58 <- readRDS("data/orthologs_PA14_LESB58.rds")

# Example data
exampleData <- read_tsv("exampleData/testPAO1IDs.txt", col_names = "Locus Tag") %>% pull(1)


# Define function for getting annotations ---------------------------------

retrieveAnnotations <- function(inputDF, strain) {

  if (strain == "PAO1") {
    inner_join(inputDF, annosPAO1, by = "Locus Tag")

  } else if (strain == "PA14") {
    inner_join(inputDF, annosPA14, by = "Locus Tag")

  } else if (strain == "LESB58") {
    inner_join(inputDF, annosLESB58,  by = "Locus Tag")

  } else {
    return(NULL)
  }
}


# Define function for ortholog mapping ------------------------------------

mapOrthosGenerally <- function(inputDF, strain1, strain2) {

  if (strain1 %in% c("PAO1", "PA14") & strain2 %in% c("PAO1", "PA14")) {
    inner_join(inputDF, orthologs_PAO1_PA14)

  } else if (strain1 %in% c("PAO1", "LESB58") & strain2 %in% c("PAO1", "LESB58")) {
    inner_join(inputDF, orthologs_PAO1_LESB58)

  } else if (strain1 %in% c("PA14", "LESB58") & strain2 %in% c("PA14", "LESB58")) {
    inner_join(inputDF, orthologs_PA14_LESB58)

  } else {
    return(NULL)
  }
}


# Function for creating alerts --------------------------------------------

insertAlert <- function(location, ID, type, content) {
  insertUI(
    selector = location,
    where = "beforeEnd",
    ui = tags$div(
      id = ID,
      class = paste0("alert alert-dissmissible alert-", type),
      tags$button(
        HTML("&times;"),
        type = "button",
        class = "close",
        `data-dismiss` = "alert"
      ),
      tags$b(content)
    )
  )
}
