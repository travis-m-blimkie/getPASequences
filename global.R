
# Load all the data files -------------------------------------------------

# Annotation files
annosPAO1 <- readRDS("data/annotations_PAO1_107.rds")
annosPA14 <- readRDS("data/annotations_UCBPP-PA14_109.rds")
annosLESB58 <- readRDS("data/annotations_LESB58_125.rds")

# Ortholog relations
orthologs_PAO1_PA14 <- readRDS("data/orthologs_PAO1_PA14.rds")
orthologs_PAO1_LESB58 <- readRDS("data/orthologs_PAO1_LESB58.rds")
orthologs_PA14_LESB58 <- readRDS("data/orthologs_PA14_LESB58.rds")


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
