
# Load all required libraries ---------------------------------------------

library(seqinr)
library(DT)
library(tidyverse)


# Load all the data files -------------------------------------------------

# Annotation files
annosPAO1   <- readRDS("data/annotations_PAO1_107.rds")
annosPA14   <- readRDS("data/annotations_UCBPP-PA14_109.rds")
annosLESB58 <- readRDS("data/annotations_LESB58_125.rds")

# Ortholog relations
orthologs_PAO1_PA14   <- readRDS("data/orthologs_PAO1_PA14.rds")
orthologs_PAO1_LESB58 <- readRDS("data/orthologs_PAO1_LESB58.rds")
orthologs_PA14_LESB58 <- readRDS("data/orthologs_PA14_LESB58.rds")

# Example data
exampleData <-
  read_tsv("exampleData/testPAO1IDs.txt", col_names = "Locus Tag") %>% pull(1)
