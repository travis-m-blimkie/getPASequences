
# This script covers the munging of gene annotations for each of the supported
# Pseduomonas aeruginosa strains. If one wishes to update the database files
# used in the mapping, this is the place to start! Simply drop the new files
# into the `raw_data` folder and run through this code.
# NOTE: files are downloaded from "pseudomonas.com", as the TSV annotations on
# the strain info page.


# Load libraries
library(tidyverse)

# List the annotation files to be used. Note the use of inverted `grep()` to
# omit the ortholog files and only grab the annotation files.
fileNames <- list.files("rawData", pattern = "txt$", full.names = TRUE) %>%
  grep(., pattern = "orthologs", value = TRUE, invert = TRUE)

# Create variable names for each file/strain
varNames <- fileNames %>%
  map_chr(~str_remove_all(., pattern = "rawData/Pseudomonas_aeruginosa_|\\.txt"))

# Read in the files, and set the names of the list
myFiles <- fileNames %>% map(
  ~read_tsv(., comment = "#")
) %>% set_names(varNames)

# Select and rename desired columns
selectCols <- myFiles %>%
  map(~select(.,
              "Locus_Tag" = `Locus Tag`,
              Start,
              End,
              Strand,
              Name,
              "Product_Name" = `Product Name`,
              Accession,
              "Nucleotide_Sequence" = `Nucleotide Sequence`,
              "Amino_Acid_Sequence" = `Amino Acid Sequence`))

# Replace NA's in the "Name" column with the locus tag
fixName <- selectCols %>%
  map(~mutate(., Name = case_when(is.na(Name) ~ "", TRUE ~ Name)))

# Save the cleaned files as Rds objects
map2(
  fixName,
  names(fixName),
  ~saveRDS(.x, file = paste0("data/annotations_", .y, ".rds"))
)
