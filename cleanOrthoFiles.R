
# This script covers the munging of orthologs for each of the supported
# Pseduomonas aeruginosa strains. If one wishes to update the ortholog files
# used in the mapping, this is the place to start! Simply drop the new files
# into the `raw_data` folder and run through this code.
# NOTE: files are downloaded from "pathogenomics.sfu.ca/ortholugedb" (TSV), as
# the full list of orthologs between two strains (done for each combination).


# Load libraries
library(tidyverse)

# List the ortholog files to be used. Note the use of non-inverted grep to get
# just the ortholog files and not the annotations.
fileNames <- list.files("./rawData/", pattern = "txt$") %>%
  grep(., pattern = "orthologs", value = TRUE, invert = FALSE)
