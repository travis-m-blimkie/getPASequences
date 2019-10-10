
# This script covers the munging of orthologs for each of the supported
# Pseduomonas aeruginosa strains. If one wishes to update the ortholog files
# used in the mapping, this is the place to start! Simply drop the new files
# into the `raw_data` folder and run through this code.
# NOTE: files are downloaded from "pathogenomics.sfu.ca/ortholugedb" (TSV), as
# the full list of orthologs between two strains (done for each combination).


# Load libraries
library(tidyverse)


# Function to assist in renaming columns
cleanOrthos <- function(dataFrame, strains) {
  dataFrame %>%
    rename(!!sym(strains[1]) := `Locus Tag (Strain 1)`,
           !!sym(strains[2]) := `Locus Tag (Strain 2)`)
}


# List the ortholog files to be used. Note the use of non-inverted grep to get
# just the ortholog files and not the annotations.
fileNames <- list.files("rawData/", pattern = "txt$") %>%
  grep(., pattern = "orthologs", value = TRUE, invert = FALSE)


# Create variable names for each file/strain
varNames <- fileNames %>%
  map_chr(~str_remove_all(., pattern = "_orthologs|\\.txt"))


# Read in the files, and set the names of the list
myFiles <- fileNames %>% map(
  ~read_tsv(paste0("./rawData/", .), comment = "#")
) %>% set_names(varNames)


# Select desired columns and fix LESB58 IDs
orthoData_1 <- myFiles %>% map(
  ~select(., contains("Locus Tag (Strain")) %>%
    mutate(
      `Locus Tag (Strain 1)` = str_replace(
        `Locus Tag (Strain 1)`,
        pattern = "PLES",
        replacement = "PALES"
      ),
      `Locus Tag (Strain 2)` = str_replace(
        `Locus Tag (Strain 2)`,
        pattern = "PLES",
        replacement = "PALES"
      )
    )
)


# Get strain pairs for next step
strainPairs <- names(orthoData_1) %>% map(
  ~toupper(.) %>%
    str_split(pattern = "_", n = 2)
) %>% flatten()


# Rename using strain pairs defined above
orthoData_2 <- map2(orthoData_1, strainPairs, ~cleanOrthos(.x, strains = .y))


# Save the tables as R data objects
map2(
  orthoData_2,
  names(orthoData_2),
  ~saveRDS(.x, file = paste0("data/orthologs_", .y, ".rds"))
)
