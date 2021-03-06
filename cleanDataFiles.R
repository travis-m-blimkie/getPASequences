# Cleaning Raw Data Files


# Overview
# ~~~~~~~~
# This document will cover the steps needed to download and process the data files
# used by the Shiny app.


# Retrieving Annotation Files
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Navigate to [PGDB](pseudomonas.com) and get to the page for a specific strain
# (e.g. [PAO1](http://pseudomonas.com/strain/show?id=107)). Scroll down to the
# "Download Gene Annotations" section, and click on the "TAB" button to download
# gene annotations, along with nucleotide and amino acid sequences, in
# tab-delimited format. Place these files as-is into the `rawData` folder.
#
# We'll start by creating a named list for the files and reading all three in at
# once.
# Note the use of inverted `grep()` to omit the ortholog files and only grab the
# annotation files.

annoFiles <- list.files("rawData", pattern = "txt$", full.names = TRUE) %>%
  grep(., pattern = "orthologs", value = TRUE, invert = TRUE)

# Create variable names for each file/strain
annoNames <- annoFiles %>% map_chr(
  ~str_remove_all(., pattern = "rawData/Pseudomonas_aeruginosa_|\\.txt")
)

# Read in the files, and set the names of the list
annoData_1 <- annoFiles %>% map(
  ~read_tsv(., comment = "#")
) %>% set_names(annoNames)


# Cleaning
# ~~~~~~~~
# Next we select and rename the desired columns. Here we also replace any `NA`s
# in the name column with an empty string to make for a cleaner output.

annoData_2 <- annoData_1 %>% map(
  ~select(.,
          `Locus Tag`,
          Start,
          End,
          Strand,
          Name,
          "Description" = `Product Name`,
          Accession,
          `Nucleotide Sequence`,
          `Amino Acid Sequence`)
)

# Fix NA's by replacing with an empty string
annoData_3 <- annoData_2 %>% map(
  ~mutate(., Name = case_when(is.na(Name) ~ "", TRUE ~ Name))
)

# Saving
# ~~~~~~
# Finally we can save each annotation table into an RDS object for easy loading
# by the app.
map2(
  annoData_3,
  names(annoData_3),
  ~saveRDS(.x, file = paste0("data/annotations_", .y, ".rds"))
)


# Download Ortholog Mappings
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Go to [OrtholugeDB](http://pseudoluge.pseudomonas.com/) and follow the steps
# below to download the necessary files
# - Click the `Start now!` button to "Obtain orthologs between two genomes"
# - In the `Genome 1` box, type and select PAO1. Then type and select PA14 in
#   the `Genome 2` box. Hit `Submit`.
# - On the results page click the `TAB` button to download the full ortholog
#   table. Once downloaded rename the file "orthologs_pao1_pa14.txt" and place
#   into the `rawData` folder.
# - Repeat for the other two mappings, generating files with the names
#   "orthologs_pao1_lesb58.txt" and "orthologs_pa14_lesb58.txt".

# Function to assist in renaming columns
renameOrthoCols <- function(dataFrame, strains) {
  dataFrame %>%
    rename(!!sym(strains[1]) := `Locus Tag (Strain 1)`,
           !!sym(strains[2]) := `Locus Tag (Strain 2)`)
}

# List the ortholog files to be used. Note the use of non-inverted grep to get
# just the ortholog files and not the annotations.
orthoFiles <- list.files("rawData/", pattern = "txt$") %>%
  grep(., pattern = "orthologs", value = TRUE, invert = FALSE)

# Create variable names for each file/strain
orthoNames <- orthoFiles %>%
  map_chr(~str_remove_all(., pattern = "orthologs_|\\.txt") %>% toupper())

# Read in the files, and set the names of the list
orthoData_1 <- orthoFiles %>% map(
  ~read_tsv(paste0("rawData/", .), comment = "#")
) %>% set_names(orthoNames)


# Cleaning Ortholog Files
# ~~~~~~~~~~~~~~~~~~~~~~~
# Select desired columns and fix LESB58 IDs
orthoData_2 <- orthoData_1 %>% map(
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
strainPairs <- names(orthoData_2) %>% map(
  ~toupper(.) %>% str_split(pattern = "_", n = 2)
) %>% flatten()

# Rename using strain pairs defined above
orthoData_3 <- map2(orthoData_2, strainPairs, ~renameOrthoCols(.x, strains = .y))

# Fix column names for joining later
colnames(orthoData_3$PA14_LESB58) <- paste0(colnames(orthoData_3$PA14_LESB58), " Locus Tag")
colnames(orthoData_3$PAO1_LESB58) <- paste0(colnames(orthoData_3$PAO1_LESB58), " Locus Tag")
colnames(orthoData_3$PAO1_PA14) <- paste0(colnames(orthoData_3$PAO1_PA14), " Locus Tag")


# Adding annotations
# ~~~~~~~~~~~~~~~~~~
# Add the previously cleaned and tidied annotations to the ortholog mappings
annoData_4 <- annoData_3 %>% map(~select(., -c(`Nucleotide Sequence`, `Amino Acid Sequence`)))

# Fix column names for clean joins
colnames(annoData_4$LESB58_125) <- paste0("LESB58 ", colnames(annoData_4$LESB58_125))
colnames(annoData_4$PAO1_107) <- paste0("PAO1 ", colnames(annoData_4$PAO1_107))
colnames(annoData_4$`UCBPP-PA14_109`) <- paste0("PA14 ", colnames(annoData_4$`UCBPP-PA14_109`))

# Run all the joins inside a list
orthoData_4 <- list(
  PA14_LESB58 = orthoData_3$PA14_LESB58 %>%
    left_join(., annoData_4$`UCBPP-PA14_109`) %>%
    left_join(., annoData_4$LESB58_125),

  PAO1_LESB58 = orthoData_3$PAO1_LESB58 %>%
    left_join(., annoData_4$PAO1_107) %>%
    left_join(., annoData_4$LESB58_125),

  PAO1_PA14 = orthoData_3$PAO1_PA14 %>%
    left_join(., annoData_4$PAO1_107) %>%
    left_join(., annoData_4$`UCBPP-PA14_109`)
)


# Saving the Ortholog Files
# ~~~~~~~~~~~~~~~~~~~~~~~~~
map2(
  orthoData_4,
  names(orthoData_4),
  ~saveRDS(.x, file = paste0("data/orthologs_", .y, ".rds"))
)
