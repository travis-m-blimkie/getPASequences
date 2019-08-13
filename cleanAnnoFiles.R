
library(tidyverse)


fileNames <- list.files("../Downloads/", pattern = ".txt$")


varNames <- fileNames %>% 
  map_chr(~str_replace(., pattern = "\\.txt", replacement = ""))


myFiles <- fileNames %>% 
  map(~read_tsv(paste0("../Downloads/", .), comment = "#")) %>% 
  set_names(varNames)


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


fixName <- selectCols %>% 
  map(~mutate(., Name = case_when(is.na(Name) ~ Locus_Tag, TRUE ~ Name)))


map2(fixName, names(fixName), 
     ~saveRDS(.x, file = paste0("../Downloads/", .y, ".Rds")))
