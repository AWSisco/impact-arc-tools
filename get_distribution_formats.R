# Name:         get_distribution_formats.R
# Author:       Adam W. Sisco
# Last updated: 12 Jul 2019
# Description:  The get_distribution_formats() function returns a data frame of the data format listed in each of a provider's
#               UMM records. Simply specify the provider ID as such:
#               
#               get_distrubution_formats(prpvoider_id = "GHRC")
#
#               ***THE FUNCTION CURRENTLY READS ONLY THE FIRST FORMAT IT SEES. PROVIDERS THAT SUPPLY MORE THAN ONE DATA FORMAT
#               WILL RETURN INACCURATE RESULTS***

#================================================

library(dplyr) #For the group_by and summarise functions
library(RJSONIO) #For working with JSON

#================================================

get_distribution_formats <- function(provider_id = ""){
  
  #Set up empty vectors
  concept_id <- vector()
  distribution_format <- vector()
  
  #Read the provider holdings JSON
  master_json <- fromJSON(paste0('https://cmr.earthdata.nasa.gov/search/provider_holdings.json?provider_id=', provider_id))
  
  #Extract the concept IDs
  for(i in 1:length(master_json)){
    concept_id[i] <- master_json[[i]]['concept-id'][[1]]
  }
  
  #For each concept ID, read umm-json record
  for(i in 1:length(concept_id)){
    umm_json <- fromJSON(paste('https://cmr.earthdata.nasa.gov/search/concepts/', 
                               concept_id[i], '.umm-json', sep = ''))
    
    #Extract the data format as a character string
    distribution_format[i] <- as.character(umm_json$ArchiveAndDistributionInformation$FileDistributionInformation[[1]]['Format'])
  }
  
  return_df <- data.frame(concept_id, distribution_format)
  return_df$distribution_format <- toupper(return_df$distribution_format) #Change all formats to upper case
  return(return_df)
}

#================================================

formats <- get_distribution_formats(provider_id = "GHRC") #Get the GHRC data formats; this may take a couple minutes
formats %>% group_by(distribution_format) %>% summarise(format_n = n()) %>% data.frame() -> format_summary #Get totals of all types used

#Write totals to a tab delimited text file
write.table(format_summary, file = "12jul2019_ghrc_format_summary.txt", quote = TRUE, row.names = FALSE, sep = "\t")
