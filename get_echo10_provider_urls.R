# Name:         get_echo10_provider_urls.R
# Author:       Adam W. Sisco
# Last updated: 12 Jul 2019
# Description:  The get_urls() function returns a data frame of every OnlineResource in the CMR for an ECHO10 provider.
#               It accepts the provider ID of the ECHO10 provider of interest. For example, to get the URLs for GHRC:
#               
#               get_urls(provider_id = "GHRC")

#================================================

library(xml2) #For working with XML
library(dplyr) #For the group_by and summarise functions
library(RJSONIO) #For working with JSON

#================================================

get_echo10_provider_urls <- function(provider_id = ""){
  
  #Set up empty vectors
  concept_id <- vector()
  url_vector <- vector()
  type_vector <- vector()
  mime_vector <- vector()
  id_vector <- vector()
  
  #Read the provider holdings JSON
  master_json <- fromJSON(paste0("https://cmr.earthdata.nasa.gov/search/provider_holdings.json?provider_id=", provider_id))
  
  #Extract the concept IDs
  for(i in 1:length(master_json)){
    concept_id[i] <- master_json[[i]]['concept-id'][[1]]
  }
  
  #For each concept ID, read the ECHO10 XML
  for(i in 1:length(concept_id)){
    xml_result <- read_xml(paste('https://cmr.earthdata.nasa.gov/search/concepts/',
                                 concept_id[i], '.echo10', sep = ''))
    ors <- xml_find_all(xml_result, '/Collection/OnlineResources/OnlineResource') #Find all OnlineResource instances
    idText <- concept_id[i] #Grab the concept ID for use in the next loop
    
    #For each OnlineResource, read the URL, Type, and MimeType elements
    for(i in 1:length(ors)){
      url <- xml_find_all(ors[i], ".//URL") #Get the URL
      type <- xml_find_all(ors[i], ".//Type") 
      mime <- xml_find_all(ors[i], ".//MimeType")
      
      urlText <- xml_text(url) #Change the URL to character
      typeText <- xml_text(type) #Change the URL type to character
      
      #If a mime type is present, change it to character; NA otherwise
      if(length(mime)==1){
        mimeText <- xml_text(mime)
      } else{
        mimeText <- NA
      }
      
      #Add extracted items to the appropriate vectors
      url_vector <- append(url_vector, urlText)
      type_vector <- append(type_vector, typeText)
      id_vector <- append(id_vector, idText)
      mime_vector <- append(mime_vector, mimeText)
    }
    
    url_df <- data.frame(id_vector, url_vector, type_vector, mime_vector) #Build the data frame
    colnames(url_df) <- c("concept_id", "url", "type", "mime") #Change the column names
    url_df$type <- toupper(url_df$type) #Change all URL types to upper case
  }
  
  return(url_df)
}

#================================================

ghrc <- get_echo10_provider_urls(provider_id = "GHRC") #Get the GHRC URLs; this may take a couple minutes
dim(ghrc) #View dimensions of the data frame
ghrc %>% group_by(type) %>% summarise(type_n = n()) %>% data.frame() -> type_summary #Get totals of all types used

#Write totals to a tab-delimited text file
write.table(type_summary, file = "12jul2019_ghrc_type_summary.txt", quote = TRUE, row.names = FALSE, sep = "\t")
