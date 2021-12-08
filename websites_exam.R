# Try to download websites
require(tidyverse)

findWebURL <- function(first, last) {
  websites <- data.frame()
  
  for (year in first:last) {
    webURL <- paste0("https://www.federalreserve.gov/monetarypolicy/fomchistorical",
                     year,
                     ".htm")
    
    # load in the webpage
    thepage <- readLines(webURL, warn = FALSE)
    
    # have searched through the source code and located a line only represented
    # in the lines we want to extract. use grep function to extract these.
    goodlines <- ">Transcript "
    
    # clean the links
    links <- thepage %>% 
      grep(goodlines, ., value = TRUE) %>% 
      gsub('<p><a href=\\"', "", .) %>% 
      gsub('\\">Transcript.*', "", .) %>% 
      str_squish()
    
    year <- thepage %>% 
      grep("<h3>\\d{4}</h3>", ., value = TRUE) %>% 
      gsub("<h3>(\\d{4})</h3>", "\\1", .) %>% 
      str_squish()
    
    dates <- thepage %>% 
      grep(goodlines, ., value = TRUE) %>% 
      gsub('<p><a href=\\"', "", .) %>% 
      gsub('\\">Transcript.*', "", .) %>% 
      gsub("\\D", "",.) %>%
      str_squish()
 
    # make it into a dataframe
    df <- data.frame(links, year, dates)
    
    # insert the dataframe at the end of the list for every loop
    websites <- rbind(websites, df)
  }
  
  for (i in 1:nrow(websites)) {
    websites$links[i] <- paste0("https://www.federalreserve.gov",
                                              websites$links[i])
  }
  
  return(websites)
}

allWebURL <- findWebURL(1980,2015)





































for (year in 2015) {
  for (month in 1:12) {
    for (date in 1:31) {
      # when month is smaller than 10 we need a leading zero
      if (month < 10) {
        if (date < 10) {
          webURL <- paste0("https://www.federalreserve.gov/monetarypolicy/files/FOMC",
                           year,
                           "0", 
                           month,
                           "0",
                           date,
                           "meeting.pdf")
        } else {
          webURL <- paste0("https://www.federalreserve.gov/monetarypolicy/files/FOMC",
                           year,
                           "0", 
                           month,
                           date,
                           "meeting.pdf")
        }
        
      } else if (month >= 10) {
        if (date < 10) {
          webURL <- paste0("https://www.federalreserve.gov/monetarypolicy/files/FOMC",
                           year,
                           month,
                           "0",
                           date,
                           "meeting.pdf")
        } else {
          webURL <- paste0("https://www.federalreserve.gov/monetarypolicy/files/FOMC",
                           year,
                           month,
                           date,
                           "meeting.pdf")
        }
      }
      # load in the webpage
      #thepage <- pdf_text(webURL)
      print(webURL)
      
      
      
      # insert the dataframe at the end of the list for every loo
    }
  }
}



