library(doParallel)
library(tictoc)

# The function detectCores finds the number of cores
# available on the machine. We update the "Cores"-value
# to the minimum of the chosen cores and the available cores.
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)

# Instantiate the cores:
cl <- makeCluster(Cores)

# Next we register the cluster..
registerDoParallel(cl)

# Take the time as before:
tic(paste0("Parallel loop, ", Cores, " cores"))

x <- foreach(i = 1:nrow(Governors),
                  .packages = c("tidyverse",
                                "pdftools",
                                "qdapRegex")
                  ) %dopar%
  #for (i in 1) {        # iterate through FED members
    for (j in Governors$TermStart[i]:Governors$TermEnd[i]) {
      for (k in 1:nrow(allWebURL)) {
        
        if (allWebURL$year[k] == j) {
          
          webURL <- allWebURL$links[k]
          
          thepage <- pdf_text(webURL)
          
          transcript <- thepage %>% 
            gsub("^.*\\d{2,}.?\\n\\n\\n\\n", "", .) %>% 
            gsub('"', "", .) %>% 
            paste(collapse = "") %>% 
            str_squish() %>%
            gsub("MR", "NO", .) %>% 
            gsub("MS", "NO", .) %>% 
            gsub("CHAIR", "NO", .) %>% 
            gsub("VICE", "NO", .) %>% 
            gsub("PARTICIPANT", "NO", .) %>% 
            gsub("END", "NO", .) %>% 
            rm_between(paste0(Governors$Name[i],"."), "NO", extract = TRUE) %>% 
            paste(collapse = "") %>% 
            gsub('\", \n\"', " ", .) %>% 
            gsub('\", \"', " ", .) %>% 
            gsub('\"\n)', "", .) %>% 
            gsub('\")', "", .)
          
          write_lines(transcript, paste0("Transcripts/",
                                         Governors$Name[i],
                                         "_",
                                         allWebURL$dates[k],
                                         ".txt"))
        }
        
      }
      
    }
  #}


# Now that we're done, we close off the clusters
stopCluster(cl)

toc(log = TRUE)

# We get many documents with NA. Remove the files that only have 3 bytes of content
setwd("~/Documents/NHH/H21/BAN432/Exam/Transcripts")
doc <- list.files(pattern = "*.txt")
inds <- file.size(doc) == 3
file.remove(doc[inds])





# ----------- NOTES ------------

for (i in 1:nrow(Governors)) {        # iterate through FED members
  for (j in Governors$TermStart[i]:Governors$TermEnd[i]) {
    for (k in 1:nrow(allWebURL)) {
      
      if (allWebURL$year[k] == j) {
        
        webURL <- allWebURL$links[k]
        
        thepage <- pdf_text(webURL)
        
        x <- thepage %>% 
          gsub("^.*\\d{2,}.?\\n\\n\\n\\n", "", .) %>% 
          gsub('"', "", .) %>% 
          paste(collapse = "") %>% 
          str_squish() %>%
          gsub("MR", "NO", .) %>% 
          gsub("MS", "NO", .) %>% 
          gsub("CHAIR", "NO", .) %>% 
          gsub("VICE", "NO", .) %>% 
          gsub("PARTICIPANT", "NO", .) %>% 
          gsub("END", "NO", .) %>% 
          rm_between(paste0(Governors$Name[i],"."), "NO", extract = TRUE) %>% 
          paste(collapse = "") %>% 
          gsub('\", \n\"', " ", .) %>% 
          gsub('\", \"', " ", .) %>% 
          gsub('\"\n)', "", .) %>% 
          gsub('\")', "", .)
        
        write_lines(x, paste0("Transcripts/",Governors$Name[i],"_", allWebURL$dates[k], ".txt"))
      }
      
    }
    
  }
}


for (i in allWebURL$dates[68:72]) {
  webURL <- paste0("https://www.federalreserve.gov/monetarypolicy/files/FOMC",
                   i,
                   "meeting.pdf")
  
  thepage <- pdf_text(webURL)
  
  x <- thepage %>% 
    gsub("^.*\\d{2,}.?\\n\\n\\n\\n", "", .) %>% 
    gsub('"', "", .) %>% 
    #gsub("([A-z])\\n\\n([A-z])", "\\1 \\2", .) %>% 
    paste(collapse = "") %>% 
    str_squish() %>%
    gsub("MR", "NO", .) %>% 
    gsub("MS", "NO", .) %>% 
    gsub("CHAIR", "NO", .) %>% 
    gsub("VICE", "NO", .) %>% 
    gsub("PARTICIPANT", "NO", .) %>% 
    gsub("END", "NO", .) %>% 
    rm_between("WALLICH", "NO", extract = TRUE) %>% 
    paste(collapse = "") %>% 
    gsub('\", \n\"', " ", .) %>% 
    gsub('\", \"', " ", .) %>% 
    gsub('\"\n)', "", .) %>% 
    gsub('\")', "", .)
  
  #print(x)
  write_lines(x, paste0("Transcripts/","WALLICH_", i, ".txt"))
}
webURL <- "https://www.federalreserve.gov/monetarypolicy/files/FOMC20150128meeting.pdf"
thepage <- pdf_text(webURL)

readTranscript <- function(yearMeeting) {
  if (allWebURL$year == yearMeeting) {
    thepage <- pdf_text(webURL)
    
    thepage %>% 
      gsub("^January.*\\n\\n\\n\\n", " ", .) %>% 
      gsub("([A-z])\\n\\n([A-z])", "\\1 \\2", .) %>% 
      paste(collapse = "") %>% 
      str_squish() %>% 
      gsub("MR", "NO", .) %>% 
      gsub("MS", "NO", .) %>% 
      gsub("CHAIR", "NO", .) %>% 
      gsub("VICE", "NO", .) %>% 
      rm_between("BRAINARD.", "NO", extract = TRUE) %>% 
      paste(collapse = "") %>% 
      gsub('\", \n\"', " ", .) %>% 
      gsub('\", \"', " ", .) %>% 
      gsub('\"\n)', "", .) %>% 
      gsub('\")', "", .) %>% 
      gsub('^c\\(\"', "", .) %>% 
      gsub("\\d", "", .) %>% 
      tolower()
       
    
  } else {
    next
  }
}


