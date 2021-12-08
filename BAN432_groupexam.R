# BAN432 Group Exam

# set working directory
setwd("~/Documents/NHH/H21/BAN432/Exam/")

# load packages
require(pdftools)
require(dplyr)
require(qdapRegex)
require(textreadr)
require(tidyverse)
require(RCurl)
require(stringr)
require(tidytext)
require(doParallel)
require(tictoc)
require(readr)
require(tibble)
require(tidyr)
require(tm)
require(slam)
require(quanteda)
require(gofastr)
require(qdap)
require(ngram)
require(stopwords)
require(purrr)
require(topicmodels)
require(wordcloud)
require(udpipe)
require(ggplot2)
require(RColorBrewer)
require(htmltab)


# --------- FIND ALL TRANSCRIPT WEB PAGES TO DOWNLOAD ------------

# make function that finds URL for all transcripts between the years used as input
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
    
    # also want to extract the year of the meeting. Locate line with grep and clean with gsub
    year <- thepage %>% 
      grep("<h3>\\d{4}</h3>", ., value = TRUE) %>% 
      gsub("<h3>(\\d{4})</h3>", "\\1", .) %>% 
      str_squish()
    
    # also want to extract the full date of the meetin. Locate line and clean with gsub
    dates <- thepage %>% 
      grep(goodlines, ., value = TRUE) %>% 
      gsub('<p><a href=\\"', "", .) %>% 
      gsub('\\">Transcript.*', "", .) %>% 
      gsub("\\D", "",.) %>%
      str_squish()
    
    # make it into a dataframe
    df <- data.frame(links, year, dates)
    
    # insert the dataframe at the end of the dataframe for every loop
    websites <- rbind(websites, df)
  }
  
  # change the link column so the start of the URL is added in every row
  for (i in 1:nrow(websites)) {
    websites$links[i] <- paste0("https://www.federalreserve.gov",
                                websites$links[i])
  }
  
  return(websites)
}

# run the function with the years 2000 and 2015 as input and save it in allWebURL
allWebURL <- findWebURL(2000,2015)


# ---------- FED Members -----------
# Creating a data frame with all the Board members in the FED

#Collecting the table of Board members from Wikipedia 
Governors <- htmltab("https://en.wikipedia.org/wiki/Federal_Reserve_Board_of_Governors", 
                     4, #Table nr 4 
                     #Dont delete any of the data if there is empty rows or cols
                     rm_nodata_cols = F,rm_nodata_rows = F) %>% 
  mutate(Party = NA) %>% #Creating a new column 
  as_tibble() 

#Deleting Columns that we dont use 
Governors <- Governors %>% select(-"Regional Bank", -"Notes", -"Years served") 

# Cleaning the text
Governors$`Term start` <- Governors$`Term start` %>% gsub(".*,", "",.)
Governors$`Term end` <- Governors$`Term end` %>% gsub(".*,", "",.)
Governors$Initialappointment <- Governors$Initialappointment %>% sub('^.* ([[:alnum:]]+)$', '\\1',.)
Governors$Name <- Governors$Name %>% sub('^.* ([[:alnum:]]+)$', '\\1',.) %>% toupper()

#Creating two vectors one with the Republican presidents and one with the Democratic ones 
Democrat <- c("Wilson", "Roosevelt", "Obama", "Truman", "Kennedy", "Johnson", "Carter", "Clinton")
Republican <- c("Bush", "Trump", "Harding", "Coolidge", "Hoover", "Eisenhower", "Nixon", "Ford", "Reagan")

#Looping over to check if the presidents are Democratic or Republic 
for(i in 1:nrow(Governors)) {
  if (Governors$Initialappointment[i] %in% Republican) {
    Governors$Party[i] <- "R"} 
  else if(Governors$Initialappointment[i] %in% Democrat) {
    Governors$Party[i] <- "D" }
  else {Governors$Party[i] = NA}
  rm(i)
}

#Changing the Names 
colnames(Governors)<- c("Name","TermStart","TermEnd", "appointment", "Party")

#Changing the Term End date from character to numeric 
Governors$TermEnd <- as.numeric(as.character(Governors$TermEnd)) 
Governors$TermStart <- as.numeric(as.character(Governors$TermStart))

#Filtering out all the lines where they ended their term before 2000
Governors <- Governors %>% filter(TermEnd > 2000) %>% filter(TermStart <2016)

rm(Democrat)
rm(Republican)





# ----------- PARALLEL WRITE TRANSCRIPTS TO TXT FILES --------
# in this part the transcripts are cleaned and saved to your local computer.
# may take some time. Took between 10 and 30 min on different computers. Because
# of this, we have appended all the files in wiseflow in addition.

# make directory for files
dir.create("Transcripts")

# set the maximum number of cores to be equal to eight. The detectCores
# function finds the number of cores that is available on your machine
# we choose the minimum value of the available cores and the maximum cores of eight
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)

# we initiate the cores
cl <- makeCluster(Cores)

# we then register the cluster
registerDoParallel(cl)

# this takes the time for the loop. This is just for curiosity.
tic(paste0("Parallel loop, ", Cores, " cores"))

# run the program in the loop with multiple cores
x <- foreach(i = 1:nrow(Governors), # iterate through FED members
             .packages = c("tidyverse",
                           "pdftools",
                           "qdapRegex")
) %dopar%
  # iterate from the FED members start year to end year
  for (j in Governors$TermStart[i]:Governors$TermEnd[i]) {
  # iterate through all web URLs in the FED members term period
      for (k in 1:nrow(allWebURL)) {
      
        # if the year for the FED meeting is equal to the year in the iteration
        # we run the program. If not, nothing happens
      if (allWebURL$year[k] == j) {
        
        # choose the URL to use for every specific iteration
        webURL <- allWebURL$links[k]
        
        # read the page from the URL
        thepage <- pdf_text(webURL)
        
        # clean up the transcript we load in. And only end up with the transcript
        # from one person from one meeting (for every iteration).
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
          gsub('\")', "", .) %>% 
          gsub('^c\\(\"', "", .) %>% 
          gsub("\\d", "", .) %>% 
          tolower()
        
        # we save the transcript we find in a txt file in the map "Transcript"
        # in our working directory
        write_lines(transcript, paste0("Transcripts/",
                                       Governors$Name[i],
                                       "_",
                                       allWebURL$dates[k],
                                       ".txt"))
      }
      
    }
    
  }


# when the loop is finished, we stop the cluster
stopCluster(cl)

# stop the timer and print out how much time it used
toc(log = TRUE)

# We get many documents with NA. Remove the files that only have 3 bytes of content
setwd(paste0(getwd(),"/Transcripts") )
doc <- list.files(pattern = "*.txt")
inds <- file.size(doc) == 3
file.remove(doc[inds])

setwd("")


# --------- RETRIEVE CONGRESS SPEACHES -----------
# make list to use to collect data in loop
datalist_speakerMap <- list()
datalist_speeches <- list()

# Collecting data and storing it in a df (must have all the files in your directory)
for (i in 107:114) {
  speakerMap <- read_delim(paste0(getwd(),"/congressional speech/",i,"_SpeakerMap.txt"),
                           delim = "|")
  datalist_speakerMap[[length(datalist_speakerMap) + 1]] <- speakerMap
}
# make it a dataframe
df_speakerMap <- do.call(rbind, datalist_speakerMap)

for (i in 107:114) {
  speeches <- read_delim(paste0(getwd(),"/congressional speech","/speeches_",i,".txt"),
                         delim = "|")
  datalist_speeches[[length(datalist_speeches) + 1]] <- speeches
}
# make it a dataframe
df_speeches <- do.call(rbind, datalist_speeches)

# Joining the two data frames and filtering out what is not needed
final_df <- full_join(df_speakerMap, df_speeches) %>% 
  select(party, speech) %>% 
  na.omit() %>% #removing all rows where political party cannot be identified
  subset(party != "A" & party != "I" & party != "P") # removing other political parties than R and D


# List of the two df containing democrat speech and republican speech
df_list <- split(final_df, final_df$party)


rm(datalist_speakerMap)
rm(datalist_speeches)
rm(df_speakerMap)
rm(df_speeches)
rm(final_df)
rm(speakerMap)
rm(speeches)
rm(i)

# ---------- MAKE CUSTOM STOPWORDS -------------
# Us states 
us.states <- datasets::state.name
us.states <- gsub("\\s", ".", us.states) %>% tolower() %>% as.vector() #take away space and turn to lower 

# Governors --> make sure that there is no duplicates
unique_Governors <- unique(Governors$Name) %>% tolower() %>% as_vector()

# standard stopwords
stopwords <- stopwords() %>% as.vector()

# Words from web page: 
NewWords <- c("absent", "committee", "gentlelady", "hereabout", "hereinafter", "hereto", "herewith",
              "nay", "pro", "sir", "thereabout", "therebeforn", "therein", "theretofore",
              "therewithal", "whereat", "whereinto", "whereupon", "yea", "adjourn", "con", 
              "gentleman", "hereafter", "hereinbefore", "heretofore", "month", "none", 
              "republican", "speak", "thereafter", "thereby", "thereinafter", "thereunder",
              "today", "whereby", "whereof", "wherever", "yes", "ask", "democrat", "gentlemen",
              "hereat", "hereinto", "hereunder", "mr", "Mr", "now", "say", "speaker", "thereagainst",
              "therefor", "thereof", "thereunto", "whereabouts", "wherefore", "whereon", "wherewith",
              "yield", "can", "etc", "gentlewoman", "hereby", "hereof", "hereunto", "mrs", "part",
              "senator", "tell", "thereat", "therefore", "thereon", "thereupon", "whereafter", 
              "wherefrom", "whereto", "wherewithal", "chairman", "gentleladies", "gentlewomen",
              "herein", "hereon", "hereupon", "nai", "per", "shall", "thank", "therebefore",
              "therefrom", "thereto", "therewith", "whereas", "wherein", "whereunder", "will", "President",
              "to")


# add them to otherTerms so we also remove these in dtmFIveHundred
#Final_Stopwords <- append(stopwords, us.states, unique_Governors)
Final_Stopwords <- c(stopwords, us.states, unique_Governors, NewWords)

rm(NewWords)
rm(stopwords)
rm(unique_Governors)
rm(us.states)

# ---------- MAKE DTM -------------
# Democrat speech corpus and document term matrix
dem_corpus <- Corpus(VectorSource(pull(df_list$D, speech)))

# make dtm matrix with removal of custom stopwords, remove numbers, punctuation
# and make reasonable bounds to only include words with length 4 to 50 and 
# words that appear in more than 100 documents
dtm_dem <- DocumentTermMatrix(dem_corpus, 
                              control = list(
                                removePunctuation = T,
                                removeNumbers = T,
                                stopwords = Final_Stopwords,
                                stemming = F,
                                tolower = F,
                                stripWhitespace = T,
                                wordLengths = c(4, 50),
                                bounds = list(global = c(100, Inf))
                                ))

# Republican speech corpus and document term matrix
rep_corpus <- Corpus(VectorSource(pull(df_list$R, speech)))

# Same process as for the dtm_dem
dtm_rep <- DocumentTermMatrix(rep_corpus, 
                              control = list(
                                removePunctuation = T,
                                removeNumbers = T,
                                stopwords = Final_Stopwords,
                                stemming = F,
                                tolower = F,
                                stripWhitespace = T,
                                wordLengths = c(4, 50),
                                bounds = list(global = c(100, Inf))
                                ))


# ---------- CREATE TOPIC MODEL ---------

# ---- Topic model Democratic
# filter out empty/sparse documents
dtm_dem_topic <- dtm_dem[row_sums(dtm_dem) > 10,]

# estimate topic model
topic_dem <- LDA(dtm_dem_topic,  # insert our dtm
                 k = 20, # specify to find 20 topics
                 method = "Gibbs",
                 control = list(
                   seed = 123,     # for replication
                   burnin = 100,  # how often sampled before estimation recorded???
                   iter = 50,     # use 50 iterations
                   keep = 1,    # save some additional data
                   save = F,     # save logLiklihood
                   verbose = 10  # repoport progress every 10 iterations
                 ))


# make term distribution for all 20 topics
beta_dem <- exp(topic_dem@beta) 

# specify which topic you want to look at for i in "beta_rep[i,]". See 100 most
# frequent words
head(topic_dem@terms[order(beta_dem[14,], decreasing = T)], 100)

# inspect the 10 most frequent terms in each topic
apply(topic_dem@beta, 1, function(x) head(topic_dem@terms[order(x, decreasing = T)],10))



#wordcloud with term distribution for topic 3 
terms.top.40 <- head(topic_dem@terms[order(beta_dem[3,], decreasing = T)], 50) 
prob.top.40 <- head(sort(beta_dem[3,], decreasing = T), 50)

# Make a wordcloud 
filename <- "wordcloud_Economy.png"
png(filename, 2000, 2000)
wordcloud(words = terms.top.40, 
          freq = prob.top.40, 
          random.order = F, 
          scale = c(20, 3)) 
dev.off()


# ---- Topic model Republican 

# filter out empty/sparse documents
dtm_rep_topic <- dtm_rep[row_sums(dtm_rep) > 10,]

# estimate topic model
topic_rep <- LDA(dtm_rep_topic,  # insert our dtm
                 k = 20,         # specify to find 20 topics
                 method = "Gibbs", # use Gibbs method for fitting
                 control = list(
                   seed = 1234,    # for replication
                   burnin = 100,  # how often sampled before estimation recorded????
                   iter = 50,  # use 50 iterations
                   keep = 1,    # save some additional data
                   save = F,     # save logLiklihood
                   verbose = 10  # repoport progress every 10 iterations
                 ))


# make term distribution for all 20 topics
beta_rep <- exp(topic_rep@beta)

# specify which topic you want to look at for i in "beta_rep[i,]". See 100 most
# frequent words
head(topic_rep@terms[order(beta_rep[18,], decreasing = T)], 100)

# inspect the 10 most frequent terms in each topic
apply(topic_rep@beta, 1, function(x) head(topic_rep@terms[order(x, decreasing = T)],10))



# ----------- KWIC with WORDCLOUD ----------

# In the following section we will do a qualitative analysis of the democratic
# and republican speeches. We will use the words found in the topic analysis
# in a kwic analysis. We will check for partisanship and include the 20 most frequent
# bigrams, but exclude words based on if both parties use them and if it clearly
# is not a partisan word. All words in the vector "pattern_terms" is used, but the
# model is not automatic and you therefore have to go through the whole process
# term by term. Because of this we have appended the final dataframe of bigrams
# for the democratic and republican speeches in wiseflow


# make empty dataframes to use below
dem_bigrams <- data.frame()
rep_bigrams <- data.frame()
newdf <- tibble()

# terms that we found through topic analysis and that we use in the following section
# to use as pattern in a kwic analysis. We look at the words on each side of these
# terms and use a qualitative analysis to find bigrams to use to make a score for
# partisanship.
pattern_terms <- c("budget", "spending", "increase", "deficit", "rate", "debt", 
               "tax", "economy", "raise", "fund", "government", "cut", "income",
               "revenue", "Obama", "crisis", "stimulus", "benefit", "growth", "interest",
               "Social", "future", "reduction", "job", "business", "investment", "bank",
               "risk", "employer", "labor", "credit", "trade", "Congress","economic",  "cut", 
               "responsibility","cost","trust", "reduction", "policy",
               "fiscal", "bill", "paid", "companies", "interest", "financ",
               "Internet", "reform", "corporations", "regulatory", "loan", "work", "politic",
               "fair", "regulation", "Homeland", "compani", "wage", "oil", "climat",
               "Private",  "energy", "health", "unemploy", "care", "job", "education")


# DEMOCRATS
# make all democratic speeches into a character vector
x <- pull(df_list$D, speech)

# tokenize the vector of democratic speeches
tokenDem <- tokens(x,
                   what = "word",
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_numbers = TRUE)

# use kwic function on all the words found in the topic model analysis. Insert
# them into pattern and we then find all words on both sides of the word. Use
# qualitative analysis to make a list of words that is partisan against democratics
kwicDem <- kwic(tokenDem, 
                pattern = "Congress", 
                window = 1,
                valuetype = "regex",
                case_insensitive = TRUE)

# REPUBLICANS 
# make all republican speeches into a character vector
y <- pull(df_list$R, speech)

# tokenize the vector of democratic speeches
tokenRep <- tokens(y,
                   what = "word",
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_numbers = TRUE)

# use kwic function on all the words found in the topic model analysis. Insert
# them into pattern and we then find all words on both sides of the word. Use
# qualitative analysis to make a list of words that is partisan against republicans
kwicRep <- kwic(tokenRep, 
                pattern = "Congress", 
                window = 1,
                valuetype = "regex",
                case_insensitive = TRUE)




#### ---- Left context word for democratic speech
# Remove some of the words from 'right.context.freq' that obviously don't have
# anything to do with the pattern.
custom.stopwords1 <- c("free", "international", "world", "u.s" ,"unfair", "fair", "publicly",
                       "federal", "normal")

kwicDem %>%
  as_tibble() %>%                                 # make it into tibble
  select(pre) %>%                                 # only select the words in front of pattern
  unnest_tokens(output = tokens, 
                input = pre,
                to_lower = TRUE) %>%              # unnest tokens from column
  filter(!tokens %in% Final_Stopwords) %>%        # filter out the stopword list we made
 filter(!tokens %in% custom.stopwords1) %>%       # filter out custom stopwords for specific pattern
  count(tokens, sort = TRUE) -> left.context.freq # count the tokens from the text

# Make a wordcloud
filename <- "wordcloud1.png"
png(filename, 2000, 2000)
wordcloud(words = left.context.freq$tokens[1:20],
          freq  = left.context.freq$n[1:20],
          scale = c(20,3))
dev.off()

# combine the 20 first tokens with pattern and add it with its frequency to the dataframe newdf
for (i in 1:20) {
  newdf[i, 1] <- paste(left.context.freq$tokens[i],
                       kwicDem$pattern[i])
  newdf[i, 2] <- left.context.freq$n[i]
  
}
# add the new bigrams to the final dataframe dem_bigrams
dem_bigrams <- bind_rows(dem_bigrams, newdf)

# check that all bigrams are unique and remove duplicates
dem_bigrams <- unique(dem_bigrams)






#### ---- Left context word for republican speech
# Do the same process for the republican speeches. In our qualitative analysis we
# compare the kwic for democratic and republican speeches. Remove those that is
# very apparent for both or is clarly not partisan

# Remove some of the words from 'right.context.freq' that obviously don't have
# anything to do with pattern.
custom.stopwords2 <-  c("free", "international", "world", "u.s" ,"unfair", "fair", "publicly",
                        "federal", " normal")

kwicRep %>%
  as_tibble() %>%                                 # make it into tibble
  select(pre) %>%                                 # only select the words in front of pattern
  unnest_tokens(output = tokens,
                input = pre,
                to_lower = TRUE) %>%              # unnest tokens from column
  filter(!tokens %in% Final_Stopwords) %>%        # filter out the stopword list we made
  filter(!tokens %in% custom.stopwords2) %>%      # filter out custom stopwords for specific pattern
  count(tokens, sort = TRUE) -> left.context.freq2 # count the tokens from the text

# Make a wordcloud
filename <- "wordcloud2.png"
png(filename, 2000, 2000)
wordcloud(words = left.context.freq2$tokens[1:20],
          freq  = left.context.freq2$n[1:20],
          scale = c(20,3))
dev.off()

# combine the 20 first tokens with pattern and add it with its frequency to the dataframe newdf
for (i in 1:20) {
  newdf[i, 1] <- paste(left.context.freq2$tokens[i],
                                     kwicRep$pattern[i])
  newdf[i, 2] <- left.context.freq2$n[i]
                                    
}

# add the new bigrams to the final dataframe dem_bigrams
rep_bigrams <- bind_rows(rep_bigrams, newdf)

# check that all bigrams are unique and remove duplicates
rep_bigrams <- unique(rep_bigrams)





#### ---- Right context word for democratic speech
# In the following section we do the excact same process except for that we
# look at the words on the right side of the pattern instead of the left.
# Will therefore not comment the code, but the process is to use a qualitative
# analysis to find partisan bigrams to use in our analysis of the partisanship
# of the FED meetings
custom.stopwords1 <- c("agreement", "agreements", "center") #example of custom stopwords

kwicDem %>%
  as_tibble() %>%                                  
  select(post) %>%                                 
  unnest_tokens(output = tokens,
                input = post,
                to_lower = TRUE) %>%               
  filter(!tokens %in% Final_Stopwords) %>% 
  filter(!tokens %in% custom.stopwords1) %>% 
  count(tokens, sort = TRUE) -> right.context.freq 

# Make a wordcloud
filename <- "wordcloud1.png"
png(filename, 2000, 2000)
wordcloud(words = right.context.freq$tokens[1:20],
          freq  = right.context.freq$n[1:20],
          scale = c(20,3))
dev.off()

for (i in 1:20) {
  newdf[i, 1] <- paste(kwicDem$pattern[i],
                       right.context.freq$tokens[i])
  newdf[i, 2] <- right.context.freq$n[i]
  
}

dem_bigrams <- bind_rows(dem_bigrams, newdf)





#### ---- Right context word for republican speech
custom.stopwords2 <- c("agreement", "agreements", "center") # example of custom stopwords

kwicRep %>%
  as_tibble() %>%                                  
  select(post) %>%                                 
  unnest_tokens(output = tokens,
                input = post,
                to_lower = TRUE) %>%               
  filter(!tokens %in% Final_Stopwords) %>% 
  filter(!tokens %in% custom.stopwords2) %>% 
  count(tokens, sort = TRUE) -> right.context.freq2 

# Make a wordcloud
filename <- "wordcloud2.png"
png(filename, 2000, 2000)
wordcloud(words = right.context.freq2$tokens[1:20],
          freq  = right.context.freq2$n[1:20],
          scale = c(20,3))
dev.off()

for (i in 1:20) {
  newdf[i, 1] <- paste(kwicRep$pattern[i],
                       right.context.freq2$tokens[i])
  newdf[i, 2] <- right.context.freq2$n[i]
  
}

rep_bigrams <- bind_rows(rep_bigrams, newdf)


# save the dataframe of all the bigrams in a RData fil in working directory
save(rep_bigrams, file = "rep_bigrams.RData")

# save the dataframe of all the bigrams in a RData fil in working directory
save(dem_bigrams, file = "dem_bigrams.RData")



# ------ MAKE GOVERNOR BIGRAMS AND CALCULATE PARTISANSHIP SCORE -----
# because the making of the bigrams for the democratic and republican speeches
# is time consuming and not an automatic process, we have appended two dataframes
# with the bigrams for democratic and republican speech
load("dem_bigrams.RData")
load("rep_bigrams.RData")

# change column names for the dataframes with democratic bigrams
dem_bigrams <- dem_bigrams %>% 
  rename(bigrams = ...1,
         freq = ...2)

# change column names for the dataframes with republican bigrams
rep_bigrams <- rep_bigrams %>% 
  rename(bigrams = ...1,
         freq = ...2)

#list of all the document names 
nameFiles = list.files(path = "Transcripts/", pattern = "*.txt") %>%
  as_tibble()

# Make sure no governors are added more than once 
unique_Governors <- unique(Governors$Name) %>% as_tibble()

# make dem and rep bigrams into vectors
dem_bigrams_vector <- as.vector(dem_bigrams$bigrams)
rep_bigrams_vector <- as.vector(rep_bigrams$bigrams)

# make empty dataframe to add scores to
scoreGovernors <- tibble()


# loop through number of rows in unique governors
for(x in 1:nrow(unique_Governors)) {  
  #choosing only one Governor and selecting those meetings 
  goodlines <- unique_Governors$value[x]
  
  #selecting one governors meetings
  fang <- nameFiles$value %>% 
    grep(goodlines, ., value = TRUE)
  
  # make empty vector to append the transcripts of one person to
  transcripts <- vector()
  
  # reading all the files from one person and saving it in a combined value
  for (i in fang) {
    y <- read_file(paste0("Transcripts/",i)) # read all files for one person
    transcripts <- c(transcripts, y) # merging all the transcripts of one person
   
  }
  # use transcripts as input, tokenize them into bigrams, rmeove
  scoreBigramsDem <- data.frame(transcripts) %>% 
    unnest_ngrams(bigrams, transcripts, n=2) %>% 
    separate(bigrams,
             into = c("w1", "w2"),
             sep = " ",
             remove = F) %>% 
    filter(!w1 %in% stopwords() & !w2 %in% stopwords()) %>% # remove stopwords
    count(bigrams, sort = TRUE) %>% # count frequenzie of bigrams
    mutate(sum = sum(n)) %>% # make a column with sum of all bigrams
    # only keep bigrams included in bigrams found in qualitative analysis from the
    # democratic speeches
    filter(bigrams %in% dem_bigrams_vector) %>%  
    left_join(dem_bigrams, by = "bigrams") %>% # join in the frequency of bigram in democratic speech
    mutate(score = (n * freq)/sum) # make a column to calculate the score
  scoreDem <- sum(scoreBigramsDem$score)
  
  # same procedure for the republican speeches
  scoreBigramsRep <- data.frame(transcripts) %>% 
    unnest_ngrams(bigrams, transcripts, n=2) %>% 
    separate(bigrams,
             into = c("w1", "w2"),
             sep = " ",
             remove = F) %>% 
    filter(!w1 %in% stopwords() & !w2 %in% stopwords()) %>% 
    count(bigrams, sort = TRUE) %>% 
    mutate(sum = sum(n)) %>%
    filter(bigrams %in% rep_bigrams_vector) %>% 
    left_join(rep_bigrams, by = "bigrams") %>% 
    mutate(score = (n * freq)/sum)
  scoreRep <- sum(scoreBigramsRep$score)

  # save the name of governor and score for democratic and republican speech in df
  scoreEach <- data.frame(unique_Governors$value[x], scoreDem, scoreRep)
  
  # add the values in scoreEach in this df for each iteration
  scoreGovernors <- bind_rows(scoreGovernors, scoreEach)
  
}


# -------- GGPLOT ------------

# make empty dataframe to add data that is used in ggplot
df_plot <- data.frame()

# make dataframe to use as input in ggplot
for (i in 1:nrow(scoreGovernors)) {
  governors <- c(rep(scoreGovernors$unique_Governors.value.x.[i], 2))
  party <- c("D", "R")
  score <- c(scoreGovernors$scoreDem[i], scoreGovernors$scoreRep[i])
  data <- data.frame(governors, party, score)
  
  df_plot <- rbind(df_plot, data)
}

# make the plot of the partisanship score for each governor from first to last
df_plot %>% 
  mutate(governors = fct_relevel(governors, "KELLEY", "GREENSPAN", "MEYER", "GRAMLICH",
                                 "FERGUSON", "OLSON", "BIES", "BERNANKE",
                                 "KOHN", "WARSH", "KROSZNER", "MISHKIN", 
                                 "DUKE", "TARULLO", "YELLEN", "RASKIN",
                                 "POWELL", "STEIN", "FISCHER", "BRAINARD")) %>% 
  ggplot(aes(x = governors, 
             y = score, fill = party)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Partisanship score for each Governor") + 
  xlab("Governors") + 
  ylab("Scores") +
  scale_fill_discrete(name = "Party") +
  theme_classic() +
  theme(axis.text = element_text(size=10),
        axis.text.x = element_text(angle=45, hjust = 1),
        plot.title = element_text(size = 20))


# ---------- END -------------


















