library(rvest, warn.conflicts=FALSE)
library(RSelenium)
setwd("/Users/sarahcox/Documents/Applications/Tests/DataIncubator/IncubatorProject/")

###########
#FUNCTIONS#
########### 

open.page <- function(url){
  checkForServer() # check if Selenium Server package is installed and if not, install now
  startServer(invisible = FALSE, log = FALSE) # start server
  browser <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox")
  
  
  browser$open()
  browser$navigate(url)
  #wait_till_page_load(500000000)
  return(browser)
}


get.reviews <- function(url,source,review.css,price.css){
	html <- read_html(url)
	text <- html_nodes(html, review.css)
	text <- html_text(text)
	text <- gsub("\\r", "", text)
	text <- gsub("\\n", "", text)
	
	price <- html_nodes(html, price.css)
	price <- html_text(price)
	
	lst <- list(text=text,source=source,price=price)
	df <- data.frame(lst$text,lst$source,lst$price)
	names(df) <- c("text","source","price")
	
	df$price <- as.numeric(gsub("[^0-9.]","",df$price))
	
	return(df)
}



####################
#USING THIS CRAWLER#
####################

sources <- read.csv("sourcedata.csv")
sources$url <- as.character(sources$url)
sources$review.css <- as.character(sources$review.css)
sources$price.css <- as.character(sources$price.css)


merged.reviews <- data.frame(matrix(0,0,3))
names(merged.reviews) <- c("text","source","price")
for (i in 1:nrow(sources)){
	rev <- get.reviews(sources$url[i],sources$source[i],sources$review.css[i],sources$price.css[i])
	merged.reviews <- rbind(merged.reviews,rev)
}


################################
#WHEN ALL DATA HAS BEEN SCRAPED#
################################
library(qdap)

# number of misspelled words
for (i in 1:nrow(merged.reviews)){
	merged.reviews$n.misspelled[i] <- length(which_misspelled(merged.reviews$text[i]))
}

# word count
for (i in 1:nrow(merged.reviews)){
	merged.reviews$n.words[i] <- word_count(merged.reviews$text[i])
}

# percent misspelled
for (i in 1:nrow(merged.reviews)){
	merged.reviews$percent.misspelled[i] <- merged.reviews$n.misspelled[i]/merged.reviews$n.words[i]
}

library(koRpus)
#tokenizing so that koRpus will accept the object
tokenized <- lapply(merged.reviews$text[1:nrow(merged.reviews)], tokenize, format = "obj", lang="en")

text.features <- data.frame(matrix(0,0,9))

for (i in 1:length(tokenized)){
	text.features <- rbind(text.features,textFeatures(tokenized[[i]]))
}

data <- cbind(merged.reviews,text.features)


################
#####NOTES######
################


####Predictors######

# need to find a way to handle emoticons ( :), <3, etc). I want to keep them. Perhaps their presence, or even the number of them, could be an interesting predictor.
# need to find a way to deal with non-English reviews (ex row 48 is Spanish)
# need to find a way to deal with improper word usage (i.e. "where" vs. "wear", "cause" vs. "because", etc.)

####Outcome#####

# the problem concerns whether comment features predict PERCEPTION of item value, not true price necessarily. Need to get enough data on comment perception to build a valuable model. May need funding?
# need to decide how to record outcome. As a value on a scale (in theory best, but can someone taking a survey really do that easily?) or as category buckets (are the categories fast-fashion, luxury, couture really universal?)