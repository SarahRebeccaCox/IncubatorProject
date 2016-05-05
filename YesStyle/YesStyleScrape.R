#library(lasso2)
#library(tm)           # Framework for text mining.
#library(SnowballC)    # Provides wordStem() for stemming.
#library(qdap)         # Quantitative discourse analysis of transcripts.
#library(qdapDictionaries)
#library(dplyr)        # Data preparation and pipes %>%.
#library(rjson)
#library(RJSONIO)
library(rvest, warn.conflicts=FALSE)
library(RSelenium)
#library(jsonlite)
setwd("/Users/sarahcox/Documents/Applications/Tests/DataIncubator/IncubatorProject/YesStyle")

##################
#Getting comments#
##################


#open the page
wait_till_page_load<-function(page_load_time_out=60){
  t0<-Sys.time()
  while(browser$executeScript("return document.readyState;")[[1]]!="complete" & (Sys.time()-t0)<=page_load_time_out){
    Sys.sleep(0.5)
  }
  invisible(0)
}

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
	names(df) <- c("comments","source","price")
	
	df$price <- as.numeric(gsub("[^0-9.]","",df$price))
	
	return(df)
}





#create yesstyle comment list
yesstyle.comments <- function(url){ #works from open browser
	comments <- read_html(url) # reading the HTML code
	text <- html_nodes(comments, ".reviewContent") # identify the CSS selector
	text # content of CSS selector
	text <- html_text(text)
	text <- gsub("\\r", "", text)
	text <- gsub("\\n", "", text)
	price <- html_nodes(comments, ".finalprice")
	price <- html_text(price)
	source <- 'yesstyle'
 
	lst <- list(text=text,source=source,price=price)
	df <- data.frame(lst$text,lst$source,lst$price)
	names(df) <- c("comments","source","price")  
	df$price <- as.numeric(gsub("[^0-9.]","",df$price))
	return(df)
} 


dillards.comments <- function(url){ #works from open browser
  comments <- read_html(url) # reading the HTML code
  text <- html_nodes(comments, ".pr-comments") # identify the CSS selector
  text <- html_text(text)
  price <- html_nodes(comments, "#mainPrice .price-number")
  price <- html_text(price)
  source <- 'dillards'
  lst <- list(text=text,source=source,price=price)
  df <- data.frame(lst$text,lst$source,lst$price)
  names(df) <- c("comments","source","price")  
  return(df)
} 

nordstrom.comments <- function(url){ #works from open browser
  comments <- read_html(url) # reading the HTML code
  text <- html_nodes(comments, ".BVRRReviewText") # identify the CSS selector
  text <- html_text(text)
  #text <- gsub("\\r", "", text)
  #text <- gsub("\\n", "", text)
  
  return(text)
} 




zappos.comments <- function(url){ #works from open browser
  comments <- read_html(url) # reading the HTML code
  text <- html_nodes(comments, ".reviewContent") # identify the CSS selector
  text <- html_text(text) 
  source <- 'zappos'
  price <- html_nodes(comments, ".nowPrice")
  price <- html_text(price)
  #price <- gsub("$","",price)
  lst <- list(text=text,source=source,price=price)
  df <- data.frame(lst$text,lst$source,lst$price)
  names(df) <- c("text","source","price")
  return(df)
} 

####################
#USING THIS CRAWLER#
####################


###############################################
#Steps for when all the functions are defined.# 
###############################################

########YESSTYLE############

#the main URL to pull from
url <- 'http://www.yesstyle.com/en/jy-shoes-elastic-over-the-knee-boots-black-39/info.html/pid.1037603317'
browser <- open.page(url)

#STEP 2: GET DEM COMMENTS
comments <- yesstyle.comments(url)
source <- "yesstyle"
price <- 42.21

data <- data.frame(comments,source,price)


######ZAPPOS########

url <- "http://www.zappos.com/stuart-weitzman-lowland"
url2 <- 'http://www.zappos.com/dirty-laundry-rain-dance-black'

browser <- open.page(url)

zappos <- zappos.comments(url)
zappos2 <- zappos.comments(url2)

zappos <- rbind(zappos,zappos2)

#DILLARDS#


url <- 'http://www.dillards.com/p/ugg-bailey-button-over-the-knee-boots/505633183?di=04591261_zi_black&categoryId=590159&facetCache=pageSize%3D100%26beginIndex%3D0%26orderBy%3D5%26facet%3D-1747911810111432841041013275110101101'

dillards <- dillards.comments(url)
source <- dillards
price <- 295

dillards.data <- data.frame(dillards,source,price)
names(dillards.data) <- c("comments","source","price")


setwd("/Users/sarahcox/Documents/Applications/Tests/DataIncubator/IncubatorProject/")
sources <- read.csv("sourcedata.csv")
sources$url <- as.character(sources$url)
sources$review.css <- as.character(sources$review.css)
sources$price.css <- as.character(sources$price.css)


merged.data <- data.frame(matrix(0,0,3))
names(merged.data) <- c("text","source","price")
for (i in 1:nrow(sources)){
	rev <- get.reviews(sources$url[i],sources$source[i],sources$review.css[i],sources$price.css[i])
	merged.data <- rbind(merged.data,rev)
}


################################
#WHEN ALL DATA HAS BEEN SCRAPED#
################################

merged.data <- rbind(data,dillards.data)

library(qdap)

# number of misspelled words
for (i in 1:nrow(merged.data)){
	merged.data$n.misspelled[i] <- length(which_misspelled(merged.data$comments[i]))
}

# word count
for (i in 1:nrow(merged.data)){
	merged.data$n.words[i] <- word_count(merged.data$comments[i])
}

# percent misspelled
for (i in 1:nrow(merged.data)){
	merged.data$percent.misspelled[i] <- merged.data$n.misspelled[i]/merged.data$n.words[i]
}

library(koRpus)
#tokenizing so that koRpus will accept the object
tokenized <- lapply(merged.data$comments[1:nrow(merged.data)], tokenize, format = "obj", lang="en")

text.features <- data.frame(matrix(0,0,9))

for (i in 1:length(tokenized)){
	text.features <- rbind(text.features,textFeatures(tokenized[[i]]))
}

merged.data <- cbind(merged.data,text.features)



