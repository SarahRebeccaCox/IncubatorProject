data <- read.csv("Boot_Prices.csv")


data <- data[,-c(1:8)]
data <- data[,-c(1:3)]
data <- data[,-c(73:75)]

names(data) <- c("Guess1","Guess2","Guess3","Guess4","Guess5","Guess6",
                 "Guess7","Guess8","Guess9","Guess10","Guess11",
                 "Guess12","Guess13","Guess14","Guess15","Guess16","Guess17",
                 "Guess18","Spelling1","Grammar1","Length1","Spelling2","Grammar2","Length2","Spelling2","Grammar2","Length2",
                 "Spelling3","Grammar3","Length3","Spelling4","Grammar4","Length4","Spelling5","Grammar5","Length5",
                 "Spelling6","Grammar6","Length6","Spelling7","Grammar7","Length7","Spelling8","Grammar8","Length8",
                 "Spelling9","Grammar9","Length9","Spelling10","Grammar10","Length10","Spelling11","Grammar11","Length11",
                 "Spelling12","Grammar12","Length12","Spelling13","Grammar13","Length13","Spelling14","Grammar14","Length14",
                 "Spelling15","Grammar15","Length15","Spelling16","Grammar16","Length16","Spelling17","Grammar17","Length17")



for (col in 1:18){
  for (row in 1:nrow(data)){
    levels(data[,col]) <- c(levels(data[,col]),"1","2","3")
    if (data[row,col] == "30-75 USD"){
      data[row,col] <- "1"
    }
    if (data[row,col] == "100-300 USD"){
      data[row,col] <- "2"
    }
    if (data[row,col] == "600-900 USD"){
      data[row,col] <- "3"
    }
  }
  data[,col] <- as.numeric(data[,col])
}




for (col in 19:ncol(data)){
  for (row in 1:nrow(data)){
    levels(data[,col]) <- c(levels(data[,col]),"1","2","3","4","5")
    if (data[row,col] == "Very Poor"){
      data[row,col] <- "1"
    }
    if (data[row,col] == "Poor"){
      data[row,col] <- "2"
    }
    if (data[row,col] == "Decent"){
      data[row,col] <- "3"
    }
    if (data[row,col] == "Good"){
      data[row,col] <- "4"
    }
    if (data[row,col] == "Very Good"){
      data[row,col] <- "5"
    }
  }

  data [,col] <- as.numeric(as.character(data[,col]))

}


plot(data$Guess2,data$Spelling2)
