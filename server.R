library(shiny)
library(RCurl)
library(httr)
library(ngram)
library(dplyr)
library(stringr)

shinyServer(function(input, output) {
  mydata <- reactive({
    if (is.null(input$file1))
      return(NULL)
    
    data1 <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    data1<- data1[seq(dim(data1)[1],1),]
    row.names(data1) <- seq(1,nrow(data1),1)
    
    if (is.null(input$file2))
      return(NULL)
    data2 <- read.csv(input$file2$datapath, stringsAsFactors = FALSE)
    data2<- data2[seq(dim(data2)[1],1),]
    row.names(data2) <- seq(1,nrow(data2),1)
    
    separate <- function(x){
      for(i in 1:nrow(x)){ 
        separate <- strsplit(x$Commentary[i], split = ", ")
        split <- strsplit(separate[[1]][1], " to ")
        x$Bowler[i] <- split[[1]][1]
        x$Batsman[i] <- split[[1]][2]
        x$Commentary[i] <- paste(separate[[1]][-c(1,2)], sep = ", ",collapse =
                                   ", ")}
      x
    }
    values <- function(x){
      for(i in 1:nrow(x)){
        if(nchar(x$Runs[i])>1){
          x$Extra[i]<- x$Runs[i]
          x$Runs[i] <- gsub('[^[:digit:]]', '', x$Runs[i])
        }
        else{
          x$Extra[i]<- '0'
        }
      }
      
      j <- 1
      for(i in 1:nrow(x)){
        if(x$Runs[i] == 'W'){
          x$Runs[i] = 0
          x$Wicket[i] = j 
          j <- j +1}
        else
          x$Wicket[i] = 0
        
        if(x$Wicket[i] != 0){
          presplit <- (gsub('\\D', ' ', x$Commentary[i]))
          split <- strsplit(presplit, ' ')
          run <- ifelse(split[[1]][1]== '', 0,as.numeric(split[[1]][1]))
          x$Runs[i] <- as.character(as.numeric(x$Runs[i]) + run)
        }
      }
      x
    }
    rating <- function(x){
      x$Runs<- as.numeric(x$Runs)
      
      players<- unique(c(x$Batsman, x$Bowler))
      players<- data.frame(players)
      
      x$batI<- ifelse(x$Extra==0, x$Runs, 0 )
      for(i in 1:(nrow(x)-1)){
        x$legalBall[i]<- ifelse(x$Ball[i]==x$Ball[i+1], 0, 1)
        x$legalBall[nrow(x)]<- 1
      }
      players$score<- 0
      players$ballsFaced<- 0
      for(i in 1:nrow(players)){
        for(j in 1:nrow(x)){
          if(x$Batsman[j] == players$players[i]){
            players$score[i] <- players$score[i] + x$Runs[j]
            players$ballsFaced[i] <- players$ballsFaced[i] + x$legalBall[j]
          }
        }
      }
      
      
      x$batI<- x$batI + ifelse(x$Runs==4, 1 , 0)
      
      x$batI<- x$batI + ifelse(x$Runs==6, 2 , 0)
      
      
      for(j in 1:nrow(x)){
        x$bowlI[j]<- ifelse(x$Wicket[j]!=0, ifelse(str_count(tolower(x$Commentary[j]), c("throw", "direct hit"))==0, 19, 0), 0)
      }
      x$bowlI<- x$bowlI + ifelse(x$Runs==0, 1, 0)
      
      for(i in 1:(nrow(x)-1)){
        x$bowlI[i]<- x$bowlI[i] + ifelse(x$Ball[i]==x$Ball[i+1], -1, 0)
      }
      x$bowlI[nrow(x)]<- x$bowlI[nrow(x)]
      
      
      
      players$batIndex<- 0
      players$bowlIndex<- 0
      players$ballsBowled <- 0
      players$RunssConceded <- 0
      players$wicketsTaken <- 0
      for(i in 1:nrow(players)){
        for(j in 1:nrow(x)){
          if(x$Batsman[j] == players$players[i])
            players$batIndex[i] <- players$batIndex[i] + x$batI[j] 
          if(x$Bowler[j] == players$players[i]){
            players$bowlIndex[i] <- players$bowlIndex[i] + x$bowlI[j]
            
            players$ballsBowled[i]<- players$ballsBowled[i] + x$legalBall[j]
            
            if(x$Extra[j]==0)
              players$RunssConceded[i] <- players$RunssConceded[i] + x$Runs[j]
            else
              players$RunssConceded[i] <- players$RunssConceded[i] +
              ifelse(x$Ball[j] == x$Ball[j+1], x$Runs[j], 0)
            
            players$wicketsTaken[i]<- players$wicketsTaken[i]+ 
              ifelse(x$Wicket[j] != 0, ifelse(str_count(tolower(x$Commentary[j]), c("throw", "direct hit"))==0, 1, 0),0)
          }
        }
      }
      
      
      players$allroundIndex<- ifelse(players$batIndex+players$bowlIndex!=0,(players$batIndex*players$bowlIndex)/(players$batIndex+players$bowlIndex),0)
      
      players$strikeRate <- ifelse(players$ballsFaced!=0, players$score*100/players$ballsFaced,0)
      
      
      players$economy<- ifelse(players$ballsBowled != 0,players$RunssConceded*6/players$ballsBowled,0)
      
      players$batIndex <- players$batIndex + ifelse(players$score>=50, 
                                                    ifelse(players$score>=100, 8, 4), 0)
      players$bowlIndex<- players$bowlIndex + ifelse(players$wicketsTaken>= 3,
                                                     ifelse(players$wicketsTaken>= 5, 8, 4), 0)
      
      players$batIndex<- players$batIndex+ ifelse(players$ballsFaced>10, (players$strikeRate-100)*0.1, 0)
      players$bowlIndex<- players$bowlIndex + ifelse(players$ballsBowled>12, 6-players$economy, 0)
      players
    }
    
    data1 <- values(separate(data1))
    data2 <- values(separate(data2))
    
    md <- rbind(data1, data2)
    
    playerratings <- rating(md)
    playerratings[,c(1,2,3,10,6,7,8,11,4,5,9)]
  })
  
  output$table <- renderTable({
    mydata()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("rating",
            gsub("\\D","",input$file1$name),
            gsub("\\D","",input$file2$name),
            ".csv", sep = "")
    },
    content = function(file) {
      write.csv(mydata(), file, row.names = FALSE)
    }
  )
})