####################################
#### Facebook Analysis - server.R ###
####################################

library(shiny)
library(plyr)
library(ggplot2)
library(gdata)                   # load gdata package 
#help(read.xls)                   # documentation 
#mydata = read.xls("mydata.xls")

load("analytics.Rdata") # load the dataframe

shinyServer(function(input, output) { # server is defined within these parentheses
  
  # prep data once and then pass around the program
  
  passData <- reactive({
    
    analytics <- analytics[analytics$Date %in% seq.Date(input$dateRange[1], input$dateRange[2], by = "days"),]
    
    analytics <- analytics[analytics$Hour %in% as.numeric(input$minimumTime) : as.numeric(input$maximumTime),]
    
    if(class(input$domainShow)=="character"){
      
      analytics <- analytics[analytics$Domain %in% unlist(input$domainShow),]
      
    }
    
    analytics
    
  })
  
  output$monthGraph <- renderPlot({
    
    graphData <- ddply(passData(), .(Domain, Date), numcolwise(sum))
    
    if(input$outputType == "visitors"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = visitors, group = Domain, colour = Domain)) + geom_line() +
        ylab("Unique visitors")
      
    }
    
    if(input$outputType == "bounceRate"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = bounces / visits * 100, group = Domain, colour = Domain)) +
        geom_line() + ylab("Bounce rate %")
      
    }
    
    if(input$outputType == "timeOnSite"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = timeOnSite / visits, group = Domain, colour = Domain)) +
        geom_line() + ylab("Average time on site")
      
    }
    
    if(input$smoother){
      
      theGraph <- theGraph + geom_smooth()
      
    }
    
    print(theGraph)
    
  })
  
  output$hourGraph <- renderPlot({
    
    graphData = ddply(passData(), .(Domain, Hour), numcolwise(sum))
    
    if(input$outputType == "visitors"){
      
      theGraph <- ggplot(graphData, aes(x = Hour, y = visitors, group = Domain, colour = Domain)) + geom_line() +
        ylab("Unique visitors")
      
    }
    
    if(input$outputType == "bounceRate"){
      
      theGraph <- ggplot(graphData, aes(x = Hour, y = bounces / visits * 100, group = Domain, colour = Domain)) +
        geom_line() + ylab("Bounce rate %")
      
    }
    
    if(input$outputType == "timeOnSite"){
      
      theGraph <- ggplot(graphData, aes(x = Hour, y = timeOnSite / visits, group = Domain, colour = Domain)) +
        geom_line() + ylab("Average time on site")
      
    }
    
    if(input$smoother){
      
      theGraph <- theGraph + geom_smooth()
      
    }
    
    print(theGraph)
    
  })
  
  
  output$textDisplay <- renderText({ 
    
    paste(
      length(seq.Date(input$dateRange[1], input$dateRange[2], by = "days")),
      " days are summarised. There were", sum(passData()$visitors), "visitors in this time period.
     
       : This helps us to find out the facebook site visitors vrs other sites .
      How much time a person spents his/her time in facebook or in other sites based on per day ,months. Detail analysis can be found out if we can have exact site visit data .
      This is just a sample data which is not accurate , we can compares trends of site statistics if we can have exact data. .


      Thinking of the project in Shinny apps as:
      Let’s drill into your Facebook data to extract your story.   :
      http://www.slideshare.net/anil_maharjan/using-power-query-to-tell-your-story-form-your-facebook-data
     
     "
    )
    
  })
})
