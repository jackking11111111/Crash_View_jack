#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(magrittr)
library(plotly)
library(plyr)

library(shiny)

# # This loads messages data
# messageData <- data.frame(
#   from = c("Admininstrator", "New User", "Support"),
#   message = c(
#     "Sales are steady this month.",
#     "How do I register?",
#     "The new server is ready."
#   ),
#   stringsAsFactors = FALSE
# )
# 
# 
# #This renders Menu and add menu items
#   
# output$messageMenu <- renderMenu({
#   # Code to generate each of the messageItems here, in a list. messageData
#   # is a data frame with two columns, 'from' and 'message'.
#   # Also add on slider value to the message content, so that messages update.
#   msgs <- apply(messageData, 1, function(row) {
#     messageItem(
#       from = row[["from"]],
#       message = paste(row[["message"]], input$slider)
#     )
#   })
#   
#   dropdownMenu(type = "messages", .list = msgs)
# })


for (yr in 2001:2014){
  load(paste("data/Accbygender",yr,".Rda",sep=""))
  assign(paste("Accbygender",yr,sep=""), Accbygender)
}

m <- get(paste("Accbygender",input$year,sep = ""))[order(get(paste("Accbygender",input$year,sep = ""))$count,decreasing=TRUE)[1:5],]

a <- list(
  x=m$date,
  y=m$count,
  text=paste(m$count,m$color,"Crashes on", m$date),
  xref="x",
  yref="y",
  showarrow=T,
  arrowhead=3,
  ax=20,
  zy=-40
)

load(paste("data/basicinfo",yr,".Rda",sep=""))

for (yr in 2001:2014){
  load(paste("data/AccDoW",yr,".Rda",sep=""))
  assign(paste("AccDoW",yr,sep=""), AccDoW)
}

load("data/piechart.Rda")

load(paste("data/barchart",".Rda",sep=""))
assign(paste("barchart",2001,sep=""), barchart)






# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$approvalBox <- renderInfoBox({
    valueBox(width ="20px",
      paste0(25 + input$count), "Like us", icon = icon("thumbs-up", lib = "glyphicon"),color = "yellow"
    )
  })
  
  
  output$caption <- renderPrint(print(varclass))
  
  
   
  output$trendline <- renderPlotly({

    
   p<-  plot_ly(get(paste("Accbygender",input$year,sep = "")),x=~date, y=~count) %>% 
      add_lines(color=~color,colors=c('Male'="#2b8cbe", 'Female'="#feb24c",'Unknown'="#636363")) %>%
     add_markers(
       hoverinfo = "text",
       text = ~paste(count,color,"Crashes on",date),
       marker=list(line = list(color = "black",width=1), opacity=0.3)
     )%>%
      layout( title="NJ Accidents by gender from 2001 to 2014", 
              
              #Add annotations
              annotations=a,
              autosize=T,
              # width=1680,
              # height=600,
              titlefont=list(
                color="grey",
                size=20
              ),
              xaxis = list(
                # title=~paste("Date in Year",input$year),
                nticks=24,
                showgrid=T,
                color="grey",
                showticksuffix="all",
                titlefont=list(
                  color="black",
                  zeroline=T),
                
                rangeselector = list(
                  buttons = list(
                    list(
                      count = 3, 
                      label = "3 mo", 
                      step = "month",
                      stepmode = "backward"),
                    list(
                      count = 6, 
                      label = "6 mo", 
                      step = "month",
                      stepmode = "backward"),
                    list(
                      count = 1, 
                      label = "1 yr", 
                      step = "year",
                      stepmode = "backward"),
                    list(
                      count = 1, 
                      label = "YTD", 
                      step = "year",
                      stepmode = "todate"),
                    list(step = "all")))
                # ,
                # 
                # rangeslider = list(type = "date",
                #                    thickness=0.2,
                #                    autosize=T,
                #                    yanchor="bottom",
                #                    xanchor="center")
                ),
              yaxis=list(title="Number of drivers in accident per day")
      )
      p
  })
  
  
  
  output$trendline2 <- renderPlotly({

    
    
    ###Annotations
    
    m <- get(paste("AccDoW",input$year,sep = ""))[order(get(paste("AccDoW",input$year,sep = ""))$count,decreasing=TRUE)[1:5],]
    
    
    # rownames(m)
    
    
    a <- list(
      x=m$date,
      y=m$count,
      text=paste(m$count,m$color,"Crashes on", m$date),
      xref="x",
      yref="y",
      showarrow=T,
      arrowhead=3,
      ax=20,
      zy=-40
    )
    
    
    p<-  plot_ly(get(paste("AccDoW",input$year,sep = "")),x=~date, y=~count) %>% 
      add_lines(color=~color,colors=c('Monday'="#2b8cbe", 'Tuesday'="#feb24c",'Wednesday'="#636363",'Thursday'="red",'Friday'="black",'Saturday'="green",'Sunday'="blue")) %>%
      add_markers(
        hoverinfo = "text",
        text = ~paste(count,color,"Crashes on",date),
        marker=list(line = list(color = "black",width=1), opacity=0.3)
      )%>%
  
    layout( title="NJ Accidents by Day of Week from 2001 to 2014", 
            
            #Add annotations
            annotations=a,
            autosize=T,
            # width=1680,
            # height=600,
            titlefont=list(
              color="grey",
              size=20
            ),
            xaxis = list(
              # title=~paste("Date in Year",input$year),
              nticks=24,
              showgrid=T,
              color="grey",
              showticksuffix="all",
              titlefont=list(
                color="black",
                zeroline=T),
              
              rangeselector = list(
                buttons = list(
                  list(
                    count = 3, 
                    label = "3 mo", 
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 6, 
                    label = "6 mo", 
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 1, 
                    label = "1 yr", 
                    step = "year",
                    stepmode = "backward"),
                  list(
                    count = 1, 
                    label = "YTD", 
                    step = "year",
                    stepmode = "todate"),
                  list(step = "all")))
   
            ),
            yaxis=list(title="Number of drivers in accident per day")
    )
    p
  })
  
  
  
  # Pie chart
  

  
  output$pie <- renderPlotly({

    s <- seq(1:8)

    ax <- list(title="",
               showline=F,
               # titlefont=f1,
               gridwidth=0,
               showgrid=F,
               showticklabels=F,
               zeroline=F)
    ay <- list(title="", showline=F, gridwidth=0, showgrid=F,showticklabels=F, zeroline=F)


    library(plotly)
    q <- plot_ly(countyfreq, labels=countyfreq$Var1, values=countyfreq$Freq,type="pie",hole=0.6) %>%
      # add_trace(y = ~exp(s), name = "exponential") %>%
      # add_trace(y =  ~s, name = "linear")
      layout(title="Proportion of Accidents by County",
             xaxis=ax,
             yaxis=ay,
             showlegend=T)

   q

  })
  
  
  # Bar chart
  


  
  
  
  output$bar <- renderPlotly({
    ax <- list(title="",
               showline=F, 
               # titlefont=f1,
               gridwidth=0, 
               showgrid=F,
               showticklabels=T, 
               zeroline=F)
    
   z <-  plot_ly(barchart2001, x=~barchart2001$County.Name, y=~barchart2001$freq,color=~barchart2001$Crash.Day.Of.Week, type="bar") %>%
      add_histogram()
    
  z
  
  })
  
  
  #Insert the map app iframe
  
  output$frame <- renderUI({
    input$Member
    my_test <- tags$iframe(width="100%",height="520",frameborder="0",src="https://chefele1.carto.com/viz/127afc18-933d-11e6-93c5-0e233c30368f/embed_map")
    print(my_test)
    my_test
  })
  
  
  
  
  
  
})
