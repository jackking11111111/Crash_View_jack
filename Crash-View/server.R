#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
require(magrittr)
require(plotly)
# require(plyr)
require(dplyr)
require(shiny)
require(DT)
require(shinydashboard)
require(shinyjs)
require(shinyEvents)

#######################Load me data



#Trendline data

for (yr in 2001:2014){
  load(paste("data/Accbygender",yr,".Rda",sep=""))
  assign(paste("Accbygender",yr,sep=""), Accbygender)
}

for (yr in 2001:2014){
  load(paste("data/AccDoW",yr,".Rda",sep=""))
  assign(paste("AccDoW",yr,sep=""), AccDoW)
}


#Bike data

load("data/bike_mercer.Rda")
assign("bike_mercer",bike1)

load("data/bike_ocean.Rda")
assign("bike_ocean",bike2)


#Descriptive Data

load("data/basicinfo.Rdata")


#Piechart data
load("data/piechart.Rda")

#Barchart data
load(paste("data/barchart",".Rda",sep=""))
assign(paste("barchart",2001,sep=""), barchart)

#Speed Sign Data
load("data/Radar_Sign_4_at_Pine_Street.Rda")







#Define server logic required to draw a histogram

shinyServer(function(input, output) {
  
  addClass(selector = "body", class = "sidebar-collapse")
  
  # observeEvent(TRUE, {
  #   shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  # })
  # observeEvent(input$hideSidebar, {
  #   shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  # })
  # 
  
  
  
  output$approvalBox <- renderInfoBox({
    valueBox(width ="20px",
      paste0(25 + input$count), "Like us", icon = icon("thumbs-up", lib = "glyphicon"),color = "yellow"
    )
  })
  
  
  # output$counter <- renderText({
  #   if(!file.exists("counter.Rdata"))
  #     counter <- counter+1
  #     save(counter,file="counter.Rdata")
  #     paste0
  # })
  
  
  # output$caption <- renderDataTable(data.frame(varclass))
  
  output$tbl = DT::renderDataTable(data.frame(varclass), options = list(
    lengthChange = TRUE,
    pageLength=10
    # ,
    # initComplete=JS('function(setting,json){alert("done");}')
    ))
  
   
  output$trendline <- renderPlotly({

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
              # autosize=T,
              # width=1680,
              # height=400,
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
            # autosize=T,
            # width=1680,
            # height=400,
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


    q <- plot_ly(countyfreq, labels=countyfreq$Var1, values=countyfreq$Freq,type="pie",hole=0.6) %>%
  
      layout(title="Proportion of Accidents by County",
             xaxis=ax,
             yaxis=ay,
             showlegend=T)

   q

  })
  
  
  # Bar chart
  
  output$bar <- renderPlotly({

   z <-  plot_ly(barchart2001, x=~barchart2001$County.Name, y=~barchart2001$freq,color=~barchart2001$Crash.Day.Of.Week, type="bar") %>%
      add_histogram()%>%
      layout(title="NJ Crash by Day of Week",
             titlefont=list(
               color="grey"
               # size=20
             ),
                               showline=TRUE, 
                               # titlefont=f1,
                               gridwidth=0, 
                               showgrid=F,
                               showticklabels=T, 
                               height=400,
                               zeroline=F)
    
  z
  
  })
  
  
  #Insert the map app iframe
  
  output$frame <- renderUI({
    input$Member
    my_test <- tags$iframe(width="100%",height="520",frameborder="0",src="https://chefele1.carto.com/viz/127afc18-933d-11e6-93c5-0e233c30368f/embed_map")
    print(my_test)
    my_test
  })
  
  
  output$frame2 <- renderUI({
    input$Member
    my_test <- tags$iframe(width="100%",height="520",frameborder="0",src="https://chefele1.carto.com/viz/ed3e6638-06d7-11e7-af00-0ecd1babdde5/embed_map")
    print(my_test)
    my_test
  })
  
  output$frame3 <- renderUI({
    input$Member
    my_test <- tags$iframe(width="100%",height="520",frameborder="0",src="http://arcg.is/2kXqeSf")
    print(my_test)
    my_test
  })
  
##Bike data basics
  bike_datasetInput <- reactive({
    switch(
      input$dataset,
      "Mercer County" = bike_mercer,
      "Ocean County" = bike_ocean
    )
  })
  
  output$bike1=DT::renderDataTable(bike_mercer,filter = 'top',
                                   options = list(autoWidth = TRUE))
  

  
  output$bike_summary <- renderPrint({

    vardata <- switch(input$var,
                      "ID" = bike_mercer$ID,
                      "Crash Year" = bike_mercer$CRASH_YEAR
    )
    # vardata <- ifelse(input$var == "ID", bike_mercer$ID,
    #             ifelse(input$var =="Crash Year", bike_mercer$CRASH_YEAR, bike_mercer$MUNICIPALITY ))
        summary(vardata)
  })
  
  output$bike_bar_title <- renderPrint({
    input$var
  })
  
  
  output$bike_bar <- renderPlotly({
    vardata <- switch(input$var,
                      "ID" = bike_mercer$ID,
                      "Crash Year" = bike_mercer$CRASH_YEAR
    )
    fit <- density(vardata)
    z <- plot_ly(x=vardata, alpha=0.6, type="histogram",name="Histogram")%>%
      add_trace(x=fit$x, y=fit$y,mode="lines", fill="tozeroy", yaxis="y2",names="Density")%>%
      layout(yaxis2 = list(overlaying = "y", side = "right"))
  })

  output$view <- renderTable({
    vardata <- switch(input$var,
                      "ID" = bike_mercer$ID,
                      "Crash Year" = bike_mercer$CRASH_YEAR
    )
    head(bike_datasetInput(), n=input$obs)
  })
  
  
  ########Speed Sign tab#####
  output$Speed_Sign_effect <- renderPlotly({
    plot_ly(speed_sign_avg,x=~Time, y=~avg.speed,name="Average Speed",type = "bar")%>%
      add_trace(y=~peak.speed,name="Peak Speed")%>%
      add_trace(y=~vioratio, type='scatter',mode='markers', marker=list(color="#FFFFFF",symbol="circle-open-dot",size=10),name='Ratio of Violators', yaxis='y2',line=list(color='#DC143C',width='5'))%>%
      add_annotations( text=paste0(speedlimit,"<br>"), xref="paper", yref="paper",
                     x=1.13, xanchor="left",
                     y=0.8, yanchor="bottom",    # Same y as legend below
                     legendtitle=TRUE, showarrow=FALSE ) %>%
      layout(autosize=T,
           margin=m,
           title=paste0("Average vehicle speeds at ",Location),
           yaxis=list(title='Speed'),
           yaxis2 = list(range=c(0,1),side = 'right', overlaying = "y", title = 'Rate of Violators', showgrid = FALSE, zeroline = FALSE),
           barmode='group',
           xaxis=list(title=paste0(RadarSign,":     ", format(start,"%b%d, %Y"), " - ", format(end,"%b%d, %Y")),tickangle=45,categoryorder="trace",showticklabels=FALSE),
           legend=list(x=1.13,y=0.8, yanchor="top")
    )
  })
})
