#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(plotly)

library(shiny)

library(shinydashboard)


dashboardPage(
  
  skin="yellow",
  
  #This is the header of the dashboard
  dashboardHeader(
    title="NJ Crash Report",
                  # Dynamic Message
                  # dropdownMenuOutput("messageMenu"),
                  
                  #Disable Header Bar
                  disable = F,
             
                  # THis is the dropdown menu
                  dropdownMenu(
                               icon = icon("users"),
                               type = "message",
                               messageItem(
                                 icon=icon("user"),
                                 from = "Jiahuan KANG",
                                 message = " "
                               ),
                               messageItem(
                                 icon=icon("user"),
                                 from = "Manbir Mohindru",
                                 message = " "
                               ),
                               messageItem(
                                   icon=icon("user-plus"),
                                   from = "Join us",
                                   message = "New member"
                               )
                          ),
                  dropdownMenu(
                    icon = icon("tasks"),
                    type = "tasks",
                    taskItem(
                      "Create UI",
                      value=10,
                      color="green"
                      # href = ""
                    ),
                    taskItem(
                      "Fix data issues",
                      value=50,
                      color="aqua"
                      # href = ""
                    ),
                    taskItem(
                      "Create basic statistics",
                      value=20,
                      color="yellow"
                      # href = ""
                    ),
                    taskItem(
                      "Create data visulizations",
                      value=30,
                      color="aqua"
                      # href = ""
                    ),
                    taskItem(
                      "Write up stories for data and visualizations",
                      value=5,
                      color="red"
                      # href = ""
                    )
                  )
                  
                  
                  # dropdownMenu(
                  #   icon = icon("tasks"),
                  #   type = "notification",
                  #   notificationItem("text", icon = shiny::icon("warning"), status = "success",
                  #                    href = NULL)
                  # 
                  # )

                  
             ),
  
  
  # This is the side bar of the dashboard
  dashboardSidebar(
    # disable=TRUE,
    
    
    #search form server side code: input$searchText and input$searchButton
    sidebarSearchForm("searchText", "searchButton", label = "Search...",
                      icon = shiny::icon("search")),
    
    
    
    sidebarMenu(
      
      menuItem("Dashboard",tabName = "Dashboard",icon = icon("dashboard"),fluidRow(
        sliderInput("year", label = "Year:",min = 2008, max = 2014, value = 2001, sep="", ticks=FALSE, animate=TRUE)
      )),
      menuItem("Home",tabName = "Home",icon = icon("home")),
      
      menuItem("Accidents", tabName = "accidents", icon=icon("th"),badgeLabel = "updating", badgeColor = "green"),
      menuItem("Driver", tabName = "driver", icon=icon("th"),badgeLabel = "updating", badgeColor = "green"),
      menuItem("Code for Princeton",icon=icon("th"),href="http://codeforprinceton.org/",newtab =F),
      menuItem("Github",icon=icon("th"),href="https://github.com/codeforprinceton/uncrash",newtab = F)
    )
  ),
  
  # This is the body of Dashboard
  dashboardBody(
    
  tabItems(
    #tab 0
    tabItem(tabName="select"    ),

    #tab 1
    tabItem(tabName="Home",
            
          
           
            fluidRow(
              tabBox(
                id="tabset1",
                width = "100%",
              tabPanel("Trendline",collapsible=TRUE,solidHeader=TRUE,status="warning",plotlyOutput("trendline", width = "auto", height = "100%"),width="1280px",height="500px")
              ,
              tabPanel("Pie chart",collapsible=TRUE,solidHeader=TRUE,status="warning",plotlyOutput("pie", width = "auto", height = "500px"),width="1280px",height="500px"),
              tabPanel("Pie chart",collapsible=TRUE,solidHeader=TRUE,status="warning",plotlyOutput("bar", width = "auto", height = "500px"),width="1280px",height="500px")
              )
              ),
        h2("NJ Crash Report"),
        p("From year 2001 till 2014, 9,058 people were killed and 1,386,636 people were injured in 4,062,688 car accidents in the state of NJ,
              that is, on average, 647 people killed and 99,045 people injured every year on NJ roads. 
              "),
        
        # Add Approval box
        fluidRow(
          column(11," "),
          column(1,
          valueBoxOutput("approvalBox", width="5%"),
          actionButton("count","Click")
          )
        )
        
        
    ),
    
    #tab 3
    tabItem(tabName="accidents",
            p("From year 2001 till 2014, 9,058 people were killed and 1,386,636 people were injured in 4,062,688 car accidents in the state of NJ,
              that is, on average, 647 people killed and 99,045 people injured every year on NJ roads. 
              ")
    ),
    
    # tab 4
    tabItem(tabName="driver",
            h2("Widgets tab content")
       
         )
    
    # # tab 4
    # tabItem(tabName="Code for Princeton",
    #         h2("Widgets tab content")
    #         
    # ),
    # 
    # # tab 5
    # tabItem(tabName="Github",
    #         h2("Widgets tab content")
    #         
    # )
     )
  )
)









# sidebarLayout(
#   sliderInput("year", label = h4("Select year:"),
#               min = 2001, max = 2014, value = 2001, sep="", ticks=FALSE, animate=TRUE),
#   
#   plotlyOutput("trendline", width = "100%", height = 900)
# )
# 
# )) 
  
  

    # sidebarLayout(
    #   sidebarPanel(
    #     sliderInput("year", label = h4("Select year:"),
    #                 min = 2008, max = 2014, value = 2014, sep="", ticks=FALSE, animate=TRUE)
    #   )
    #   ,
    #   fluidRow(
    #     mainPanel(
    #       plotlyOutput("trendline", width = "100%", height = "100%")
    #     )
    #   ) 
    # )
  

  
  
  # Show a plot of the generated distribution


