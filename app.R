#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(cowplot)
library(shiny)

#set wd
#setwd("/home/ben/2016 Sem 1/Data Visualisation/Assignment3/Sal")

# table1
load("salary_field_gender")

# table2
load("salary_field")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("Starting Salary by Field"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            checkboxGroupInput(inputId = "GroupID",
                               label = "" ,
                               choices = levels(table1$field)[levels(table1$field)!="Median"]
            )
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Starting Salary", plotOutput("plot1")),
                        tabPanel("Separate by Gender", plotOutput("plot2")),
                        tabPanel("About the Data", 
                                 textOutput("text0"),
                                 textOutput("text1"),
                                 helpText(a("\n     Click here to view the download page, pick the Education xls file", href = "http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/4125.0Feb%202016?OpenDocument")),
                                 helpText(a("\n     or click here to download the data set directly", href = "http://www.abs.gov.au/ausstats/SUBSCRIBER.NSF/log?openagent&41250ds0002_201602.xls&4125.0&Data%20Cubes&68EB79EDE3345E74CA257F61002084F0&0&Feb%202016&23.02.2016&Latest")),
                                 textOutput("text2")
                        ),
                        class = "rightAlign")
        )
    )#end of sidebarLayout
))


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    
    #set wd
   # setwd("/home/ben/2016 Sem 1/Data Visualisation/Assignment3/Starting_Salary")
    
    # table1
    load("salary_field_gender")
    
    # table2
    load("salary_field")
    
    
    tt <- reactive({ as.character(input$GroupID) })
    output$plot1 <- renderPlot({
        #print(tt())
        g1 <- ggplot(table2 %>% filter( field %in% tt() ),
                     aes(x = year, y = avg.salary, colour = field))+
            geom_point()+
            geom_line() +
            labs(y = "Starting Salary (AU$ thousand)") +
            ylim(c(30,90))
        g2 <- geom_line(data = table2 %>% filter(field == "Median"),
                        aes(x = year, y = avg.salary, colour = field), colour = "black")#+
        g3 <- geom_point(data = table2 %>% filter(field == "Median"),
                         aes(x = year, y = avg.salary, colour = field), colour = "black")#+
        g4 <- g1+g2+g3
        ggdraw(add_sub(g4, "\n *The black line in the median starting salary for all fields"))
        
    })
    
    
    output$plot2 <- renderPlot({
        g1 <- ggplot(table1 %>%  filter( field %in% c("Median", tt()) ),
                     aes(x = year, y = salary, colour = gender))+
            geom_point()+
            geom_line() +
            labs(y = "Starting Salary (AU$ thousand)") +
            facet_grid(.~field)
        g1
    })
    
    output$text0 <- renderText({ "\n "})
    output$text1 <- renderText({ "The Data is from the Australian Bureau of Statistics.
      \n     Gender Indicators, Australia, Feb 2016"})
    output$text2 <- renderText({ "\n   - Ben Phillips" })
})


# Run the application 
shinyApp(ui = ui, server = server)

