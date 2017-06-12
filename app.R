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
library(plotly)

#set wd
#setwd("C:/Users/ben/Documents/code/R/First_ShinyApp")

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
                        tabPanel("Starting Salary", plotlyOutput("plot1", width = "100%")),
                        tabPanel("Separate by Gender", plotlyOutput("plot2", width = "100%")),
                        tabPanel("About this App", 
                                 textOutput("text0"),
                                 textOutput("text1"),
                                 helpText(a("\n     Click here to view the download page, pick the Education xls file", href = "http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/4125.0Feb%202016?OpenDocument")),
                                 helpText(a("\n     Click here to download the data set directly", href = "http://www.abs.gov.au/ausstats/SUBSCRIBER.NSF/log?openagent&41250ds0002_201602.xls&4125.0&Data%20Cubes&68EB79EDE3345E74CA257F61002084F0&0&Feb%202016&23.02.2016&Latest")),
                                 helpText(a("\n     Click here to view the code that created this app", href = "https://github.com/BenjaminPhillips22/First_ShinyApp")),
                                 textOutput("text2")
                        )#,
                        )#class = "rightAlign")
        )
    )#end of sidebarLayout
))


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    
    #set wd
   # setwd("C:/Users/ben/Documents/code/R/First_ShinyApp")
    
    # table1
    load("salary_field_gender")
    
    # table2
    load("salary_field")
    
    my_colours <- c("#9d6031","#546dd6","#a0b53c","#9f5ed1","#55b64f","#c74cae","#529d68","#dd4785","#4bc1b5","#cf4d2c","#4c9ad1","#e0883b","#9d95df","#c5a44d","#000000","#735a9e","#6d7930","#d585c4","#c94250","#a04b6d","#e18680")
    my_fields <- c("Accounting","Agricultural Science","Architecture & Building","Art & Design ",
                   "Biological Sciences","Computer Science","Dentistry","Earth Sciences",
                   "Economics, Business","Education","Engineering","Humanities","Law","Mathematics",
                   "Median","Medicine","Paramedical Studies","Pharmacy (pre-reg)","Physical Sciences",
                  "Psychology","Social Sciences")
    
    

    
    tt <- reactive({ as.character(input$GroupID) })
    
    
    
    output$plot1 <- renderPlotly({
        temp_colours <- my_colours[my_fields %in% c("Median",tt())]
        #print(temp_colours)
        g1 <- ggplot(table2 %>% filter( field %in% c("Median",tt()) ),
                     aes(x = year, y = avg.salary, colour = field))+
            geom_point()+
            geom_line() +
            labs(y = "Starting Salary (AU$ thousand)") +
            ylim(c(30,90))+
            scale_colour_manual(values = temp_colours)+
            scale_x_continuous(breaks = seq(2001, 2015, 3))+
            theme(legend.title=element_blank())
            
        ggdraw(add_sub(g1, "\n *The black line is the median starting salary for all fields"))
        
        ggplotly(g1)
        
    })
    
    
    output$plot2 <- renderPlotly({
        g2 <- ggplot(table1 %>%  filter( field %in% c("Median", tt()) ),
                     aes(x = year, y = salary, colour = gender))+
            geom_point()+
            geom_line() +
            labs(y = "Starting Salary (AU$ thousand)") +
            facet_grid(.~factor(field, levels = c("Median", my_fields[my_fields %in% tt()] )))+
            scale_x_continuous(breaks = seq(2001, 2015, 3))+
            theme(axis.text.x = element_text(angle=80, hjust = 0.5, vjust = 0.5, size = 8),
                  legend.title=element_blank())+
            scale_color_manual(values = c("#4bb092", "#b460bd"))
        ggplotly(g2)
    })
    
    output$text0 <- renderText({ "\n "})
    output$text1 <- renderText({ "The Data is from the Australian Bureau of Statistics.
      \n     Gender Indicators, Australia, Feb 2016"})
    output$text2 <- renderText({ "\n   - Ben Phillips" })
})


# Run the application 
shinyApp(ui = ui, server = server)

