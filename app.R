# Calling libraries
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
library("mice")


# Reading the data

ColClasses=c(rep("numeric",9))
DATOS=read.csv2("diabetes.csv",sep = ",",header = T, colClasses = ColClasses, dec = "." )
feature.list <- colnames(DATOS)

#Converting key variable into discrete
DATOS$Outcome <- as.factor(DATOS$Outcome)


# Missing value Treatment

data <- DATOS[2:8]
data[data==0] <-  NA
DATOS[2:8] <- data
DATOS <- mice(DATOS,m=1,method="pmm")
DATOS <- complete(DATOS)
attach(DATOS)

# Define UI for application


ui <- navbarPage("Diabetes app",
                 tabPanel("Feature Inspection",
                          fluidRow(
                              tags$h2("Add a shiny app background image"),
                              setBackgroundImage(src="https://fedesp.es/wp-content/uploads/2019/07/diagnostico-de-diabetes.jpg"),
                              column(4, selectInput("featureDisplay_x", 
                                                    label = h3("X-Axis Feature"), 
                                                    choices = feature.list,
                                                    selected = feature.list[1])),
                              column(4, selectInput("featureDisplay_y", 
                                                    label = h3("Y-Axis Feature"), 
                                                    choices = feature.list,
                                                    selected = feature.list[2]))
                              
                          ),
                          fluidRow(
                              column(4,
                                     plotOutput("distPlotA")
                              ),                              
                              column(4,
                                     plotOutput("distPlotB")      
                              ),
                              column(4,
                                     plotOutput("ScatterPlot")
                              )
                          )
                          
                          
                 ),
                 
                 
                 tabPanel("Feature Descriptions",
                          fluidRow(
                              column(10,
                                     includeMarkdown("Untitled.Rmd")
                              )
                          )
                          
                 ),
                 
                 tabPanel("General Information",
                          fluidRow(
                              column(10,
                                     includeMarkdown("references.Rmd")
                              )
                          )
                          
                 )
)

server <- function(input, output, session) {
    observe({
        input_feature_x <- as.symbol(input$featureDisplay_x)
        input_feature_y <- as.symbol(input$featureDisplay_y)
        
        output$distPlotA <- renderPlot({
            ggplot(DATOS, aes_string(input$featureDisplay_x, fill = "Outcome")) + 
                geom_histogram(position = "dodge") + 
                labs(x = input$featureDisplay_x,
                     y = "Count") + fte_theme() +
                scale_fill_manual(guide = F,values=c("#7A99AC", "#E4002B")) 
            
        })
        
        output$distPlotB <- renderPlot({
            ggplot(DATOS, aes_string(input$featureDisplay_y, 
                                  fill = "Outcome")) + 
                geom_histogram(position = "dodge") +
                labs(x = input$featureDisplay_y,
                     y = "Count") + fte_theme() +
                scale_fill_manual(guide = F,values=c("#7A99AC", "#E4002B")) 
            
            
        })
        
        output$ScatterPlot <- renderPlot({
            ggplot(DATOS, aes_string(x = input$featureDisplay_x, 
                                  y = input$featureDisplay_y, 
                                  color = "Outcome")) + 
                geom_point(size = 4, position = position_jitter(w = 0.1, h = 0.1)) + 
                labs(x = input$featureDisplay_x,
                     y = input$featureDisplay_y) +
                fte_theme() + 
                scale_color_manual(name = "Diabetes",values=c("#7A99AC", "#E4002B")) 
        })          
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
