# Calling libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(RColorBrewer)
library("mice")
library("devtools")
library("tidyverse")


# Reading the data

ColClasses=c(rep("numeric",9))
DATOS <- df <- read.csv("https://query.data.world/s/6fv7uilqs5np32khyfq7wdtg44me5o", header=TRUE, stringsAsFactors=FALSE)

ColClasses=c(rep("numeric",9))

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

variables <- setdiff(names(DATOS), "Outcome")


# Define UI for application


ui <- navbarPage("Diabetes app",
                 tabPanel("Feature Inspection",
                          fluidRow(
                              #tags$h2("Add a shiny app background image"),
                              #setBackgroundImage(src="https://fedesp.es/wp-content/uploads/2019/07/diagnostico-de-diabetes.jpg"),
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
                 tabPanel("k-means clustering",
                     sidebarPanel(
                         selectInput("select1", label= "Choose X Variable", variables, selected = 1),
                         selectInput("select2", label= "Choose Y Variable", variables, selected = 2),
                         sliderInput('clusters', label= "Number of Clusters", 1, min = 1, max = 9)
                     ),
                     mainPanel(
                         plotOutput('plot_cluster')
                     ),
                     dataPanel <- tabPanel("Data",
                                           tableOutput("data")
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
                geom_point(size = 2) + 
                labs(x = input$featureDisplay_x,
                     y = input$featureDisplay_y) +
                fte_theme()
        })          
        
    })
    
    variables_elegidas <- reactive({
        DATOS[, c(input$select1, input$select2)]
    })
    
    clusters <- reactive({
        kmeans(variables_elegidas(), input$clusters)
    })
    
    
    output$plot_cluster <- renderPlot({
        par(mar = c(5.1, 4.1, 0, 1))
        plot(variables_elegidas(), main = "K-mean Clustering for Diabetic Sample",
             col = clusters()$cluster,
             pch = 10, cex = 1)
        points(clusters()$centers, pch = 10, cex = 3, lwd = 3)
    })
    
    
    

    output$data <- renderTable({
            fit = kmeans(variables_elegidas(), input$clusters)
            Assigned_Cluster <-  fit$cluster
            d = data.frame(Assigned_Cluster,DATOS)
            d[order(d$Assigned_Cluster),]
        
        })
    
    
}
    


# Run the application 
shinyApp(ui = ui, server = server)
