# Calling libraries
library("shiny")
library("ggplot2")
library("shinyWidgets")
library("RColorBrewer")
library("mice")
library("tidyverse")
library("scatterplot3d")


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
                         selectInput("select3", label= "Choose Z Variable", variables, selected = 3),
                         sliderInput('clusters', label= "Number of Clusters", 1, min = 1, max = 9)
                     ),
                     mainPanel(
                         plotOutput('plot_cluster'),

                     ),
                     dataPanel <- tabPanel("Data",
                                           radioButtons("radio", 
                                                        label = HTML('<FONT color="red"><FONT size="5pt">Output Selection</FONT></FONT><br> <b>Choose a table output for the k-mean analysis ?</b>'),
                                                        choices = list("Data" = 1, "Cluster Centroids" = 2, "Size" = 3, "Within-Cluster Sum of Squares" = 4, "Between-Cluster Sum of Squares" = 5),
                                                        selected = 1,
                                                        inline = T,
                                                        width = "100%"),      
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
        DATOS[, c(input$select1, input$select2, input$select3)]
    })
    
    clusters <- reactive({
        kmeans(variables_elegidas(), input$clusters)
    })
    
    
    output$plot_cluster <- renderPlot({
        scatterplot3d(variables_elegidas(), color = clusters()$cluster,
                      angle=40,pch=16,grid=TRUE,box=FALSE)
                     
    })
    
    
    

    output$data <- renderTable({
        if(input$radio == 1){
            fit <- kmeans(variables_elegidas(), input$clusters)
            Assigned_Cluster <-  fit$cluster
            d <- data.frame(Assigned_Cluster,DATOS)
            d[order(d$Assigned_Cluster),]}
        else if  (input$radio == 2){
            fit <- kmeans(variables_elegidas(), input$clusters)
            Centroids <- fit$centers
            f <- data.frame(Centroids)
            f}
        else if ( input$radio == 3){
            fit <- kmeans(variables_elegidas(), input$clusters)
            Size <- data.frame(fit$size)
            cbind(Size,row_number(Size))
            }
        else if ( input$radio == 4){
            fit <- kmeans(variables_elegidas(), input$clusters)
            With <- data.frame(fit$withinss)
            With
            }
        else{
            fit <- kmeans(variables_elegidas(), input$clusters)
            Bet <- data.frame(fit$betweenss)
            Bet}
        
        })
    
    
}
    


# Run the application 
shinyApp(ui = ui, server = server)
