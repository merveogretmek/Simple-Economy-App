#STAT292 FINAL PROJECT
#MERVE ÖĞRETMEK - 2219806

library(shiny)
library(ggplot2)
library(plotly)
library(aod)
library(htmltools)
library(DT)



#I have collected unemployment, inflation, interest rate, GDP growth rate(%), nominal GDP($) from World Bank Data and put them into
#a Numbers file and converted to a .csv file. Then, I imported it into the R Studio and named columns as following:



#Naming variables in the dataset
names(dataset)[1] <- "Years"
names(dataset)[2] <- "Unemployment Rate (%)"
names(dataset)[3] <- "Inflation Rate (%)"
names(dataset)[4] <- "Real Interest Rate (%)"
names(dataset)[5] <- "GDP per capita Growth (%)"
names(dataset)[6] <- "Gross Domestic Product ($)"

#I will use these for naming axes in the line chart.
a <- list(title = "Years") 
b <- list(title = "in percentage(%)")


#Creating user interface
ui <- shinyUI(fluidPage(
    
    
    #Title of the app
    titlePanel("Economic Research"),
    
    #Added a sidebar
    sidebarLayout(
        
        #Added a panel in sidebar
        sidebarPanel(
          
                      #Information about the app, I have put the information part in the sidebar panel 
                      #because there is more space in sidebar than mainpanel, I thought app looks more clear this way
                      tags$div(class = "header", checked = NA,
                               list(
                                 tags$p("-For this app, I gathered unemployment, inflation, real interest rate, GDP growth and nominal GDP data."),
                                 tags$p("-In 'Linear Model' part, I created a regression where dependent variables are GDP growth and nominal GDP where explanatory variables are unemployment, inflation and real interest rate since I wanted to see their effect on GDP general."),
                                 tags$p("-In 'Data Table', we can observe the dataset and search for a year in order to see the values of variables in that year."),
                                 tags$p("-In 'Scatterplot', scatter diagram of relationship between dependent and explanatory variables are visiualized."),
                                 tags$p("-'Histogram' part shows histogroms of the explanatory variables"),
                                 tags$p("-'Line Graph' tab contains two line graphs which I believe gives important implications.")
                                 
                                 
                                )),
                      
                      #Selection option for dependent variable
                      selectInput("outcome", label = h3("Dependent Variable"),
                                  choices = list("GDP per capita Growth (%)"= "GDP per capita Growth (%)",
                                                 "Gross Domestic Product ($)" = "Gross Domestic Product ($)"), selected = 1),
                      
                      #Selection option for explanatory variable
                      selectInput("indepvar", label = h3("Explanatory variable"),
                                  choices = list("Unemployment Rate (%)" = "Unemployment Rate (%)",
                                                 "Inflation Rate (%)" = "Inflation Rate (%)",
                                                 "Real Interest Rate (%)" = "Real Interest Rate (%)"), selected = 1),
                      
                      tags$strong("Note: Selection option for dependent and explanatory variables is for the 'Regression', 'Scatterplot and 'Histogram' tabs.")),

        #Added main panel
        mainPanel(tags$div(class = "header", checked = NA,
                           list(
                             
                             #Information about the data source
                             tags$h4("Data Source"),
                             tags$p(tags$strong("Unemployment Rate:"), "https://data.worldbank.org/indicator/SL.UEM.TOTL.NE.ZS"),
                             tags$p(tags$strong("Inflation Rate:"), "https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG"),
                             tags$p(tags$strong("Real Interest Rate:"), "https://data.worldbank.org/indicator/FR.INR.RINR"),
                             tags$p(tags$strong("GDP per Capita Growth Rate:"), "https://data.worldbank.org/indicator/NY.GDP.PCAP.KD.ZG"),
                             tags$p(tags$strong("Gross Domestic Product:"), "https://data.worldbank.org/indicator/NY.GDP.MKTP.CD"))),
                          
            
            
            
            
            #Added panel for tabs
            tabsetPanel(type = "tabs",
                        
                        #Added different tabs for different types of output
                        
                        #Tab for Linear and Logistic Regression
                        tabPanel("Linear and Logistic Regression", icon = icon("list-alt"),
                                tags$h4("Linear Model Summary"),
                                verbatimTextOutput("summary1"),
                                
                                #Comment on result of summary of regression
                                tags$strong("By looking at summary table, we can see that p-value for inflation variable 
                                         is less than 0.05. This indicates that inflation has significance importance 
                                         in GDP growth. For unemployment and real interest rate we can't say the same 
                                         thing since p-values are higher."),
                                tags$h4("Logistic Model - Odds Ratio"),
                                verbatimTextOutput("oddsratio"),
                                tags$strong("Odds ratio is an another way to interpret the relationship between dependent and explanatory variable."),
                                tags$strong("An odds ratio with value larger than 1 means there is positive relationship where with value smallet than"),
                                tags$strong("1 means negative relationship. Result is consistent with the findings in summary of linear model.")),
                        
                        #Tab for data table
                        tabPanel("Data Table", icon = icon("table"),
                                 dataTableOutput('table')),
                        
                        
                        #Tab for scatter plot
                        tabPanel("Scatterplot", icon = icon("fas fa-ellipsis-h"),
                                 plotOutput("scatterplot"),
                                 
                                 #Comment on result of scatterplot
                                 tags$strong("There is a small positive relationship between unemployment rate and GDP growth (%),
                                             Nominal GDP. Actually, we expect negative relationship. The reason behind this positive
                                             effect is technology revolution at the beginning of 2000s. Due to development in technology 
                                             even when unemployment increases, total production (GDP) also increased. This phenomenon 
                                             is known as 'Jobless Growth'*. If we observe the
                                             real interest rate relationship, we will see there is a negative relationship which is expected
                                             since higher interest rates result in less investment and less production.")),
                        
                        #Tab for histogram
                        tabPanel("Histogram", icon = icon("bar-chart-o"),
                                 
                                 #Selection option for number of bins 
                                 sliderInput(inputId = "bins",
                                             label = "Number of bins:",
                                             min = 1,
                                             max = 50,
                                             value = 30),
                                 
                                 plotOutput("histogram"),
                                 
                                 #Comment on result of histogram
                                 tags$strong("According to the above figure, unemployment rate in US is usually between 5% and 6% which is actually called the 'Natural Rate of Unemployment'*."), 
                                 tags$strong("Economists says that unemployment rises above 5% only in the times of economic crises. Therefore, we frequently observed a rate of 4.5-6% in this histogram."),
                                 tags$strong("For the inflation rate, there is also another important observation. Mostly occured inflation rate in US is 2% which makes a lot of sense."),
                                 tags$strong("since the inflation target of the Central Bank of US is 2%* which implicates Central Bank is successful at achieving the inflation target.")),
                        
                        #Tab for line graphs        
                        tabPanel("Line Graphs", icon = icon("fas fa-chart-line"),
                                 
                                 plotlyOutput("linegraph1"),
                                 
                                 #Comment on result of linegraph1
                                 tags$strong("There is a general tendency for interest rates and GDP growth to have a negative 
                                            relationship. Generally, when interest rates are low, total spending in a country increases which boost 
                                            the economy. Conversely, when interest rates are high, total spending decreases and economy slows down. The line 
                                            plot above also proves this relationship."),
                                 
                                plotlyOutput("linegraph2"), 
                                
                                #Comment on result of linegraph2
                                tags$strong("According to Fisher Equation*, real interest rate and inflation rate is inversely
                                                      related. When general prices are increasing strongly, return from an investment 
                                                      would be small in real terms."),
                                
                                #Showing the equation as a footnote
                                tags$footer("Fisher Equation: real interest rate = nominal interest rate - inflation rate", align = "left",
                                             style = "position:absolute;
                                                      bottom:-50;
                                                      width:100%;
                                                      height:10px;")),
                        
                        tabPanel("References", icon = icon("fas fa-asterisk"),
                                 tags$h3("References"),
                                 tags$p(tags$strong("1. "), "Caballero, R., & Hammour, M. (1997). Jobless Growth: Appropriability, Factor Substitution, and Unemployment."),
                                 tags$p(tags$strong("2. "), "Kagan, J. (2020, May 20). What Is Natural Unemployment? Retrieved from https://www.investopedia.com/terms/n/naturalunemployment.asp"),
                                 tags$p(tags$strong("3. "), "(2019, December 11). Retrieved from https://www.federalreserve.gov/faqs/money_12848.htm"),
                                 tags$p(tags$strong("4. "), "Fisher Equation - Overview, Formula and Example. (2020, January 16). Retrieved from https://corporatefinanceinstitute.com/resources/knowledge/economics/fisher-equation/"),
                                 
                        )
                        
                        
                      
               
                                 
                                            
                                         
                                          
                                          
                         )))))



#Creating server
server <- shinyServer(function(input, output) {
    
    
    #Output for summary of simple linear regression
    output$summary1 <- renderPrint({
      
        #Creating the linear regression
        lm <- lm(dataset[,input$outcome] ~ dataset[,input$indepvar])
        
        #Naming coeffficients
        names(lm$coefficients) <- c("Intercept", input$indepvar)
        
        #Summary of the linear regression
        summary(lm)
        
    })
    
    output$oddsratio <- renderPrint({
      
        #Creating logistic regression
        glm <- glm(dataset[,input$outcome] ~ dataset[,input$indepvar])
        
        #Naming coefficient
        names(glm$coefficients) <- c("Intercept", input$indepvar)
        
        #Calculating the odds ratio
        exp(coefficients(glm))
      
      
    })
    
  
    #Output for data table
    output$table <- renderDataTable({
        
        datatable(dataset, options = list(lengthChange = FALSE))
    })
    
    #Output for the scatter plot
    output$scatterplot <- renderPlot({
      
        #Creating the scatter plot
        plot(dataset[,input$indepvar], dataset[,input$outcome], main="Scatterplot",
             xlab=input$indepvar, ylab=input$outcome, pch=19)
      
        #Adding the regression line
        abline(lm(dataset[,input$outcome] ~ dataset[,input$indepvar]), col="orange")
    }, height=400)
    
    #Output for histogram
    output$histogram <- renderPlot({
        
        #Assigning the data
        x    <- dataset[,input$indepvar]
        
        #Creating bins
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        #Creating the histogram
        hist(x, breaks = bins, col = "blue", border = "white",
             main = "Histogram of the Explanatory Variable", xlab = input$indepvar)
        
    })
    
    #Output for line graph
    output$linegraph1 <- renderPlotly({
        
        #Assigning the data
        x <- dataset$Years
        y <- dataset$`Real Interest Rate`
        z <- dataset$`GDP per capita Growth`
        
        r <- dataset$`Inflation Rate (%)`
        s <- dataset$`Unemployment Rate (%)`
        
        
        #Creating dataframes
        data1 <- data.frame(x,y)
        data2 <- data.frame(x,z)
        data3 <- data.frame(x,r)
        
        
        #Creating the linegraph1
        fig1 <- plot_ly(data1, x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = "Real Interest Rate(%)")
        fig1 <- fig1 %>% add_trace(data2, x = ~x, y = ~z, type = 'scatter', mode = 'lines', name = "GDP per capita Growth(%)")
        fig1 <- fig1 %>% layout(xaxis = a, yaxis = b, showlegend = TRUE)   })
    
   output$linegraph2 <- renderPlotly({
     
        #Creating the linegraph2
        fig2 <- plot_ly(data3, x= ~x, y = ~y, type = 'scatter', mode = 'lines', name = "Real Interest Rate (%)")
        fig2 <- fig2 %>% add_trace(data3, x = ~x, y = ~z, type = 'scatter', mode = 'lines', name = "Inflation Rate (%)")
        fig2 <- fig2 %>% layout(xaxis = a, yaxis = b, showlegend = TRUE)
        
   })
   
})
   
   


shinyApp(ui,server)






