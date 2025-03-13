#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Anna Rakes & Lexi Waller

library(shiny)
library(data.table)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shiny App Project - Anna Rakes & Lexi Waller"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("distribution",
                        "Choose a distribution:",
                        choices = c("Normal Distribution",
                        "T-distribution"),
                        selected = "Normal Distribution",
                        multiple = FALSE),
            h5("Only if you chose a T-distribution, enter the degrees of freedom"),
            numericInput("df",
                         "Degrees of Freedom",
                         value=2,
                         min=0),
            numericInput("testStatistic",
                         "Test Statistic:",
                         value=1),
            radioButtons("direction",
                         "Select the direction of the p-value:",
                         choices = c("Two-Tailed","Left", "Right"),
                         select = "Two-Tailed")
                        
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      if (input$distribution == "Normal Distribution"){
        normalDistribution <- data.frame(
          x = seq(-4,4, by = 0.01),
          y = dnorm(seq(-4,4, by = 0.01))
        )
        if (input$direction == "Two-Tailed"){
          criticalValues <- c(-1*abs(input$testStatistic),abs(input$testStatistic))
          
          shadeNormalTwoTailedLeft <- rbind(c(criticalValues[1],0), subset(normalDistribution, x < criticalValues[1]))
          
          shadeNormalTwoTailedRight <- rbind(c(criticalValues[2],0), subset(normalDistribution, x > criticalValues[2]), c(3,0))
          
          ggplot(normalDistribution, aes(x,y)) +
            geom_line() +
            geom_polygon(data = shadeNormalTwoTailedLeft, aes(x=x, y=y, fill="red")) +
            geom_polygon(data = shadeNormalTwoTailedRight, aes(x=x, y=y, fill="red")) +
            guides(fill="none") +
            geom_hline(yintercept = 0) +
            geom_segment(aes(x = criticalValues[1], y = 0, xend = criticalValues[1], yend = dnorm(criticalValues[1]))) +
            geom_segment(aes(x = criticalValues[2], y = 0, xend = criticalValues[2], yend = dnorm(criticalValues[2]))) 
        }
        else if (input$direction == "Left"){
          criticalValue <- input$testStatistic
          
          shadeNormal <- rbind(c(criticalValue,0), subset(normalDistribution, x < criticalValue))
          
          ggplot(normalDistribution, aes(x,y)) +
            geom_polygon(data = shadeNormal, aes(x=x, y=y, fill="red")) +
            geom_line() +
            guides(fill="none") +
            geom_hline(yintercept = 0) +
            geom_segment(aes(x = criticalValue, y = 0, xend = criticalValue, yend = dnorm(criticalValue))) 
          
        }
        else {
          criticalValue <- input$testStatistic
          
          shadeNormal <- rbind(c(criticalValue,0), subset(normalDistribution, x > criticalValue), c(3,0))
          
          ggplot(normalDistribution, aes(x,y)) +
            geom_line() +
            geom_polygon(data = shadeNormal, aes(x=x, y=y, fill="red")) +
            guides(fill="none") +
            geom_hline(yintercept = 0) +
            geom_segment(aes(x = criticalValue, y = 0, xend = criticalValue, yend = dnorm(criticalValue))) 
          
        }
        
      }
      else if (input$distribution == "T-distribution") {
        tDistribution <- data.frame(
          x = seq(-4,4, by = 0.01),
          y = dt(seq(-4,4, by = 0.01),input$df)
        )
        if (input$direction == "Two-Tailed"){
          criticalValues <- c(-1*abs(input$testStatistic),abs(input$testStatistic))
          
          shadeNormalTwoTailedLeft <- rbind(c(criticalValues[1],0),c(-4,0), subset(tDistribution, x < criticalValues[1]))
          
          shadeNormalTwoTailedRight <- rbind(c(criticalValues[2],0), subset(tDistribution, x > criticalValues[2]), c(4,0))
          
          
          
          ggplot(tDistribution, aes(x,y)) +
            geom_line() +
            geom_polygon(data = shadeNormalTwoTailedLeft, aes(x=x, y=y, fill="red")) +
            geom_polygon(data = shadeNormalTwoTailedRight, aes(x=x, y=y, fill="red")) +
            guides(fill="none") +
            geom_hline(yintercept = 0) +
            geom_segment(aes(x = criticalValues[1], y = 0, xend = criticalValues[1], yend = dt(criticalValues[1],input$df))) +
            geom_segment(aes(x = criticalValues[2], y = 0, xend = criticalValues[2], yend = dt(criticalValues[2],input$df))) 
        }
        else if (input$direction == "Left"){
          criticalValue <- input$testStatistic
          
          shadeNormal <- rbind(c(criticalValue,0),c(-4,0), subset(tDistribution, x < criticalValue))
          
          ggplot(tDistribution, aes(x,y)) +
            geom_polygon(data = shadeNormal, aes(x=x, y=y, fill="red")) +
            geom_line() +
            guides(fill="none") +
            geom_hline(yintercept = 0) +
            geom_segment(aes(x = criticalValue, y = 0, xend = criticalValue, yend = dt(criticalValue,input$df))) 
          
        }
        else {
          criticalValue <- input$testStatistic
          
          shadeNormal <- rbind(c(criticalValue,0), subset(tDistribution, x > criticalValue), c(4,0))
          
          ggplot(tDistribution, aes(x,y)) +
            geom_line() +
            geom_polygon(data = shadeNormal, aes(x=x, y=y, fill="red")) +
            guides(fill="none") +
            geom_hline(yintercept = 0) +
            geom_segment(aes(x = criticalValue, y = 0, xend = criticalValue, yend = dt(criticalValue,input$df))) 
          
        }
        
      }
      }
        
      )
    output$text <- renderText({
      if (input$distribution == "Normal Distribution"){
        if (input$direction == "Two-Tailed"){
          criticalValues <- c(-1*abs(input$testStatistic),abs(input$testStatistic))
          pvalue <- 2*pnorm(criticalValues[1])
          print(paste("The p-value for a test statistic of", input$testStatistic, "from a two-tailed", input$distribution, "with a mean of 0 and a standard deviation of 1 is", round(pvalue,4)))
        }
        
        else if (input$direction == "Left"){
          criticalValue <- input$testStatistic
          pvalue <- pnorm(criticalValue)
          print(paste("The p-value for a test statistic of", input$testStatistic, "from a left-tailed", input$distribution, "with a mean of 0 and a standard deviation of 1 is", round(pvalue,4)))
        }
        
        else {
          criticalValue <- input$testStatistic
          pvalue <- pnorm(criticalValue, lower.tail=FALSE)
          print(paste("The p-value for a test statistic of", input$testStatistic, "from a right-tailed", input$distribution, "with a mean of 0 and a standard deviation of 1 is", round(pvalue,4)))
          
        }
        

      }
      else if (input$distribution == "T-distribution"){
        if (input$direction == "Two-Tailed"){
          criticalValues <- c(-1*abs(input$testStatistic),abs(input$testStatistic))
          pvalue <- 2*pt(criticalValues[1],input$df)
          print(paste("The p-value for a test statistic of", input$testStatistic,"from a two-tailed", input$distribution, "with",input$df, "degrees of freedom is", round(pvalue,4)))
        }
        else if (input$direction == "Left"){
          criticalValue <- input$testStatistic
          pvalue <- pt(criticalValue,input$df)
          print(paste("The p-value for a test statistic of", input$testStatistic, "from a left-tailed", input$distribution, "with", input$df, "degrees of freedom is", round(pvalue,4)))
        }
        else {
          criticalValue <- input$testStatistic
          pvalue <- pt(criticalValue,input$df, lower.tail=FALSE)
          print(paste("The p-value for a test statistic of", input$testStatistic, "from a right-tailed", input$distribution, "with", input$df, "degrees of freedom is", round(pvalue,4)))
          
        }
      }
    }
)
}

# Run the application 
shinyApp(ui = ui, server = server)
