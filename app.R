# Library used----
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")

# loading the data ----
sdat <- read.csv("sdt.csv")
sdata <- gather(sdat, substratum, observations, 2:6)


# Stratified Two Stage Function ----
Straified_2s <- function(df, Nh, Mh){
  
  stage1 <- df[[1]]
  stage2 <- df[[2]]
  x <- df[[3]]
  dat <- data.frame(stage1, stage2, x)
  
  # calculating the number of sub-stratum selected in stratum h (i.e. nh)
  
  nh <- dat %>% group_by(stage1, stage2) %>% tally() %>%
    select(stage1, stage2) %>% group_by(stage1) %>% tally() %>% .$n

  # calculating the number of element selected from each selected
  # sampling unit in stratum h (i.e. mh)
  
  mh <- dat %>% group_by(stage1, stage2) %>%
    tally() %>% select(stage1, n) %>% group_by(stage1) %>%
    summarise(mhs = mean(n)) %>% .$mhs
  
  f1h <- nh/Nh
  
  f2h <- mh/Mh
  
  SubStratum_Means <- dat %>% group_by(stage1, stage2) %>%
    summarise(SubStratum_Mean = mean(x)) ## SubStratum_Means  calculation of sub units means
  
  StratumMeans <- SubStratum_Means %>% group_by(stage1) %>%
    summarise(Stratum_Means = mean(SubStratum_Mean)) ## 2ndStage_Means <- SubUnits_Means %>%
  
  Var.h1 <- SubStratum_Means %>% group_by(stage1) %>%
    summarise(Var.B = var(SubStratum_Mean)) # Variance between units mean
  
  Var.h2 <- dat %>% group_by(stage1, stage2) %>% 
    summarise(SubStratum_Variance = var(x)) %>% ## SubStratum_Variance
    group_by(stage1) %>% summarise(Var.W = mean(SubStratum_Variance)) # Variance Within Primary Units
  
  Var_St.h <- (((1-f1h)/nh)*Var.h1$Var.B) + (f1h*(1-f2h)*Var.h2$Var.W/(mh*nh))
  
  Estimates <- StratumMeans %>% full_join(Var.h1, by = "stage1") %>%
    full_join(Var.h2, by = "stage1") %>% data.frame(Var_St.h)
  
  # Estimates of Population Mean, its Standard Error and the Confidence Interval
  
  Pop_Mean <- sum((Nh*Mh)/(length(unique(stage1))*Nh*Mh)*StratumMeans$Stratum_Means)
  
  Pop_Mean.Var <- sum(((Nh*Mh)/(length(unique(stage1))*Nh*Mh))^2*Var_St.h)
  
  Std.Error_Mean <- sqrt(Pop_Mean.Var)/sqrt(length(unique(stage1))*Nh*Mh)
  
  Lower_Mean <- Pop_Mean - 1.96*Std.Error_Mean
  
  Upper_Mean <- Pop_Mean + 1.96*Std.Error_Mean
  
  
  # Estimate of Population Total, its Standard Error and the Confidence Interval
  
  Pop_Total <- (length(unique(stage1))*Nh*Mh)*Pop_Mean
  
  Pop_Total.Var <- ((length(unique(stage1))*Nh*Mh)^2)*Pop_Mean.Var
  
  Std.Error_Total <- sqrt(Pop_Total.Var)/sqrt(length(unique(stage1))*Nh*Mh)
  
  Lower_Total <- Pop_Total - 1.96*Std.Error_Total
  
  Upper_Total <- Pop_Total + 1.96*Std.Error_Total
  
  EstPopMean <- data.frame(Pop_Mean, Pop_Mean.Var, Std.Error_Mean, Lower_Mean, Upper_Mean)
  names(EstPopMean) = c("Population Mean", "Variance", "Standard Error", "Lower", "Upper")
  
  EstPopTotal <- data.frame(Pop_Total, Pop_Total.Var, Std.Error_Total, Lower_Total, Upper_Total)
  names(EstPopTotal) = c("Population Total", "Variance", "Standard Error", "Lower", "Upper")
  
  return(
    list(
      Estimates = Estimates,
      
      Estimate_Population_Mean = EstPopMean,
      
      Estimate_Population_Total = EstPopTotal
    )
  )
}


# Define UI for application that draws a histogram----
ui <- fluidPage(

    # Application title
    titlePanel("SHINY BY Ridwan Oduola @MOOR METRICS"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectInput("stratum", "1. Select Stratum",
                    choices = c(
                                "Stratum 1" = "1",
                                "Stratum 2" = "2",
                                "Stratum 3" = "3",
                                "Stratum 4" = "4",
                                "Stratum 5" = "5"
                                ),
                    selected = "1"
                    ), br(),
            selectInput(
              "substratum", "2. Select Substratum",
                        choices = names(sdat[2:6]),
                        selected = "A"
                        ),br(),
            sliderInput("bin", "3. Select number of histogram bins", min=1, max=25,
                        value= c(10)
                        ),br(),
            selectInput("pry", "4. Select Primary Unit Sample Size",
                        choices = (2:4),
                        selected = 2
                        ), br(),
            selectInput("secondary", "5. Select Secondary Unit Sample Size",
                        choices = c(4,8,12,16),
                        selected = 4
                        ),br()
            ),
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Histogram Plot",
                                       plotOutput("myHist")
                                       ),
                              tabPanel("Density Plot",
                                       plotOutput("myDensity")
                                       ),
                              tabPanel("Stratified Sampling Estimator",
                                       tableOutput("myTable1"),
                                       tableOutput("myTable2"),
                                       tableOutput("myTable3")
                                       )
                              )
                  )
      )
    )

# Define server logic required to draw a histogram
# and calculate the estimates of two stage stratified sampling----
server <- function(input, output) {

    output$myHist <- renderPlot(
      ggplot(sdata, aes(x = observations)) +
        geom_histogram(bins = input$bin,
                       #group = input$substratum & input$stratum,
                       data = sdata[(sdata$substratum == input$substratum) & (sdata$stratum == input$stratum),],
                       colour = "black")
    )
    
    output$myDensity <- renderPlot(
      ggplot(sdata, aes(x = observations)) +
        geom_density(data = sdata[(sdata$stratum == input$stratum) & (sdata$substratum == input$substratum),],
                     colour = "black"
                     )
    )
    
    output$myTable1 <- renderTable(sdat %>%
                                     select(1, sample(2:6, input$pry)) %>%
                                      .[c((sample(1:20, input$secondary)), (sample(21:40, input$secondary)),
                                          (sample(41:60, input$secondary)), (sample(61:80, input$secondary)),
                                          (sample(81:100, input$secondary))),] %>%
                                      gather(substratum, observations, c(2:length(.))) %>%
                                      Straified_2s(Nh = 5, Mh = 20) %>% 
                                      .$Estimates)
    output$myTable2 <- renderTable(sdat %>%
                                     select(1, sample(2:6, input$pry)) %>%
                                     .[c((sample(1:20, input$secondary)), (sample(21:40, input$secondary)),
                                         (sample(41:60, input$secondary)), (sample(61:80, input$secondary)),
                                         (sample(81:100, input$secondary))),] %>%
                                     gather(substratum, observations, c(2:length(.))) %>%
                                     Straified_2s(Nh = 5, Mh = 20) %>%
                                     .$Estimate_Population_Mean)
    output$myTable3 <- renderTable(sdat %>%
                                     select(1, sample(2:6, input$pry)) %>%
                                     .[c((sample(1:20, input$secondary)), (sample(21:40, input$secondary)),
                                         (sample(41:60, input$secondary)), (sample(61:80, input$secondary)),
                                         (sample(81:100, input$secondary))),] %>%
                                     gather(substratum, observations, c(2:length(.))) %>%
                                     Straified_2s(Nh = 5, Mh = 20) %>%
                                     .$Estimate_Population_Total)
    
}

# Run the application----
shinyApp(ui = ui, server = server)






