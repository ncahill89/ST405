#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Exploring Priors",id = "chooseAction",

    # Application title
    #titlePanel("Happiness Example"),

    # Sidebar with a slider input for number of bins 
    tabPanel(title = "Simple Priors", value = "simple",style="height: 500px",
        sidebarPanel(
            radioButtons("prior","Choose prior form",
                         c("Triangular","Uniform")),
            numericInput("grid",
                        paste("Choose a grid size for",expression(theta)),
                        min = 10,
                        max = 1000,
                        value = 11),
            numericInput("N",
                         "Choose N (for Binomal likelihood)",
                         min = 1,
                         max = 1000,
                         value = 20),
            numericInput("Y",
                         "Choose Y (# of sucesses)",
                         min = 1,
                         max = 1000,
                         value = 14)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ptheta")
        )
    ),
    
    tabPanel(title = "Beta Priors", value = "beta",style="height: 500px",
             sidebarPanel(
               sliderInput("a",
                            "Choose a for Be(a,b)",
                            min = 0,
                            max = 10,
                            value = 1),
               sliderInput("b",
                            "Choose b for Be(a,b)",
                            min = 0,
                            max = 10,
                            value = 1),
               numericInput("N2",
                            "Choose N (for Binomal likelihood)",
                            min = 1,
                            max = 1000,
                            value = 20),
               numericInput("Y2",
                            "Choose Y (# of sucesses)",
                            min = 1,
                            max = 1000,
                            value = 14)
               
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("ptheta2")
             )
    )
    
    
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$ptheta <- renderPlot({
        theta <- seq(0,1,length = input$grid) # grid of theta values
        if(input$prior == "Triangular")
        {
        Prior <- pmin(theta, 1-theta) # triangular shape
        Prior = Prior/sum(Prior) # make sum to 1
        }
        if(input$prior == "Uniform")
        {
            Prior <- 1/input$grid
        }
        ptheta_dat <- tibble::tibble(theta, Prior)
        
        Likelihood <- dbinom(input$Y,input$N,prob = theta)
        ptheta_dat$Likelihood <- Likelihood
        
        Posterior <- Likelihood*Prior
        ptheta_dat$Posterior <- Posterior
        
        
        ptheta_dat2 <- ptheta_dat %>% pivot_longer(cols = Prior:Posterior,
                                                   names_to = "type",
                                                   values_to = "value") %>%
            mutate(type = factor(type, levels =c("Prior","Likelihood","Posterior")))
        
        ggplot(ptheta_dat2, aes(x = theta, y = value)) +
            geom_segment(aes(xend=theta,yend=0)) +
            facet_wrap(~type, scales = "free_y", nrow = 3) +
            xlab(expression(theta)) +
            ylab("") +
            theme_bw()
    })
    
    output$ptheta2 <- renderPlot({
      y <- input$Y2
      n <- input$N2
      a <- input$a
      b <- input$b
      
      n_grid = 1000
      theta <- seq(0,1,length = n_grid) # grid of theta values
      Prior <- dbeta(theta,a,b)
      
      ptheta_dat <- tibble::tibble(theta, Prior)
      
      Likelihood <- dbinom(y,n,prob = theta)
      ptheta_dat$Likelihood <- Likelihood
      
      
      Posterior <- dbeta(theta,y+a,n-y+b)
      ptheta_dat$Posterior <- Posterior
      
      
      ptheta_dat2 <- ptheta_dat %>% pivot_longer(cols = Prior:Posterior,
                                                 names_to = "type",
                                                 values_to = "value") %>%
        mutate(type = factor(type, 
                             levels =c("Prior","Likelihood","Posterior"))) %>% 
        mutate(type = recode_factor(type, `Prior` = paste0("theta %~% Be(",a,",",b,")"), `Likelihood` = "y %~% Binomial(N,theta)",`Posterior` =  paste0("theta / y %~% Be(y+",a,", y-N+",b,")")))
      
      
      ggplot(ptheta_dat2, aes(x = theta, y = value)) +
        geom_line() +
        #geom_segment(aes(xend=theta,yend=0),alpha = 0.2) +
        facet_wrap(~type, label = "label_parsed", nrow = 3, scales = "free_y") +
        xlab(expression(theta)) +
        ylab("") +
        theme_bw() 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
