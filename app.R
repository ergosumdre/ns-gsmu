library(shiny)
library("dplyr")
library("data.table")
library("ggplot2")
library("tidyverse")
ui <- fluidPage(

    titlePanel("Data Exploration"),

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "party",
                        label = "Select Party",
                        choices = c("Republican", "Democratic", "Both"))
        ),

        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Line Chart", 
                                 fluidRow(
                                     column(8, plotOutput("line_plot_rep")),
                                     column(12, plotOutput("line_plot_dem"))
                                 )),
                        tabPanel("Engagement Histogram", 
                                 fluidRow(
                                     column(8, plotOutput("histogram_rep")),
                                     column(12, plotOutput("histogram_dem"))
                                 ))
        )
    )
)
)

server <- function(input, output) {
    dt <- data.table::fread("/data/state_fb_covid_2020.csv", integer64 = "character")
    rep <- dt %>% filter(gov_party == "republican")
    dem <- dt %>% filter(gov_party == "democratic")
    
    output$line_plot_rep <- renderPlot({
        library("data.table")
        library("dplyr")

        ggplot(rep) +
            geom_line(aes(Created, log(daily_positive_eng)), color="blue") +
            geom_line(aes(Created, log(daily_negative_eng)), color="red") +
            facet_wrap(~state) +
            labs(title="Republican States")
        
    })
    
    output$line_plot_dem <- renderPlot({
        ggplot(dem) +
            geom_line(aes(Created, log(daily_positive_eng)), color="blue") +
            geom_line(aes(Created, log(daily_negative_eng)), color="red") +
            facet_wrap(~state) +
            labs(title="Democratic States")
        
    })
    
    output$histogram_rep <- renderPlot({
        logged_rep <- rep %>% select(daily_comments, daily_negative_eng, daily_positive_eng,
                                     daily_total_interactions, daily_shares)
        logged_rep <- logged_rep %>% mutate(logged_daily_comments = log(daily_comments),
                                            logged_daily_negative_eng = log(daily_negative_eng),
                                            logged_daily_positive_eng = log(daily_positive_eng),
                                            logged_daily_total_interactions = log(daily_total_interactions),
                                            logged_daily_shares = log(daily_shares))
        
        logged_rep %>%
            keep(is.numeric) %>% 
            gather() %>% 
            ggplot(aes(value)) + theme_minimal() +
            facet_wrap(~ key, scales = "free") +
            geom_histogram(bins = 50, color = "red") + 
            theme(axis.title=element_text(size=16,face="bold")) + 
            theme(text = element_text(size = 20)) +
            labs(title="Republican States")
        
    })
    
    output$histogram_dem <- renderPlot({
        logged_dem <- dem %>% select(daily_comments, daily_negative_eng, daily_positive_eng,
                                     daily_total_interactions, daily_shares)
        logged_dem <- logged_dem %>% mutate(logged_daily_comments = log(daily_comments),
                                            logged_daily_negative_eng = log(daily_negative_eng),
                                            logged_daily_positive_eng = log(daily_positive_eng),
                                            logged_daily_total_interactions = log(daily_total_interactions),
                                            logged_daily_shares = log(daily_shares))
        
        logged_dem %>%
            keep(is.numeric) %>% 
            gather() %>% 
            ggplot(aes(value)) + theme_minimal() +
            facet_wrap(~ key, scales = "free") +
            geom_histogram(bins = 50, color = "blue") + 
            theme(axis.title=element_text(size=16,face="bold")) + 
            theme(text = element_text(size = 20)) +
            labs(title="Democratic States")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
