library("shiny")
library("dplyr")
library("data.table")
library("ggplot2")
library("tidyverse")
library("shinyjs")


ui <- fluidPage(


    titlePanel("Data Exploration"),

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "party",
                        label = "Currently not working",
                        choices = c("Republican", "Democratic", "Both"))
        ),

        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Raw Data", 
                                 fluidRow(
                                     column(12, dataTableOutput("data"))
                                 )),
                        tabPanel("Positive vs Negative Engagement", 
                                 fluidRow(
                                     column(12, plotOutput("line_plot_rep")),
                                     column(12, plotOutput("line_plot_dem"))
                                 )),
                        tabPanel("Social Media Interactions vs Death Cases", 
                                 fluidRow(
                                     column(12, plotOutput("line_plot_eng_vs_death_rep")),
                                     column(12, plotOutput("line_plot_eng_vs_death_dem"))
                                 ))
        )
    )
)
)

server <- function(input, output) {
    dt <- data.table::fread("./data/state_fb_covid_2020.csv", integer64 = "character")
    rep <- dt %>% filter(gov_party == "republican")
    dem <- dt %>% filter(gov_party == "democratic")
    
    
    output$data = DT::renderDataTable({
        dt
    })
    output$line_plot_rep <- renderPlot({

        ggplot(rep) +
            geom_line(aes(Created, log(daily_positive_eng)), color="blue") +
            geom_smooth(aes(Created, log(daily_positive_eng), alpha = .25), color = "green") +
            geom_line(aes(Created, log(daily_negative_eng)), color="red") +
            geom_smooth(aes(Created, log(daily_negative_eng), alpha = .25), color = "pink") +
            facet_wrap(~state) +
            labs(title="Republican States")
        
    })
    
    output$line_plot_dem <- renderPlot({
        ggplot(dem) +
            geom_line(aes(Created, log(daily_positive_eng)), color="blue") +
            geom_smooth(aes(Created, log(daily_positive_eng)), color = "green", alpha = .25) +
            geom_line(aes(Created, log(daily_negative_eng)), color="red") +
            geom_smooth(aes(Created, log(daily_negative_eng)), color = "pink", alpha = .25) +
            facet_wrap(~state) +
            labs(title="Democratic States")
        
    })
    
    output$line_plot_eng_vs_death_rep <- renderPlot({
        ggplot(rep) +
            geom_line(aes(Created, log(daily_total_interactions)), color="blue") +
            geom_smooth(aes(Created, log(daily_total_interactions)), color = "green", alpha = .25) +
            geom_line(aes(Created, log(total_deaths)), color = "red") +
            geom_smooth(aes(Created, log(total_deaths)), color = "pink", alpha = .25) +
            facet_wrap(~state) +
            labs(title="Republican States")
        
    })
    
    
    output$line_plot_eng_vs_death_dem <- renderPlot({
        ggplot(dem) +
            geom_line(aes(Created, log(daily_total_interactions)), color="blue") +
            geom_smooth(aes(Created, log(daily_total_interactions)), color = "green", alpha = .25) +
            geom_line(aes(Created, log(total_deaths)), color = "red") +
            geom_smooth(aes(Created, log(total_deaths)), color = "pink", alpha = .25) +
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
        
        logged_rep <- logged_rep %>% select(logged_daily_comments, logged_daily_negative_eng, logged_daily_positive_eng,
                                            logged_daily_total_interactions, logged_daily_shares)
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
        logged_dem <- logged_dem %>% select(logged_daily_comments, logged_daily_negative_eng, logged_daily_positive_eng,
                                            logged_daily_total_interactions, logged_daily_shares)
        
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
