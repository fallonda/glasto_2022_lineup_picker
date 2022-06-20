#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### Code to run on startup
# Packages
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(ggrepel)
source("./main.R")

# Source main here. 


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hi, welcome to glasto 2022 lineup picker!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("day_pick", label = "Pick your day:", choices = day_list, selected = "FRIDAY"),
            pickerInput("stages_pick", label = "Pick your stages to compare:", choices = stages_list, selected = key_stages, multiple = T,
                        options = list(
                            "actions-box" = T, size = 20, "selected-text-format" = "count > 2")
                        ),
            actionButton("about_button", label = "About", icon = icon("question")),
            br(""),
            actionButton("reset_picks", label = "Clear all picks", icon = icon("trash")),
            br(""),
            downloadButton("download_picks", label = "Download", icon = icon("save"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                # Tab1
                tabPanel(
                    title = "Pick your lineup",
                    # Top split 70%
                    fluidRow(
                        style = "overflow-y: scroll; height:60vh",
                        uiOutput("plot_ui")
                    ),
                    fluidRow(
                        br(h4("Your selected lineup:"))
                    ),
                    # Bottom split 25%
                    fluidRow(
                        style = "overflow-y: scroll; height:30vh",
                        dataTableOutput("test_text")
                    ),
                    style = "height:100vh"),
                # tab2
                tabPanel(
                    title = "Search all acts",
                    dataTableOutput("picked_stages")
                    ),
                # tab3
                tabPanel(
                    title = "Map",
                    img(src = "glasto_map.png",
                        width = "120%"), 
                    style = "overflow-y: scroll; overflow-x: scroll; height:100vh"
                )
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Stop app when browser is closed. 
    session$onSessionEnded(function() {
        stopApp()
    })
    
    # About modal box
    observeEvent(input$about_button, {
        showModal(modalDialog(title = "About the Glastonbury 2022 lineup picker",
                              p("- Select a day and stages to view in the left-hand-side menu."),
                              p("- This will populate the plot on the right, which you can select acts that you want to save to your lineup."),
                              p("- You can also `right click Save Image As...` for this plot if you like."),
                              p("- You can save your lineup as a csv file by clicking the `Download` button."),
                              p("- Data accurate as of 2022-06-19. Please double check for updates on the glastonbury website for your final pick."),
                              p(""),
                              p("David Fallon, david.j.fallon@gmail.com."),
                              tags$a(href = "https://www.linkedin.com/in/david-fallon-828973a6", "LinkedIn"), p(""),
                              tags$a(href = "https://github.com/fallonda/glasto_2022_lineup_picker", "Github repository"),
                              size = "l",
                              easyClose = T))
    })
    
    # Count how many stages have been selected to calculate plot length
    plot_length <- reactive({
        length(unique(unlist(input$stages_pick))) * 200
    })
    
    # Plot output
    output$plot_ui <- renderUI({
        plotOutput("comp_plot", height = paste0(plot_length(), "px"), click = "selected_acts")
    })
    
    pivot_time_w_picked <- reactive({
        pivot_time %>%
            left_join(values$indices_df, by = "index")
    })
    
    # Filter pivot table based on picked stages and selected day. 
    active_pivot_time <- reactive({
        pivot_time_w_picked() %>%
            filter(stage %in% input$stages_pick) %>%
            filter(day == input$day_pick)
    })
    
    # test test
    
    observeEvent(input$selected_acts, {
        index_clicked <- unique(nearPoints(pivot_time, input$selected_acts)$index)
        values$indices_df[index_clicked, "picked"] <- !values$indices_df[index_clicked, "picked"]
    })
    
    all_stages_w_picked <- reactive({
        all_stages %>%
            mutate(day = factor(day, levels = days, ordered = T)) %>%
            left_join(values$indices_df, by = "index") %>%
            select(act, day, time_range, stage, picked) %>%
            arrange(day, time_range)
    })
    
    output$test_text <- renderDataTable({ all_stages_w_picked() %>% filter(picked) })
    
    # Plot call
    active_pivot_plot <- reactive({
        CompPlot(active_pivot_time())
    })
    
    output$comp_plot <- renderPlot({ active_pivot_plot() })
    
    # table output for 2nd tab. 
    output$picked_stages <- renderDataTable({ all_stages_w_picked() })
    
    # Clear outputs if clicked. 
    observeEvent(input$reset_picks, {
      values$indices_df$picked <- F  
    })
    
    # Download picks
    output$download_picks <- downloadHandler(
        filename = function() {
            paste0("glasto_picks_", str_replace_all(Sys.time(), "[:punct:]|[:space:]", ""), ".csv")
        },
        content = function(file) {
            write.csv(all_stages_w_picked() %>% filter(picked), file, row.names = F)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
