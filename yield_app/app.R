# load libraries
library(shiny)
library(tidyverse)
library(plotly)

# Load data
yield_data <- readRDS("yields_tidy.rds")

#Explore the data
top_crops <- yields_tidy %>%
    count(crop, sort = TRUE) %>%
    head(9) %>%
    pull(crop)


# Counting the country frequency
#yield_data %>% count(entity, sort = T) %>% view()

# filter 3 countries
agric <- yield_data %>%
    filter(entity %in% c("Tanzania", 
                         "Kenya", 
                         "Zimbabwe")) %>% 
    # filter for 4 crops
    filter(crop %in% c("Maize",
                       "Beans",
                       "Bananas"))



# plot one crop and one country over the years

plot_yields <- function(dataframe, facet_scales = "fixed") {
    
    g <- dataframe %>%
        mutate(crop = fct_reorder(crop, -yield)) %>%
        mutate(entity = fct_reorder(entity, -yield)) %>%
        ggplot(aes(year, yield, color = entity)) +
        geom_line() +
        expand_limits(y = 0) +
        facet_wrap(~ crop, scales = facet_scales) +
        labs(x = "Year",
             y = "Yield (tonnes per hectare)",
             title = "Crop yields over time",
             color = "Country")
    
    ggplotly(g)
}

#plot(agric)

ui <- fluidPage(
    
    # Application title
    titlePanel("Agicultural produce"),

sidebarLayout(
    
    # Sidebar with a select input
    sidebarPanel(
        selectInput("entity",
                    label = "Country/Continent/Region:",
                    choices = unique(yields_tidy$entity),
                    selected = c("United States", "India"),
                    selectize = TRUE,
                    multiple = TRUE),
        selectInput("crop",
                    label = "Crops:",
                    choices = unique(yields_tidy$crop),
                    selected = top_crops,
                    selectize = TRUE,
                    multiple = TRUE),
        radioButtons("facet_scales",
                     label = "",
                     choices = c("Free y-axis" = "free_y",
                                 "Shared y-axis" = "fixed"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        plotlyOutput ("plot1")
    )
)
)

# Server logic
server <- function(input, output) {
    output$plot1 <- renderPlotly({
        yields_tidy %>%
            filter(entity %in% input$entity,
                   crop %in% input$crop) %>%
            plot_yields(facet_scales = input$facet_scales)
    })
    
        
}

# Complete app with UI and server components
shinyApp(ui, server)


