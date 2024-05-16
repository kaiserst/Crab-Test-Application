library(shiny)
library(tidyverse)
library(readxl)
library(sf)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Dungeness Crab Test Fishery Data Visualization"),

    # Sidebar with a file input to upload .xlsx with data
    sidebarLayout(
        sidebarPanel(
          fileInput("upload", "Upload a file", accept = c(".xlsx"))
        ),
        
        # Main panel with map of sample locations and table
        mainPanel(
          plotOutput("CPUE"),
          tableOutput("CPUE_tab")
        )
    )
)

# Define server function
server <- function(input, output) {
  # Create reactive data based on .xlsx input
  data <- reactive({
    req(input$upload)
    read_excel(input$upload$datapath)
  })
  
  # Do initial cleaning of the data
  crab <- reactive({
    data() %>%
      mutate_at(vars('shell_condition', 'M_F_Sub'), ~str_replace(., "FEM_", "")) %>%
      mutate_at(vars('shell_condition', 'M_F_Sub'), ~str_replace(., "1-1M", "1_1")) %>%
      mutate_at(vars('shell_condition', 'M_F_Sub'), ~str_replace(., "1_1M", "1_1")) %>%
      mutate_at(vars('shell_condition', 'M_F_Sub'), ~str_replace(., "EGGS", "1_1")) %>%
      mutate_at(vars('shell_condition', 'M_F_Sub'), ~str_replace(., "RS", "1_1")) %>%
      mutate_at(vars('shell_condition', 'M_F_Sub'), ~str_replace(., "1-1", "1_1")) %>%
      mutate_at(vars('shell_condition', 'M_F_Sub'), ~str_replace(., "1-2", "1_2")) %>%
      mutate_at(vars('shell_condition', 'M_F_Sub'), ~str_replace(., "2-1", "2_1")) %>%
      mutate_at(vars('shell_condition', 'M_F_Sub'), ~str_replace(., "2-2", "2_2")) %>%
      mutate_at(vars('shell_condition', 'M_F_Sub'), ~str_replace(., "3-1", "3_1")) %>%
      mutate_at(vars('shell_condition', 'M_F_Sub'), ~str_replace(., "1-3", "1_3")) %>%
      mutate(stage = case_when(
        startsWith(shell_condition, "1") ~ "1",
        startsWith(shell_condition, "2_1") ~ "2_1",
        startsWith(shell_condition, "2_2") ~ "2_2",
        startsWith(shell_condition, "3") ~ "3"),
        Pot = as.character(Pot)) %>%
      unite('demo', M_F_Sub, stage, remove = FALSE)
  })
  
  # Calculate relevant CPUE columns
  cpue <- reactive({
    crabdat <- crab() %>%
      group_by(Pot, Lat, Long, demo) %>%
      summarise(count = n()) %>%
      pivot_wider(names_from = demo, values_from = count, values_fill = list(count = 0)) %>%
      rowwise() %>%
      mutate(total = sum(c_across(1:11)),
             LSM = sum(c(LM_1, LM_2_1, LM_2_2, LM_3)),
             HLSM = sum(c(LM_1, LM_2_1)),
             PCT_HLSM = (HLSM / LSM)*100)
  })
  
  # Call create sf object and call map output
  output$CPUE <- renderPlot({
    cpue_dat <- cpue()
    CPUE_sf <- st_as_sf(cpue_dat, coords = c('Long', 'Lat'), crs = 4326)
    ggplot() +
      geom_sf(data = CPUE_sf, aes(color = LSM)) +
      theme_bw()
  })
  
  # Call table output
  output$CPUE_tab <- renderTable({cpue()})
}


# Run the application 
shinyApp(ui = ui, server = server)
