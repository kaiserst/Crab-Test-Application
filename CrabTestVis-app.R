library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(sf)
library(tmap)

# Define radio button options
sexes <- c("M", "F")
ctype <- c("Histogram", "Density Plot")

# Define UI for application
ui <- page_sidebar(
  # Set Theme
  theme = bs_theme(bootswatch = "minty"),
  
  # Application title
  title = "Dungeness Crab Test Fishery Data: Simple Visualization",
  sidebar = sidebar(
    strong("Application Use Instructions"),
    p("This application is designed to assist in the rapid visualization of Dungeness 
      crab test fishery data. Upon uploading a properly formatted .xlsx file the user can
      click through the available tabs to interact with various charts, tables & graphics 
      that describe the data."),
    fileInput("upload", "Upload a File", accept = c(".xlsx")),
    strong("Options for Map Display"),
    p("fill in some more here"),
    strong("Options for Size-Frequency Display"),
    radioButtons("sextype", "Sex type", sexes),
    radioButtons("charttype", "Chart type", ctype),
    width = 350
  ),
  
  navset_card_pill(
    title = "Visualizations",
    nav_panel("Pot Locations & CPUE Summary", 
              tmapOutput("CPUE"),
              tableOutput("CPUE_tab"),
              code("Summary of test data by area. Numbers represent the mean number of crabs 
                of each demographic per pot (CPUE)")),
    nav_panel("Size-Frequency",
              plotOutput("sizefreq"),
              code("Dotted Red line represents threshold of legal size male crab (158.75mm)"))
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
        Pot = as.character(Pot),
        Location = GenLocation) %>%
      unite('demo', M_F_Sub, stage, remove = FALSE)
  })
  
  # Calculate relevant CPUE columns
  cpue <- reactive({
    crab() %>%
      group_by(Location, Pot, Lat, Long, demo) %>%
      summarise(count = n()) %>%
      pivot_wider(names_from = demo, values_from = count, values_fill = list(count = 0)) %>%
      rowwise() %>%
      mutate(total = sum(c_across(1:11)),
             LSM = sum(c(LM_1, LM_2_1, LM_2_2, LM_3)),
             FEM = sum(c(FEM_1, FEM_2_1, FEM_2_2)),
             SUB = sum(c(SUB_1, SUB_2_1, SUB_2_2, SUB_3)),
             HLSM = sum(c(LM_1, LM_2_1)),
             PCT_HLSM = (HLSM / LSM)*100) %>% 
      replace(is.na(.), 0)
  })
  
  #Create table for display
  cpue.tab <- reactive({
    cpue() %>%
      select(Location, Pot, Lat, Long, total, LSM, FEM, SUB, HLSM, PCT_HLSM) %>%
      group_by(Location) %>%
      summarise(`All Crab` = mean(total),
                `Legal Male` = mean(LSM),
                `Hard Legal Male` = mean(HLSM),
                `Percent Hard LSM` = mean(PCT_HLSM),
                `Female` = mean(FEM),
                `Sublegal` = mean(SUB))
  })
  
  # Subset Male Crab for SF
  filtcrab <- reactive({
    crab() %>% filter(M_F == input$sextype)
  })
  
  # Call create sf object and call map output
  output$CPUE <- renderTmap({
    cpue_dat <- cpue()
    CPUE_sf <- st_as_sf(cpue_dat, coords = c('Long', 'Lat'), crs = 4326)
    tmap_mode("view")
    tm_layout(title = "Test Pot Locations") +
      tm_shape(CPUE_sf) +
      tm_bubbles(size = 0.3, col = 'LSM', id = "Pot",
                 popup.vars = c(
                   'Total' = 'total',
                   'Hard LSM' = 'HLSM',
                   'Percent Hard' = 'PCT_HLSM'
                 )) + 
      tm_text("HLSM", size = 0.85, col = "darkred", fontface = "bold", just = "left", xmod = 0.5, ymod = 0.5)
  })
  
  # Call table output
  output$CPUE_tab <- renderTable({cpue.tab()}, 
                                 digits = 1, 
                                 align = 'c')
  
  # Call sf output
  chartype <- reactive({input$charttype})
  
  output$sizefreq <- renderPlot({
    if (chartype() == "Histogram") {
      ggplot(data = filtcrab(), aes(x = Rnd_CW)) +
        geom_histogram(aes(col = I("black"), fill = shell_condition),
                       binwidth = 1,
                       position = "stack",
                       size = 0.2) +
        labs(x = "Carapace Width (mm)", y = "Frequency of Crab") +
        geom_vline(aes(xintercept = 159.75),
                   color = "darkred", size = 1, linetype = "dashed") +
        guides(fill = guide_legend("Shell Condition")) + 
        theme_minimal() + 
        theme(text = element_text(size = 14))
      
      } else {
      ggplot(data = filtcrab(), aes(x = Rnd_CW)) +
        geom_density(fill = "lightblue", alpha = 0.5) +
        labs(x = "Carapace Width (mm)", y = "Probability Density") +
          geom_vline(aes(xintercept = 159.75),
                     color = "darkred", size = 1, linetype = "dashed") +
        scale_y_continuous(expand = c(0, 0)) +
        theme_minimal() + 
        theme(text = element_text(size = 14))
    }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
