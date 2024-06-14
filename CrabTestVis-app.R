library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(shinyjs)
library(leaflet)
library(htmlwidgets)
library(tools)

# Define Map Options
colVar <- c("LSM", "FEM", "SUB", "HLSM", "PCT_HLSM", "total")

# Define SF Options
sexes <- c("M", "F")
ctype <- c("Histogram", "Density Plot")
loc <- c("Entire Test", "General Location")

# Define UI for application
ui <- page_sidebar(
  # Initialize shinyjs
  useShinyjs(), 
  # Set Theme
  theme = bs_theme(bootswatch = "minty"),
  # Center figures in the document
  tags$head(
    tags$style(HTML("
      .centerFigure {
        display: flex;
        justify-content: center;
      }
    "))
  ),
  # Application title
  title = "Dungeness Crab Test Fishery Data: Simple Visualization",
  sidebar = sidebar(
    strong("Application Use"),
    p("Use the upload button below to import test fishery data and explore data.
      Figures should populate automatically and update upon user selection. For data
      structuring requirements see Formatting Instructions tab."),
    fileInput("upload", "Upload a File", accept = c(".xlsx")),
    
    # Conditional options for Map tab
    conditionalPanel(
      condition = "input.tabs == 'Pot Locations & CPUE Summary'",
      strong("Options for Map Display"),
      selectInput("colvar", "Variable of Interest", colVar),
      uiOutput("file_select_map"),
      downloadButton("download_map", "Download Map"),
      strong("Explanation:"),
      p("Summary of test data by area. Numbers represent the mean number of crabs 
      of each demographic per pot (CPUE). Legal Sized Male (LSM) and Sublegal (SUB)
      correspond to crab greater or less than 158.75mm, respectively. Hard corresponds
        to shell conditions 1-1 through 2-1.")),
    
    # Conditional options for Size-Frequency tab
    conditionalPanel(
      condition = "input.tabs == 'Size-Frequency'",
      strong("Options for Size-Frequency Display"),
      selectInput("sextype", "Sex type", sexes),
      selectInput("charttype", "Chart type", ctype),
      uiOutput("file_select_sf"),
      strong("Explanation:"),
      
    ),
    
    # Conditional options for Comparison tab
    conditionalPanel(
      condition = "input.tabs == 'Multiple Test Comparison'",
      strong("Input another dataset to compare"),
      fileInput("upload2", "Upload a File", accept = c(".xlsx")), 
      selectInput("viewby", "View By:", loc)
    ),
    width = 350
  ),
  
  navset_card_pill(
    # Define first tab to be auto selected
    selected = "Formatting Instructions", 
    # Define the ID for the tabset
    id = "tabs", 
    nav_panel("Pot Locations & CPUE Summary", 
              leafletOutput("CPUE"),
              tableOutput("CPUE_tab")),
    nav_panel("Size-Frequency",
              plotOutput("sizefreq"),
              p("Dotted red line represents threshold of legal size male crab (158.75mm)")),
    nav_panel("Multiple Test Comparison",
              plotOutput("CPUEcomp")),
    nav_panel("Formatting Instructions",
              p("To use this application the input data must be formatted correctly to allow for 
                back-end code to run properly. The current formatting scheme should be able to accept
                the Size Frequency tab from the finalized Tulalip Tribes crab test data files created during or after the
                summer season of 2023, however any data with the requisite columns will be accepted. An example is below:"),
              tags$figure(
                class = "centerFigure",
                tags$image(
                  src = "exampleTab.png",
                  width = "800",
                ),
              ),
              p("In this version of the application files must be in .xlsx format. The columns included in 
              the uploaded datasheet must be labeled exactly as shown above (case and space sensative). Additional 
              columns will not inhibit the applications processes, however they will not be available for 
              visualization due to built in filtering. For any questions or assistance contact the app developer Sam Kaiser (skaiser@tulaliptribes-nsn.gov)."
              ),
    )
  )
)

# Define server function
server <- function(input, output, session) {
  # Show modal on application load
  shinyjs::runjs('
    $(document).ready(function() {
      Shiny.setInputValue("startup_modal", true);
    });
  ')
  
  observeEvent(input$startup_modal, {
    showModal(
      modalDialog(
        title = "Application Use Instructions",
        size = "l",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Accept")
        ),
        tags$div(
          p("Welcome to this Dungeness crab test fishery data visualization application. The data visualizations provided by this application are intended to assist in the rapid analysis of Dungeness 
             crab test fishery data for use in management discussions. The application is designed to be used with data 
             formatted according to the guidelines provided in the Formatting Instructions tab."),
          p("Please ensure that the uploaded .xlsx files meet the specified formatting requirements. The accuracy of the 
             visualizations depends on the accuracy of the input data. We do not take responsibility for any discrepancies 
             or inaccuracies in the uploaded data."),
          p("For further assistance, contact the application developer Sam Kaiser at skaiser@tulaliptribes-nsn.gov.")
        )
      )
    )
  })
  
  observeEvent(input$upload, {
    # Check if a file is uploaded
    if (!is.null(input$upload)) {
      # Switch to "Pot Locations & CPUE Summary"
      updateTabsetPanel(session, "tabs", selected = "Pot Locations & CPUE Summary")
    }
  })
  
  # Create reactive data based on .xlsx input
  data1 <- reactive({
    req(input$upload)
    read_excel(input$upload$datapath)
  })
  
  # Create reactive data based on second .xlsx input
  data2 <- reactive({
    req(input$upload2)
    read_excel(input$upload2$datapath)
  })
  
  # Get the names of uploaded files without the extension
  file_names <- reactive({
    files <- list()
    if (!is.null(input$upload)) {
      file_name_1 <- file_path_sans_ext(input$upload$name)
      files[[file_name_1]] <- data1()
    }
    if (!is.null(input$upload2)) {
      file_name_2 <- file_path_sans_ext(input$upload2$name)
      files[[file_name_2]] <- data2()
    }
    files
  })
  
  # Generate UI for selecting dataset in Map tab
  output$file_select_map <- renderUI({
    selectInput("selected_file_map", "Select File", choices = names(file_names()))
  })
  
  # Generate UI for selecting dataset in Size-Frequency tab
  output$file_select_sf <- renderUI({
    selectInput("selected_file_sf", "Select File", choices = names(file_names()))
  })
  
  # Reactive expression to get the selected data for Map tab
  selected_data_map <- reactive({
    req(input$selected_file_map)
    file_names()[[input$selected_file_map]]
  })
  
  # Reactive expression to get the selected data for Size-Frequency tab
  selected_data_sf <- reactive({
    req(input$selected_file_sf)
    file_names()[[input$selected_file_sf]]
  })
  
  # Do initial cleaning of the selected data for Map tab
  crab_map <- reactive({
    req(selected_data_map())
    selected_data_map() %>%
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
  
  # Do initial cleaning of the selected data for Size-Frequency tab
  crab_sf <- reactive({
    req(selected_data_sf())
    selected_data_sf() %>%
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
    req(crab_map())
    crab_map() %>%
      group_by(Location, Pot, Lat, Long, demo) %>%
      summarise(count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = demo, values_from = count, values_fill = list(count = 0)) %>%
      rowwise() %>%
      mutate(total = sum(c_across(starts_with(c("LM_", "FEM_", "SUB_")))),
             LSM = sum(c_across(starts_with("LM_"))),
             FEM = sum(c_across(starts_with("FEM_"))),
             SUB = sum(c_across(starts_with("SUB_"))),
             HLSM = sum(c_across(c("LM_1", "LM_2_1"))),
             PCT_HLSM = if_else(LSM == 0, 0, round(((HLSM / LSM) * 100),1))) %>% 
      replace(is.na(.), 0)
  })
  
  # Create table for display
  cpue.tab <- reactive({
    req(cpue())
    cpue() %>%
      select(Location, Pot, Lat, Long, total, LSM, FEM, SUB, HLSM, PCT_HLSM) %>%
      group_by(Location) %>%
      summarise(`All Crab` = mean(total),
                `Legal Male` = mean(LSM),
                `Hard Legal Male` = mean(HLSM),
                `Percent Hard LSM` = mean(PCT_HLSM),
                `Female` = mean(FEM),
                `Sublegal` = mean(SUB),
                .groups = 'drop') %>%
      mutate(TestDate = as.character(testdate()))
  })
  
  # Create reactive testdate variable 
  testdate <- reactive({
    req(data1())
    as.Date(max(data1()$DatePulled, na.rm = TRUE), origin = "1970-01-01")
  })
  
  # Create reactive testdate variable for the second input
  testdate2 <- reactive({
    req(data2())
    as.Date(max(data2()$DatePulled, na.rm = TRUE), origin = "1970-01-01")
  })
  
  # Calculate relevant CPUE columns of second input 
  cpue2 <- reactive({
    req(data2())
    data2() %>%
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
      unite('demo', M_F_Sub, stage, remove = FALSE) %>%
      group_by(Location, Pot, Lat, Long, demo) %>%
      summarise(count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = demo, values_from = count, values_fill = list(count = 0)) %>%
      rowwise() %>%
      mutate(total = sum(c_across(starts_with(c("LM_", "FEM_", "SUB_")))),
             LSM = sum(c_across(starts_with("LM_"))),
             FEM = sum(c_across(starts_with("FEM_"))),
             SUB = sum(c_across(starts_with("SUB_"))),
             HLSM = sum(c_across(c("LM_1", "LM_2_1"))),
             PCT_HLSM = if_else(LSM == 0, 0, round(((HLSM / LSM) * 100),1))) %>% 
      replace(is.na(.), 0)
  })
  
  # Create table for display for second input
  cpue.tab2 <- reactive({
    req(cpue2())
    cpue2() %>%
      select(Location, Pot, Lat, Long, total, LSM, FEM, SUB, HLSM, PCT_HLSM) %>%
      group_by(Location) %>%
      summarise(`All Crab` = mean(total),
                `Legal Male` = mean(LSM),
                `Hard Legal Male` = mean(HLSM),
                `Percent Hard LSM` = mean(PCT_HLSM),
                `Female` = mean(FEM),
                `Sublegal` = mean(SUB),
                .groups = 'drop') %>%
      mutate(TestDate = as.character(testdate2()))
  })
  
  # Create combined table of inputs 1 & 2 for comparison 
  cpue.tab.comb <- reactive({
    req(cpue.tab(), cpue.tab2())
    rbind(cpue.tab(), cpue.tab2()) %>%
      select(-`Percent Hard LSM`) %>%
      pivot_longer(cols = c(`All Crab`,
                            `Legal Male`,
                            `Hard Legal Male`,
                            `Female`,
                            `Sublegal`),
                   names_to = "Demo") 
  })
  
  # Subset Male Crab for SF
  filtcrab <- reactive({
    req(crab_sf())
    crab_sf() %>% filter(M_F == input$sextype)
  })
  
  # Define reactive variable for point colors in the map
  colorvariable <- reactive({input$colvar})
  
  # Call create sf object and call map output
  leaflet_map <- reactive({
    req(cpue())
    cpue_dat <- cpue()
    CPUE_sf <- st_as_sf(cpue_dat, coords = c('Long', 'Lat'), crs = 4326)
    CPUE_sf$label <- paste("Pot ", CPUE_sf$Pot)
    map <- tm_shape(CPUE_sf) +
      tm_bubbles(size = 0.3, col = colorvariable(), id = "label",
                 popup.vars = c(
                   'Total Crab' = 'total',
                   '# Hard LSM' = 'HLSM',
                   '% Hard LSM' = 'PCT_HLSM'
                 )) + 
      tm_text(colorvariable(), size = 0.85, col = "darkred", fontface = "bold", just = "left", xmod = 0.85, ymod = 0.85) +
      tm_layout(title = paste("Test Pot Locations: Variable Displayed - ", colorvariable()))
    tmap_leaflet(map)
  })
  
  # Call Map Output
  output$CPUE <- renderLeaflet({
    leaflet_map()
  })
  
  output$download_map <- downloadHandler(
    filename = function() {
      paste("map-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      map <- leaflet_map()
      
      # Save the leaflet map as an HTML file
      saveWidget(map, file, selfcontained = TRUE)
    }
  )
  
  # Call table output
  output$CPUE_tab <- renderTable({cpue.tab()}, 
                                 digits = 1, 
                                 align = 'c')
  
  # Call sf output
  chartype <- reactive({input$charttype})
  
  output$sizefreq <- renderPlot({
    req(filtcrab())
    if (chartype() == "Histogram") {
      ggplot(data = filtcrab(), aes(x = Rnd_CW)) +
        geom_histogram(aes(col = I("black"), fill = shell_condition),
                       binwidth = 1,
                       position = "stack",
                       size = 0.2) +
        labs(x = "Carapace Width (mm)", y = "Frequency of Crab") +
        geom_vline(aes(xintercept = 159.75),
                   color = "darkred", size = 1, linetype = "dashed") +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(limits = c(110, 210)) +
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
        scale_x_continuous(limits = c(110, 210)) +
        theme_minimal() + 
        theme(text = element_text(size = 14))
    }
  })
  
  # Call Comparison Output
  locat <- reactive({input$viewby})
  cpue.tab.comb.all <- reactive({
    req(cpue.tab.comb())
    cpue.tab.comb() %>%
      group_by(Demo, TestDate) %>%
      summarise(value = mean(value), .groups = 'drop') %>%
      ungroup()
  })
  
  output$CPUEcomp <- renderPlot({
    if (locat() == "Entire Test") {
      ggplot(cpue.tab.comb.all(), aes(x = reorder(Demo, -value), y = value, fill = TestDate)) + 
        geom_col(position = position_dodge(width = 0.7), width = 0.6) + 
        geom_text(aes(label = round(value, 2)), 
                  position = position_dodge(width = 0.7), 
                  vjust = -0.5, 
                  size = 4) + 
        labs(x = "Demographic", y = "CPUE", title = "CPUE Comparison Between Tests") +
        theme_minimal() +
        theme(text = element_text(size = 14), 
              axis.text = element_text(size = 12),  
              axis.title = element_text(size = 14),  
              plot.title = element_text(size = 16)) +
        scale_fill_manual(values = c("#E69F00", "#56B4E9"))
      
    } else {
      ggplot(cpue.tab.comb(), aes(x = reorder(Demo, -value), y = value, fill = TestDate)) + 
        geom_col(position = position_dodge(width = 0.7), width = 0.6) + 
        geom_text(aes(label = round(value, 2)), 
                  position = position_dodge(width = 0.7), 
                  vjust = -0.5, 
                  size = 4) + 
        labs(x = "Demographic", y = "CPUE", title = "CPUE Comparison Between Tests") +
        facet_grid(rows = vars(Location)) +
        theme_minimal() +
        theme(text = element_text(size = 14), 
              axis.text = element_text(size = 12),  
              axis.title = element_text(size = 14),  
              plot.title = element_text(size = 16)) +
        scale_fill_manual(values = c("#E69F00", "#56B4E9"))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
