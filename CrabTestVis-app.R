library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(shinyjs)
library(leaflet)
library(htmlwidgets)

#Define Map Options
colVar <- c("LSM", "FEM", "SUB", "HLSM", "PCT_HLSM", "total")

# Define SF Options
sexes <- c("M", "F")
ctype <- c("Histogram", "Density Plot")

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
    p("Use the upload button below to import test fishery data in question. Figures will populate
      automatically from the most recently uploaded file."),
    fileInput("upload", "Upload a File", accept = c(".xlsx")),
    
    # Conditional options for Map tab
    conditionalPanel(
      condition = "input.tabs == 'Pot Locations & CPUE Summary'",
      strong("Options for Map Display"),
      selectInput("colvar", "Variable of Interest", colVar),
      downloadButton("download_map", "Download Map")
    ),
    
    # Conditional ptions for Size-Frequency tab
    conditionalPanel(
      condition = "input.tabs == 'Size-Frequency'",
      strong("Options for Size-Frequency Display"),
      selectInput("sextype", "Sex type", sexes),
      selectInput("charttype", "Chart type", ctype)
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
              p("Summary of test data by area. Numbers represent the mean number of crabs 
                of each demographic per pot (CPUE). Legal Sized Male (LSM) and Sublegal (SUB)
                correspond to crab greater or less than 158.75mm, respectively. Hard corresponds
                to shell conditions 1-1 through 2-1."),
              tableOutput("CPUE_tab")),
    nav_panel("Size-Frequency",
              plotOutput("sizefreq"),
              p("Dotted red line represents threshold of legal size male crab (158.75mm)")),
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
              visualization due to built in filtering. For any questions or techincal assistance contact the app developer Sam Kaiser (skaiser@tulaliptribes-nsn.gov)."
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
          p("Welcome to the Tulalip Tribes Shellfish Program's Dungeness crab test fishery data 
            visualization application. The data visualizations provided by this application are intended to assist in the rapid analysis of Dungeness 
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
  
  # Define reactive variable for point colors in the map
  colorvariable <- reactive({input$colvar})
  
  # Call create sf object and call map output
  leaflet_map <- reactive({
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
  
  output$CPUE <- renderLeaflet({
    leaflet_map()
  })
  
  output$download_map <- downloadHandler(
    filename = function() {
      paste("map-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      saveWidget(leaflet_map(), file, selfcontained = TRUE)
    }
  )
  
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
}

# Run the application 
shinyApp(ui = ui, server = server)
