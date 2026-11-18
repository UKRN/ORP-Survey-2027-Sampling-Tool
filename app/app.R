# =====================================================
# UKRN / REF STRATIFIED SAMPLING TOOL
# app.R 
# =====================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)
library(DT)
library(digest)
library(plotly)
library(shinylive)
library(httpuv)

# =====================================================
# EMBEDDED LOOKUP TABLES
# =====================================================

hecos_2_coh <- read_csv(
  "data/hecos_2_coh.csv",
  show_col_types = FALSE
)

disciplines <- read_csv(
  "data/academic_disciplines.csv",
  show_col_types = FALSE
) |>
  clean_names()

# =====================================================
# USER INTERFACE
# =====================================================

ui <- dashboardPage(
  
  skin = "purple",
  
  dashboardHeader(
    title = "UKRN Sampling Tool"
  ),
  
  dashboardSidebar(
    
    h4("Upload your HESA data"),
    
    fileInput(
      "staff_file",
      "Staff Dataset (required)",
      accept = ".csv"
    ),
    
    helpText(
      "Please upload a single csv file."
    ),
    
    uiOutput("staff_mappings"),
    
    hr(),
    
    fileInput(
      "pgr_file",
      "PGR Dataset (required)",
      accept = ".csv"
    ),
    
    helpText(
      "Please upload a single csv file."
    ),
    
    uiOutput("pgr_mappings"),
    
    hr(),
    
    # ===================================================
    # NEW: Sample Size Control Slider
    # ===================================================
    h4("Sample size settings"),
    
    sliderInput(
      "sample_pct",
      "What percentage of your population do you want to sample?",
      min = 5,
      max = 100,
      value = 10,
      step = 5,
      post = "%"
    ),
    
    br(),
    
    actionButton(
      "run_sampling",
      "Generate Random Sample",
      width = "80%",  style = "color: white; background-color: grey;"
    )
    
  ),
  
  dashboardBody(
    
    fluidRow(
      
      box(
        width = 12,
        title = "Instructions",
        status = "primary",
        
        p(
          "This tool has been created for the UKRN Open Research Programme Survey 2027. It allows UK institutions to generate a representative subset of their research community for the purpose of running surveys. If you have any questions about the use of this tool, please contact Samantha Ofili" ,
        tags$a(
          href = "mailto:samantha.ofili@bristol.ac.uk?subject=ORP%20Survey%20Sampling%20Tool",
          "(📧 samantha.ofili@bristol.ac.uk)"
              )
        ),
        tags$b("Step 1: UPLOAD CSV FILES"),
        p(
          "Upload csv files containing HESA returns for all staff and PGR at your institution. The files must contain the following information:"),
        tags$ul(
          tags$li("Name"),
          tags$li("HeCOS code for a member's primary current academic discipline or a PGR's qualification (e.g. 100003)"),
          tags$li("End date of staff contract or PGR course"),
          tags$li("Staff contract levels according to UCEA or XpertHR defined level of the contract (e.g. J0)"),
          tags$li("Whether the staff member has Significant Responsibility for Research (SR4R) or not (coded 1/2 or 'Yes'/'No')"),
          tags$li("Whether the staff member is a research assistant or not (coded 0/1 or 'Not a research assistant'/'Research assistant')")
        ),
        tags$b("Step 2: SPECIFY COLUMNS"),
        p(
          "Specify which columns in your dataset match the corresponding headings"
        ),
        tags$b("Step 3: CHOOSE THE SIZE OF YOUR RANDOM SAMPLE"),
        p(
          "Use the sidebar slider to choose the percentage of the population you want to invite to take part in the survey. Please plan for approximately 10% of all invited researchers taking part in the survey. For example, for 50 survey responses, please contact 500 reseachers."
        ),
        tags$b("Step 4: GET A RANDOM SAMPLE"),
        p(
          "Click 'Generate Random Sample' and download the generated random sample file directly from 'RESULTS: Random Sample' to encrypted or secure folders compliant with your institution’s data protection policies. Email the sample list directly to invite them to complete the survey."
        ),
        tags$b("DATA PROTECTION NOTICE:"),
        p(
          "This tool has been designed to run within your local R session memory or hosted statically via GitHub Pages using your web browser's local sandbox memory. The tool does not transmit, upload, or store any data from your uploaded CSV files, or generated sample lists to any external servers, cloud storage, GitHub or third-party databases. Closing or refreshing the tool immediately terminates the session and clears all loaded datasets and sample lists from your R session or browser memory."
        )
      )
    ),
    
    fluidRow(
      
      valueBoxOutput("population_box"),
      valueBoxOutput("sample_box")
      
    ),
    
    tabBox(
      
      width = 12,
      
      tabPanel(
        
        "RESULTS: Uploaded data",
        
        br(),
        
        plotlyOutput("population_plot"),
        
        br()
        
      ),
      
      tabPanel(
        
        "RESULTS: Random Sample",
        
        br(),
        
        plotlyOutput("sample_plot"),
        
        br(),
        
        downloadButton(
          "download_sample",
          "Download Random Sample (csv file)"
        ),
        br(),        
                DTOutput("sample_table"),
        
        br()
        

      )
      
    )
  )
)

# =====================================================
# SERVER
# =====================================================

server <- function(input, output, session) {
  
  # ===================================================
  # RAW STAFF DATA
  # ===================================================
  
  staff_raw <- reactive({
    
    req(input$staff_file)
    
    read_csv(
      input$staff_file$datapath,
      show_col_types = FALSE
    ) |>
      clean_names()
    
  })
  
  # ===================================================
  # RAW PGR DATA
  # ===================================================
  
  pgr_raw <- reactive({
    
    req(input$pgr_file)
    
    read_csv(
      input$pgr_file$datapath,
      show_col_types = FALSE
    ) |>
      clean_names()
    
  })
  
  # ===================================================
  # STAFF COLUMN MAPPINGS
  # ===================================================
  
  output$staff_mappings <- renderUI({
    
    req(staff_raw())
    
    cols <- names(staff_raw())
    
    tagList(
      
      h4("Please specify the corresponding headings in your staff dataset"),
      
      selectInput(
        "firstname_col",
        "Full Name or First Name(s)",
        cols
      ),
      
      selectInput(
        "surname_col",
        "Surname (optional)",
        choices = c("", cols),
        selected = ""
      ),
      
      selectInput(
        "hecos_col",
        "HeCOS Code",
        cols
      ),
      
      selectInput(
        "contract_end_col",
        "Contract End Date",
        cols
      ),
      
      selectInput(
        "sr4r_col",
        "Significant Responsibility for Research (SR4R)",
        cols
      ),
      
      selectInput(
        "ra_col",
        "Research Assistant",
        cols
      ),
      
      selectInput(
        "grade_col",
        "Contract level",
        cols
      )
      
    )
    
  })
  
  # ===================================================
  # PGR COLUMN MAPPINGS
  # ===================================================
  
  output$pgr_mappings <- renderUI({
    
    req(pgr_raw())
    
    cols <- names(pgr_raw())
    
    tagList(
      
      h4("Please specify the corresponding headings in your PGR dataset"),
      
      selectInput(
        "pgr_fname_col",
        "Full Name or First Name(s)",
        cols
      ),
      
      selectInput(
        "pgr_surname_col",
        "Surname (optional)",
        choices = c("", cols),
        selected = ""
      ),
      
      selectInput(
        "pgr_hecos_col",
        "HeCOS Code",
        cols
      ),
      
      selectInput(
        "pgr_end_col",
        "PGR course session end date",
        cols
      )
      
    )
    
  })
  
  # ===================================================
  # STAFF PROCESSING
  # ===================================================
  
  staff_data <- eventReactive(
    input$run_sampling,
    {
      
      junior <- c("K0")
      
      mid <- c(
        "I0",
        "J0"
      )
      
      senior <- c(
        "B0",
        "C1",
        "D1",
        "D2",
        "D3",
        "F1"
      )
      
      staff_raw() |>
        mutate(
          name = if (input$surname_col == "") {
            word(.data[[input$firstname_col]], 1)
          } else {
            paste(
              word(.data[[input$firstname_col]], 1),
              .data[[input$surname_col]]
            )
          }) |>

        transmute(
          
          name = name,

          hecos =
            .data[[input$hecos_col]],
          
          contract_end =
            .data[[input$contract_end_col]],
          
          contract_sigres =
            .data[[input$sr4r_col]],
          
          contract_resast =
            .data[[input$ra_col]],
          
          contract_levels =
            .data[[input$grade_col]]
          
        ) |>
        
        mutate(
          contract_end_date = lubridate::dmy(contract_end)
        ) |>
        
        filter(
          is.na(contract_end) |
            contract_end == "" |
            contract_end_date > as.Date("2027-04-01")
        ) |>
        
        mutate(
          contract_resast =
            case_when(
              is.na(contract_resast) ~ "no",
              
              grepl(
                "1|research assistant",
                tolower(as.character(contract_resast))
              ) ~ "yes",
              
              TRUE ~ "no"
            ),
          
          contract_sigres =
            case_when(
              
              str_detect(
                contract_sigres,
                "Has"
              ) ~ "yes",
              
              grepl("1", as.character(contract_sigres)) ~ "yes",
              
              TRUE ~ "no"
              
            )
          
        ) |>
        
        filter(
          contract_sigres == "yes"
        ) |>
        
        distinct(
          name,
          .keep_all = TRUE
        ) |>
        
        left_join(
          disciplines,
          by = c(
            "hecos" = "code"
          )
        ) |>
        
        left_join(
          hecos_2_coh,
          by = c(
            "hecos" = "HECoS_Code"
          )
        ) |>
        
        mutate(
          
          seniority = case_when(
            
            contract_resast == "yes" ~
              "junior",
            
            contract_levels %in% junior ~
              "junior",
            
            contract_levels %in% mid ~
              "mid",
            
            contract_levels %in% senior ~
              "senior",
            
            TRUE ~
              "unknown"
            
          )
          
        ) |>
        
        transmute(
          
          name,
          
          cah_number =
            CAH1_Code,
          
          cah_name =
            CAH1_Label,
          
          seniority
          
        ) 
      
    }
  )
  
  # ===================================================
  # PGR PROCESSING
  # ===================================================
  
  pgr_data <- reactive({
    
    pgr_raw() |>
      
      mutate(name = if (input$pgr_surname_col == "") {
          word(.data[[input$pgr_fname_col]], 1)
        } else {
          paste(
            word(.data[[input$pgr_fname_col]], 1),
            .data[[input$pgr_surname_col]]
          )
        },
        
        hecos =
          .data[[input$pgr_hecos_col]],
        
        course_end =
          .data[[input$pgr_end_col]]
        
      ) |>
      
      filter(
        is.na(course_end) |
          course_end == ""
      ) |>
      
      left_join(
        hecos_2_coh,
        by = c(
          "hecos" = "HECoS_Code"
        )
      ) |>
      
      distinct(
        name,
        .keep_all = TRUE
      ) |>
      
      transmute(
        
        name,
        
        cah_number =
          CAH1_Code,
        
        cah_name =
          CAH1_Label,
        
        seniority =
          "pgr"
        
      )
    
  })
  
  # ===================================================
  # COMBINED DATA
  # ===================================================
  
  all_data <- reactive({
    
    req(staff_data())
    
    bind_rows(
      staff_data(),
      pgr_data()
    ) |>
      
      mutate(
        
        cah = paste(
          cah_number,
          cah_name,
          sep = " - "
        )
        
      ) |>
      
      select(
        name,
        cah,
        seniority
      )
    
  })
  
  # ===================================================
  # STRATA TABLE (Updated to use dynamic sample_pct)
  # ===================================================
  
  strata_data <- reactive({
    
    data <- all_data()
    
    # Convert input percentage (e.g., 10) to a decimal fraction (0.10)
    current_sample_percentage <- input$sample_pct / 100
    
    sample_size <-
      round(
        nrow(data) *
          current_sample_percentage
      )
    
    data |>
      
      count(
        cah,
        seniority
      ) |>
      
      mutate(
        
        prop =
          n / sum(n),
        
        sample_n =
          pmax(
            round(
              prop *
                sample_size
            ),
            1
          )
        
      )
    
  })
  
  # ===================================================
  # SAMPLE DATA
  # ===================================================
  
  sampled_data <- reactive({
    
    data <- all_data()
    
    strata <- strata_data()
    
    data |>
      
      left_join(
        strata,
        by = c(
          "cah",
          "seniority"
        )
      ) |>
      
      mutate(
        
        score =
          sapply(
            
            paste(
              name,
              123 # SAMPLING_SEED
            ),
            
            function(x) {
              
              strtoi(
                substr(
                  digest(
                    x,
                    algo = "md5"
                  ),
                  1,
                  7
                ),
                16L
              )
              
            }
            
          )
        
      ) |>
      
      group_by(
        cah,
        seniority
      ) |>
      
      arrange(
        score,
        .by_group = TRUE
      ) |>
      
      mutate(
        rank =
          row_number()
      ) |>
      
      filter(
        rank <= first(sample_n)
      ) |>
      
      ungroup() |>
      
      select(
        name,
        cah,
        seniority
      )
    
  })
  
  # ===================================================
  # VALUE BOXES
  # ===================================================
  
  output$population_box <- renderValueBox({
    
    valueBox(
      nrow(all_data()),
      "Uploaded dataset",
      icon = icon("users"),
      color = "teal"
    )
    
  })
  
  output$sample_box <- renderValueBox({
    
    valueBox(
      nrow(sampled_data()),
      "Random Sample",
      icon = icon("users-viewfinder"),
      color = "purple"
    )
    
  })
  
  # ===================================================
  # TABLES
  # ===================================================
  
  output$sample_table <- renderDT({
    
    datatable(sampled_data())
    
  })
  
  # ===================================================
  # PLOTS
  # ===================================================
  
  output$population_plot <- renderPlotly({
    
    p <- all_data() |>
      
      count(
        cah,
        seniority
      ) |>
      
      ggplot(
        
        aes(
          x = reorder(cah, n),
          y = n,
          fill = seniority
        )
        
      ) +
      
      geom_col() +
      
      coord_flip() +
      
      theme_minimal() +
      
      labs(
        x = "CAH Discipline",
        y = "Count",
        fill = "Seniority"
      )
    
    ggplotly(p)
    
  })
  
  output$sample_plot <- renderPlotly({
    
    p <- sampled_data() |>
      
      count(
        cah,
        seniority
      ) |>
      
      ggplot(
        
        aes(
          x = reorder(cah, n),
          y = n,
          fill = seniority
        )
        
      ) +
      
      geom_col() +
      
      coord_flip() +
      
      theme_minimal() +
      
      labs(
        x = "CAH Discipline",
        y = "Count",
        fill = "Seniority"
      )
    
    ggplotly(p)
    
  })
  
  # ===================================================
  # DOWNLOAD
  # ===================================================
  
  output$download_sample <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "ORP_survey_sample_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      write.csv(
        sampled_data(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
}

# =====================================================
# RUN APP
# =====================================================

shinyApp(
  ui = ui,
  server = server
)


