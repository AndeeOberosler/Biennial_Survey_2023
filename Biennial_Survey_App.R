# LOADING PACKAGES
package.list <- c("bslib", "readxl", "reactable", "htmltools", "tidyr", "dplyr", "shiny", "shinyfilter")
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0){
  install.packages(new.packages)}
invisible(lapply(package.list, library, character.only = TRUE))

# TEST UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  tabsetPanel(
    tabPanel("Import Data",
             fileInput("file", NULL, buttonLabel = "Upload...", multiple = FALSE,
                       accept = ".xlsx")),
    tabPanel("Explore Survey Data",
             titlePanel("Biennial Survey 2021 & 2023 COVID Data"),
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 
                 selectizeInput(inputId = "sel_Year", label = "Year", 
                                multiple = TRUE, 
                                choices = c("2021", "2023")),
                 
                 selectizeInput(inputId = "sel_CRDP", label = "CRDP Participant",
                                multiple = TRUE,
                                choices = c("Yes", "No", "Former")),
                 
                 selectizeInput(inputId = "sel_Depository", label = "Depository Type",
                                multiple = TRUE,
                                choices = c("Selective", "Regional")),
                 
                 selectizeInput(inputId = "sel_Size", label = "Library Size",
                                multiple = TRUE,
                                choices = c("Small (less than 250,000 volumes in the library)",   
                                            "Medium (250,000 - 1,000,000 volumes in the library)",
                                            "Large (more than 1,000,000 volumes in the library)")),
                 
                 selectizeInput(inputId = "sel_Type", label = "Library Type",
                                multiple = TRUE,
                                choices = c("Highest State Court Library (SC)",
                                            "Academic General (AG)",           
                                            "Federal Agency Library (FA)",
                                            "Public Library (PU)",             
                                            "Academic, Law Library (AL)",
                                            "Academic, Community College (AC)",
                                            "State Library (SL)",
                                            "Special Library (SP)",            
                                            "Federal Court Library (FC)",                               
                                            "Service Academy (SA)")),
                 
                 selectizeInput(inputId = "sel_NCSA", label = "NCSA",
                                multiple = TRUE,
                                choices = c("South",
                                            "West",
                                            "Northeast",
                                            "Midwest")),
                 
                 selectizeInput(inputId = "sel_State", label = "State",
                                multiple = TRUE,
                                choices = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT",
                                            "DC", "DE", "FL", "FM", "GA", "HI", "IA",
                                            "ID", "IL", "IN", "KS", "KY", "LA", "MA",
                                            "MD", "ME", "MI", "MN", "MO", "MS", "MT",
                                            "NC", "ND", "NE", "NH", "NJ", "NM", "NV", 
                                            "NY", "OH", "OK", "OR", "PA", "PR", "RI",
                                            "SC", "SD", "TN", "TX", "UT", "VA", "VT",
                                            "WA", "WI", "WV", "WY")),
                 
                 selectizeInput(inputId = "sel_Partnership", label = "Partnership Agreement",
                                multiple = TRUE,
                                choices = c("Yes", "No", "Former")),
                 
                 checkboxInput("Check", "Only include libraries that participated in both 2021 and 2023 survey"),
                 
                 actionButton("SetFilter", "Apply Filters"),
                 
                 use_tooltips(background = "#1B3F8C", foreground = "#FFFFFF")
               ),
               mainPanel(
                 reactableOutput(outputId = "survey_tbl")
               )
             )
    ),
    tabPanel("Download Data",
             browsable(
               tagList(
                 tags$button("Download as a CSV", 
                             onclick = "Reactable.downloadDataCSV('survey_tbl', 'filtered_survey_data.csv')"))))))

# TEST SERVER
server <- function(input, output, session){
  
  observe({
    req(input$file)
    # Loading data
    yr1 <- read_excel(input$file$datapath, sheet = 1)
    yr2 <- read_excel(input$file$datapath, sheet = 2)
    
    # Sorting dataframes
    yr1 <- yr1[,order(colnames(yr1))]
    yr2 <- yr2[,order(colnames(yr2))]
    
    # Fixing column names
    colnames(yr2)[2] <- colnames(yr1)[2]
    colnames(yr2)[7] <- colnames(yr1)[7]
    colnames(yr2)[13] <- colnames(yr1)[13]
    colnames(yr2)[17] <- colnames(yr1)[17]
    colnames(yr2)[18] <- colnames(yr1)[18]
    
    # Adding year attribute
    yr1 <- yr1 %>% mutate(Year = 2021, .before = `Changes were made due to COVID-19 but now back to pre-COVID policies`)
    yr2 <- yr2 %>% mutate(Year = 2023, .before = `Changes were made due to COVID-19 but now back to pre-COVID policies`)
    
    # Reordering dataframes
    yr1 <- yr1 %>% select(`Year`, `CRDP Participant`, `Depository Type`,
                              `Library Size`, `Library Type`, `NCSA`, 
                              `State`, `Partnership Agreement`, `Library Number`,
                              `Changes were made due to COVID-19 but now back to pre-COVID policies`,
                              `Collection Maintenance and Weeding`, `Instructional and Educational Services`,
                              `Interlibrary Loan Services`, `Item Selection / FDLP Selection Profile`,
                              `No new policies or procedures implemented`, `Outreach Services`, `Processing`,
                              `Public Access`, `Reference Services`, `Shelving`,
                              `Staffing`, `Storage`, `Other`)
    
    yr2 <- yr2 %>% select(`Year`, `CRDP Participant`, `Depository Type`,
                              `Library Size`, `Library Type`, `NCSA`, 
                              `State`, `Partnership Agreement`, `Library Number`,
                              `Changes were made due to COVID-19 but now back to pre-COVID policies`,
                              `Collection Maintenance and Weeding`, `Instructional and Educational Services`,
                              `Interlibrary Loan Services`, `Item Selection / FDLP Selection Profile`,
                              `No new policies or procedures implemented`, `Outreach Services`, `Processing`,
                              `Public Access`, `Reference Services`, `Shelving`,
                              `Staffing`, `Storage`, `Other`)
    
    # Combining dataframes
    comb <- rbind(yr1, yr2)
    sorted_comb <- comb[order(comb$`Library Number`, decreasing = FALSE), ]  
    
    # Changing missing to "NO" for CRDP and Participant
    sorted_comb <- sorted_comb %>% mutate(`CRDP Participant` = ifelse(is.na(`CRDP Participant`), "No", `CRDP Participant`))
    sorted_comb <- sorted_comb %>% mutate(`Partnership Agreement` = ifelse(is.na(`Partnership Agreement`), "No", `Partnership Agreement`))
    
    # Getting libraries in common
    libs <- intersect(yr1$`Library Number`, yr2$`Library Number`)
    
    # Handling checkbox input
    if(input$Check == FALSE){
      sorted_comb <- sorted_comb
    } else if (input$Check == TRUE){
      sorted_comb <- sorted_comb[sorted_comb$`Library Number` %in% libs, ]
    }
    
    # Converting to data frame
    sorted_comb <- as.data.frame(sorted_comb)
    
    r <- reactiveValues(mydata = sorted_comb)
    
    # Setting up Reactabl
    define_filters(input,
                   "survey_tbl",
                   c(sel_Year = "Year",
                     sel_CRDP = "CRDP Participant",
                     sel_Depository = "Depository Type",
                     sel_Size = "Library Size",
                     sel_Type = "Library Type",
                     sel_NCSA = "NCSA",
                     sel_State = "State",
                     sel_Partnership = "Partnership Agreement"),
                   sorted_comb)
    
    observeEvent(input$SetFilter, {
      r$mydata <- update_filters(input, session, "survey_tbl")
      update_tooltips("survey_tbl",
                      session,
                      tooltip = TRUE, 
                      title_avail = "Available is:", 
                      title_nonavail = "Currently not available is:",
                      popover_title = "My filters",
                      max_avail = 10,
                      max_nonavail = 10)
    }
    )
    
    output$survey_tbl <- renderReactable({
      reactable(data = r$mydata,
                filterable = TRUE,
                rownames = FALSE,
                selection = "multiple",
                showPageSizeOptions = TRUE,
                paginationType = "jump",
                showSortable = TRUE,
                highlight = TRUE,
                resizable = TRUE,
                rowStyle = list(cursor = "pointer"),
                onClick = "select")
    })
  })
}

shinyApp(ui, server)