library(shiny)
library(tidyverse)
library(ggplot2)
library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)

# Connect
satoken <- "biostat-203b-2024-winter-313290ce47a6.json"
bq_auth(path = satoken)
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)


#Data preparation
sid_info <- tbl(con_bq, 
                "patients") 
sid_adt <- tbl(con_bq, 
               "transfers") 
sid_adm <- tbl(con_bq, 
               "admissions") 
sid_lab <- tbl(con_bq, 
               "labevents") 
sid_proc <- tbl(con_bq, 
                "procedures_icd") 
sid_dicd <- tbl(con_bq, 
                "d_icd_procedures") 
sid_proc <- sid_proc %>% 
  left_join(sid_dicd, 
            by = c("icd_code", "icd_version")) 
sid_diag <- tbl(con_bq, 
                "diagnoses_icd") 
sid_didc <- tbl(con_bq, 
                "d_icd_diagnoses") 
sid_diag <- sid_diag %>% 
  left_join(sid_didc, 
            by = c("icd_code", "icd_version"))

mimic_icu_cohort <- readRDS("../mimiciv_shiny/mimic_icu_cohort.rds")

patient_id <- mimic_icu_cohort %>% 
  select(subject_id) %>% 
  collect() %>%
  pull(subject_id) 

ui <- fluidPage(
  titlePanel("MIMIC-IV Data Overview"),
  h3("Author: Yuhui Wang"),
  tabsetPanel(
    tabPanel("Patient characteristics",
           sidebarLayout(
             sidebarPanel(
               selectInput("variable", "Variable of interst",
                           choices = c("First care unit" = "first_careunit", 
                                       "Last care unit" = "last_careunit", 
                                       "Admission type" = "admission_type", 
                                       "Admission location" = "admission_location",
                                       "Discharge location" = "discharge_location", 
                                       "Insurance" = "insurance", 
                                       "Language" = "language", 
                                       "Marital status" = "marital_status",
                                       "Race" = "race",
                                       "Hospital expire flag" = "hospital_expire_flag",
                                       "Gender" = "gender",
                                       "Labevents" = "lab_measurements",
                                       "Vitals" = "vitals"
                           )),
               checkboxInput("remove", 
                             "Remove outliers in IQR method for measurements?")
             ),
             mainPanel(
               plotOutput("SummaryPlot"))
           )
  ),
  tabPanel("Patient's ADT and ICU stay information", fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               helpText("Select a patient."),
               selectizeInput("patientID", 
                              "Patient ID", 
                              choices = patient_id, 
                              multiple = FALSE)
             ),
             mainPanel(
               plotOutput("ADTPlot"))
           )
  )
  )
)

demographics <- mimic_icu_cohort %>%
  select(first_careunit, 
         last_careunit, 
         los, 
         admission_type, 
         admission_location, 
         discharge_location, 
         insurance, 
         language, 
         marital_status, 
         race, 
         dod, 
         gender, 
         hospital_expire_flag)

lab_measurements <- mimic_icu_cohort %>%
  select(sodium, 
         chloride, 
         creatinine, 
         potassium, 
         glucose, 
         hematocrit, 
         wbc, 
         bicarbonate)

vitals <- mimic_icu_cohort %>%
  select(heart_rate, 
         non_invasive_blood_pressure_systolic, 
         non_invasive_blood_pressure_diastolic, 
         respiratory_rate, 
         temperature_fahrenheit)


server <- function(input, output, session) {
  
  reactiveData <- reactive({
    data <- switch(input$variable,
                   "first_careunit" = mimic_icu_cohort %>% 
                     select(first_careunit) %>% 
                     na.omit() %>% 
                     collect(),
                   "last_careunit" = mimic_icu_cohort %>% 
                     select(last_careunit) %>% 
                     na.omit() %>% 
                     collect(),
                   "admission_type" = mimic_icu_cohort %>% 
                     select(admission_type) %>% 
                     na.omit() %>% 
                     collect(),
                   "admission_location" = mimic_icu_cohort %>% 
                     select(admission_location) %>% 
                     na.omit() %>% 
                     collect(),
                   "discharge_location" = mimic_icu_cohort %>% 
                     select(discharge_location) %>% 
                     na.omit() %>% 
                     collect(),
                   "insurance" = mimic_icu_cohort %>% 
                     select(insurance) %>% 
                     na.omit() %>% 
                     collect(),
                   "language" = mimic_icu_cohort %>% 
                     select(language) %>% 
                     na.omit() %>% 
                     collect(),
                   "marital_status" = mimic_icu_cohort %>% 
                     select(marital_status) %>% 
                     na.omit() %>% 
                     collect(),
                   "race" = mimic_icu_cohort %>% 
                     select(race) %>% 
                     na.omit() %>% 
                     collect(),
                   "hospital_expire_flag" = mimic_icu_cohort %>% 
                     select(hospital_expire_flag) %>% 
                     na.omit() %>% 
                     collect(),
                   "gender" = mimic_icu_cohort %>% 
                     select(gender) %>% 
                     na.omit() %>% 
                     collect(),
                   "lab_measurements" = lab_measurements %>% 
                     pivot_longer(cols = everything(), 
                                  names_to = "variable", 
                                  values_to = "value"),
                   "vitals" = vitals %>% 
                     pivot_longer(cols = everything(), 
                                  names_to = "variable", 
                                  values_to = "value")
    )
    
    # Check for outliers if requested
    if (input$remove) {
      if ("value" %in% names(data) && 
          is.numeric(data$value)) {
        # Apply the filter
        data <- data %>%
          filter(!is.na(value) & !is.infinite(value)) %>%  
          group_by(variable) %>%
          filter({
            bounds <- quantile(value, 
                               probs = c(0.25, 0.75), 
                               na.rm = TRUE)
            iqr_value <- IQR(value, 
                             na.rm = TRUE)
            value > (bounds[1] - 1.5 * iqr_value) & 
              value < (bounds[2] + 1.5 * iqr_value)
          }) %>%
          ungroup()  
      }
    }
    
    return(data)
  })
  
  output$SummaryPlot <- renderPlot({
    # Fetch the reactive data
    data <- reactiveData()
    req(data)
    
    is_categorical <- input$variable %in% 
      names(demographics)
    
    if (is_categorical) {
      ggplot(data, 
             aes_string(x = names(data))) +
        geom_bar() +
        coord_flip() +
        labs(title = "Patient count by variable", 
             x = input$variable, 
             y = "Count") +
        theme_minimal()
    } else {
      # For numeric data, create appropriate plots
      ggplot(data, 
             aes(x = variable, 
                 y = value)) +
        geom_boxplot() +
        coord_flip() +
        labs(title = "Distribution of measurements", 
             x = "", 
             y = "Value") +
        theme_minimal() 
    }
  })
  

#2
  
reactiveData2 <- reactive({
  req(input$patientID)  
  sid <- as.numeric(input$patientID)
  
  data_list <- list(
    sid = sid,
    sid_info = sid_info %>% 
      filter(subject_id == sid) %>% 
      collect(),
    sid_adt = sid_adt %>% 
      filter(subject_id == sid) %>% 
      collect(),
    sid_adm = sid_adm %>% 
      filter(subject_id == sid) %>% 
      collect(),
    sid_lab = sid_lab %>% 
      filter(subject_id == sid) %>% 
      collect(),
    sid_proc = sid_proc %>% 
      filter(subject_id == sid) %>% 
      collect(),
    sid_diag = sid_diag %>% 
      filter(subject_id == sid) %>% 
      collect()
  )
  
  return(data_list)
})
  
  output$ADTPlot <- renderPlot({
    # Fetch the reactive data
    data_list <- reactiveData2()
    req(data_list)  
    sid <- data_list$sid
    sid_info <- data_list$sid_info
    sid_adt <- data_list$sid_adt
    sid_adm <- data_list$sid_adm
    sid_lab <- data_list$sid_lab
    sid_proc <- data_list$sid_proc
    sid_diag <- data_list$sid_diag
    
    
    #plot
    ggplot() +
      geom_segment(
        data = sid_adt %>% 
          filter(eventtype != "discharge"),
          mapping = aes(
            x = intime,
            xend = outtime,
            y = "ADT",
            yend = "ADT",
            color = careunit,
            linewidth = str_detect(careunit, "(ICU|CCU)")
          ),
      ) +
      geom_point(
        data = sid_lab %>%
          distinct(charttime, 
                   .keep_all = TRUE),
        mapping = aes(x = charttime, 
                      y = "Lab"),
        shape = '+',
        size = 5
      ) +
      geom_jitter(
        data = sid_proc,
        mapping = aes(
          x = chartdate + hours(12),
          y = "Procedure",
          shape = str_sub(long_title, 1, 25)
        ),
        size = 3,
        height = 0
      ) +
      labs(
        title = str_c(
          "Patient ", sid, ", ",
          sid_info$gender, ", ",
          sid_info$anchor_age + year(sid_adm$admittime[1]) - sid_info$anchor_year, 
          " years old, ",
          str_to_lower(sid_adm$race[1])
          ),
        subtitle = str_c(str_to_lower(
          sid_diag$long_title[1:3]), collapse = "\n"),
        x = "Calendar Time",
        y = "",
        color = "Care Unit",
        shape = "Procedure"
        ) +
      guides(linewidth = "none") +
      scale_y_discrete(limits = rev) +
      theme_light() +
      theme(
        legend.position = "bottom",
        legend.box = "vertical"
      )
      
})

}

shinyApp(ui, server)
