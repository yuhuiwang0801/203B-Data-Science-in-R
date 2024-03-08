library(shiny)
library(tidyverse)
library(ggplot2)

mimic_icu_cohort <- readRDS("~/Desktop/203b-homework/hw4/mimiciv_shiny/mimic_icu_cohort.rds")

ui <- fluidPage(
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
               checkboxInput("remove", "Remove outliers in IQR method for measurements?")
             ),
             mainPanel(plotOutput("summaryPlot"))
           )
  ),
  tabPanel("Patient's ADT and ICU stay information",
           sidebarLayout(
             sidebarPanel(
               helpText("Select a patient."),
               selectizeInput("patientID", "Patient ID", choices = mimic_icu_cohort$subject_id, multiple = FALSE)
             ),
             mainPanel(plotOutput("patientDetails"))
           )
  )
)

demographics <- mimic_icu_cohort %>%
  select(first_careunit, last_careunit, los, admission_type, admission_location, discharge_location, insurance, language, marital_status, race, dod)

lab_measurements <- mimic_icu_cohort %>%
  select(sodium, chloride, creatinine, potassium, glucose, hematocrit, wbc, bicarbonate)

vitals <- mimic_icu_cohort %>%
  select(heart_rate, non_invasive_blood_pressure_systolic, non_invasive_blood_pressure_diastolic, respiratory_rate, temperature_fahrenheit)


server <- function(input, output, session) {
  
  reactiveData <- reactive({
    # Get the correct dataset based on the input
    data <- switch(input$variable,
                   # Ensure all branches return a data frame
                   "first_careunit" = mimic_icu_cohort %>% select(first_careunit) %>% na.omit() %>% as.data.frame(),
                   "last_careunit" = mimic_icu_cohort %>% select(last_careunit) %>% na.omit() %>% as.data.frame(),
                   "admission_type" = mimic_icu_cohort %>% select(admission_type) %>% na.omit() %>% as.data.frame(),
                   "admission_location" = mimic_icu_cohort %>% select(admission_location) %>% na.omit() %>% as.data.frame(),
                   "discharge_location" = mimic_icu_cohort %>% select(discharge_location) %>% na.omit() %>% as.data.frame(),
                   "insurance" = mimic_icu_cohort %>% select(insurance) %>% na.omit() %>% as.data.frame(),
                   "language" = mimic_icu_cohort %>% select(language) %>% na.omit() %>% as.data.frame(),
                   "marital_status" = mimic_icu_cohort %>% select(marital_status) %>% na.omit() %>% as.data.frame(),
                   "race" = mimic_icu_cohort %>% select(race) %>% na.omit() %>% as.data.frame(),
                   "hospital_expire_flag" = mimic_icu_cohort %>% select(hospital_expire_flag) %>% na.omit() %>% as.data.frame(),
                   "gender" = mimic_icu_cohort %>% select(gender) %>% na.omit() %>% as.data.frame(),
                   "lab_measurements" = lab_measurements %>% pivot_longer(cols = everything(), names_to = "variable", values_to = "value"),
                   "vitals" = vitals %>% pivot_longer(cols = everything(), names_to = "variable", values_to = "value"),
    )
    
    # Check for outliers if requested
    if (input$remove) {
      # Assuming data is a data frame with numeric 'value' column
      data <- data %>% 
        filter(!is.infinite(value)) %>%
        group_by(variable) %>%
        filter(value > quantile(value, 0.25) - 1.5 * IQR(value) & 
               value < quantile(value, 0.75) + 1.5 * IQR(value))
    }
    
    return(data)
  })
  
  output$summaryPlot <- renderPlot({
    # Fetch the reactive data
    data <- reactiveData()
    req(data) # Ensure that 'data' is available before proceeding
    
    is_categorical <- input$variable %in% names(demographics)
    
    if (is_categorical) {
      ggplot(data, aes_string(x = names(data))) +
        geom_bar() +
        coord_flip() +
        labs(title = "Patient count by variable", x = input$variable, y = "Count") +
        theme_minimal()
    } else {
      # For numeric data, create appropriate plots
      ggplot(data, aes(x = variable, y = value)) +
        geom_boxplot() +
        coord_flip() +
        labs(title = "Distribution of measurements", x = "", y = "Value") +
        theme_minimal() 
    }
  })
  
  # Additional server code for patient details output...
  output$patientDetails <- renderPlot({
    # Fetch the reactive data
    data <- reactiveData()
    req(data) # Ensure that 'data' is available before proceeding
    
    # Create a plot based on the selected patient
    
    #plot
    ggplot() +
      geom_segment(
        data = cohort %>% 
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
        data = cohort %>%
          distinct(charttime, .keep_all = TRUE),
        mapping = aes(x = charttime, y = "Lab"),
        shape = '+',
        size = 5
      ) +
      geom_jitter(
        data = cohort,
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
          cohort$gender, ", ",
          cohort$anchor_age + year(cohort$admittime[1]) - cohort$anchor_year, " years old, ",
          str_to_lower(cohort$race[1])
          ),
        subtitle = str_c(str_to_lower(cohort$long_title[1:3]), collapse = "\n"),
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
