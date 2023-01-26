# This R script pulls data from the GCCR global clinic google sheet to create a searchable database using shiny
# Version 1.0
# Contributors: Daniel Hwang
# Date: 26 Jan 2023
library(shiny)
library(bslib)
library(googlesheets4)

# Authenticate and access the Google Sheet
#gs4_auth()

clinician_data <- read_sheet('https://docs.google.com/spreadsheets/d/1m5xPT7LhVaGm7kpXwO22CCi0ICMI12DTjStSJiCfC4c/edit#gid=2027661689')
tmp <- as.data.frame(clinician_data)
tmp$Name <- paste(tmp$Title,tmp$`First name(s), not all caps please. E.g. Oghogho`, tmp$`Last name(s), not all caps please. E.g. Braimah`)
tmp$Country <- tmp$`Clinic country...12`
tmp$Specialty <- tmp$`Specialist / fellowship training. List with the year it was completed. E.g. xxx -2012.`
tmp$'Clinic Name' <- paste0(tmp$`Clinic name...9`,", ",tmp$`Name of larger organization...10`)
tmp$Address <- tmp$`Address, please include level of detail as necessary and customary in your country, for example: building, street name and number, city, region or district, postal/zipcode, etc...11`
tmp$'Phone Number' <- tmp$`Clinic telephone number, format with international code as follows: +915398148433...13`
tmp$Email <- tmp$`Clinic email address, format with @ as follows: xxx@gmail.com...14`
tmp$Website <- tmp$`Clinic web address...15`
tmp$'Appointment Type' <- tmp$`Appointment scheduling method, check all that apply and provide links other than regular web address above under "Other"...16`
tmp <- tmp[order(tmp$Country,tmp$Name),]

# Define the UI for the app
ui <- fluidPage(
  theme = bs_theme(version = 4, primary = "green", secondary = "green"),
  titlePanel("Global Clinics Database"),
  # Add a sidebar with the filter inputs
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country:", c("All", sort(unique(tmp[,12])))),
      selectInput("name", "Clinician Name:", c("All", sort(tmp$Name))),
      checkboxGroupInput("metrics", "Information to display:", c("Country", "Name", "Specialty", "Clinic Name", "Address", "Phone Number", "Email", "Website", "Appointment Type")),
      actionButton("gccr_button", "Visit GCCR website"),
      br(),
      br(),
      img(src = "https://gcchemosensr.org/assets/img/logo_draft_white.jpg",height = "100%", width = "100%"),
      
    ),
    # Add a main panel to display the table
    mainPanel(
      dataTableOutput("clinician_table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Filter the data based on user input
  filtered_data <- reactive({
    data <- tmp
    if (input$country != "All") {
      data <- data[data$Country == input$country,]
    }
    if (input$name != "All") {
      data <- data[paste(data$Name) == input$name,]
    }
    data
  })
  
  # Display the filtered data in a table
  output$clinician_table <- renderDataTable({
    data <- filtered_data()
    data[,input$metrics,drop=F]
  })
  
  # open link to GCCR website when button is clicked
  observeEvent(input$gccr_button, {
    browseURL("https://gcchemosensr.org/")
  })
}

# Run the app
shinyApp(ui, server)

