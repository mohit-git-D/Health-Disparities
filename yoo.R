library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(leaflet)
library(shinyjs)
library(dplyr)
library(stats)
library(RSQLite)  # Added for database management
library(DBI)      # For database interactions
library(googleAuthR)
library(httr)
library(jsonlite)
options(shiny.port = 6740)

readRenviron("C://Users//sharm//Documents//.env")

google_client_id <- Sys.getenv("GOOGLE_CLIENT_ID")
google_client_secret <- Sys.getenv("GOOGLE_CLIENT_SECRET")
google_redirect_uri <- Sys.getenv("GOOGLE_REDIRECT_URI")

# Database Setup Function
setup_database <- function() {
  # Connect to SQLite database
  db_path <- "user_database.sqlite"
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Create users table if not exists
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE,
      password TEXT,
      email TEXT,
      google_id TEXT UNIQUE,
      google_email TEXT,
      profile_picture TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Close the connection
  dbDisconnect(conn)
}

# Initialize the database
setup_database()

# Function to add a new user to the database
add_user_to_database <- function(username, password, email = NULL) {
  conn <- dbConnect(RSQLite::SQLite(), "user_database.sqlite")
  
  tryCatch({
    # Prepared statement to prevent SQL injection
    query <- "INSERT INTO users (username, password, email) VALUES (?, ?, ?)"
    dbExecute(conn, query, params = list(username, password, email))
    dbDisconnect(conn)
    return(TRUE)
  }, error = function(e) {
    dbDisconnect(conn)
    return(FALSE)
  })
}

# Function to validate user credentials
validate_user <- function(username, password) {
  conn <- dbConnect(RSQLite::SQLite(), "user_database.sqlite")
  
  # Prepared statement to prevent SQL injection
  query <- "SELECT * FROM users WHERE username = ? AND password = ?"
  result <- dbGetQuery(conn, query, params = list(username, password))
  
  dbDisconnect(conn)
  
  return(nrow(result) > 0)
}

## Google OAuth
# Add these Google authentication functions
setup_google_auth <- function() {
  options(googleAuthR.scopes.selected = c(
    "https://www.googleapis.com/auth/userinfo.email",
    "https://www.googleapis.com/auth/userinfo.profile"
  ))
  
  # Replace with your OAuth credentials
  options(googleAuthR.webapp.client_id =google_client_id,
          googleAuthR.webapp.client_secret =google_client_secret,
          googleAuthR.webapp.redirect_uri =google_redirect_uri)
  # Configure for same window authentication
  # options(googleAuthR.webapp.use_basic_auth = TRUE,
  #         googleAuthR.webapp.redirect_on_signout = TRUE,
  #         googleAuthR.webapp.popup = FALSE)
}

get_google_user_info <- function(access_token) {
  # Get user info from Google using the access token directly in the header
  response <- GET(
    "https://www.googleapis.com/oauth2/v1/userinfo",
    add_headers(Authorization = paste("Bearer", access_token))
  )
  
  if (status_code(response) == 200) {
    user_info <- fromJSON(rawToChar(response$content))
    return(user_info)
  }
  return(NULL)
}

# Handle the OAuth code exchange and user authentication
handle_google_auth <- function(code, session) {
  # Exchange the authorization code for an access token
  token_response <- POST(
    "https://oauth2.googleapis.com/token",
    body = list(
      code = code,
      client_id = google_client_id,
      client_secret = google_client_secret,
      redirect_uri = google_redirect_uri,
      grant_type = "authorization_code"
    ),
    encode = "form"
  )
  
  if (status_code(token_response) == 200) {
    token_data <- fromJSON(rawToChar(token_response$content))
    access_token <- token_data$access_token
    
    # Get user info using the access token
    user_info <- get_google_user_info(access_token)
    
    if (!is.null(user_info)) {
      # Connect to database
      con <- dbConnect(RSQLite::SQLite(), "users.db")
      
      # Check if user exists
      existing_user <- dbGetQuery(con, sprintf(
        "SELECT * FROM users WHERE google_id = '%s' OR email = '%s'",
        user_info$id, user_info$email
      ))
      
      if (nrow(existing_user) == 0) {
        # Create new user
        dbExecute(con, 
                  "INSERT INTO users (username, email, google_id, google_email, profile_picture) 
           VALUES (?, ?, ?, ?, ?)",
                  params = list(
                    user_info$email,
                    user_info$email,
                    user_info$id,
                    user_info$email,
                    user_info$picture
                  )
        )
        
        # Get the newly created user
        user <- dbGetQuery(con, sprintf(
          "SELECT * FROM users WHERE google_id = '%s'",
          user_info$id
        ))
      } else {
        # Update existing user's Google info
        dbExecute(con,
                  "UPDATE users SET 
           google_id = ?, 
           google_email = ?, 
           profile_picture = ? 
           WHERE email = ?",
                  params = list(
                    user_info$id,
                    user_info$email,
                    user_info$picture,
                    user_info$email
                  )
        )
        user <- existing_user
      }
      
      dbDisconnect(con)
      
      # Return user data
      return(list(
        username = user_info$email,
        email = user_info$email,
        profile_picture = user_info$picture
      ))
    }
  }
  return(NULL)
}


# Sample Data
health_equity_data <- data.frame(
  Region = c("Urban East", "Urban West", "Rural North", "Rural South", "Suburban Midwest"),
  Healthcare_Access = c(80, 70, 60, 50, 65),
  Life_Expectancy = c(82, 80, 78, 75, 77),
  Chronic_Disease_Rate = c(30, 35, 40, 45, 38),
  Education_Level = c(90, 85, 70, 65, 80),
  stringsAsFactors = FALSE
)

demographic_trends <- data.frame(
  Year = rep(2020:2025, each = 5),
  Population_Health_Index = runif(30, 50, 100),
  Region = rep(c("Urban East", "Urban West", "Rural North", "Rural South", "Suburban Midwest"), 6)
)

# UI for Login and Dashboard
ui <-fluidPage(
  useShinyjs(),  # For enabling JavaScript
  tags$head(
    tags$script("
      Shiny.addCustomMessageHandler('redirectToGoogle', function(authUrl) {
        window.location.href = authUrl;
      });
    "),
    tags$style(HTML("
      .landing-container { 
        text-align: center;
        padding: 50px 20px;
        background-color: #f4f4f4;
        min-height: 100vh;
      }
      .landing-content {
        max-width: 800px;
        margin: 0 auto;
        background-color: white;
        padding: 40px;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .get-started-btn {
        margin-top: 20px;
        padding: 10px 20px;
        font-size: 18px;
      }
      .login-container { 
        margin-top: 50px; 
      }
      .container {
        padding-top: 50px;
      }
    "))
  ),
  uiOutput("main_ui")
)
# Server function for authentication, dashboards, and theory content
server <- function(input, output, session) {
  # Initialize page state
  appState <- reactiveVal("landing")
  user_data <- reactiveVal(NULL)
  # Authentication and State Management
  AUTH <- reactiveValues(
    logged_in = FALSE,
    username = NULL,
    current_page = "landing"
  )
  
  # Setup Google Authentication
  setup_google_auth()
  
  # Add an observer for the get started button
  observeEvent(input$get_started, {
    appState("login")
  })
  
  # Logout Logic
  # Add this to your server function
  observeEvent(input$logout_button, {
    # Reset authentication status
    AUTH$logged_in <- FALSE
    AUTH$username <- NULL
    
    # Clear user data
    user_data(NULL)
    
    # Force UI refresh to show login page
    session$reload()
  })
  
  # Handle Google Sign-In button click
  observeEvent(input$google_signin, {
    # Create OAuth URL
    auth_url <- paste0(
      "https://accounts.google.com/o/oauth2/v2/auth?",
      "client_id=", google_client_id,
      "&redirect_uri=", URLencode("http://127.0.0.1:6740", reserved = TRUE),
      "&response_type=code",
      "&scope=", URLencode("email profile openid https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email", reserved = TRUE)
    )
    
    # Redirect to Google login
    session$sendCustomMessage("redirectToGoogle", auth_url)
  })
  
  # Handle the OAuth callback observer
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code)) {
      # Handle the authorization code
      user <- handle_google_auth(query$code, session)
      
      if (!is.null(user)) {
        user_data(user)
        appState("dashboard")
        AUTH$current_page = "landing"
        AUTH$logged_in <- TRUE
        AUTH$username <- user$username
      } else {
        output$login_message <- renderText("Failed to authenticate with Google")
        appState("login")
        AUTH$current_page = "login"
      }
    }
  })
  
  
  
  # Main UI Rendering
  output$main_ui <- renderUI({
    # Check the current app state
    if (appState() == "landing") {
      # Landing Page
      div(
        class = "landing-container",
        div(
          class = "landing-content",
          h1("Health Equity Explorer", class = "mb-4"),
          p(
            "Health Equity Explorer is a comprehensive dashboard designed to analyze and 
            visualize health disparities across different regions. Our application provides 
            deep insights into healthcare access, life expectancy, and chronic disease rates 
            to support data-driven public health decision-making."
          ),
          h3("Key Features:"),
          tags$ul(
            tags$li("Interactive Regional Health Comparison"),
            tags$li("Predictive Health Trend Analysis"),
            tags$li("Detailed Healthcare Access Insights"),
            tags$li("Comprehensive Demographic Visualization")
          ),
          p(
            "Our mission is to highlight health inequities and provide actionable 
            insights for policymakers, healthcare professionals, and researchers."
          ),
          actionButton(
            "get_started", 
            "Get Started", 
            class = "btn btn-primary btn-lg get-started-btn"
          )
        )
      )
    } else if (!AUTH$logged_in) {
      # Landing Page with Enhanced Design
      div(
        class = "container",
        div(
          class = "row justify-content-center",
          div(
            class = "col-md-6 login-container",
            h1("Health Equity Insights", class = "text-center mb-4"),
            tabsetPanel(
              id = "login_tabs",
              tabPanel("Login", 
                       textInput("username", "Username", placeholder = "Enter username"),
                       passwordInput("password", "Password", placeholder = "Enter password"),
                       actionButton("login_button", "Login", class = "btn btn-primary btn-block"),
                       actionButton("google_signin", "Google Login", class = "btn btn-primary btn-block")
              ),
              tabPanel("Sign Up", 
                       textInput("new_username", "Choose Username", placeholder = "Create username"),
                       textInput("email", "Email (Optional)", placeholder = "Enter email"),
                       passwordInput("new_password", "Create Password", placeholder = "Create strong password"),
                       passwordInput("confirm_password", "Confirm Password", placeholder = "Repeat password"),
                       actionButton("signup_button", "Sign Up", class = "btn btn-primary btn-block")
              )
            )
          )
        )
      )
    } else {
      # Dashboard for Authenticated Users
      dashboardPage(
        # Updated dashboardHeader in the dashboardPage
        dashboardHeader(
          title = "Health Equity Explorer",
          tags$li(
            class = "dropdown",
            actionLink(
              inputId = "logout_button", 
              label = "Logout", 
              icon = icon("sign-out")
            )
          )
        ),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
            menuItem("Data Insights", tabName = "data_insights", icon = icon("database")),
            menuItem("Predictive Analysis", tabName = "predictions", icon = icon("chart-area")),
            menuItem("Regional Health Overview", tabName = "regional_health", icon = icon("map")),
            menuItem("Healthcare Access Analysis", tabName = "access_analysis", icon = icon("hospital"))
          )
        ),
        dashboardBody(
          tabItems(
            # Dashboard Tab
            tabItem(tabName = "dashboard",
                    fluidRow(
                      box(width = 12, title = "Health Equity Overview",
                          plotlyOutput("health_overview_plot")
                      ),
                      box(width = 12, title = "Demographic Trends",
                          plotlyOutput("demographic_trends_plot")
                      )
                    ),
                    fluidRow(
                      box(width = 12, title = "Health Equity Overview",
                          p("Health equity refers to the fair and just distribution of health resources across different regions and populations. This dashboard presents an overview of health indicators such as healthcare access, life expectancy, and chronic disease rates by region, to highlight disparities and inform policies aimed at reducing health inequities.")
                      )
                    )
            ),
            
            # Data Insights Tab
            tabItem(tabName = "data_insights",
                    fluidRow(
                      box(width = 6, title = "Health Equity Dataset",
                          DTOutput("health_equity_table")
                      ),
                      box(width = 6, title = "Regional Comparison",
                          plotlyOutput("regional_comparison_plot")
                      )
                    ),
                    fluidRow(
                      box(width = 12, title = "Data Insights",
                          p("The health equity dataset provides key health indicators for various regions. These indicators are critical for understanding how social, economic, and environmental factors influence health outcomes. By analyzing these factors, we can identify areas with disparities and target interventions for improvement.")
                      )
                    )
            ),
            
            # Predictions Tab
            tabItem(tabName = "predictions",
                    fluidRow(
                      box(width = 6, title = "Prediction Parameters",
                          selectInput("prediction_metric", "Select Metric", 
                                      choices = c("Healthcare Access", "Life Expectancy", "Chronic Disease Rate")),
                          actionButton("run_prediction", "Generate Prediction", class = "btn-primary")
                      ),
                      box(width = 6, title = "Prediction Results",
                          plotlyOutput("prediction_plot")
                      )
                    ),
                    fluidRow(
                      box(width = 12, title = "Predictive Analysis",
                          p("Predictive analytics in health equity can help forecast future trends in healthcare access, life expectancy, and chronic disease rates. These predictions can inform public health strategies and resource allocation, helping policymakers proactively address emerging health challenges in different regions.")
                      )
                    )
            ),
            
            # Regional Health Overview Tab
            tabItem(tabName = "regional_health",
                    fluidRow(
                      box(width = 12, title = "Health Metrics by Region",
                          leafletOutput("health_map")
                      )
                    ),
                    fluidRow(
                      box(width = 12, title = " Regional Health Overview",
                          p("Regional health disparities are often influenced by factors such as geography, access to healthcare services, and socioeconomic conditions. The Regional Health Overview tab provides a visual representation of these disparities across different regions, helping to identify where interventions are most needed.")
                      )
                    )
            ),
            
            # Healthcare Access Analysis Tab
            tabItem(tabName = "access_analysis",
                    fluidRow(
                      box(width = 12, title = "Healthcare Access Over Time",
                          plotlyOutput("access_over_time_plot")
                      )
                    ),
                    fluidRow(
                      box(width = 12, title = "Healthcare Access Analysis",
                          p("Healthcare access is a critical determinant of health outcomes. Inequitable access to healthcare services leads to disparities in health outcomes, including life expectancy and chronic disease rates. This tab provides insights into trends in healthcare access over time and across different regions.")
                      )
                    )
            )
          )
        )
      )
    }
  })
  
  # Login Logic with Database Validation
  observeEvent(input$login_button, {
    if (validate_user(input$username, input$password)) {
      AUTH$logged_in <- TRUE
      AUTH$username <- input$username
      showNotification("Welcome to Health Equity Explorer!", type = "message")
    } else {
      showNotification("Invalid Credentials", type = "error")
    }
  })
  
  # Sign Up Logic with Database Storage
  observeEvent(input$signup_button, {
    # Input validation
    if (input$new_password != input$confirm_password) {
      showNotification("Passwords do not match", type = "error")
      return()
    }
    
    # Attempt to add user to database
    success <- add_user_to_database(
      username = input$new_username, 
      password = input$new_password,
      email = input$email
    )
    
    if (success) {
      showNotification("Sign up successful!", type = "message")
      updateTabsetPanel(session, "login_tabs", selected = "Login")
    } else {
      showNotification("Username already exists or signup failed", type = "error")
    }
  })
  
  # Visualization Outputs
  output$health_overview_plot <- renderPlotly({
    plot_ly(health_equity_data, x = ~Region, y = ~Healthcare_Access, 
            type = 'bar', name = 'Healthcare Access',
            marker = list(color = '#2575fc')) %>%
      add_trace(y = ~Life_Expectancy, name = 'Life Expectancy', 
                marker = list(color = '#6a11cb')) %>%
      layout(title = "Health Equity Metrics by Region",
             xaxis = list(title = "Region"),
             yaxis = list(title = "Percentage"),
             barmode = 'group')
  })
  
  output$demographic_trends_plot <- renderPlotly({
    plot_ly(demographic_trends, x = ~Year, y = ~Population_Health_Index, 
            color = ~Region, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Population Health Index Trends",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Health Index"))
  })
  
  output$health_equity_table <- renderDT({
    datatable(health_equity_data, 
              options = list(pageLength = 5, 
                             scrollX = TRUE, 
                             autoWidth = TRUE),
              class = 'cell-border stripe')
  })
  
  output$regional_comparison_plot <- renderPlotly({
    plot_ly(health_equity_data, x = ~Region, y = ~Chronic_Disease_Rate, 
            type = 'bar', name = 'Chronic Disease Rate',
            marker = list(color = '#ff6347')) %>%
      add_trace(y = ~Education_Level, name = 'Education Level', 
                marker = list(color = '#3cb371')) %>%
      layout(title = "Chronic Disease Rate vs Education Level by Region",
             xaxis = list(title = "Region"),
             yaxis = list(title = "Percentage"),
             barmode = 'group')
  })
  
  output$prediction_plot <- renderPlotly({
    # Placeholder for prediction logic
    plot_ly(demographic_trends, x = ~Year, y = ~Population_Health_Index, 
            color = ~Region, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = paste("Predicted Health Metric: ", input$prediction_metric),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Predicted Value"))
  })
  
  output$health_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = c(-73.9352, -118.2437, -101.8987, -99.9018, -87.6298), 
                 lat = c(40.7128, 34.0522, 41.6331, 33.4484, 41.8781),
                 popup = c("Urban East", "Urban West", "Rural North", "Rural South", "Suburban Midwest"))
  })
  
  # Healthcare Access Analysis over Time
  output$access_over_time_plot <- renderPlotly({
    plot_ly(demographic_trends, x = ~Year, y = ~Population_Health_Index, 
            color = ~Region, type = 'bar') %>%
      layout(title = "Healthcare Access Over Time by Region",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Health Index"))
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)