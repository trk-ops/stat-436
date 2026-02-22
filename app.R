# app.R
library(shiny)
library(tidyverse)
library(readr)
library(stringr)
library(DT)
library(rsconnect)

# =========================
# 1) DATA LOADING
# =========================
DATA_URL <- "https://raw.githubusercontent.com/trk-ops/stat-436/main/Olympic_Swimming_Results_1912to2020.csv"

load_swim_data <- function(url) {
  tf <- tempfile(fileext = ".csv")
  download.file(url, tf, mode = "wb", quiet = TRUE)
  read_csv(tf, show_col_types = FALSE)
}

# =========================
# 2) HELPERS
# =========================

# Robust time parser -> numeric seconds
# Handles:
#  "50.39"
#  "2:06.38"          -> mm:ss.xx
#  "00:01:15.800000"  -> hh:mm:ss.xxxxxx (we convert to seconds)
parse_time_to_seconds <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  
  out <- sapply(x, function(val) {
    if (is.na(val) || val == "") return(NA_real_)
    
    # If it's already numeric like "50.39"
    if (!str_detect(val, ":")) {
      num <- suppressWarnings(as.numeric(val))
      if (is.na(num)) return(NA_real_)
      return(num)
    }
    
    # Split by ":" for hh:mm:ss OR mm:ss
    parts <- str_split(val, ":", simplify = TRUE)
    parts <- parts[parts != ""]
    parts_num <- suppressWarnings(as.numeric(parts))
    if (any(is.na(parts_num))) return(NA_real_)
    
    if (length(parts_num) == 2) {
      # mm:ss
      mins <- parts_num[1]
      secs <- parts_num[2]
      return(mins * 60 + secs)
    }
    
    if (length(parts_num) == 3) {
      # hh:mm:ss
      hrs  <- parts_num[1]
      mins <- parts_num[2]
      secs <- parts_num[3]
      return(hrs * 3600 + mins * 60 + secs)
    }
    
    NA_real_
  })
  
  as.numeric(out)
}

# Format numeric seconds -> "m:ss.xx" (or "h:mm:ss.xx" if >= 1 hour)
format_seconds <- function(sec) {
  ifelse(
    is.na(sec),
    NA_character_,
    {
      h <- floor(sec / 3600)
      m <- floor((sec %% 3600) / 60)
      s <- sec %% 60
      
      ifelse(
        h > 0,
        sprintf("%d:%02d:%05.2f", h, m, s),
        sprintf("%d:%05.2f", m, s)  # m:ss.xx
      )
    }
  )
}

# =========================
# 3) LOAD + CLEAN DATA
# =========================
swim <- load_swim_data(DATA_URL)

# Standardize types and create numeric time
swim <- swim %>%
  mutate(
    Year = as.integer(Year),
    Gender = as.character(Gender),
    Stroke = as.character(Stroke),
    `Distance (in meters)` = as.character(`Distance (in meters)`),
    Rank = suppressWarnings(as.numeric(Rank)),
    Results = as.character(Results),
    time_sec = suppressWarnings(parse_time_to_seconds(Results))
  )

# Choices for UI
gender_choices <- sort(unique(na.omit(swim$Gender)))
stroke_choices <- sort(unique(na.omit(swim$Stroke)))
dist_choices   <- sort(unique(na.omit(swim$`Distance (in meters)`)))

year_min <- min(swim$Year, na.rm = TRUE)
year_max <- max(swim$Year, na.rm = TRUE)

rank_min <- floor(min(swim$Rank, na.rm = TRUE))
rank_max <- ceiling(max(swim$Rank, na.rm = TRUE))

# =========================
# 4) UI
# =========================
ui <- fluidPage(
  titlePanel("Olympic Swimming: Gold Medal Times Explorer (1912–2020)"),
  
  tags$div(
    style = "max-width: 980px;",
    tags$ul(
      tags$li("Filter by Gender, Stroke, Distance, Rank, and Year range."),
      tags$li("Y-axis is reversed so higher = faster."),
      tags$li("Times are displayed as m:ss.xx."),
    ),
    tags$hr()
  ),
  
  sidebarLayout(
    sidebarPanel(
      tags$p("Tip: Start with Men / 100m / Butterfly / Rank = 1 (gold), Rank = 2 (silver), Rank = 3 (bronze)."),
      
      selectInput(
        "gender", "Gender:",
        choices = gender_choices,
        selected = if ("Men" %in% gender_choices) "Men" else gender_choices[1]
      ),
      
      selectInput(
        "stroke", "Stroke:",
        choices = stroke_choices,
        selected = if ("Butterfly" %in% stroke_choices) "Butterfly" else stroke_choices[1]
      ),
      
      selectInput(
        "distance", "Distance:",
        choices = dist_choices,
        selected = if ("100m" %in% dist_choices) "100m" else dist_choices[1]
      ),
      
      sliderInput(
        "rank", "Rank filter (1 = gold):",
        min = rank_min, max = rank_max,
        value = c(1, 1), step = 1
      ),
      
      sliderInput(
        "year", "Year range:",
        min = year_min, max = year_max,
        value = c(year_min, year_max), step = 1, sep = ""
      ),
      
      checkboxInput("show_labels", "Label each point with time", value = TRUE),
      checkboxInput("highlight_fastest", "Highlight fastest time in range", value = TRUE),
      
      downloadButton("download_csv", "Download filtered CSV")
    ),
    
    mainPanel(
      plotOutput("time_plot", height = "520px"),
      tags$h4("Filtered results (sorted by year)"),
      DTOutput("results_table")
    )
  )
)

# =========================
# 5) SERVER
# =========================
server <- function(input, output, session) {
  
  filtered_swim <- reactive({
    swim %>%
      filter(!is.na(time_sec)) %>%
      filter(
        Gender == input$gender,
        Stroke == input$stroke,
        `Distance (in meters)` == input$distance,
        Rank >= input$rank[1],
        Rank <= input$rank[2],
        Year >= input$year[1],
        Year <= input$year[2]
      ) %>%
      arrange(Year) %>%
      mutate(time_label = format_seconds(time_sec))
  })
  
  fastest_row <- reactive({
    df <- filtered_swim()
    if (nrow(df) == 0) return(NULL)
    df %>% slice_min(order_by = time_sec, n = 1, with_ties = FALSE)
  })
  
  output$time_plot <- renderPlot({
    df <- filtered_swim()
    validate(need(nrow(df) > 0, "No rows match your filters. Try widening Rank or Year range."))
    
    p <- ggplot(df, aes(x = Year, y = time_sec)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_y_reverse(labels = format_seconds) +  
      labs(
        title = paste0(input$gender, " ", input$distance, " ", input$stroke, " — Olympic Times by Year"),
        subtitle = "Medal performances across Olympic history"
        ,
        x = "Olympic Year",
        y = "Time"
      ) +
      theme_minimal(base_size = 13)
    
    if (isTRUE(input$show_labels)) {
      p <- p + geom_text(
        aes(label = time_label),
        vjust = -0.8,
        size = 3
      )
    }
    
    if (isTRUE(input$highlight_fastest)) {
      fr <- fastest_row()
      if (!is.null(fr)) {
        p <- p +
          geom_point(data = fr, aes(x = Year, y = time_sec), size = 5) +
          geom_text(
            data = fr,
            aes(x = Year, y = time_sec, label = paste0("Fastest: ", format_seconds(time_sec))),
            vjust = 1.4, size = 3
          )
      }
    }
    
    p
  })
  
  output$results_table <- renderDT({
    filtered_swim() %>%
      transmute(
        Year,
        Location,
        `Distance (in meters)`,
        Stroke,
        Gender,
        Athlete,
        Rank,
        `Time (formatted)` = time_label,
        `Original Results` = Results
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("filtered_swimming_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(filtered_swim(), file)
    }
  )
}

shinyApp(ui, server)