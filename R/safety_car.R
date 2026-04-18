# safety_car.R
# Panel 3 — Safety Car Lottery
# NOTE: DRIVER_NAMES and LOCAL_DRIVER_IDS are defined in data_pipeline.R (sourced first).

library(plotly)
library(dplyr)

detect_safety_cars <- function(lap_data, season, round, race_name) {
  
  lap_data <- lap_data %>%
    dplyr::filter(!is.na(driver), driver != "NA", driver != "") %>%
    mutate(driver = toupper(driver), ts = as.character(track_status))
  
  lap_status <- lap_data %>%
    group_by(lap_number) %>%
    summarise(
      has_sc  = any(sapply(ts, function(x) "4" %in% strsplit(x, "")[[1]])),
      has_vsc = any(sapply(ts, function(x) any(c("6", "7") %in% strsplit(x, "")[[1]]))),
      .groups = "drop"
    ) %>%
    dplyr::filter(has_sc | has_vsc) %>%
    mutate(sc_type = ifelse(has_sc, "SC", "VSC")) %>%
    arrange(lap_number) %>%
    select(lap_number, sc_type)
  
  if (nrow(lap_status) == 0) return(NULL)
  
  lap_status <- lap_status %>%
    mutate(
      prev_lap  = lag(lap_number),
      prev_type = lag(sc_type),
      new_event = is.na(prev_lap) | (lap_number - prev_lap > 1) | (sc_type != prev_type),
      event_id  = cumsum(new_event)
    ) %>%
    group_by(event_id, sc_type) %>%
    summarise(
      start_lap = min(lap_number),
      end_lap   = max(lap_number),
      .groups   = "drop"
    ) %>%
    select(-event_id) %>%
    mutate(season = season, round = round, race_name = race_name)
  
  lap_status
}

sc_pit_analysis <- function(lap_data, pit_data, sc_events) {
  
  if (is.null(sc_events) || nrow(sc_events) == 0) return(data.frame())
  
  results <- list()
  
  for (i in seq_len(nrow(sc_events))) {
    sc           <- sc_events[i, ]
    window_start <- sc$start_lap
    window_end   <- sc$end_lap + 2
    
    pitted_during_sc <- pit_data %>%
      mutate(lap = as.numeric(lap)) %>%
      dplyr::filter(lap >= window_start, lap <= window_end) %>%
      mutate(driver_id = ifelse(
        tolower(driver_id) %in% names(LOCAL_DRIVER_IDS),
        LOCAL_DRIVER_IDS[tolower(driver_id)],
        toupper(driver_id)
      )) %>%
      pull(driver_id) %>%
      unique()
    
    all_drivers <- lap_data %>%
      dplyr::filter(!is.na(driver), driver != "NA") %>%
      mutate(driver = toupper(driver)) %>%
      pull(driver) %>%
      unique()
    
    stayed_out <- setdiff(all_drivers, pitted_during_sc)
    
    if (length(pitted_during_sc) > 0) {
      results[[length(results) + 1]] <- data.frame(
        race_name = sc$race_name, round = sc$round,
        sc_type   = sc$sc_type,  start_lap = sc$start_lap,
        driver    = pitted_during_sc, action = "Pitted",
        stringsAsFactors = FALSE
      )
    }
    if (length(stayed_out) > 0) {
      results[[length(results) + 1]] <- data.frame(
        race_name = sc$race_name, round = sc$round,
        sc_type   = sc$sc_type,  start_lap = sc$start_lap,
        driver    = stayed_out,  action = "Stayed Out",
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(results) == 0) return(data.frame())
  bind_rows(results)
}

plot_safety_car_timeline <- function(sc_season_data, sc_type_filter = "all") {
  
  if (is.null(sc_season_data) || nrow(sc_season_data) == 0) {
    return(plotly::plot_ly() %>%
             plotly::layout(title = list(text = "No safety car data available",
                                         font = list(size = 14, family = "Helvetica Neue")),
                            paper_bgcolor = "#ffffff"))
  }
  
  if (sc_type_filter == "sc")  sc_season_data <- sc_season_data %>% dplyr::filter(sc_type == "SC")
  if (sc_type_filter == "vsc") sc_season_data <- sc_season_data %>% dplyr::filter(sc_type == "VSC")
  
  if (nrow(sc_season_data) == 0) {
    return(plotly::plot_ly() %>%
             plotly::layout(title = list(text = "No events match this filter"),
                            paper_bgcolor = "#ffffff"))
  }
  
  sc_season_data <- sc_season_data %>%
    mutate(hover = paste0(
      "<b>", race_name, "</b><br>", sc_type,
      " deployed on lap ", start_lap, "<br>",
      "Laps ", start_lap, " to ", end_lap
    ))
  
  race_order     <- sc_season_data %>% arrange(round) %>% pull(race_name) %>% unique()
  sc_season_data <- sc_season_data %>%
    mutate(race_name = factor(race_name, levels = rev(race_order)))
  
  max_lap  <- max(sc_season_data$end_lap, na.rm = TRUE)
  x_breaks <- seq(0, max(max_lap + 5, 70), by = 10)
  
  y_labels <- rev(race_order)
  bold_y   <- paste0("<b>", y_labels, "  </b>")
  bold_x   <- paste0("<b>", x_breaks, "</b>")
  
  plotly::plot_ly(
    sc_season_data,
    x = ~start_lap, y = ~race_name, type = "scatter", mode = "markers",
    color = ~sc_type, colors = c("SC" = "#e10600", "VSC" = "#FFC906"),
    marker = list(size = 14, symbol = "circle", line = list(color = "#1a1a1a", width = 1)),
    text = ~hover, hoverinfo = "text",
    showlegend = FALSE
  ) %>%
    plotly::layout(
      xaxis = list(
        title      = list(text = "<b>Lap Number</b>",
                          font = list(size = 12, family = "Helvetica Neue", color = "#1a1a1a")),
        range      = c(0, max(max_lap + 5, 70)),
        fixedrange = TRUE,
        gridcolor  = "#eeeeee",
        tickvals   = x_breaks,
        ticktext   = bold_x,
        tickfont   = list(size = 11, family = "Helvetica Neue", color = "#1a1a1a"),
        showline   = TRUE,
        linecolor  = "#1a1a1a",
        linewidth  = 1
      ),
      yaxis = list(
        title      = "",
        fixedrange = TRUE,
        automargin = TRUE,
        tickvals   = y_labels,
        ticktext   = bold_y,
        tickfont   = list(size = 11, family = "Helvetica Neue", color = "#1a1a1a"),
        showline   = TRUE,
        linecolor  = "#1a1a1a",
        linewidth  = 1,
        ticksuffix = "  "
      ),
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#fafafa",
      margin        = list(l = 160, r = 20, t = 20, b = 50)
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

plot_sc_winners_losers <- function(pit_analysis_data) {
  
  if (is.null(pit_analysis_data) || nrow(pit_analysis_data) == 0) {
    return(plotly::plot_ly() %>%
             plotly::layout(title = list(text = "No safety car pit data available",
                                         font = list(size = 14)),
                            paper_bgcolor = "#ffffff"))
  }
  
  summary <- pit_analysis_data %>%
    mutate(driver_label = ifelse(driver %in% names(DRIVER_NAMES), DRIVER_NAMES[driver], driver)) %>%
    group_by(driver_label) %>%
    summarise(
      times_pitted = sum(action == "Pitted"),
      times_stayed = sum(action == "Stayed Out"),
      .groups = "drop"
    ) %>%
    mutate(net_score = times_pitted - times_stayed) %>%
    arrange(desc(net_score))
  
  colours <- ifelse(summary$net_score > 0, "#d4edda",
                    ifelse(summary$net_score < 0, "#f8d7da", "#fafafa"))
  n <- nrow(summary)
  
  b <- function(x) paste0("<b>", x, "</b>")
  
  plotly::plot_ly(
    type        = "table",
    columnwidth = c(260, 90, 110, 90),   # wider Driver col; Stayed Out needs room
    header = list(
      values = c("<b>Driver</b>", "<b>SC Stops Taken</b>", "<b>SC Stops Missed</b>", "<b>Net Gain</b>"),
      fill   = list(color = "#1a1a1a"),
      font   = list(color = "white", size = 12, family = "Helvetica Neue"),
      align  = c("center", "center", "center", "center"),
      height = 36
    ),
    cells = list(
      values = list(
        b(summary$driver_label),
        b(summary$times_pitted),
        b(summary$times_stayed),
        b(summary$net_score)
      ),
      fill = list(color = list(
        rep("#fafafa", n), rep("#fafafa", n),
        rep("#fafafa", n), colours
      )),
      font   = list(size = 12, family = "Helvetica Neue", color = "#1a1a1a"),
      align  = c("center", "center", "center", "center"),
      height = 32,
      line   = list(color = "#e0e0e0", width = 1)
    )
  ) %>%
    plotly::layout(
      margin        = list(l = 10, r = 10, t = 10, b = 10),
      paper_bgcolor = "#ffffff"
    ) %>%
    plotly::config(displayModeBar = FALSE)
}