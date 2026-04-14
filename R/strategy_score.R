# strategy_score.R
# Panel 4 — Strategy Score
# NOTE: DRIVER_NAMES and LOCAL_DRIVER_IDS are defined in data_pipeline.R (sourced first).

library(plotly)
library(dplyr)

TEAM_COLOURS <- c(
  "Red Bull"     = "#3671C6", "Mercedes"   = "#27F4D2",
  "Ferrari"      = "#E8002D", "McLaren"    = "#FF8000",
  "Aston Martin" = "#229971", "Alpine"     = "#FF87BC",
  "Williams"     = "#64C4FF", "AlphaTauri" = "#6692FF",
  "Alfa Romeo"   = "#C92D4B", "Haas"       = "#B6BABD",
  "RB"           = "#6692FF", "Stake"      = "#C92D4B"
)

DRIVER_TEAM <- c(
  "VER" = "Red Bull",     "PER" = "Red Bull",
  "HAM" = "Mercedes",     "RUS" = "Mercedes",
  "LEC" = "Ferrari",      "SAI" = "Ferrari",
  "NOR" = "McLaren",      "PIA" = "McLaren",
  "ALO" = "Aston Martin", "STR" = "Aston Martin",
  "OCO" = "Alpine",       "GAS" = "Alpine",
  "ALB" = "Williams",     "SAR" = "Williams",     "COL" = "Williams",
  "TSU" = "AlphaTauri",   "RIC" = "AlphaTauri",   "LAW" = "RB",
  "BOT" = "Alfa Romeo",   "ZHO" = "Alfa Romeo",
  "MAG" = "Haas",         "HUL" = "Haas",
  "MSC" = "Haas",         "DEV" = "AlphaTauri",
  "BEA" = "Ferrari",      "VET" = "Aston Martin",
  "LAT" = "Williams",     "GIO" = "Alfa Romeo",
  "KVY" = "AlphaTauri",   "RAI" = "Alfa Romeo",
  "GRO" = "Haas",         "MAZ" = "Haas",
  "HAD" = "RB",           "ANT" = "Mercedes",
  "DOO" = "Alpine",       "BOR" = "Stake"
)

score_from_pits_only <- function(pit_data) {
  if (is.null(pit_data) || nrow(pit_data) == 0) return(data.frame())
  
  pit_data <- pit_data %>%
    mutate(driver_id = ifelse(
      tolower(driver_id) %in% names(LOCAL_DRIVER_IDS),
      LOCAL_DRIVER_IDS[tolower(driver_id)],
      toupper(driver_id)
    ))
  
  pit_counts <- pit_data %>%
    group_by(driver_id) %>%
    summarise(stops = n(), .groups = "drop")
  
  if (nrow(pit_counts) == 0) return(data.frame())
  min_stops <- min(pit_counts$stops)
  
  pit_counts %>%
    mutate(
      extra_stops  = pmax(0, stops - min_stops),
      stop_score   = pmax(0, 40 - extra_stops * 10),
      sc_score     = 17L,
      tyre_score   = 12L,
      total_score  = stop_score + sc_score + tyre_score,
      driver       = driver_id,
      driver_label = ifelse(driver_id %in% names(DRIVER_NAMES), DRIVER_NAMES[driver_id], driver_id),
      team         = ifelse(driver_id %in% names(DRIVER_TEAM),  DRIVER_TEAM[driver_id],  "Unknown")
    ) %>%
    select(driver, driver_label, team, stops, stop_score, sc_score, tyre_score, total_score) %>%
    arrange(desc(total_score))
}

normalize_pit_ids <- function(pit_data) {
  if (is.null(pit_data) || nrow(pit_data) == 0) return(pit_data)
  pit_data %>%
    mutate(driver_id = ifelse(
      tolower(driver_id) %in% names(LOCAL_DRIVER_IDS),
      LOCAL_DRIVER_IDS[tolower(driver_id)],
      toupper(driver_id)
    ))
}

score_race <- function(lap_data, pit_data, sc_events = NULL) {
  
  if (is.null(lap_data) || nrow(lap_data) == 0) return(data.frame())
  
  if (!"driver" %in% names(lap_data) && "driver_id" %in% names(lap_data))
    lap_data <- lap_data %>% dplyr::rename(driver = driver_id)
  
  if (!"stint"      %in% names(lap_data)) lap_data$stint      <- 1L
  if (!"compound"   %in% names(lap_data)) lap_data$compound   <- NA_character_
  if (!"tyre_life"  %in% names(lap_data)) lap_data$tyre_life  <- NA_integer_
  if (!"position"   %in% names(lap_data)) lap_data$position   <- NA_integer_
  if (!"lap_number" %in% names(lap_data) && "lap" %in% names(lap_data))
    lap_data <- lap_data %>% dplyr::rename(lap_number = lap)
  
  lap_data <- lap_data %>%
    dplyr::filter(!is.na(driver), driver != "NA", driver != "") %>%
    mutate(driver = toupper(driver))
  
  pit_data  <- normalize_pit_ids(pit_data)
  drivers   <- unique(lap_data$driver)
  
  pit_counts <- pit_data %>%
    group_by(driver_id) %>%
    summarise(stops = n(), .groups = "drop")
  
  min_stops <- if (nrow(pit_counts) > 0) min(pit_counts$stops, na.rm = TRUE) else 1
  
  sc_windows <- if (!is.null(sc_events) && nrow(sc_events) > 0) {
    sc_events %>% select(start_lap, end_lap)
  } else {
    data.frame(start_lap = integer(), end_lap = integer())
  }
  
  scores <- list()
  
  for (drv in drivers) {
    drv_laps <- lap_data %>% dplyr::filter(driver == drv)
    drv_pits <- pit_data %>%
      dplyr::filter(driver_id == drv) %>%
      mutate(lap = as.numeric(lap))
    
    stops       <- nrow(drv_pits)
    extra_stops <- max(0, stops - min_stops)
    stop_score  <- max(0, 40 - (extra_stops * 10))
    
    if (nrow(sc_windows) == 0) {
      sc_score <- 35
    } else {
      sc_hits <- 0
      for (w in seq_len(nrow(sc_windows))) {
        window_start     <- sc_windows$start_lap[w]
        window_end       <- sc_windows$end_lap[w] + 2
        pitted_in_window <- any(drv_pits$lap >= window_start & drv_pits$lap <= window_end)
        if (pitted_in_window) sc_hits <- sc_hits + 1
      }
      sc_score <- round(35 * (sc_hits / nrow(sc_windows)))
    }
    
    stints <- tryCatch(
      drv_laps %>%
        dplyr::filter(!is.na(compound), compound != "") %>%
        group_by(stint, compound) %>%
        summarise(laps = n(), .groups = "drop"),
      error = function(e) data.frame(stint = integer(), compound = character(), laps = integer())
    )
    
    if (nrow(stints) == 0) {
      tyre_score <- 12
    } else {
      hard_laps <- stints %>%
        dplyr::filter(toupper(compound) %in% c("HARD", "MEDIUM")) %>%
        pull(laps) %>% sum()
      soft_laps <- stints %>%
        dplyr::filter(toupper(compound) %in% c("SOFT", "SUPERSOFT", "ULTRASOFT")) %>%
        pull(laps) %>% sum()
      total_compound_laps <- hard_laps + soft_laps
      tyre_score <- if (total_compound_laps == 0) 12 else round(25 * hard_laps / total_compound_laps)
    }
    
    scores[[length(scores) + 1]] <- data.frame(
      driver      = drv,
      stops       = stops,
      stop_score  = stop_score,
      sc_score    = sc_score,
      tyre_score  = tyre_score,
      total_score = stop_score + sc_score + tyre_score,
      stringsAsFactors = FALSE
    )
  }
  
  if (length(scores) == 0) return(data.frame())
  
  bind_rows(scores) %>%
    mutate(
      driver_label = ifelse(driver %in% names(DRIVER_NAMES), DRIVER_NAMES[driver], driver),
      team         = ifelse(driver %in% names(DRIVER_TEAM),  DRIVER_TEAM[driver],  "Unknown")
    ) %>%
    arrange(desc(total_score))
}

plot_strategy_scores <- function(score_data) {
  
  if (is.null(score_data) || nrow(score_data) == 0) {
    return(plotly::plot_ly() %>%
             plotly::layout(title = list(text = "No strategy score data available", font = list(size = 14)),
                            paper_bgcolor = "#ffffff"))
  }
  
  score_data <- score_data %>%
    arrange(total_score) %>%
    mutate(
      driver_label = factor(paste0("<b>", driver_label, "</b>"), levels = paste0("<b>", driver_label, "</b>")),
      bar_colour   = ifelse(team %in% names(TEAM_COLOURS), TEAM_COLOURS[team], "#999999"),
      hover = paste0(
        "<b>", driver_label, "</b><br>",
        "Team: ", team, "<br>",
        "Total Score: ", total_score, "/100<br>",
        "Stop Efficiency: ", stop_score, "/40<br>",
        "SC Utilization: ", sc_score, "/35<br>",
        "Tyre Strategy: ", tyre_score, "/25"
      )
    )
  
  plotly::plot_ly(
    score_data,
    x            = ~total_score,
    y            = ~driver_label,
    type         = "bar",
    orientation  = "h",
    marker       = list(color = ~bar_colour, line = list(color = "#ffffff", width = 0.5)),
    text         = ~paste0("<b>", total_score, "</b>"),
    textposition = "outside",
    hovertext    = ~hover,
    hoverinfo    = "text"
  ) %>%
    plotly::layout(
      xaxis = list(
        title     = list(text = "<b>Strategy Score</b>", font = list(size = 12, color = "#1a1a1a")),
        range     = c(0, 115),
        fixedrange = TRUE,
        gridcolor  = "#eeeeee",
        dtick      = 20,
        tickfont   = list(size = 11, family = "Helvetica Neue", color = "#1a1a1a", bold = TRUE)
      ),
      yaxis = list(
        title      = "",
        fixedrange = TRUE,
        automargin = TRUE,
        tickfont   = list(size = 11, family = "Helvetica Neue", color = "#1a1a1a", bold = TRUE),
        ticksuffix = "  "
      ),
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#fafafa",
      margin        = list(l = 20, r = 80, t = 20, b = 50),
      showlegend    = FALSE
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

plot_strategy_league <- function(season_scores) {
  
  if (is.null(season_scores) || nrow(season_scores) == 0) {
    return(plotly::plot_ly() %>%
             plotly::layout(title = list(text = "No league data available", font = list(size = 14)),
                            paper_bgcolor = "#ffffff"))
  }
  
  league <- season_scores %>%
    mutate(team = case_when(
      team == "AlphaTauri" ~ "AlphaTauri/RB",
      team == "RB"         ~ "AlphaTauri/RB",
      team == "Alfa Romeo" ~ "Alfa Romeo/Stake",
      team == "Stake"      ~ "Alfa Romeo/Stake",
      TRUE ~ team
    )) %>%
    group_by(team) %>%
    summarise(
      avg_score = round(mean(total_score, na.rm = TRUE), 1),
      races     = n_distinct(round),
      .groups   = "drop"
    ) %>%
    arrange(desc(avg_score)) %>%
    mutate(
      rank       = row_number(),
      bar_colour = case_when(
        team == "AlphaTauri/RB"    ~ "#6692FF",
        team == "Alfa Romeo/Stake" ~ "#C92D4B",
        team %in% names(TEAM_COLOURS) ~ TEAM_COLOURS[team],
        TRUE ~ "#999999"
      )
    )
  
  n <- nrow(league)
  
  plotly::plot_ly(
    type = "table",
    columnwidth = c(40, 160, 80, 60),
    header = list(
      values = c("<b>Rank</b>", "<b>Team</b>", "<b>Avg Score</b>", "<b>Races</b>"),
      fill   = list(color = "#1a1a1a"),
      font   = list(color = "white", size = 12, family = "Helvetica Neue", bold = TRUE),
      align  = rep("center", 4),
      height = 36
    ),
    cells = list(
      values = list(
        paste0("<b>", league$rank, "</b>"),
        paste0("<b>&nbsp;&nbsp;", league$team, "&nbsp;&nbsp;</b>"),
        paste0("<b>", league$avg_score, "</b>"),
        paste0("<b>", league$races, "</b>")
      ),
      fill = list(color = list(
        rep("#fafafa", n),
        league$bar_colour,
        rep("#fafafa", n),
        rep("#fafafa", n)
      )),
      font = list(
        size   = c(12, 13, 13, 12),
        family = "Helvetica Neue",
        bold   = TRUE,
        color  = list(
          rep("#1a1a1a", n),
          rep("#ffffff", n),
          rep("#1a1a1a", n),
          rep("#1a1a1a", n)
        )
      ),
      align  = rep("center", 4),
      height = 32,
      line   = list(color = "#e0e0e0", width = 1)
    )
  ) %>%
    plotly::layout(margin = list(l = 10, r = 10, t = 10, b = 10), paper_bgcolor = "#ffffff") %>%
    plotly::config(displayModeBar = FALSE)
}