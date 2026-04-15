# undercut_detector.R
# Panel 2 — Undercut Detector
# NOTE: DRIVER_NAMES and LOCAL_DRIVER_IDS are defined in data_pipeline.R (sourced first).

library(plotly)
library(dplyr)

detect_undercuts <- function(lap_data, pit_data) {
  
  lap_data <- lap_data %>%
    dplyr::filter(!is.na(driver), driver != "NA", driver != "") %>%
    mutate(driver = toupper(driver))
  
  pit_data <- pit_data %>%
    mutate(
      lap      = as.numeric(lap),
      driver_id = ifelse(
        tolower(driver_id) %in% names(LOCAL_DRIVER_IDS),
        LOCAL_DRIVER_IDS[tolower(driver_id)],
        toupper(driver_id)
      )
    )
  
  positions <- lap_data %>%
    select(driver, lap_number, position) %>%
    dplyr::filter(!is.na(position))
  
  undercuts <- list()
  drivers_who_pitted <- unique(pit_data$driver_id)
  
  for (i in seq_along(drivers_who_pitted)) {
    initiator       <- drivers_who_pitted[i]
    initiator_stops <- pit_data %>% dplyr::filter(driver_id == initiator)
    
    for (j in seq_len(nrow(initiator_stops))) {
      pit_lap    <- initiator_stops$lap[j]
      pos_before <- positions %>%
        dplyr::filter(driver == initiator, lap_number == pit_lap - 1) %>%
        pull(position)
      
      if (length(pos_before) == 0) next
      target_pos <- pos_before - 1
      if (target_pos < 1) next
      
      target <- positions %>%
        dplyr::filter(lap_number == pit_lap - 1, position == target_pos) %>%
        pull(driver)
      if (length(target) == 0) next
      target <- target[1]
      
      target_stops <- pit_data %>%
        dplyr::filter(driver_id == target, lap > pit_lap, lap <= pit_lap + 3)
      if (nrow(target_stops) == 0) next
      
      target_pit_lap <- target_stops$lap[1]
      if (target_pit_lap <= pit_lap) next   # same lap — not a real undercut
      
      check_lap         <- target_pit_lap + 5
      pos_initiator_after <- positions %>%
        dplyr::filter(driver == initiator, lap_number == check_lap) %>%
        pull(position)
      pos_target_after    <- positions %>%
        dplyr::filter(driver == target, lap_number == check_lap) %>%
        pull(position)
      
      if (length(pos_initiator_after) == 0 || length(pos_target_after) == 0) next
      
      success  <- pos_initiator_after < pos_target_after
      gap_laps <- target_pit_lap - pit_lap
      
      undercuts[[length(undercuts) + 1]] <- data.frame(
        initiator      = initiator,
        target         = target,
        pit_lap        = pit_lap,
        target_pit_lap = target_pit_lap,
        gap_laps       = gap_laps,
        pos_before     = pos_before,
        pos_after      = pos_initiator_after,
        success        = success,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(undercuts) == 0) return(data.frame())
  bind_rows(undercuts)
}

plot_undercut_table <- function(undercut_data) {
  
  if (is.null(undercut_data) || nrow(undercut_data) == 0) {
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          title        = list(text = "No undercut attempts detected in this race",
                              font = list(size = 14, family = "Helvetica Neue")),
          paper_bgcolor = "#ffffff"
        )
    )
  }
  
  undercut_data <- undercut_data %>%
    mutate(
      initiator_name = ifelse(initiator %in% names(DRIVER_NAMES), DRIVER_NAMES[initiator], initiator),
      target_name    = ifelse(target    %in% names(DRIVER_NAMES), DRIVER_NAMES[target],    target),
      result_label   = ifelse(success, "&#10003;&nbsp;SUCCESS", "&#10007;&nbsp;FAILED"),
      result_colour  = ifelse(success, "#d4edda",    "#f8d7da")
    )
  
  n <- nrow(undercut_data)
  # Wrap in <br> padding to force vertical centering in plotly table cells
  bc <- function(x) paste0("<br><b>", x, "</b><br>")
  
  plotly::plot_ly(
    type        = "table",
    columnwidth = c(185, 185, 72, 120, 72, 80, 80, 115),
    header = list(
      values = c(
        "<b>Initiator</b>",
        "<b>Target</b>",
        "<b>Pit Lap</b>",
        "<b>Target Pit Lap</b>",
        "<b>Gap (laps)</b>",
        "<b>Pos Before</b>",
        "<b>Pos After</b>",
        "<b>Result</b>"
      ),
      fill   = list(color = "#1a1a1a"),
      font   = list(color = "white", size = 11, family = "Helvetica Neue"),
      align  = rep("center", 8),
      height = 44
    ),
    cells = list(
      values = list(
        bc(undercut_data$initiator_name),
        bc(undercut_data$target_name),
        bc(undercut_data$pit_lap),
        bc(undercut_data$target_pit_lap),
        bc(undercut_data$gap_laps),
        bc(undercut_data$pos_before),
        bc(undercut_data$pos_after),
        bc(undercut_data$result_label)
      ),
      fill = list(color = list(
        rep("#fafafa", n), rep("#fafafa", n),
        rep("#fafafa", n), rep("#fafafa", n),
        rep("#fafafa", n), rep("#fafafa", n),
        rep("#fafafa", n), undercut_data$result_colour
      )),
      font   = list(size = 11, family = "Helvetica Neue", color = "#1a1a1a"),
      align  = rep("center", 8),
      height = 52,
      line   = list(color = "#e0e0e0", width = 1)
    )
  ) %>%
    plotly::layout(
      margin        = list(l = 10, r = 10, t = 10, b = 10),
      paper_bgcolor = "#ffffff"
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

plot_undercut_bar <- function(undercut_data) {
  
  if (is.null(undercut_data) || nrow(undercut_data) == 0) {
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          title        = list(text = "No undercut attempts detected",
                              font = list(size = 14)),
          paper_bgcolor = "#ffffff"
        )
    )
  }
  
  summary <- undercut_data %>%
    mutate(initiator_name = ifelse(
      initiator %in% names(DRIVER_NAMES), DRIVER_NAMES[initiator], initiator
    )) %>%
    group_by(initiator_name) %>%
    summarise(
      attempts    = n(),
      successes   = sum(success),
      success_rate = round(100 * sum(success) / n(), 1),
      .groups = "drop"
    ) %>%
    arrange(success_rate) %>%
    mutate(initiator_name = paste0("<b>", initiator_name, "</b>"))
  
  plotly::plot_ly(
    summary,
    x           = ~success_rate,
    y           = ~reorder(initiator_name, success_rate),
    type        = "bar",
    orientation = "h",
    text        = ~paste0("<b>", successes, "/", attempts, "</b>"),
    textposition = "outside",
    marker = list(
      color = ~ifelse(success_rate == 100, "#43b649",
                      ifelse(success_rate == 0,  "#e10600", "#FFC906")),
      line  = list(color = "#ffffff", width = 0.5)
    ),
    hovertemplate = paste0(
      "<b>%{y}</b><br>Success rate: %{x}%<br>%{text} attempts<extra></extra>"
    ),
    textfont = list(size = 11, family = "Helvetica Neue", color = "#1a1a1a",
                    weight = "bold")
  ) %>%
    plotly::layout(
      xaxis = list(
        title      = list(text = "<b>Success Rate (%)</b>",
                          font = list(size = 12, family = "Helvetica Neue")),
        range      = c(0, 140),
        fixedrange = TRUE,
        gridcolor  = "#eeeeee",
        ticksuffix = "%",
        tickfont   = list(size = 11, family = "Helvetica Neue", weight = "bold")
      ),
      yaxis = list(
        title      = "",
        fixedrange = TRUE,
        automargin = TRUE,
        tickfont   = list(size = 11, family = "Helvetica Neue", color = "#1a1a1a",
                          weight = "bold"),
        ticklabelposition = "outside left",
        ticklen    = 8,
        tickcolor  = "rgba(0,0,0,0)"
      ),
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#fafafa",
      margin        = list(l = 10, r = 90, t = 20, b = 50)
    ) %>%
    plotly::config(displayModeBar = FALSE)
}