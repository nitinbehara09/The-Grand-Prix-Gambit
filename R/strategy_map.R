# strategy_map.R
# Panel 1 — Compound Strategy Map

library(plotly)
library(dplyr)

# ── Helper: friendly empty plot for missing data ─────────────────────────────
empty_plot_message <- function(message = "No data available for this race") {
  plotly::plot_ly() %>%
    plotly::layout(
      xaxis = list(visible = FALSE, fixedrange = TRUE),
      yaxis = list(visible = FALSE, fixedrange = TRUE),
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#fafafa",
      annotations = list(
        list(
          text      = message,
          x         = 0.5, y = 0.5,
          xref      = "paper", yref = "paper",
          showarrow = FALSE,
          font      = list(size = 14, color = "#999999", family = "Helvetica Neue")
        )
      )
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

plot_strategy_map <- function(lap_data, pit_data, driver_filter = "all",
                              season = NULL, round = NULL) {
  
  # ── Defensive guards: don't crash on missing data ─────────────────────────
  if (is.null(lap_data) || nrow(lap_data) == 0) {
    return(empty_plot_message(
      "Lap data not available for this race yet. Try another race or rebuild the cache."
    ))
  }
  if (is.null(pit_data)) pit_data <- data.frame()
  
  lap_data <- lap_data %>%
    filter(!is.na(driver), driver != "NA", driver != "") %>%
    mutate(compound = toupper(compound)) %>%
    mutate(compound = ifelse(is.na(compound) | compound == "NA" | compound == "", "UNKNOWN", compound)) %>%
    mutate(driver_label = ifelse(driver %in% names(DRIVER_NAMES), DRIVER_NAMES[driver], driver))
  
  # Second guard in case the above filter emptied it out
  if (nrow(lap_data) == 0) {
    return(empty_plot_message("No valid lap data for this race"))
  }
  
  race_last_lap <- max(lap_data$lap_number, na.rm = TRUE)
  
  driver_last_lap <- lap_data %>%
    group_by(driver, driver_label) %>%
    summarise(last_lap = max(lap_number, na.rm = TRUE), .groups = "drop")
  
  dnf_drivers <- driver_last_lap %>%
    filter(last_lap < race_last_lap - 1)
  
  dnf_tiles <- data.frame()
  if (nrow(dnf_drivers) > 0) {
    dnf_tiles <- purrr::pmap_dfr(dnf_drivers, function(driver, driver_label, last_lap) {
      remaining_laps <- seq(last_lap + 1, race_last_lap)
      if (length(remaining_laps) == 0) return(data.frame())
      data.frame(
        driver       = driver,
        driver_label = driver_label,
        lap_number   = remaining_laps,
        dnf_lap      = last_lap,
        compound     = "DNF",
        stringsAsFactors = FALSE
      )
    })
  }
  
  driver_order_df <- lap_data %>%
    group_by(driver, driver_label) %>%
    summarise(best_pos = min(position, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(best_pos))
  
  driver_order       <- driver_order_df$driver
  driver_label_order <- driver_order_df$driver_label
  
  if (driver_filter == "top10") {
    top10_drivers      <- driver_order_df %>% arrange(best_pos) %>% slice(1:10) %>% pull(driver)
    lap_data           <- lap_data %>% filter(driver %in% top10_drivers)
    pit_data           <- if (nrow(pit_data) > 0) pit_data %>% filter(toupper(driver_id) %in% toupper(top10_drivers)) else pit_data
    dnf_tiles          <- if (nrow(dnf_tiles) > 0) dnf_tiles %>% filter(driver %in% top10_drivers) else dnf_tiles
    driver_label_order <- driver_label_order[driver_order %in% top10_drivers]
    driver_order       <- driver_order[driver_order %in% top10_drivers]
  }
  
  lap_data <- lap_data %>%
    mutate(driver_label = factor(driver_label, levels = driver_label_order))
  
  if (nrow(dnf_tiles) > 0) {
    dnf_tiles <- dnf_tiles %>%
      mutate(driver_label = factor(driver_label, levels = driver_label_order))
  }
  
  sc_laps <- lap_data %>%
    filter(track_status %in% c("4", "6", "7")) %>%
    pull(lap_number) %>% unique() %>% sort()
  
  sc_rects <- data.frame()
  if (length(sc_laps) > 0) {
    groups   <- cumsum(c(1, diff(sc_laps) > 1))
    sc_rects <- data.frame(lap = sc_laps, group = groups) %>%
      group_by(group) %>%
      summarise(xmin = min(lap) - 0.5, xmax = max(lap) + 0.5, .groups = "drop")
  }
  
  max_lap <- max(lap_data$lap_number, na.rm = TRUE)
  
  racing_tiles <- lap_data %>%
    select(driver, driver_label, lap_number, compound, tyre_life, position) %>%
    mutate(dnf_lap = NA_real_, is_dnf = FALSE)
  
  dnf_plot_tiles <- if (nrow(dnf_tiles) > 0) {
    dnf_tiles %>%
      mutate(tyre_life = NA_real_, position = NA_real_, is_dnf = TRUE) %>%
      select(driver, driver_label, lap_number, compound, tyre_life, position, dnf_lap, is_dnf)
  } else {
    data.frame()
  }
  
  all_tiles <- if (nrow(dnf_plot_tiles) > 0) bind_rows(dnf_plot_tiles, racing_tiles) else racing_tiles
  
  all_tiles <- all_tiles %>%
    mutate(
      driver_label = factor(driver_label, levels = driver_label_order),
      hover_text   = ifelse(
        is_dnf,
        paste0("<b>", driver_label, "</b><br>",
               "\u26A0 DNF \u2014 retired on lap ", dnf_lap, "<br>",
               "Out of race from this point"),
        paste0("<b>", driver_label, "</b><br>",
               "Lap: ", lap_number, "<br>",
               "Compound: ", compound, "<br>",
               "Tyre Age: ", tyre_life, " laps<br>",
               "Position: ", position)
      )
    )
  
  p <- ggplot2::ggplot()
  
  if (nrow(sc_rects) > 0) {
    p <- p + ggplot2::annotate(
      "rect",
      xmin = sc_rects$xmin, xmax = sc_rects$xmax,
      ymin = -Inf, ymax = Inf,
      fill = "#aaaaaa", alpha = 0.12
    )
  }
  
  p <- p +
    ggplot2::geom_tile(
      data = all_tiles,
      ggplot2::aes(
        x      = lap_number,
        y      = driver_label,
        fill   = compound,
        colour = compound,
        text   = hover_text
      ),
      height = 0.85, width = 1, linewidth = 0.1
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "SOFT"    = "#DC0000", "MEDIUM"  = "#FFC906",
        "HARD"    = "#6b6b6b", "INTER"   = "#43b649",
        "WET"     = "#4f9deb", "UNKNOWN" = "#cccccc", "DNF" = "#f0f0f0"
      ),
      name   = "",
      labels = c(
        "SOFT" = "Soft", "MEDIUM" = "Medium", "HARD" = "Hard",
        "INTER" = "Inter", "WET" = "Wet", "UNKNOWN" = "Unknown", "DNF" = "DNF / Retired"
      ),
      breaks = c("SOFT", "MEDIUM", "HARD", "INTER", "WET", "UNKNOWN", "DNF")
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "SOFT" = "#ffffff", "MEDIUM" = "#ffffff", "HARD" = "#ffffff",
        "INTER" = "#ffffff", "WET" = "#ffffff", "UNKNOWN" = "#ffffff", "DNF" = "#bbbbbb"
      ),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      name         = "Lap Number",
      breaks       = seq(0, max_lap, by = 5),
      minor_breaks = seq(1, max_lap, by = 1),
      expand       = c(0.01, 0.01)
    ) +
    ggplot2::scale_y_discrete(limits = driver_label_order) +
    ggplot2::labs(y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.background   = ggplot2::element_rect(fill = "#fafafa", colour = NA),
      plot.background    = ggplot2::element_rect(fill = "#ffffff", colour = NA),
      panel.grid.major.x = ggplot2::element_line(colour = "#dddddd", linewidth = 0.5),
      panel.grid.minor.x = ggplot2::element_line(colour = "#eeeeee", linewidth = 0.2),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position    = "top",
      legend.text        = ggplot2::element_text(size = 10, face = "bold"),
      axis.text.y        = ggplot2::element_text(size = 9, colour = "#1a1a1a"),
      axis.text.x        = ggplot2::element_text(size = 9, face = "bold", colour = "#555555"),
      axis.title.x       = ggplot2::element_text(size = 11, face = "bold", colour = "#1a1a1a",
                                                 margin = ggplot2::margin(t = 10)),
      plot.margin        = ggplot2::margin(t = 10, r = 20, b = 10, l = 10)
    )
  
  if (nrow(pit_data) > 0) {
    pit_data <- pit_data %>%
      mutate(driver_upper = toupper(driver_id)) %>%
      filter(driver_upper %in% toupper(driver_order))
    
    if (nrow(pit_data) > 0) {
      p <- p + ggplot2::geom_vline(
        data = pit_data,
        ggplot2::aes(xintercept = as.numeric(lap)),
        colour = "#1a1a1a", linewidth = 0.5, alpha = 0.7, linetype = "dashed"
      )
    }
  }
  
  is_belgian_2021 <- !is.null(season) && !is.null(round) &&
    as.numeric(season) == 2021 && as.numeric(round) == 12
  
  n_drivers     <- length(driver_label_order)
  bold_labels   <- paste0("<b>", driver_label_order, "</b>    ")
  
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::layout(
      legend = list(
        orientation = "h",
        x           = 0,
        y           = 1.08,
        bgcolor     = "#ffffff",
        bordercolor = "#1a1a1a",
        borderwidth = 1,
        font        = list(size = 12, family = "Helvetica Neue")
      ),
      margin        = list(l = 180, r = 20, t = 40, b = 60),
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#fafafa",
      xaxis         = list(fixedrange = TRUE),
      yaxis = list(
        fixedrange = TRUE,
        tickmode   = "array",
        tickvals   = seq_len(n_drivers),
        ticktext   = bold_labels,
        tickfont   = list(size = 11, family = "Helvetica Neue", color = "#1a1a1a"),
        automargin = TRUE
      )
    ) %>%
    plotly::config(
      displayModeBar = FALSE,
      scrollZoom     = FALSE,
      doubleClick    = FALSE,
      showTips       = FALSE
    )
  
  if (is_belgian_2021) {
    fig <- fig %>%
      plotly::add_annotations(
        text        = "\u26A0 Belgian GP ended after 3 laps due to WET CONDITIONS",
        x           = 1, y = 1.01,
        xref        = "paper", yref = "paper",
        xanchor     = "right", yanchor = "bottom",
        showarrow   = FALSE,
        font        = list(size = 11, color = "#e10600", family = "Helvetica Neue"),
        bgcolor     = "#fff3cd",
        bordercolor = "#e10600",
        borderwidth = 1
      )
  }
  
  fig
}