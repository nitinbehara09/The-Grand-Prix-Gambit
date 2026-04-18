# data_pipeline.R
# Centralised lookup tables, data fetching, and caching

library(f1dataR)
library(dplyr)

# ── Python / FastF1 setup ────────────────────────────────────────────────────
# Only runs locally. On shinyapps.io we skip this because we only read cached
# RDS files anyway, and Python setup is unreliable on the server.
is_shinyapps <- nzchar(Sys.getenv("SHINY_SERVER_VERSION")) ||
  grepl("shinyapps", Sys.getenv("R_CONFIG_ACTIVE"), ignore.case = TRUE)

if (!is_shinyapps) {
  tryCatch({
    if (requireNamespace("reticulate", quietly = TRUE)) {
      python_path <- reticulate::py_config()$python
      if (!is.null(python_path) && nzchar(python_path)) {
        Sys.setenv(RETICULATE_PYTHON = python_path)
      }
      cache_path <- path.expand("~/Library/Caches/fastf1")
      dir.create(cache_path, recursive = TRUE, showWarnings = FALSE)
      reticulate::py_run_string(
        sprintf("import fastf1; fastf1.Cache.enable_cache('%s')", cache_path)
      )
    }
  }, error = function(e) NULL)
}

# Create local RDS cache folder
dir.create("data/cache", recursive = TRUE, showWarnings = FALSE)

# ── CENTRALISED LOOKUP TABLES ────────────────────────────────────────────────
DRIVER_NAMES <- c(
  "VER" = "Max Verstappen",    "HAM" = "Lewis Hamilton",
  "LEC" = "Charles Leclerc",   "SAI" = "Carlos Sainz",
  "PER" = "Sergio Perez",      "RUS" = "George Russell",
  "NOR" = "Lando Norris",      "ALO" = "Fernando Alonso",
  "STR" = "Lance Stroll",      "OCO" = "Esteban Ocon",
  "GAS" = "Pierre Gasly",      "BOT" = "Valtteri Bottas",
  "RIC" = "Daniel Ricciardo",  "TSU" = "Yuki Tsunoda",
  "MAG" = "Kevin Magnussen",   "HUL" = "Nico Hulkenberg",
  "ALB" = "Alexander Albon",   "ZHO" = "Guanyu Zhou",
  "SAR" = "Logan Sargeant",    "PIA" = "Oscar Piastri",
  "MSC" = "Mick Schumacher",   "LAT" = "Nicholas Latifi",
  "GIO" = "Antonio Giovinazzi","MAZ" = "Nikita Mazepin",
  "DEV" = "Nyck de Vries",     "LAW" = "Liam Lawson",
  "BEA" = "Oliver Bearman",    "COL" = "Franco Colapinto",
  "VET" = "Sebastian Vettel",  "RAI" = "Kimi Raikkonen",
  "GRO" = "Romain Grosjean",   "KVY" = "Daniil Kvyat",
  "ERI" = "Marcus Ericsson",   "HAR" = "Brendon Hartley",
  "VAN" = "Stoffel Vandoorne", "WEH" = "Pascal Wehrlein",
  "KUB" = "Robert Kubica",     "FIT" = "Pietro Fittipaldi",
  "AIT" = "Jack Aitken",       "HAD" = "Isack Hadjar",
  "ANT" = "Kimi Antonelli",    "DOO" = "Jack Doohan",
  "BOR" = "Gabriel Bortoleto", "SHW" = "Robert Shwartzman",
  "BUT" = "Jenson Button"
)

LOCAL_DRIVER_IDS <- c(
  "verstappen" = "VER", "hamilton" = "HAM", "leclerc" = "LEC",
  "sainz" = "SAI", "perez" = "PER", "russell" = "RUS",
  "norris" = "NOR", "alonso" = "ALO", "stroll" = "STR",
  "ocon" = "OCO", "gasly" = "GAS", "bottas" = "BOT",
  "ricciardo" = "RIC", "tsunoda" = "TSU", "magnussen" = "MAG",
  "hulkenberg" = "HUL", "albon" = "ALB", "zhou" = "ZHO",
  "sargeant" = "SAR", "piastri" = "PIA", "schumacher" = "MSC",
  "latifi" = "LAT", "giovinazzi" = "GIO", "mazepin" = "MAZ",
  "de_vries" = "DEV", "lawson" = "LAW", "bearman" = "BEA",
  "colapinto" = "COL", "vettel" = "VET", "raikkonen" = "RAI",
  "grosjean" = "GRO", "kvyat" = "KVY", "ericsson" = "ERI",
  "hartley" = "HAR", "vandoorne" = "VAN", "wehrlein" = "WEH",
  "kubica" = "KUB", "fittipaldi" = "FIT", "aitken" = "AIT",
  "hadjar" = "HAD", "antonelli" = "ANT", "doohan" = "DOO",
  "bortoleto" = "BOR", "shwartzman" = "SHW",
  "max_verstappen" = "VER", "lewis_hamilton" = "HAM",
  "charles_leclerc" = "LEC", "carlos_sainz" = "SAI",
  "sergio_perez" = "PER", "george_russell" = "RUS",
  "lando_norris" = "NOR", "fernando_alonso" = "ALO",
  "lance_stroll" = "STR", "esteban_ocon" = "OCO",
  "pierre_gasly" = "GAS", "valtteri_bottas" = "BOT",
  "daniel_ricciardo" = "RIC", "yuki_tsunoda" = "TSU",
  "kevin_magnussen" = "MAG", "nico_hulkenberg" = "HUL",
  "alexander_albon" = "ALB", "guanyu_zhou" = "ZHO",
  "logan_sargeant" = "SAR", "oscar_piastri" = "PIA",
  "mick_schumacher" = "MSC", "nicholas_latifi" = "LAT",
  "antonio_giovinazzi" = "GIO", "nikita_mazepin" = "MAZ",
  "nyck_de_vries" = "DEV", "liam_lawson" = "LAW",
  "oliver_bearman" = "BEA", "franco_colapinto" = "COL",
  "sebastian_vettel" = "VET", "kimi_raikkonen" = "RAI",
  "romain_grosjean" = "GRO", "daniil_kvyat" = "KVY",
  "marcus_ericsson" = "ERI", "brendon_hartley" = "HAR",
  "stoffel_vandoorne" = "VAN", "pascal_wehrlein" = "WEH",
  "robert_kubica" = "KUB", "pietro_fittipaldi" = "FIT",
  "jack_aitken" = "AIT", "isack_hadjar" = "HAD",
  "kimi_antonelli" = "ANT", "jack_doohan" = "DOO",
  "gabriel_bortoleto" = "BOR"
)

# ── Data fetching ─────────────────────────────────────────────────────────────
get_schedule <- function(season) {
  sched <- load_schedule(season = season)
  if (!"race_name" %in% names(sched)) {
    for (alt in c("raceName", "name", "race")) {
      if (alt %in% names(sched)) { sched <- dplyr::rename(sched, race_name = !!dplyr::sym(alt)); break }
    }
  }
  sched %>%
    dplyr::filter(!is.na(round), as.integer(round) > 0,
                  !is.na(race_name), nzchar(as.character(race_name))) %>%
    dplyr::arrange(as.integer(round))
}

get_lap_data <- function(season, round) {
  load_session_laps(
    season    = as.numeric(season),
    round     = as.numeric(round),
    session   = "R",
    log_level = "WARNING"
  )
}

get_pitstop_data <- function(season, round) {
  load_pitstops(
    season = as.numeric(season),
    round  = as.numeric(round)
  )
}

get_results <- function(season, round) {
  load_results(
    season = as.numeric(season),
    round  = as.numeric(round)
  )
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

# ── Cached versions (saves to disk after first load) ─────────────────────────
get_lap_data_cached <- function(season, round) {
  cache_path <- paste0("data/cache/laps_", season, "_", round, ".rds")
  if (file.exists(cache_path)) {
    tryCatch(return(readRDS(cache_path)), error = function(e) NULL)
  }
  laps <- tryCatch(get_lap_data(season, round), error = function(e) {
    message("lap data unavailable for season=", season, " round=", round, ": ", e$message)
    NULL
  })
  if (!is.null(laps) && nrow(laps) > 0) tryCatch(saveRDS(laps, cache_path), error = function(e) NULL)
  laps
}

get_pitstop_data_cached <- function(season, round) {
  cache_path <- paste0("data/cache/pits_", season, "_", round, ".rds")
  if (file.exists(cache_path)) {
    tryCatch(return(readRDS(cache_path)), error = function(e) NULL)
  }
  pits <- tryCatch(get_pitstop_data(season, round), error = function(e) {
    message("pit data unavailable for season=", season, " round=", round, ": ", e$message)
    NULL
  })
  if (!is.null(pits) && nrow(pits) > 0) tryCatch(saveRDS(pits, cache_path), error = function(e) NULL)
  pits
}

get_sc_cached <- function(season, round, race_name) {
  cache_path <- paste0("data/cache/sc_", season, "_", round, ".rds")
  if (file.exists(cache_path)) return(readRDS(cache_path))
  laps <- get_lap_data_cached(season, round)
  if (is.null(laps) || nrow(laps) == 0) return(data.frame())
  sc   <- detect_safety_cars(laps, season, round, race_name)
  saveRDS(sc, cache_path)
  sc
}

get_sc_pits_cached <- function(season, round, race_name) {
  cache_path <- paste0("data/cache/sc_pits_", season, "_", round, ".rds")
  if (file.exists(cache_path)) return(readRDS(cache_path))
  laps     <- get_lap_data_cached(season, round)
  pits     <- get_pitstop_data_cached(season, round)
  sc       <- get_sc_cached(season, round, race_name)
  analysis <- if (!is.null(sc) && nrow(sc) > 0) sc_pit_analysis(laps, pits, sc) else data.frame()
  saveRDS(analysis, cache_path)
  analysis
}

# ── Cache builder helper ─────────────────────────────────────────────────────
# Run this from the console to fetch and cache any missing races
# Example: build_cache(2021, c(11, 13, 14, 15, 16, 17, 18, 19, 20, 22))
build_cache <- function(season, rounds) {
  sched <- tryCatch(get_schedule(season), error = function(e) NULL)
  for (rnd in rounds) {
    cat("Fetching", season, "Round", rnd, "... ")
    race_name <- if (!is.null(sched)) {
      as.character(sched$race_name[as.integer(sched$round) == rnd])[1]
    } else paste("Round", rnd)
    tryCatch({
      get_lap_data_cached(season, rnd)
      get_pitstop_data_cached(season, rnd)
      get_sc_cached(season, rnd, race_name)
      get_sc_pits_cached(season, rnd, race_name)
      cat("done\n")
    }, error = function(e) cat("FAILED:", e$message, "\n"))
  }
  invisible(NULL)
}