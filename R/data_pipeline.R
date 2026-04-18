# data_pipeline.R
# Centralised lookup tables, data fetching, and caching

library(f1dataR)
library(dplyr)

# ── Python / FastF1 setup ────────────────────────────────────────────────────
# FIX: use reticulate::py_config() instead of system("which python3") which
#      fails on Windows and RStudio Server
tryCatch({
  python_path <- reticulate::py_config()$python
  if (!is.null(python_path) && nzchar(python_path)) {
    Sys.setenv(RETICULATE_PYTHON = python_path)
  }
  cache_path <- path.expand("~/Library/Caches/fastf1")
  dir.create(cache_path, recursive = TRUE, showWarnings = FALSE)
  reticulate::py_run_string(
    sprintf("import fastf1; fastf1.Cache.enable_cache('%s')", cache_path)
  )
}, error = function(e) NULL)

# Create local RDS cache folder
dir.create("data/cache", recursive = TRUE, showWarnings = FALSE)

# ── CENTRALISED LOOKUP TABLES ────────────────────────────────────────────────
# FIX: All lookup tables live here once. Removed duplicate definitions from
#      strategy_map.R, safety_car.R, strategy_score.R, undercut_detector.R.

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

# Single source of truth for driver ID normalisation (last name & full name → 3-letter code)
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
  # Full name format (used in 2021/2022 pit data)
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
  # Normalize column name variations across f1dataR versions
  if (!"race_name" %in% names(sched)) {
    for (alt in c("raceName", "name", "race")) {
      if (alt %in% names(sched)) { sched <- dplyr::rename(sched, race_name = !!dplyr::sym(alt)); break }
    }
  }
  # Filter: proper race rounds only — removes pre-season tests (round NA / 0)
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

# ── Helper: normalise pit stop driver IDs ─────────────────────────────────────
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
  # FIX: wrap fetch in tryCatch — FastF1 API timeouts or uncached races
  # can throw errors that previously propagated all the way to the stat boxes.
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