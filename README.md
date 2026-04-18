# The Grand Prix Gambit

> The race is won in the pit lane. You just don't see it.

An R Shiny dashboard that pulls apart every pit stop from the 2021, 2022 and 2023 F1 seasons and asks the one question nobody bothers to ask after the chequered flag. Was that smart strategy or just good luck?

## Why we built it

Pit wall calls get three seconds of airtime and a line of commentary. Then the race moves on. Nobody goes back and checks if it was actually the right call.

We did. 64 races worth.

## What's inside

Five panels. No filler.

### Compound Strategy Map
Every driver. Every lap. Every tyre compound. All in one view.
Grey shading marks the safety cars. Dashed lines mark the stops. Hover for lap time, tyre age and track position.

### Undercut Detector
Finds every undercut attempt in a race and tells you whether it paid off. Also calls out the drivers who pull the trigger most often.

### Safety Car Lottery
A full season of SC and VSC deployments on one page. Who pitted. Who stayed out. Who came out ahead.

### Optimal vs Reality
Our original Strategy Score out of 100. Builds into a season long league table so you can see who's actually winning races with their head and who's winning them with their car.

### Score Explained
An interactive calculator. Slide the inputs. See what a 94 looks like. See what a 26 looks like. Spoiler, Sargeant isn't winning this one.

## The Strategy Score

Three components. 100 points total.

| Component | Points | What it asks |
|---|---|---|
| Stop Efficiency | 40 | Did they nail the minimum number of stops |
| SC Utilisation | 35 | Did they pit for free when the safety car came out |
| Tyre Strategy | 25 | Did they actually manage their compound allocation |

## Stack

Built in R. Everything Shiny. Static visuals in ggplot2, anything hoverable in plotly. Data comes from the `f1dataR` package, which wraps FastF1 so no API keys and no paywalls.

```
shiny · shinydashboard · shinycssloaders
ggplot2 · plotly
dplyr · tidyr
f1dataR · reticulate
```

## The data

Three seasons. 64 races. 32 variables per lap. Over 1,000 rows per race.

We picked 2021 to 2023 because tyre compound data is clean and consistent across all three. That's the foundation every panel is built on.

Race data is cached locally in `data/cache/` as RDS files so repeat loads don't hit the API every time.

## Running it

```bash
git clone https://github.com/nitinbehara09/The-Grand-Prix-Gambit.git
```

Open `grand-prix-gambit.Rproj` in RStudio. Then:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinycssloaders",
  "ggplot2", "plotly", "dplyr", "tidyr",
  "f1dataR", "reticulate"
))

shiny::runApp("app.R")
```

You'll also need Python with the `fastf1` package. reticulate handles the bridge.

## Project structure

```
The-Grand-Prix-Gambit/
├── app.R                     # UI + server
├── R/
│   ├── data_pipeline.R       # Fetching, caching, lookups
│   ├── strategy_map.R        # Panel 1
│   ├── undercut_detector.R   # Panel 2
│   ├── safety_car.R          # Panel 3
│   └── strategy_score.R      # Panel 4
├── data/cache/               # Local RDS cache
└── grand-prix-gambit.Rproj
```

## Context

Final project for DATA5002 Data Visualisation at UNSW Sydney, Term 1 2026. Supervised by Pierre Lafaye de Micheaux.

## Team

**Data Sentinels**
Nitin Behara · z5715590
Akshath Velekkattu Rajeev · z5704763
