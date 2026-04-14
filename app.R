# app.R
# The Grand Prix Gambit — DATA SENTINELS
# Nitin Behara (z5715590) & Akshath Velekkattu Rajeev (z5704763)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(shinycssloaders)

source("R/data_pipeline.R")
source("R/strategy_map.R")
source("R/undercut_detector.R")
source("R/safety_car.R")
source("R/strategy_score.R")

ui <- navbarPage(
  title       = "The Grand Prix Gambit",
  id          = "nav",
  collapsible = TRUE,
  
  header = tags$head(tags$style(HTML("
    body { background-color: #f5f5f5; font-family: 'Helvetica Neue', sans-serif; }
    .navbar { background-color: #1a1a1a !important; border: none; }
    .navbar .navbar-brand { color: #ffffff !important; font-weight: 700; }
    .navbar .navbar-nav > li > a { color: #cccccc !important; font-size: 12px; letter-spacing: 1px; text-transform: uppercase; }
    .navbar .navbar-nav > .active > a { color: #ffffff !important; border-bottom: 2px solid #e10600; background: transparent !important; }
    .stat-box { background: #fff; border: 1px solid #e0e0e0; padding: 20px; text-align: center; margin-bottom: 15px; min-height: 110px; display: flex; flex-direction: column; align-items: center; justify-content: center; }
    .stat-box .stat-number { font-size: 28px; font-weight: 700; color: #1a1a1a; line-height: 1.2; }
    .stat-box .stat-label { font-size: 11px; font-weight: 700; color: #999; letter-spacing: 1.5px; text-transform: uppercase; margin-top: 6px; }
    .panel-label { font-size: 11px; color: #999; letter-spacing: 2px; text-transform: uppercase; margin-bottom: 5px; }
    .panel-title { font-size: 22px; font-weight: 700; color: #1a1a1a; margin-bottom: 3px; }
    .panel-subtitle { font-size: 13px; color: #666; margin-bottom: 20px; }
    .control-panel { background: #fff; border: 1px solid #1a1a1a; padding: 20px; height: 100%; }
    .control-panel h5 { font-size: 11px; font-weight: 700; letter-spacing: 1.5px; text-transform: uppercase; color: #1a1a1a; margin-bottom: 15px; }
    .control-panel label { font-size: 11px; font-weight: 700; letter-spacing: 1px; text-transform: uppercase; color: #555; }
    .plot-panel { background: #fff; border: 1px solid #1a1a1a; padding: 15px; }
    .sc-row > .row { display: flex; align-items: stretch; flex-wrap: wrap; }
    .sc-row .col-sm-5 { display: flex; flex-direction: column; }
    .sc-row .col-sm-5 .plot-panel { flex: 1; }
    .home-header { background: #1a1a1a; border: none; padding: 45px 40px 65px 40px; margin-bottom: 20px; }
    .home-header h2 { font-size: 52px; font-weight: 700; margin: 0 0 8px 0; color: #ffffff; letter-spacing: -1px; padding-top: 0; }
    .home-header p { color: #aaaaaa; margin: 0; font-size: 14px; }
    .home-header .red-line { width: 60px; height: 4px; background: #e10600; margin: 16px 0; }
    .preview-box { background: #fff; border: 1px solid #e0e0e0; padding: 24px; margin-bottom: 15px; min-height: 220px; transition: border-color 0.2s; }
    .preview-box:hover { border-color: #e10600; }
    .preview-box h6 { font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: #999; margin-bottom: 8px; }
    .preview-box .preview-title { font-size: 16px; font-weight: 700; color: #1a1a1a; margin-bottom: 8px; }
    .preview-box .preview-desc { font-size: 13px; color: #555; line-height: 1.6; margin-bottom: 12px; }
    .preview-box .preview-tags { display: flex; flex-wrap: wrap; gap: 6px; margin-bottom: 14px; }
    .preview-tag { font-size: 10px; font-weight: 700; letter-spacing: 1px; text-transform: uppercase; padding: 3px 8px; border: 1px solid #e0e0e0; color: #888; }
    .preview-box .preview-btn { display: inline-block; font-size: 10px; font-weight: 700; letter-spacing: 1.5px; text-transform: uppercase; background: #e10600; color: #fff; padding: 7px 16px; border: none; cursor: pointer; margin-top: 4px; }
    .preview-box .preview-btn:hover { background: #c00; }
    .insight-box { background: #fff; border-left: 3px solid #e10600; padding: 14px 18px; }
    .insight-box .insight-label { font-size: 10px; font-weight: 700; letter-spacing: 1.5px; text-transform: uppercase; color: #999; margin-bottom: 4px; }
    .insight-box .insight-value { font-size: 15px; font-weight: 700; color: #1a1a1a; }
    .insight-box .insight-sub { font-size: 12px; color: #777; margin-top: 2px; }
    hr.f1-divider { border: none; border-top: 2px solid #e10600; margin: 10px 0 20px 0; width: 40px; }
    .footer-ids { text-align: right; font-size: 11px; color: #bbb; padding: 10px 0; }
    select, .form-control { border-radius: 0 !important; border: 1px solid #ccc !important; font-size: 13px !important; }
    .component-card { background: #fff; border: 1px solid #e0e0e0; border-left: 4px solid #e10600; padding: 20px; margin-bottom: 15px; }
  "))),
  
  # ── HOME ──────────────────────────────────────────────────────────────────
  tabPanel("Home",
           div(
             div(class = "home-header",
                 h2("The Grand Prix Gambit"),
                 div(class = "red-line"),
                 p("Reverse-engineering F1 pit stop strategy across the 2021 \u2014 2023 seasons"),
                 p(style = "color:#777; font-size:12px; margin-top:6px;",
                   "DATA SENTINELS \u00b7 Nitin Behara & Akshath Velekkattu Rajeev \u00b7 DATA5002 \u00b7 UNSW")
             ),
             div(style = "padding: 0 20px;",
                 fluidRow(
                   column(2, div(class="stat-box", div(class="stat-number","64"),      div(class="stat-label","Races Analyzed"))),
                   column(2, div(class="stat-box", div(class="stat-number","2.4"),     div(class="stat-label","Avg Stops / Race"))),
                   column(2, div(class="stat-box", div(class="stat-number","72.4"),    div(class="stat-label","Avg Strategy Score"))),
                   column(2, div(class="stat-box", div(class="stat-number","10"),      div(class="stat-label","Teams Covered"))),
                   column(2, div(class="stat-box", div(class="stat-number","3"),       div(class="stat-label","Seasons"))),
                   column(2, div(class="stat-box", div(class="stat-number","2021-23"), div(class="stat-label","Era")))
                 ),
                 
                 tags$div(style = "margin-bottom: 15px;",
                          tags$div(id = "insight-carousel",
                                   style = "position: relative; overflow: hidden; height: 80px;",
                                   tags$div(class = "insight-slide",
                                            style = "display:flex; gap:15px; position:absolute; width:100%; transition:opacity 0.8s ease;",
                                            tags$div(class="insight-box", style="flex:1;",
                                                     div(class="insight-label","Undercut King - 2021"),
                                                     div(class="insight-value","Max Verstappen"),
                                                     div(class="insight-sub","Most positions gained via undercut across the season")
                                            ),
                                            tags$div(class="insight-box", style="flex:1;",
                                                     div(class="insight-label","SC Master - 2021"),
                                                     div(class="insight-value","Lewis Hamilton"),
                                                     div(class="insight-sub","Pitted under every SC deployment at Abu Dhabi")
                                            ),
                                            tags$div(class="insight-box", style="flex:1;",
                                                     div(class="insight-label","Strategy Disaster - 2021"),
                                                     div(class="insight-value","Ferrari"),
                                                     div(class="insight-sub","Lowest average strategy score across the season")
                                            )
                                   ),
                                   tags$div(class = "insight-slide",
                                            style = "display:flex; gap:15px; position:absolute; width:100%; transition:opacity 0.8s ease; opacity:0;",
                                            tags$div(class="insight-box", style="flex:1;",
                                                     div(class="insight-label","Undercut King - 2022"),
                                                     div(class="insight-value","Carlos Sainz"),
                                                     div(class="insight-sub","Highest undercut success rate across the season")
                                            ),
                                            tags$div(class="insight-box", style="flex:1;",
                                                     div(class="insight-label","SC Master - 2022"),
                                                     div(class="insight-value","Max Verstappen"),
                                                     div(class="insight-sub","Capitalised on every safety car window in 2022")
                                            ),
                                            tags$div(class="insight-box", style="flex:1;",
                                                     div(class="insight-label","Strategy Disaster - 2022"),
                                                     div(class="insight-value","Ferrari"),
                                                     div(class="insight-sub","Lowest avg strategy score across the season")
                                            )
                                   ),
                                   tags$div(class = "insight-slide",
                                            style = "display:flex; gap:15px; position:absolute; width:100%; transition:opacity 0.8s ease; opacity:0;",
                                            tags$div(class="insight-box", style="flex:1;",
                                                     div(class="insight-label","Most Undercuts - 2023"),
                                                     div(class="insight-value","Pierre Gasly"),
                                                     div(class="insight-sub","3/3 attempts succeeded at Bahrain")
                                            ),
                                            tags$div(class="insight-box", style="flex:1;",
                                                     div(class="insight-label","SC Master - 2023"),
                                                     div(class="insight-value","Fernando Alonso"),
                                                     div(class="insight-sub","Net +6 across all SC deployments")
                                            ),
                                            tags$div(class="insight-box", style="flex:1;",
                                                     div(class="insight-label","Strategy Disaster - 2023"),
                                                     div(class="insight-value","Alpine"),
                                                     div(class="insight-sub","Most erratic pit strategy across the season")
                                            )
                                   )
                          ),
                          tags$div(style = "text-align:center; margin-top:10px;",
                                   tags$span(id="dot-0", onclick="manualSlide(0)",
                                             style="display:inline-block;width:8px;height:8px;border-radius:50%;background:#e10600;margin:0 4px;cursor:pointer;"),
                                   tags$span(id="dot-1", onclick="manualSlide(1)",
                                             style="display:inline-block;width:8px;height:8px;border-radius:50%;background:#ccc;margin:0 4px;cursor:pointer;"),
                                   tags$span(id="dot-2", onclick="manualSlide(2)",
                                             style="display:inline-block;width:8px;height:8px;border-radius:50%;background:#ccc;margin:0 4px;cursor:pointer;")
                          ),
                          tags$script(HTML("
            (function() {
              var slides = document.querySelectorAll('.insight-slide');
              var dots = [document.getElementById('dot-0'),
                          document.getElementById('dot-1'),
                          document.getElementById('dot-2')];
              var current = 0, timer;
              function showSlide(n) {
                slides[current].style.opacity = '0';
                dots[current].style.background = '#ccc';
                current = n;
                slides[current].style.opacity = '1';
                dots[current].style.background = '#e10600';
              }
              function startTimer() {
                timer = setInterval(function() { showSlide((current + 1) % slides.length); }, 2500);
              }
              window.manualSlide = function(n) { clearInterval(timer); showSlide(n); startTimer(); };
              startTimer();
            })();
          "))
                 ),
                 
                 fluidRow(
                   column(8,
                          div(class="preview-box",
                              h6("Panel 1"),
                              div(class="preview-title","Compound Strategy Map"),
                              div(class="preview-desc",
                                  "A lap-by-lap tyre compound timeline for every driver in a selected race. ",
                                  "Coloured tiles show compound per stint, dashed lines mark pit stops, and grey shading highlights safety car periods."
                              ),
                              div(class="preview-tags",
                                  div(class="preview-tag","Lap Data"),
                                  div(class="preview-tag","Tyre Compounds"),
                                  div(class="preview-tag","Pit Windows"),
                                  div(class="preview-tag","Safety Car")
                              ),
                              tags$button(class="preview-btn",
                                          onclick="document.querySelector(\"[data-value='Compound Map']\").click();",
                                          "Explore Compound Map \u2192")
                          )
                   ),
                   column(4,
                          div(class="preview-box",
                              h6("Panel 2"),
                              div(class="preview-title","Undercut Detector"),
                              div(class="preview-desc",
                                  "Detects every undercut attempt in a race who pitted early, whether it worked, and which drivers are serial undercut operators."
                              ),
                              div(class="preview-tags",
                                  div(class="preview-tag","Pit Timing"),
                                  div(class="preview-tag","Position Changes"),
                                  div(class="preview-tag","Success Rate")
                              ),
                              tags$button(class="preview-btn",
                                          onclick="document.querySelector(\"[data-value='Undercut Detector']\").click();",
                                          "Detect Undercuts \u2192")
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(4,
                          div(class="preview-box",
                              h6("Panel 3"),
                              div(class="preview-title","Safety Car Lottery"),
                              div(class="preview-desc",
                                  "Maps every SC and VSC deployment across a full season. Shows who pitted under each window and their net position gain/loss."
                              ),
                              div(class="preview-tags",
                                  div(class="preview-tag","SC Timeline"),
                                  div(class="preview-tag","VSC Events"),
                                  div(class="preview-tag","Net Winners")
                              ),
                              tags$button(class="preview-btn",
                                          onclick="document.querySelector(\"[data-value='Safety Car Lottery']\").click();",
                                          "See SC Impact \u2192")
                          )
                   ),
                   column(4,
                          div(class="preview-box",
                              h6("Panel 4"),
                              div(class="preview-title","Optimal vs Reality"),
                              div(class="preview-desc",
                                  "Scores every driver\u2019s strategy on stop efficiency, SC utilisation, and tyre management. Builds a season-long league table."
                              ),
                              div(class="preview-tags",
                                  div(class="preview-tag","Strategy Score"),
                                  div(class="preview-tag","Stop Efficiency"),
                                  div(class="preview-tag","League Table")
                              ),
                              tags$button(class="preview-btn",
                                          onclick="document.querySelector(\"[data-value='Strategy Score']\").click();",
                                          "Score Strategies \u2192")
                          )
                   ),
                   column(4,
                          div(class="preview-box",
                              h6("About This Project"),
                              div(class="preview-title","How We Built It"),
                              div(class="preview-desc",
                                  "All data sourced live from FastF1 via f1dataR. Built in R Shiny with ggplot2 and plotly. ",
                                  "Strategy Score is an original metric combining three independently measurable components."
                              ),
                              div(class="preview-tags",
                                  div(class="preview-tag","R Shiny"),
                                  div(class="preview-tag","FastF1"),
                                  div(class="preview-tag","f1dataR"),
                                  div(class="preview-tag","plotly")
                              )
                          )
                   )
                 ),
                 
                 div(class="footer-ids", "DATA SENTINELS \u00b7 z5715590 \u00b7 z5704763")
             )
           )
  ),
  
  # ── PANEL 1: COMPOUND MAP ─────────────────────────────────────────────────
  tabPanel("Compound Map",
           div(style = "padding: 20px;",
               div(class="panel-label","Panel 1 \u2014 What Happened on Track"),
               div(class="panel-title","Compound Strategy Map"),
               div(class="panel-subtitle",
                   tags$span(style="background:#e10600;color:#fff;padding:2px 8px;font-size:11px;margin-right:6px;","SOFT"),
                   tags$span(style="background:#ffd700;color:#111;padding:2px 8px;font-size:11px;margin-right:6px;","MEDIUM"),
                   tags$span(style="background:#e0e0e0;color:#111;padding:2px 8px;font-size:11px;margin-right:6px;","HARD"),
                   tags$span(style="color:#999;font-size:12px;float:right;","Grey shading = safety car")
               ),
               fluidRow(
                 column(3,
                        div(class="control-panel",
                            h5("Controls"),
                            selectInput("sm_season","Season", choices=c("2021","2022","2023"), selected="2023"),
                            uiOutput("sm_race_selector"),
                            radioButtons("sm_drivers","Show Drivers",
                                         choices=c("All"="all","Top 10"="top10"), selected="all")
                        )
                 ),
                 column(9,
                        div(class="plot-panel",
                            h6(style="font-size:13px;font-weight:700;letter-spacing:1.5px;text-transform:uppercase;color:#555;","Compound Timeline"),
                            hr(class="f1-divider"),
                            withSpinner(plotlyOutput("strategy_map_plot", height="500px"),
                                        type=4, color="#e10600", size=0.7)
                        )
                 )
               ),
               div(class="footer-ids","DATA SENTINELS \u00b7 z5715590 \u00b7 z5704763")
           )
  ),
  
  # ── PANEL 2: UNDERCUT DETECTOR ────────────────────────────────────────────
  tabPanel("Undercut Detector",
           div(style = "padding: 20px;",
               div(class="panel-label","Panel 2 \u2014 The Weapon Explained"),
               div(class="panel-title","Undercut Detector"),
               fluidRow(
                 column(2,
                        div(class="control-panel",
                            h5("Filters"),
                            selectInput("ud_season","Season", choices=c("2021","2022","2023"), selected="2023"),
                            uiOutput("ud_race_selector"),
                            radioButtons("ud_result","Result",
                                         choices=c("All"="all","Success"="success","Failed"="failed"),
                                         selected="all")
                        )
                 ),
                 column(5,
                        div(class="plot-panel",
                            h6(style="font-size:11px;font-weight:700;letter-spacing:1.5px;text-transform:uppercase;color:#555;","Undercut Attempts Table"),
                            hr(class="f1-divider"),
                            withSpinner(plotlyOutput("undercut_table_plot", height="500px"),
                                        type=4, color="#e10600", size=0.7)
                        )
                 ),
                 column(5,
                        div(class="plot-panel",
                            h6(style="font-size:11px;font-weight:700;letter-spacing:1.5px;text-transform:uppercase;color:#555;","Success Rate by Driver"),
                            hr(class="f1-divider"),
                            withSpinner(plotlyOutput("undercut_bar_plot", height="500px"),
                                        type=4, color="#e10600", size=0.7)
                        )
                 )
               ),
               div(class="footer-ids","DATA SENTINELS \u00b7 z5715590 \u00b7 z5704763")
           )
  ),
  
  # ── PANEL 3: SAFETY CAR ───────────────────────────────────────────────────
  tabPanel("Safety Car Lottery",
           div(style = "padding: 20px;",
               div(class="panel-label","Panel 3 \u2014 Chaos and Who Wins From It"),
               div(class="panel-title","Safety Car Lottery"),
               div(class = "sc-row",
                   fluidRow(
                     column(2,
                            div(class="control-panel",
                                h5("Controls"),
                                selectInput("sc_season","Season", choices=c("2021","2022","2023"), selected="2023"),
                                radioButtons("sc_type","SC Type",
                                             choices=c("All"="all","Safety Car"="sc","Virtual SC"="vsc"),
                                             selected="all")
                            )
                     ),
                     column(5,
                            div(class="plot-panel",
                                h6(style="font-size:11px;font-weight:700;letter-spacing:1.5px;text-transform:uppercase;color:#1a1a1a;","Safety Car Timeline"),
                                hr(class="f1-divider"),
                                div(style="display:inline-flex;align-items:center;gap:16px;border:1px solid #1a1a1a;padding:5px 12px;background:#fff;margin-bottom:10px;font-size:12px;font-family:'Helvetica Neue',sans-serif;",
                                    tags$span(style="font-weight:700;margin-right:4px;","Type"),
                                    tags$span(
                                      tags$span(style="display:inline-block;width:12px;height:12px;border-radius:50%;background:#e10600;vertical-align:middle;margin-right:4px;"),
                                      tags$span(style="vertical-align:middle;font-size:12px;","SC")
                                    ),
                                    tags$span(
                                      tags$span(style="display:inline-block;width:12px;height:12px;border-radius:50%;background:#FFC906;vertical-align:middle;margin-right:4px;"),
                                      tags$span(style="vertical-align:middle;font-size:12px;","VSC")
                                    )
                                ),
                                withSpinner(plotlyOutput("safety_car_plot", height="500px"),
                                            type=4, color="#e10600", size=0.7)
                            )
                     ),
                     column(5,
                            div(class="plot-panel",
                                h6(style="font-size:11px;font-weight:700;letter-spacing:1.5px;text-transform:uppercase;color:#1a1a1a;","Net Winners / Losers"),
                                hr(class="f1-divider"),
                                withSpinner(plotlyOutput("safety_car_table_plot", height="500px"),
                                            type=4, color="#e10600", size=0.7)
                            )
                     )
                   )
               ),
               div(class="footer-ids","DATA SENTINELS \u00b7 z5715590 \u00b7 z5704763")
           )
  ),
  
  # ── PANEL 4: STRATEGY SCORE ───────────────────────────────────────────────
  tabPanel("Strategy Score",
           div(style = "padding: 20px;",
               div(class="panel-label","Panel 4 \u2014 The Centrepiece"),
               div(class="panel-title","Strategy Score"),
               fluidRow(
                 column(4, uiOutput("ss_box_avg_score")),
                 column(4, uiOutput("ss_box_best")),
                 column(4, uiOutput("ss_box_worst"))
               ),
               fluidRow(
                 column(2,
                        div(class="control-panel",
                            h5("Controls"),
                            selectInput("ss_season","Season", choices=c("2021","2022","2023"), selected="2023"),
                            uiOutput("ss_race_selector")
                        )
                 ),
                 column(5,
                        div(class="plot-panel",
                            h6(style="font-size:11px;font-weight:700;letter-spacing:1.5px;text-transform:uppercase;color:#555;","Driver Strategy Score"),
                            hr(class="f1-divider"),
                            uiOutput("ss_data_note"),
                            withSpinner(plotlyOutput("strategy_score_plot", height="500px"),
                                        type=4, color="#e10600", size=0.7)
                        )
                 ),
                 column(5,
                        div(class="plot-panel",
                            h6(style="font-size:11px;font-weight:700;letter-spacing:1.5px;text-transform:uppercase;color:#555;","Strategy Score League Table"),
                            hr(class="f1-divider"),
                            withSpinner(plotlyOutput("strategy_league_plot", height="500px"),
                                        type=4, color="#e10600", size=0.7)
                        )
                 )
               ),
               div(class="footer-ids","DATA SENTINELS \u00b7 z5715590 \u00b7 z5704763")
           )
  ),
  
  # ── PANEL 5: SCORE EXPLAINED ─────────────────────────────────────────────
  tabPanel("Score Explained",
           div(style = "padding: 20px;",
               div(class="panel-label","Panel 5 \u2014 Behind the Number"),
               div(class="panel-title","How the Strategy Score Works"),
               div(style="font-size:13px; color:#666; margin-bottom:20px;",
                   "The Strategy Score is an original metric built from three independently measurable components. ",
                   "Each race, every driver is scored out of 100 based on the decisions their team made and not the result."
               ),
               
               # Component cards
               fluidRow(
                 column(4,
                        div(class="component-card",
                            div(style="font-size:10px;font-weight:700;letter-spacing:2px;text-transform:uppercase;color:#999;margin-bottom:6px;","Component 1"),
                            div(style="font-size:20px;font-weight:700;color:#1a1a1a;margin-bottom:6px;","Stop Efficiency"),
                            div(style="font-size:32px;font-weight:700;color:#e10600;margin-bottom:8px;","40 pts"),
                            div(style="font-size:13px;color:#555;line-height:1.6;",
                                "Full marks if you matched the minimum stops in the race. ",
                                "Lose 10 points for every extra stop made. Reflects how clean and decisive the pit strategy was."
                            )
                        )
                 ),
                 column(4,
                        div(class="component-card",
                            div(style="font-size:10px;font-weight:700;letter-spacing:2px;text-transform:uppercase;color:#999;margin-bottom:6px;","Component 2"),
                            div(style="font-size:20px;font-weight:700;color:#1a1a1a;margin-bottom:6px;","SC Utilisation"),
                            div(style="font-size:32px;font-weight:700;color:#e10600;margin-bottom:8px;","35 pts"),
                            div(style="font-size:13px;color:#555;line-height:1.6;",
                                "Points awarded proportionally based on how many safety car windows the team used to pit. ",
                                "Pitting under SC is essentially a free stop for teams that capitalise are rewarded."
                            )
                        )
                 ),
                 column(4,
                        div(class="component-card",
                            div(style="font-size:10px;font-weight:700;letter-spacing:2px;text-transform:uppercase;color:#999;margin-bottom:6px;","Component 3"),
                            div(style="font-size:20px;font-weight:700;color:#1a1a1a;margin-bottom:6px;","Tyre Strategy"),
                            div(style="font-size:32px;font-weight:700;color:#e10600;margin-bottom:8px;","25 pts"),
                            div(style="font-size:13px;color:#555;line-height:1.6;",
                                "Rewards drivers who ran more laps on harder compounds (Medium/Hard). ",
                                "Reflects tyre conservation and race management for a key indicator of strategic depth."
                            )
                        )
                 )
               ),
               
               # Live calculator
               fluidRow(
                 column(5,
                        div(class="control-panel", style="margin-bottom:15px;",
                            h5("Live Score Calculator"),
                            div(style="font-size:11px;color:#888;margin-bottom:15px;",
                                "Adjust the inputs to see how a strategy would be scored."),
                            sliderInput("calc_stops","Number of Pit Stops", min=1, max=4, value=2, step=1),
                            sliderInput("calc_min_stops","Minimum Stops in Race", min=1, max=4, value=2, step=1),
                            sliderInput("calc_sc_hit","SC Windows Used (out of total)", min=0, max=3, value=1, step=1),
                            sliderInput("calc_sc_total","Total SC Windows in Race", min=0, max=3, value=2, step=1),
                            sliderInput("calc_hard_pct","% Laps on Medium/Hard Tyres", min=0, max=100, value=60, step=5, post="%")
                        )
                 ),
                 column(7,
                        div(class="plot-panel", style="margin-bottom:15px;",
                            h6(style="font-size:11px;font-weight:700;letter-spacing:1.5px;text-transform:uppercase;color:#555;","Calculated Score"),
                            hr(class="f1-divider"),
                            uiOutput("calc_score_display"),
                            withSpinner(plotlyOutput("calc_score_plot", height="260px"),
                                        type=4, color="#e10600", size=0.7)
                        )
                 )
               ),
               
               # Worked example
               fluidRow(
                 column(12,
                        div(class="plot-panel",
                            h6(style="font-size:11px;font-weight:700;letter-spacing:1.5px;text-transform:uppercase;color:#555;",
                               "Worked Example \u2014 2023 Bahrain GP"),
                            hr(class="f1-divider"),
                            div(style="font-size:12px;color:#666;margin-bottom:15px;",
                                "Verstappen vs Sargeant \u2014 same race, very different strategic outcomes."),
                            withSpinner(plotlyOutput("worked_example_plot", height="280px"),
                                        type=4, color="#e10600", size=0.7)
                        )
                 )
               ),
               
               div(class="footer-ids","DATA SENTINELS \u00b7 z5715590 \u00b7 z5704763")
           )
  )
  
) # /navbarPage


# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  isolate({
    tryCatch({
      get_lap_data_cached(2023, 1)
      get_pitstop_data_cached(2023, 1)
    }, error = function(e) NULL)
  })
  
  output$sm_race_selector <- renderUI({
    req(input$sm_season)
    schedule <- get_schedule(as.numeric(input$sm_season))
    choices  <- setNames(schedule$round, paste("Round", schedule$round, "-", schedule$race_name))
    selectInput("sm_round", "Race", choices = choices, selected = choices[1])
  })
  
  output$ud_race_selector <- renderUI({
    req(input$ud_season)
    schedule <- get_schedule(as.numeric(input$ud_season))
    choices  <- setNames(schedule$round, paste("Round", schedule$round, "-", schedule$race_name))
    selectInput("ud_round", "Race", choices = choices, selected = choices[1])
  })
  
  output$ss_race_selector <- renderUI({
    req(input$ss_season)
    schedule <- get_schedule(as.numeric(input$ss_season))
    choices  <- setNames(schedule$round, paste("Round", schedule$round, "-", schedule$race_name))
    selectInput("ss_round", "Race", choices = choices, selected = choices[1])
  })
  
  # ── Panel 1 ────────────────────────────────────────────────────────────────
  sm_lap_data <- reactive({
    req(input$sm_season, input$sm_round)
    tryCatch(get_lap_data_cached(as.numeric(input$sm_season), as.numeric(input$sm_round)),
             error = function(e) NULL)
  })
  
  sm_pit_data <- reactive({
    req(input$sm_season, input$sm_round)
    tryCatch(get_pitstop_data_cached(as.numeric(input$sm_season), as.numeric(input$sm_round)),
             error = function(e) NULL)
  })
  
  output$strategy_map_plot <- renderPlotly({
    req(input$sm_round)
    laps <- sm_lap_data()
    pits <- sm_pit_data()
    req(!is.null(laps), !is.null(pits), nrow(laps) > 0)
    plot_strategy_map(laps, pits, input$sm_drivers,
                      season = input$sm_season, round = input$sm_round)
  })
  
  # ── Panel 2 ────────────────────────────────────────────────────────────────
  ud_lap_data <- reactive({
    req(input$ud_season, input$ud_round)
    tryCatch(get_lap_data_cached(as.numeric(input$ud_season), as.numeric(input$ud_round)),
             error = function(e) NULL)
  })
  
  ud_pit_data <- reactive({
    req(input$ud_season, input$ud_round)
    tryCatch(get_pitstop_data_cached(as.numeric(input$ud_season), as.numeric(input$ud_round)),
             error = function(e) NULL)
  })
  
  ud_undercuts <- reactive({
    laps <- ud_lap_data()
    pits <- ud_pit_data()
    req(!is.null(laps), !is.null(pits))
    undercuts <- tryCatch(detect_undercuts(laps, pits), error = function(e) data.frame())
    if (is.null(undercuts) || nrow(undercuts) == 0) return(data.frame())
    if (input$ud_result == "success") undercuts <- undercuts %>% dplyr::filter(success == TRUE)
    if (input$ud_result == "failed")  undercuts <- undercuts %>% dplyr::filter(success == FALSE)
    undercuts
  })
  
  belgian_warning_plot <- function() {
    plotly::plot_ly() %>%
      plotly::layout(
        paper_bgcolor = "#ffffff", plot_bgcolor = "#fafafa",
        xaxis = list(visible = FALSE, fixedrange = TRUE),
        yaxis = list(visible = FALSE, fixedrange = TRUE),
        annotations = list(list(
          text = paste0(
            "<b>\u26a0\ufe0f  2021 Belgian Grand Prix \u2014 Race Never Started</b><br><br>",
            "Only 3 laps were completed behind the Safety Car<br>",
            "due to heavy rain and wet track conditions.<br><br>",
            "No strategy data is available for this event."
          ),
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "middle", showarrow = FALSE,
          font = list(size = 13, color = "#856404", family = "Helvetica Neue"),
          bgcolor = "#fff3cd", bordercolor = "#e10600",
          borderpad = 16, borderwidth = 1.5
        ))
      ) %>%
      plotly::config(displayModeBar = FALSE)
  }
  
  output$undercut_table_plot <- renderPlotly({
    is_belgian_2021 <- !is.null(input$ud_season) && !is.null(input$ud_round) &&
      as.numeric(input$ud_season) == 2021 && as.numeric(input$ud_round) == 12
    if (is_belgian_2021) return(belgian_warning_plot())
    plot_undercut_table(ud_undercuts())
  })
  
  output$undercut_bar_plot <- renderPlotly({
    is_belgian_2021 <- !is.null(input$ud_season) && !is.null(input$ud_round) &&
      as.numeric(input$ud_season) == 2021 && as.numeric(input$ud_round) == 12
    if (is_belgian_2021) return(belgian_warning_plot())
    plot_undercut_bar(ud_undercuts())
  })
  
  # ── Panel 3 ────────────────────────────────────────────────────────────────
  sc_data <- reactiveValues(timeline = NULL, pits = NULL)
  
  observeEvent(input$sc_season, {
    season   <- as.numeric(input$sc_season)
    schedule <- get_schedule(season)
    all_sc       <- list()
    all_analysis <- list()
    withProgress(message = "Loading safety car data...", {
      for (i in seq_len(nrow(schedule))) {
        r     <- as.integer(schedule$round[i])
        rname <- schedule$race_name[i]
        incProgress(1 / nrow(schedule), detail = rname)
        tryCatch({
          sc       <- get_sc_cached(season, r, rname)
          analysis <- get_sc_pits_cached(season, r, rname)
          if (!is.null(sc)       && nrow(sc) > 0)       all_sc[[length(all_sc) + 1]]             <- sc
          if (!is.null(analysis) && nrow(analysis) > 0) all_analysis[[length(all_analysis) + 1]] <- analysis
        }, error = function(e) NULL)
      }
    })
    sc_data$timeline <- if (length(all_sc)       > 0) bind_rows(all_sc)       else data.frame()
    sc_data$pits     <- if (length(all_analysis) > 0) bind_rows(all_analysis) else data.frame()
  }, ignoreNULL = FALSE)
  
  output$safety_car_plot <- renderPlotly({
    req(sc_data$timeline)
    plot_safety_car_timeline(sc_data$timeline, input$sc_type)
  })
  
  output$safety_car_table_plot <- renderPlotly({
    req(sc_data$pits)
    plot_sc_winners_losers(sc_data$pits)
  })
  
  # ── Panel 4 ────────────────────────────────────────────────────────────────
  ss_lap_data <- reactive({
    s <- input$ss_season; r <- input$ss_round
    if (is.null(s) || is.null(r) || !nzchar(as.character(r))) return(NULL)
    tryCatch(get_lap_data_cached(as.numeric(s), as.numeric(r)), error = function(e) NULL)
  })
  
  ss_pit_data <- reactive({
    s <- input$ss_season; r <- input$ss_round
    if (is.null(s) || is.null(r) || !nzchar(as.character(r))) return(NULL)
    tryCatch(get_pitstop_data_cached(as.numeric(s), as.numeric(r)), error = function(e) NULL)
  })
  
  ss_score_data <- reactive({
    laps <- ss_lap_data(); pits <- ss_pit_data()
    if (is.null(pits) || nrow(pits) == 0) return(data.frame())
    if (is.null(laps) || nrow(laps) == 0)
      return(tryCatch(score_from_pits_only(pits), error = function(e) data.frame()))
    result <- tryCatch(score_race(laps, pits, sc_events = NULL), error = function(e) NULL)
    if (is.null(result) || nrow(result) == 0)
      return(tryCatch(score_from_pits_only(pits), error = function(e) data.frame()))
    result
  })
  
  ss_data_mode <- reactive({
    laps <- ss_lap_data()
    if (is.null(laps) || nrow(laps) == 0) "partial" else "full"
  })
  
  output$ss_data_note <- renderUI({
    if (ss_data_mode() == "partial")
      div(style="font-size:11px;color:#e67e22;margin-bottom:6px;",
          "\u26a0\ufe0f Lap data unavailable \u2014 scores based on pit stops only")
  })
  
  output$ss_box_avg_score <- renderUI({
    data <- ss_score_data()
    val  <- if (!is.null(data) && nrow(data) > 0) round(mean(data$total_score), 1) else "\u2014"
    div(class="stat-box", div(class="stat-number", val), div(class="stat-label","Avg Race Score"))
  })
  
  output$ss_box_best <- renderUI({
    data <- ss_score_data()
    val  <- if (!is.null(data) && nrow(data) > 0)
      data %>% dplyr::filter(total_score == max(total_score)) %>% slice(1) %>% pull(team)
    else "\u2014"
    div(class="stat-box", div(class="stat-number", val), div(class="stat-label","Best Strategist"))
  })
  
  output$ss_box_worst <- renderUI({
    data <- ss_score_data()
    val  <- if (!is.null(data) && nrow(data) > 0)
      data %>% dplyr::filter(total_score == min(total_score)) %>% slice(1) %>% pull(team)
    else "\u2014"
    div(class="stat-box", div(class="stat-number", val), div(class="stat-label","Worst Strategist"))
  })
  
  ss_season_scores <- reactive({
    req(input$ss_season, input$ss_round)
    season <- as.numeric(input$ss_season)
    schedule <- get_schedule(season)
    all_scores <- list()
    withProgress(message = "Loading season strategy scores...", {
      for (i in seq_len(nrow(schedule))) {
        r     <- as.integer(schedule$round[i])
        rname <- schedule$race_name[i]
        incProgress(1 / nrow(schedule), detail = rname)
        tryCatch({
          laps <- get_lap_data_cached(season, r)
          pits <- get_pitstop_data_cached(season, r)
          sc   <- get_sc_cached(season, r, rname)
          s    <- score_race(laps, pits, sc) %>% mutate(round = r)
          if (!is.null(s) && nrow(s) > 0) all_scores[[length(all_scores) + 1]] <- s
        }, error = function(e) NULL)
      }
    })
    if (length(all_scores) == 0) return(data.frame())
    bind_rows(all_scores) %>% dplyr::filter(team != "Unknown")
  })
  
  output$strategy_score_plot <- renderPlotly({
    is_belgian_2021 <- !is.null(input$ss_season) && !is.null(input$ss_round) &&
      as.numeric(input$ss_season) == 2021 && as.numeric(input$ss_round) == 12
    if (is_belgian_2021) return(belgian_warning_plot())
    data <- ss_score_data()
    req(!is.null(data), nrow(data) > 0)
    plot_strategy_scores(data)
  })
  
  output$strategy_league_plot <- renderPlotly({
    data <- ss_season_scores()
    req(!is.null(data), nrow(data) > 0)
    plot_strategy_league(data)
  })
  
  # ── Panel 5: Score Explained ───────────────────────────────────────────────
  calc_scores <- reactive({
    extra  <- max(0, input$calc_stops - input$calc_min_stops)
    stop_s <- max(0, 40 - extra * 10)
    sc_s   <- if (input$calc_sc_total == 0) 35 else
      round(35 * input$calc_sc_hit / input$calc_sc_total)
    tyre_s <- round(25 * input$calc_hard_pct / 100)
    list(stop = stop_s, sc = sc_s, tyre = tyre_s, total = stop_s + sc_s + tyre_s)
  })
  
  output$calc_score_display <- renderUI({
    s <- calc_scores()
    div(style="display:flex;gap:10px;margin-bottom:15px;",
        div(style="flex:1;text-align:center;background:#f5f5f5;padding:15px;",
            div(style="font-size:10px;font-weight:700;color:#999;letter-spacing:1px;text-transform:uppercase;margin-bottom:6px;","Stop Efficiency"),
            div(style="font-size:26px;font-weight:700;color:#1a1a1a;", paste0(s$stop,"/40"))
        ),
        div(style="flex:1;text-align:center;background:#f5f5f5;padding:15px;",
            div(style="font-size:10px;font-weight:700;color:#999;letter-spacing:1px;text-transform:uppercase;margin-bottom:6px;","SC Utilisation"),
            div(style="font-size:26px;font-weight:700;color:#1a1a1a;", paste0(s$sc,"/35"))
        ),
        div(style="flex:1;text-align:center;background:#f5f5f5;padding:15px;",
            div(style="font-size:10px;font-weight:700;color:#999;letter-spacing:1px;text-transform:uppercase;margin-bottom:6px;","Tyre Strategy"),
            div(style="font-size:26px;font-weight:700;color:#1a1a1a;", paste0(s$tyre,"/25"))
        ),
        div(style="flex:1;text-align:center;background:#1a1a1a;padding:15px;",
            div(style="font-size:10px;font-weight:700;color:#999;letter-spacing:1px;text-transform:uppercase;margin-bottom:6px;","Total Score"),
            div(style="font-size:26px;font-weight:700;color:#e10600;", paste0(s$total,"/100"))
        )
    )
  })
  
  output$calc_score_plot <- renderPlotly({
    s <- calc_scores()
    df <- data.frame(
      component = c("Stop Efficiency","SC Utilisation","Tyre Strategy"),
      score     = c(s$stop, s$sc, s$tyre),
      max_score = c(40, 35, 25),
      colour    = c("#e10600","#FFC906","#3671C6")
    )
    plotly::plot_ly(df, x=~score, y=~component, type="bar", orientation="h",
                    marker=list(color=~colour),
                    text=~paste0(score," / ",max_score), textposition="outside",
                    hoverinfo="none"
    ) %>%
      plotly::layout(
        xaxis=list(title="", range=c(0,55), fixedrange=TRUE, gridcolor="#eeeeee",
                   tickfont=list(size=11, family="Helvetica Neue", bold=TRUE)),
        yaxis=list(title="", fixedrange=TRUE, automargin=TRUE,
                   tickfont=list(size=12, family="Helvetica Neue", color="#1a1a1a", bold=TRUE)),
        paper_bgcolor="#ffffff", plot_bgcolor="#fafafa",
        margin=list(l=10, r=80, t=10, b=20),
        showlegend=FALSE
      ) %>%
      plotly::config(displayModeBar=FALSE)
  })
  
  output$worked_example_plot <- renderPlotly({
    df <- data.frame(
      driver    = rep(c("Max Verstappen","Logan Sargeant"), each=3),
      component = rep(c("Stop Efficiency","SC Utilisation","Tyre Strategy"), 2),
      score     = c(40, 35, 19, 20, 0, 6)
    )
    plotly::plot_ly(df, x=~score, y=~driver, color=~component,
                    type="bar", orientation="h",
                    colors=c("Stop Efficiency"="#e10600","SC Utilisation"="#FFC906","Tyre Strategy"="#3671C6"),
                    text=~score, textposition="inside",
                    hovertemplate="<b>%{y}</b><br>%{fullData.name}: %{x} pts<extra></extra>"
    ) %>%
      plotly::layout(
        barmode="stack",
        xaxis=list(title=list(text="<b>Strategy Score</b>"),
                   range=c(0,115), fixedrange=TRUE, gridcolor="#eeeeee",
                   tickfont=list(size=11, family="Helvetica Neue", bold=TRUE)),
        yaxis=list(title="", fixedrange=TRUE, automargin=TRUE,
                   tickfont=list(size=12, family="Helvetica Neue", color="#1a1a1a", bold=TRUE)),
        legend=list(orientation="h", x=0, y=1.2),
        paper_bgcolor="#ffffff", plot_bgcolor="#fafafa",
        margin=list(l=10, r=30, t=50, b=50),
        annotations=list(
          list(x=94, y="Max Verstappen", text="<b>Total: 94</b>",
               xanchor="left", yanchor="middle", showarrow=FALSE,
               font=list(size=12, color="#1a1a1a")),
          list(x=26, y="Logan Sargeant", text="<b>Total: 26</b>",
               xanchor="left", yanchor="middle", showarrow=FALSE,
               font=list(size=12, color="#1a1a1a"))
        )
      ) %>%
      plotly::config(displayModeBar=FALSE)
  })
  
}

shinyApp(ui, server)