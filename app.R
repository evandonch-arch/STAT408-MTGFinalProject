library(shiny)
library(tidyverse)
library(jsonlite)
library(DT)

# ── Data loading ──────────────────────────────────────────────────────────────
bulk_info    <- fromJSON("https://api.scryfall.com/bulk-data")
download_url <- bulk_info$data$download_uri[
  bulk_info$data$type == "default_cards"
]

mtg_small <- fromJSON(download_url, flatten = TRUE) |>
  as_tibble() |>
  select(
    oracle_id, name, mana_cost, cmc, type_line, oracle_text,
    colors, color_identity, set, rarity, prices.usd,
    legalities.standard, legalities.modern, legalities.commander
  )

mtg_cheapest <- mtg_small |>
  mutate(price = as.numeric(prices.usd)) |>
  filter(!is.na(price)) |>
  group_by(oracle_id) |>
  slice_min(price, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    color_str = sapply(colors, function(x)
      if (length(x) == 0) "Colorless" else paste(sort(x), collapse = "")),
    color_count = sapply(colors, length),
    color_label = case_when(
      color_count == 0             ~ "Colorless",
      color_count >= 2             ~ "Multicolor",
      color_str == "W"             ~ "White",
      color_str == "U"             ~ "Blue",
      color_str == "B"             ~ "Black",
      color_str == "R"             ~ "Red",
      color_str == "G"             ~ "Green",
      TRUE                         ~ "Other"
    ),
    card_supertype = case_when(
      str_detect(type_line, "Creature")  ~ "Creature",
      str_detect(type_line, "Instant")   ~ "Instant",
      str_detect(type_line, "Sorcery")   ~ "Sorcery",
      str_detect(type_line, "Enchantment") ~ "Enchantment",
      str_detect(type_line, "Artifact")  ~ "Artifact",
      str_detect(type_line, "Land")      ~ "Land",
      str_detect(type_line, "Planeswalker") ~ "Planeswalker",
      TRUE                               ~ "Other"
    ),
    rarity = factor(rarity,
      levels = c("common", "uncommon", "rare", "mythic", "special", "bonus"))
  )

all_rarities <- levels(droplevels(mtg_cheapest$rarity))
cmc_range    <- range(mtg_cheapest$cmc, na.rm = TRUE)
price_max    <- ceiling(max(mtg_cheapest$price, na.rm = TRUE))

# MTG color palette
mtg_colors <- c(
  White     = "#f0e6c0", Blue  = "#1e6eb5", Black    = "#3a3340",
  Red       = "#d44239", Green = "#2a7a45", Colorless = "#a0a0a0",
  Multicolor = "#c8a032"
)

# Rarity palette
rarity_colors <- c(
  common = "#9e9e9e", uncommon = "#5dade2",
  rare = "#d4ac0d", mythic = "#e67e22", special = "#8e44ad"
)

# Card type palette
type_colors <- c(
  Creature = "#2a7a45", Instant = "#1e6eb5", Sorcery = "#d44239",
  Enchantment = "#8e44ad", Artifact = "#7f8c8d", Land = "#a0522d",
  Planeswalker = "#c8a032", Other = "#555555"
)

plot_descriptions <- list(
  hist       = "Shows how card prices are distributed. Most cards are cheap, with a long tail of expensive cards.",
  rarity_bar = "Compares the average market price across each rarity tier. Higher rarity cards tend to command higher prices.",
  cmc_hist   = "Shows how many cards exist at each mana cost value. Most playable cards fall between CMC 1–5.",
  scatter    = "Plots each card's price against its CMC. The blue curve shows the general trend.",
  color_bar  = "Counts how many unique cards belong to each color. Multi-color cards are counted as Multicolor.",
  color_box  = "Compares the spread of card prices across colors. The box shows the middle 50% of prices; dots are outliers.",
  type_bar   = "Shows the average price for each card type (Creature, Instant, etc.).",
  type_count = "Shows how many cards exist for each card type in the current filter."
)

# ── CSS ───────────────────────────────────────────────────────────────────────
app_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');

  * { box-sizing: border-box; }

  body {
    background: #0d1117;
    font-family: 'Inter', 'Segoe UI', sans-serif;
    font-size: 15px;
    color: #e6edf3;
  }

  /* ── Header ── */
  .app-header {
    background: linear-gradient(135deg, #161b22 0%, #1c2a3a 100%);
    border-bottom: 2px solid #c8a032;
    padding: 20px 32px 16px;
    display: flex;
    align-items: center;
    gap: 18px;
  }
  .app-header .mana-icon {
    font-size: 2rem;
    line-height: 1;
  }
  .app-header h1 {
    margin: 0;
    font-size: 1.75rem;
    font-weight: 700;
    color: #c8a032;
    letter-spacing: 0.5px;
  }
  .app-header p {
    margin: 3px 0 0;
    font-size: 0.9rem;
    color: #8b949e;
  }

  /* ── Layout ── */
  .outer-row { margin: 0 !important; }
  .sidebar-col {
    background: #161b22;
    border-right: 1px solid #30363d;
    min-height: calc(100vh - 72px);
    padding: 20px 16px 40px;
  }
  .main-col {
    padding: 24px 28px;
    background: #0d1117;
  }

  /* ── Sidebar section headers ── */
  .sec-label {
    font-size: 0.72rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1.2px;
    color: #8b949e;
    margin: 22px 0 8px;
    padding-bottom: 5px;
    border-bottom: 1px solid #21262d;
  }
  .sec-label:first-child { margin-top: 0; }

  /* ── Inputs ── */
  .form-control, .selectize-input {
    background: #21262d !important;
    border: 1px solid #30363d !important;
    color: #e6edf3 !important;
    border-radius: 8px !important;
    font-size: 0.9rem !important;
    padding: 7px 11px !important;
  }
  .form-control:focus, .selectize-input.focus {
    border-color: #c8a032 !important;
    box-shadow: 0 0 0 3px rgba(200,160,50,0.2) !important;
  }
  .selectize-dropdown {
    background: #21262d !important;
    border: 1px solid #30363d !important;
    color: #e6edf3 !important;
    border-radius: 8px !important;
  }
  .selectize-dropdown-content .option:hover,
  .selectize-dropdown-content .option.active {
    background: #30363d !important;
    color: #c8a032 !important;
  }

  /* ── Checkboxes ── */
  .checkbox label, .radio label { font-size: 0.9rem; color: #c9d1d9; }
  input[type=checkbox] { accent-color: #c8a032; width: 14px; height: 14px; }

  /* ── Sliders ── */
  .irs--shiny .irs-bar { background: #c8a032; border-color: #c8a032; }
  .irs--shiny .irs-handle { border-color: #c8a032 !important; background: #c8a032 !important; }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    background: #c8a032; color: #0d1117; font-weight: 600; font-size: 0.8rem;
  }
  .irs--shiny .irs-line { background: #30363d; border-color: #30363d; }
  .irs--shiny .irs-grid-text { color: #8b949e; font-size: 0.75rem; }
  .irs--shiny .irs-min, .irs--shiny .irs-max { color: #8b949e; font-size: 0.75rem; }

  /* ── CMC numeric inputs ── */
  .cmc-row { display: flex; flex-direction: column; gap: 6px; }
  .cmc-row .form-group { margin-bottom: 0; }
  .cmc-row .form-group label { font-size: 0.8rem; color: #8b949e; margin-bottom: 3px; display: block; }

  /* ── Stat cards ── */
  .stats-row { display: flex; gap: 14px; margin-bottom: 20px; }
  .stat-card {
    flex: 1;
    background: #161b22;
    border: 1px solid #30363d;
    border-radius: 12px;
    padding: 16px 18px;
    border-top: 3px solid #c8a032;
    transition: transform 0.15s;
  }
  .stat-card:hover { transform: translateY(-2px); }
  .stat-card.blue-top  { border-top-color: #1e6eb5; }
  .stat-card.red-top   { border-top-color: #d44239; }
  .stat-card.green-top { border-top-color: #2a7a45; }
  .stat-label {
    font-size: 0.72rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: #8b949e;
    margin-bottom: 6px;
  }
  .stat-value {
    font-size: 1.05rem;
    font-weight: 600;
    color: #e6edf3;
    line-height: 1.4;
  }

  /* ── Tabs ── */
  .nav-tabs {
    border-bottom: 2px solid #21262d;
    margin-bottom: 0;
  }
  .nav-tabs > li > a {
    color: #8b949e !important;
    font-weight: 600;
    font-size: 0.95rem;
    border: none !important;
    border-bottom: 3px solid transparent !important;
    padding: 10px 22px;
    margin-bottom: -2px;
    background: transparent !important;
    border-radius: 0 !important;
  }
  .nav-tabs > li > a:hover { color: #c9d1d9 !important; }
  .nav-tabs > li.active > a {
    color: #c8a032 !important;
    border-bottom: 3px solid #c8a032 !important;
  }
  .tab-content {
    background: #161b22;
    border: 1px solid #21262d;
    border-top: none;
    border-radius: 0 0 12px 12px;
    padding: 24px;
  }

  /* ── Plot description ── */
  .plot-desc {
    background: #1c2a3a;
    border-left: 3px solid #c8a032;
    padding: 10px 16px;
    font-size: 0.88rem;
    color: #8b949e;
    margin-bottom: 16px;
    border-radius: 0 8px 8px 0;
    line-height: 1.5;
  }

  /* ── Axis range controls ── */
  .axis-controls {
    background: #0d1117;
    border: 1px solid #21262d;
    border-radius: 10px;
    padding: 14px 16px 6px;
    margin-bottom: 16px;
  }
  .axis-controls .axis-title {
    font-size: 0.75rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: #8b949e;
    margin-bottom: 10px;
  }
  .axis-row { display: flex; gap: 16px; }
  .axis-row .shiny-input-container { flex: 1; }

  /* ── DataTable dark theme ── */
  .dataTables_wrapper { color: #e6edf3 !important; font-size: 0.9rem; }
  .dataTables_wrapper .dataTables_length select,
  .dataTables_wrapper .dataTables_filter input {
    background: #21262d !important;
    border: 1px solid #30363d !important;
    color: #e6edf3 !important;
    border-radius: 6px;
  }
  table.dataTable thead th {
    background: #21262d !important;
    color: #c8a032 !important;
    border-bottom: 1px solid #30363d !important;
    font-size: 0.85rem;
    font-weight: 700;
  }
  table.dataTable tbody tr { background: #161b22 !important; color: #c9d1d9 !important; }
  table.dataTable tbody tr:hover td { background: #21262d !important; }
  table.dataTable tbody tr.even td { background: #1a1f27 !important; }
  .dataTables_wrapper .dataTables_paginate .paginate_button {
    color: #8b949e !important; border-radius: 6px !important;
  }
  .dataTables_wrapper .dataTables_paginate .paginate_button.current {
    background: #c8a032 !important; color: #0d1117 !important;
    border-color: #c8a032 !important;
  }
  .dataTables_wrapper .dataTables_info { color: #8b949e !important; font-size: 0.85rem; }

  hr { border-color: #21262d; margin: 14px 0; }
"

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),

  # Header
  div(class = "app-header",
    div(class = "mana-icon", "\u2605"),
    div(
      h1("MTG Card Explorer"),
      p("Magic: The Gathering  \u2022  Powered by Scryfall")
    )
  ),

  fluidRow(class = "outer-row",
    # Sidebar
    column(2, class = "sidebar-col",

      div(class = "sec-label", "Colors"),
      checkboxGroupInput("filter_colors", label = NULL,
        choices  = c("White" = "W", "Blue" = "U", "Black" = "B",
                     "Red"   = "R", "Green" = "G", "Colorless" = "Colorless"),
        selected = c("W", "U", "B", "R", "G", "Colorless")
      ),

      div(class = "sec-label", "Multi-color Mode"),
      selectInput("color_mode", label = NULL,
        choices  = c("Exact match" = "exact", "Includes any selected" = "any"),
        selected = "exact"
      ),

      div(class = "sec-label", "Rarity"),
      checkboxGroupInput("filter_rarity", label = NULL,
        choices  = all_rarities,
        selected = all_rarities
      ),

      div(class = "sec-label", "Mana Cost (CMC)"),
      div(class = "cmc-row",
        div(class = "form-group",
          tags$label("Min"),
          numericInput("cmc_min", label = NULL, value = cmc_range[1],
            min = cmc_range[1], max = cmc_range[2], step = 1)
        ),
        div(class = "form-group",
          tags$label("Max"),
          numericInput("cmc_max", label = NULL, value = cmc_range[2],
            min = cmc_range[1], max = cmc_range[2], step = 1)
        )
      ),

      div(class = "sec-label", "Card Type"),
      checkboxGroupInput("filter_type", label = NULL,
        choices  = c("Creature", "Instant", "Sorcery", "Enchantment",
                     "Artifact", "Land", "Planeswalker", "Other"),
        selected = c("Creature", "Instant", "Sorcery", "Enchantment",
                     "Artifact", "Land", "Planeswalker", "Other")
      ),

      div(class = "sec-label", "Price (USD)"),
      sliderInput("filter_price", label = NULL,
        min = 0, max = price_max,
        value = c(0, price_max), step = 1
      ),

      div(class = "sec-label", "Format Legality"),
      selectInput("filter_format", label = NULL,
        choices  = c("Any", "Standard", "Modern", "Commander"),
        selected = "Any"
      ),

      hr(),
      div(class = "sec-label", "Chart Type"),
      selectInput("plot_type", label = NULL,
        choices = c(
          "Price Distribution"        = "hist",
          "Avg Price by Rarity"       = "rarity_bar",
          "CMC Distribution"          = "cmc_hist",
          "Price vs CMC (Scatter)"    = "scatter",
          "Card Count by Color"       = "color_bar",
          "Price by Color (Boxplot)"  = "color_box",
          "Avg Price by Card Type"    = "type_bar",
          "Card Count by Type"        = "type_count"
        )
      )
    ),

    # Main
    column(10, class = "main-col",

      # Stat cards
      div(class = "stats-row",
        div(class = "stat-card",
          div(class = "stat-label", "Cards Shown"),
          div(class = "stat-value", textOutput("stat_count", inline = TRUE))
        ),
        div(class = "stat-card blue-top",
          div(class = "stat-label", "Avg Price (USD)"),
          div(class = "stat-value", textOutput("stat_avg", inline = TRUE))
        ),
        div(class = "stat-card red-top",
          div(class = "stat-label", "Most Expensive"),
          div(class = "stat-value", textOutput("stat_max", inline = TRUE))
        ),
        div(class = "stat-card green-top",
          div(class = "stat-label", "Cheapest"),
          div(class = "stat-value", textOutput("stat_min", inline = TRUE))
        )
      ),

      tabsetPanel(
        tabPanel("Plot",
          br(),
          div(class = "plot-desc", textOutput("plot_description")),

          # Axis range controls (shown conditionally)
          uiOutput("axis_controls_ui"),

          plotOutput("main_plot", height = "460px")
        ),
        tabPanel("Table",
          br(),
          DTOutput("card_table")
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Filtered data
  filtered <- reactive({
    df <- mtg_cheapest
    selected_colors <- input$filter_colors

    df <- df |> filter(
      mapply(function(cs, cc) {
        if (cs == "Colorless") return("Colorless" %in% selected_colors)
        card_colors <- strsplit(cs, "")[[1]]
        if (input$color_mode == "exact") {
          color_sel <- selected_colors[selected_colors != "Colorless"]
          setequal(card_colors, color_sel)
        } else {
          any(card_colors %in% selected_colors)
        }
      }, color_str, color_count)
    )

    df <- df |> filter(rarity %in% input$filter_rarity)

    df <- df |> filter(card_supertype %in% input$filter_type)

    mn <- max(cmc_range[1], min(input$cmc_min, input$cmc_max), na.rm = TRUE)
    mx <- min(cmc_range[2], max(input$cmc_min, input$cmc_max), na.rm = TRUE)
    df <- df |> filter(cmc >= mn, cmc <= mx)

    df <- df |> filter(price >= input$filter_price[1], price <= input$filter_price[2])

    if (input$filter_format == "Standard")  df <- df |> filter(legalities.standard  == "legal")
    if (input$filter_format == "Modern")    df <- df |> filter(legalities.modern    == "legal")
    if (input$filter_format == "Commander") df <- df |> filter(legalities.commander == "legal")

    df
  })

  # Axis range UI — shown only for continuous-axis plots
  output$axis_controls_ui <- renderUI({
    df <- filtered()
    if (nrow(df) == 0) return(NULL)

    pt <- input$plot_type
    if (!pt %in% c("hist", "scatter", "cmc_hist", "color_box")) return(NULL)

    if (pt == "hist") {
      pr <- range(df$price, na.rm = TRUE)
      div(class = "axis-controls",
        div(class = "axis-title", "X-Axis Range (Price)"),
        div(class = "axis-row",
          sliderInput("xaxis_range", label = NULL,
            min = floor(pr[1]), max = ceiling(pr[2]),
            value = c(floor(pr[1]), ceiling(pr[2])), step = 0.5)
        )
      )
    } else if (pt == "cmc_hist") {
      cr <- range(df$cmc, na.rm = TRUE)
      div(class = "axis-controls",
        div(class = "axis-title", "X-Axis Range (CMC)"),
        div(class = "axis-row",
          sliderInput("xaxis_range_cmc", label = NULL,
            min = cr[1], max = cr[2],
            value = c(cr[1], cr[2]), step = 1)
        )
      )
    } else if (pt == "scatter") {
      pr <- range(df$price, na.rm = TRUE)
      cr <- range(df$cmc,   na.rm = TRUE)
      div(class = "axis-controls",
        div(class = "axis-title", "Axis Ranges"),
        div(class = "axis-row",
          div(
            tags$small(style = "color:#8b949e; font-size:0.8rem;", "X — CMC"),
            sliderInput("xaxis_range_scatter", label = NULL,
              min = cr[1], max = cr[2], value = c(cr[1], cr[2]), step = 1)
          ),
          div(
            tags$small(style = "color:#8b949e; font-size:0.8rem;", "Y — Price (USD)"),
            sliderInput("yaxis_range_scatter", label = NULL,
              min = floor(pr[1]), max = ceiling(pr[2]),
              value = c(floor(pr[1]), min(ceiling(pr[2]), 100)), step = 1)
          )
        )
      )
    } else if (pt == "color_box") {
      pr <- range(df$price, na.rm = TRUE)
      div(class = "axis-controls",
        div(class = "axis-title", "Y-Axis Range (Price)"),
        div(class = "axis-row",
          sliderInput("yaxis_range_box", label = NULL,
            min = floor(pr[1]), max = ceiling(pr[2]),
            value = c(floor(pr[1]), min(ceiling(pr[2]), 50)), step = 0.5)
        )
      )
    }
  })

  # Stats
  output$stat_count <- renderText({ format(nrow(filtered()), big.mark = ",") })

  output$stat_avg <- renderText({
    df <- filtered(); if (nrow(df) == 0) return("\u2014")
    paste0("$", format(round(mean(df$price, na.rm = TRUE), 2), nsmall = 2))
  })

  output$stat_max <- renderText({
    df <- filtered(); if (nrow(df) == 0) return("\u2014")
    r <- df |> slice_max(price, n = 1, with_ties = FALSE)
    paste0(r$name, "  ($", round(r$price, 2), ")")
  })

  output$stat_min <- renderText({
    df <- filtered(); if (nrow(df) == 0) return("\u2014")
    r <- df |> slice_min(price, n = 1, with_ties = FALSE)
    paste0(r$name, "  ($", round(r$price, 2), ")")
  })

  # Plot description
  output$plot_description <- renderText({ plot_descriptions[[input$plot_type]] })

  # Shared ggplot theme (dark)
  dark_theme <- function() {
    theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid.major = element_line(color = "#21262d"),
        panel.grid.minor = element_blank(),
        plot.title    = element_text(color = "#c8a032", face = "bold", size = 16),
        plot.subtitle = element_text(color = "#8b949e", size = 12),
        axis.title    = element_text(color = "#8b949e", size = 13),
        axis.text     = element_text(color = "#c9d1d9", size = 12),
        legend.background = element_rect(fill = "#161b22", color = NA),
        legend.text   = element_text(color = "#c9d1d9", size = 11),
        legend.title  = element_text(color = "#8b949e", size = 11),
        strip.text    = element_text(color = "#c9d1d9")
      )
  }

  # Main plot
  output$main_plot <- renderPlot({
    df <- filtered()
    if (nrow(df) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No cards match the selected filters.",
                 size = 6, color = "#8b949e") +
        theme_void() +
        theme(plot.background = element_rect(fill = "#161b22", color = NA))
    } else switch(input$plot_type,

      hist = {
        xlim_vals <- if (!is.null(input$xaxis_range)) input$xaxis_range else NULL
        p <- ggplot(df, aes(price)) +
          geom_histogram(bins = 60, fill = "#c8a032", color = "#161b22", linewidth = 0.2) +
          labs(title = "Distribution of Card Prices (USD)", x = "Price (USD)", y = "Count") +
          dark_theme()
        if (!is.null(xlim_vals)) p <- p + coord_cartesian(xlim = xlim_vals)
        p
      },

      rarity_bar = {
        df |>
          filter(rarity != "bonus") |>
          group_by(rarity) |>
          summarize(avg_price = mean(price, na.rm = TRUE), .groups = "drop") |>
          mutate(rarity = factor(rarity,
            levels = c("common","uncommon","rare","mythic","special"))) |>
          ggplot(aes(rarity, avg_price, fill = rarity)) +
          geom_col(show.legend = FALSE, width = 0.6) +
          geom_text(aes(label = paste0("$", round(avg_price, 2))),
                    vjust = -0.5, size = 4, color = "#c9d1d9") +
          scale_fill_manual(values = rarity_colors) +
          labs(title = "Average Card Price by Rarity", x = "Rarity", y = "Avg Price (USD)") +
          dark_theme()
      },

      cmc_hist = {
        xlim_vals <- if (!is.null(input$xaxis_range_cmc)) input$xaxis_range_cmc else NULL
        p <- ggplot(df, aes(cmc)) +
          geom_histogram(binwidth = 1, fill = "#1e6eb5", color = "#161b22", linewidth = 0.2) +
          scale_x_continuous(breaks = seq(0, cmc_range[2], 2)) +
          labs(title = "Distribution of Converted Mana Cost (CMC)", x = "CMC", y = "Count") +
          dark_theme()
        if (!is.null(xlim_vals)) p <- p + coord_cartesian(xlim = xlim_vals)
        p
      },

      scatter = {
        xlim_vals <- if (!is.null(input$xaxis_range_scatter)) input$xaxis_range_scatter else NULL
        ylim_vals <- if (!is.null(input$yaxis_range_scatter)) input$yaxis_range_scatter else NULL
        p <- df |>
          mutate(color_label = factor(color_label,
            levels = c("White","Blue","Black","Red","Green","Colorless","Multicolor"))) |>
          ggplot(aes(cmc, price, color = color_label)) +
          geom_point(alpha = 0.3, size = 1.4) +
          geom_smooth(aes(group = 1), method = "loess", se = FALSE,
                      color = "#c8a032", linewidth = 1.4) +
          scale_color_manual(values = mtg_colors, name = "Color") +
          labs(title = "Card Price vs. Converted Mana Cost",
               x = "Converted Mana Cost", y = "Price (USD)") +
          dark_theme() +
          guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))
        if (!is.null(xlim_vals) || !is.null(ylim_vals))
          p <- p + coord_cartesian(xlim = xlim_vals, ylim = ylim_vals)
        p
      },

      color_bar = {
        df |>
          count(color_label) |>
          mutate(color_label = factor(color_label,
            levels = c("White","Blue","Black","Red","Green","Colorless","Multicolor"))) |>
          ggplot(aes(reorder(color_label, -n), n, fill = color_label)) +
          geom_col(show.legend = FALSE, width = 0.65) +
          geom_text(aes(label = format(n, big.mark = ",")),
                    vjust = -0.5, size = 4, color = "#c9d1d9") +
          scale_fill_manual(values = mtg_colors) +
          labs(title = "Number of Cards by Color", x = "Color", y = "Card Count") +
          dark_theme()
      },

      color_box = {
        ylim_vals <- if (!is.null(input$yaxis_range_box)) input$yaxis_range_box else NULL
        p <- df |>
          mutate(color_label = factor(color_label,
            levels = c("White","Blue","Black","Red","Green","Colorless","Multicolor"))) |>
          ggplot(aes(color_label, price, fill = color_label)) +
          geom_boxplot(show.legend = FALSE, outlier.alpha = 0.2, outlier.size = 0.8,
                       outlier.color = "#8b949e") +
          scale_fill_manual(values = mtg_colors) +
          labs(title = "Card Price Distribution by Color",
               subtitle = "Use the axis range slider below the chart description to zoom in",
               x = "Color", y = "Price (USD)") +
          dark_theme()
        if (!is.null(ylim_vals)) p <- p + coord_cartesian(ylim = ylim_vals)
        p
      },

      type_bar = {
        df |>
          group_by(card_supertype) |>
          summarize(avg_price = mean(price, na.rm = TRUE), n = n(), .groups = "drop") |>
          filter(n >= 5) |>
          ggplot(aes(reorder(card_supertype, avg_price), avg_price, fill = card_supertype)) +
          geom_col(show.legend = FALSE, width = 0.65) +
          geom_text(aes(label = paste0("$", round(avg_price, 2))),
                    hjust = -0.15, size = 4, color = "#c9d1d9") +
          scale_fill_manual(values = type_colors) +
          coord_flip() +
          labs(title = "Average Card Price by Type", x = NULL, y = "Avg Price (USD)") +
          dark_theme()
      },

      type_count = {
        df |>
          count(card_supertype) |>
          ggplot(aes(reorder(card_supertype, n), n, fill = card_supertype)) +
          geom_col(show.legend = FALSE, width = 0.65) +
          geom_text(aes(label = format(n, big.mark = ",")),
                    hjust = -0.15, size = 4, color = "#c9d1d9") +
          scale_fill_manual(values = type_colors) +
          coord_flip() +
          labs(title = "Card Count by Type", x = NULL, y = "Number of Cards") +
          dark_theme()
      }
    )
  }, bg = "#161b22")

  # Table
  output$card_table <- renderDT({
    filtered() |>
      select(
        Name        = name,
        `Mana Cost` = mana_cost,
        CMC         = cmc,
        Type        = type_line,
        Color       = color_label,
        Rarity      = rarity,
        Set         = set,
        `Price USD` = price,
        Standard    = legalities.standard,
        Modern      = legalities.modern,
        Commander   = legalities.commander
      ) |>
      datatable(
        filter   = "top",
        options  = list(pageLength = 15, scrollX = TRUE, dom = "lfrtip"),
        rownames = FALSE,
        class    = "stripe hover"
      )
  })
}

shinyApp(ui, server)
