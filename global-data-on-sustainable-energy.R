library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
library(countrycode)
library(leaflet) 
library(sf)               
library(rnaturalearth)  

# 1. PRÉPARATION DES DONNÉES


setwd("C:/Users/thoma/OneDrive/Desktop/COURS - IUT (2eme annee)/Développement décisionnel")
donnees_brutes <- read.csv("global-data-on-sustainable-energy (1).csv", stringsAsFactors = FALSE)
data_energie <- donnees_brutes %>%
  rename(
    Pays = Entity, Annee = Year,
    Acces_Elec = Access.to.electricity....of.population.,
    Part_Renouvelable = Renewable.energy.share.in.the.total.final.energy.consumption....,
    Emissions_CO2 = Value_co2_emissions_kt_by_country,
    PIB_Habitant = gdp_per_capita,
    Lat = Latitude, Lon = Longitude,
    Elec_Fossile = Electricity.from.fossil.fuels..TWh.,
    Elec_Nucleaire = Electricity.from.nuclear..TWh.,
    Elec_Renouvelable = Electricity.from.renewables..TWh.
  ) %>%
  select(Pays, Annee, Acces_Elec, Part_Renouvelable, Emissions_CO2, PIB_Habitant, Lat, Lon, Elec_Fossile, Elec_Nucleaire, Elec_Renouvelable) %>%
  drop_na(Pays, Annee, Acces_Elec, Part_Renouvelable, Emissions_CO2, PIB_Habitant) %>%
  mutate(ISO3 = countrycode(sourcevar = Pays, origin = "country.name", destination = "iso3c")) %>%
  mutate(Continent = countrycode(sourcevar = Pays, origin = "country.name", destination = "continent")) %>%
  mutate(Continent = case_when(
    Continent == "Africa" ~ "Afrique", Continent == "Americas" ~ "Amériques",
    Continent == "Asia" ~ "Asie", Continent == "Europe" ~ "Europe",
    Continent == "Oceania" ~ "Océanie", TRUE ~ "Autre"
  )) %>%
  mutate(Pays = countrycode(sourcevar = Pays, origin = "country.name", destination = "cldr.name.fr")) %>%
  filter(!is.na(Pays) & !is.na(ISO3))

annees_dispos <- sort(unique(data_energie$Annee))
continents_dispos <- c("Tous", sort(unique(data_energie$Continent)))
pays_dispos <- sort(unique(data_energie$Pays))

labels_variables <- c(
  "Emissions_CO2" = "Émissions de CO2 (kt)",
  "Part_Renouvelable" = "Part d'Énergie Renouvelable (%)",
  "PIB_Habitant" = "PIB par habitant ($)",
  "Acces_Elec" = "Accès à l'électricité (%)"
)

# Fonction de calcul de croissance
calculer_croissance <- function(df, var) {
  df_s <- df %>% arrange(Annee) %>% drop_na(all_of(var))
  n <- nrow(df_s); if (n < 2) return(NA_real_)
  v1 <- df_s[[var]][n-1]; v2 <- df_s[[var]][n]
  if (is.na(v1) || v1 == 0) return(NA_real_)
  (v2 - v1) / abs(v1) * 100
}

# Carte du monde
carte_monde <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso_a3 = case_when(
    admin == "France" ~ "FRA", admin == "Norway" ~ "NOR", TRUE ~ iso_a3
  )) %>%
  mutate(Continent_Fr = case_when(
    continent == "Africa" ~ "Afrique", continent == "Americas" ~ "Amériques",
    continent == "Asia" ~ "Asie", continent == "Europe" ~ "Europe",
    continent == "Oceania" ~ "Océanie", TRUE ~ "Autre"
  ))

# 2. DESIGN BSLIB & CSS
mon_theme <- bs_theme(
  version = 5, bootswatch = "zephyr", 
  primary = "#2c3e50", success = "#27ae60", info = "#2980b9", warning = "#e67e22", danger = "#e74c3c"
)

style_perso <- tags$style(HTML("
  .kpi-badge { 
    display: inline-block; 
    width: fit-content; 
    margin-top: 8px; 
    border-radius: 20px; 
    padding: 4px 12px; 
    font-size: 0.90rem; 
    font-weight: bold; 
    background-color: #ffffff; 
    box-shadow: 0 2px 6px rgba(0,0,0,0.25); 
  }
  .badge-hausse   { color: #27ae60; } 
  .badge-baisse { color: #e74c3c; } 
"))

# 3. UI
ui <- page_navbar(
  theme = mon_theme,
  title = "🌍 Tableau de Bord - Transition Écologique",
  bg = "#2c3e50", 
  fillable = FALSE, 
  header = style_perso, 
  
  #   ACCUEIL  
  nav_panel(
    title = "Accueil", icon = icon("home"),
    card(
      card_header("🌍 Comprendre et Visualiser la Transition Énergétique Mondiale", class = "bg-primary text-white"),
      card_body(
        markdown("
        ### 📊 D'où proviennent les données ?
        Ce tableau de bord interactif réalisé par **Thomas AYMARD, Mancef HENNICHE, Yusuf ASLAN et Malik BESSABIS** dans le cadre de notre BUT Science des Données s'appuie sur le jeu de données public disponible sur Kaggle : **Global Data on Sustainable Energy**. Il compile des informations cruciales sur plusieurs décennies. Ce dashboard se concentre sur 4 indicateurs majeurs :
        * **L'accès à l'électricité :** Part de la population ayant accès à l'électricité (%).
        * **La part des énergies renouvelables :** Leur part dans la consommation finale d'énergie (%).
        * **Les émissions de CO2 :** Le volume d'émissions (en kilotonnes).
        * **Le PIB par habitant :** Mesuré en dollars, pour mettre en perspective la richesse face aux enjeux climatiques.

        ### 🎯 À quoi sert ce tableau de bord ?
        Il permet de transformer des données brutes en un outil visuel interactif pour :
        * Cartographier la répartition mondiale des indicateurs énergétiques.
        * Analyser l'évolution temporelle des pays.
        * Mettre en évidence les corrélations (ex: croissance économique vs émissions de CO2).
        * Filtrer et exporter les données pour des analyses externes.

        ### 👥 À qui s'adresse-t-il ?
        * **Les décideurs publics et ONG :** Pour évaluer l'efficacité des politiques énergétiques.
        * **Les étudiants et chercheurs :** Comme support d'analyse.
        * **Le grand public :** Pour vulgariser les enjeux climatiques.

        ### 💡 À quels besoins répond-il ?
        Il répond à un besoin d'**accessibilité**. Il supprime la barrière technique pour permettre à tous de se concentrer sur l'interprétation des faits grâce au téléchargement en un clic (CSV/TXT) et aux graphiques interactifs.
        ")
      )
    )
  ),
  
  #   VISUALISATIONS  
  nav_panel(
    title = "Visualisations", icon = icon("chart-bar"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "📈️ Visualisations", bg = "#f8f9fa",
        selectInput("choix_continent", "Région :", choices = continents_dispos, selected = "Tous"),
        sliderInput("choix_annee", "Période :", min = min(annees_dispos), max = max(annees_dispos), value = c(max(annees_dispos) - 5, max(annees_dispos)), step = 1, sep = ""),
        selectInput("choix_variable", "Indicateur (Carte, Barres, Boxplot) :", choices = c("Émissions de CO2 (kt)" = "Emissions_CO2", "Part d'Énergie Renouvelable (%)" = "Part_Renouvelable", "PIB par habitant ($)" = "PIB_Habitant", "Accès à l'électricité (%)" = "Acces_Elec")),
        numericInput("nb_pays", "Nombre de pays (Top) :", value = 15, min = 5, max = 50),
        tags$hr(),
        p(strong("Options Nuage de points :")),
        selectInput("choix_var_x", "Axe X (Nuage) :", choices = c("PIB par habitant ($)" = "PIB_Habitant", "Émissions de CO2 (kt)" = "Emissions_CO2", "Part d'Énergie Renouvelable (%)" = "Part_Renouvelable", "Accès à l'électricité (%)" = "Acces_Elec"), selected = "PIB_Habitant"),
        checkboxInput("choix_log_x", "Échelle log (Axe X)", value = FALSE)
      ),
      
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        value_box(title = "Moyenne", value = textOutput("valeur_moyenne"), showcase = icon("globe"), theme = "success"),
        value_box(title = "Premier pays", value = textOutput("pays_top"), showcase = icon("trophy"), theme = "info"),
        value_box(title = "Maximum", value = uiOutput("valeur_max_ui"), showcase = icon("arrow-up"), theme = "primary"),
        value_box(title = "Minimum", value = uiOutput("valeur_min_ui"), showcase = icon("arrow-down"), theme = "warning")
      ),
      
      card(full_screen = TRUE, card_header("🗺️ Carte Mondiale Interactive", class = "bg-primary text-white"), leafletOutput("carte_mondiale", height = "600px")),
      
      card(full_screen = TRUE, card_header("🏆 Top Pays", class="bg-info text-white"), plotlyOutput("graphique_barres", height = "500px")),
      
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE, card_header("💡 Analyse croisée (Nuage de points)", class="bg-success text-white"), plotlyOutput("graphique_nuage", height = "500px")),
        card(full_screen = TRUE, card_header("📦 Distribution par Continent", class="bg-warning text-white"), plotlyOutput("graphique_boxplot", height = "500px"))
      )
    )
  ),
  
  #   COMPARAISON  
  nav_panel(
    title = "Comparaison", icon = icon("scale-balanced"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "⚖️ Comparaison", bg = "#f8f9fa",
        selectInput("pays_comp_1", "Pays 1 :", choices = pays_dispos, selected = pays_dispos[1]),
        selectInput("pays_comp_2", "Pays 2 :", choices = pays_dispos, selected = pays_dispos[2]),
        sliderInput("annee_comp", "Période :", min = min(annees_dispos), max = max(annees_dispos), value = c(min(annees_dispos), max(annees_dispos)), step = 1, sep = ""),
        selectInput("var_comp", "Indicateur :", choices = c("Émissions de CO2 (kt)" = "Emissions_CO2", "Part d'Énergie Renouvelable (%)" = "Part_Renouvelable", "PIB par habitant ($)" = "PIB_Habitant", "Accès à l'électricité (%)" = "Acces_Elec"))
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        value_box(title = textOutput("titre_pays_1"), value = textOutput("valeur_pays_1"), showcase = icon("flag"), theme = "info"),
        value_box(title = textOutput("titre_pays_2"), value = textOutput("valeur_pays_2"), showcase = icon("flag"), theme = "success")
      ),
      card(full_screen = TRUE, card_header("📈 Évolution comparative", class = "bg-primary text-white"), plotlyOutput("graphique_comparaison", height = "500px")),
      card(full_screen = TRUE, card_header("📋 Tableau comparatif", class = "bg-secondary text-white"), DTOutput("tableau_comparaison"))
    )
  ),
  
  #   ÉVOLUTION PAYS  
  nav_panel(
    title = "Évolution Pays", icon = icon("flag"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "🔍 Evolution Pays", bg = "#f8f9fa",
        selectInput("pays_focus", "Choisir un pays :", choices = pays_dispos, selected = "France"),
        checkboxGroupInput("variables_focus", "Indicateurs à étudier :", choices = c("Accès Electricité en %" = "Acces_Elec", "Part énergies renouvelables en %" = "Part_Renouvelable", "Emissiosn de CO2 en kt" = "Emissions_CO2", "PIB/habitants en $" = "PIB_Habitant"), selected = c("Acces_Elec", "Part_Renouvelable", "Emissions_CO2", "PIB_Habitant")),
        tags$hr(),
        selectInput("var_croissance", "Variable (Taux de croissance) :", choices = c("Accès à l'électricité" = "Acces_Elec", "Part Renouvelable" = "Part_Renouvelable", "Émissions de CO2" = "Emissions_CO2", "PIB par habitant" = "PIB_Habitant"), selected = "PIB_Habitant")
      ),
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        value_box(title = "Accès électricité en %", value = uiOutput("vb_elec"), theme = "success"),
        value_box(title = "Part des énergies renouvelables (%)", value = uiOutput("vb_renouv"), theme = "info"),
        value_box(title = "Émissions de CO2 (kt)", value = uiOutput("vb_co2"), theme = "warning"),
        value_box(title = "PIB par habitant ($)", value = uiOutput("vb_pib"), theme = "primary")
      ),
      card(card_header("📈 Évolution Temporelle", class="bg-primary text-white"), plotlyOutput("graphique_evolution", height = "450px")),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("🥧 Mix Électrique (Dernière année dispo)", class="bg-success text-white"), plotlyOutput("graphique_camembert", height = "450px")),
        card(card_header("📉 Taux de croissance annuel (%)", class="bg-danger text-white"), plotlyOutput("graphique_croissance", height = "450px"))
      )
    )
  ),
  
  #   BASE DE DONNÉES  
  nav_panel(
    title = "Base de données", icon = icon("table"),
    
    card(
      style = "overflow: visible !important;",
      layout_columns(
        col_widths = c(6, 3, 3),
        selectizeInput("filtre_pays_data", "Filtrer par Pays :", choices = c("Tous", pays_dispos), selected = "Tous", multiple = TRUE, options = list(dropdownParent = 'body')),
        div(style = "margin-top: 32px;", downloadButton("download_csv", "Télécharger CSV", class = "btn-success w-100")),
        div(style = "margin-top: 32px;", downloadButton("download_txt", "Télécharger TXT", class = "btn-info w-100"))
      )
    ),
    
    card(
      full_screen = TRUE,
      card_header("Tableau de données", class="bg-primary text-white"), 
      DTOutput("tableau_donnees")
    )
  )
)

# 4. SERVER
server <- function(input, output, session) {
  
  observeEvent(input$choix_continent, {
    if (input$choix_continent != "Tous") {
      proxy <- leafletProxy("carte_mondiale")
      region_poly <- carte_monde %>% filter(Continent_Fr == input$choix_continent)
      if (nrow(region_poly) > 0) {
        bbox <- st_bbox(region_poly)
        proxy %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
      }
    }
  })
  
  donnees_filtrees <- reactive({
    df <- data_energie %>% filter(Annee >= input$choix_annee[1] & Annee <= input$choix_annee[2])
    if(input$choix_continent != "Tous") df <- df %>% filter(Continent == input$choix_continent)
    
    df %>% group_by(Pays, ISO3, Continent) %>%
      summarise(across(c(Acces_Elec, Part_Renouvelable, Emissions_CO2, PIB_Habitant), ~mean(.x, na.rm = TRUE)), .groups = "drop")
  })
  
  output$valeur_moyenne <- renderText({
    df <- donnees_filtrees()
    if(nrow(df) == 0) return("N/A")
    round(mean(df[[input$choix_variable]], na.rm = TRUE), 1)
  })
  
  output$pays_top <- renderText({
    df <- donnees_filtrees() %>% arrange(desc(.data[[input$choix_variable]]))
    if(nrow(df) == 0) return("N/A")
    df$Pays[1]
  })
  
  output$valeur_max_ui <- renderUI({
    df <- donnees_filtrees()
    if(nrow(df) == 0) return("N/A")
    df_max <- df %>% arrange(desc(.data[[input$choix_variable]])) %>% slice(1)
    HTML(paste0("<b>", round(df_max[[input$choix_variable]], 1), "</b><br><small>", df_max$Pays, "</small>"))
  })
  
  output$valeur_min_ui <- renderUI({
    df <- donnees_filtrees()
    if(nrow(df) == 0) return("N/A")
    df_min <- df %>% arrange(.data[[input$choix_variable]]) %>% slice(1)
    HTML(paste0("<b>", round(df_min[[input$choix_variable]], 1), "</b><br><small>", df_min$Pays, "</small>"))
  })
  
  output$carte_mondiale <- renderLeaflet({
    req(input$choix_variable)
    donnees_carte <- carte_monde %>% left_join(donnees_filtrees(), by = c("iso_a3" = "ISO3"))
    valeurs <- donnees_carte[[input$choix_variable]]
    pal <- colorNumeric(palette = "viridis", domain = valeurs, na.color = "transparent")
    
    infobulles_carte <- sprintf("<strong>%s</strong><br/>Moyenne : %s",
                                ifelse(is.na(donnees_carte$Pays), donnees_carte$name_fr, donnees_carte$Pays),
                                ifelse(is.na(valeurs), "Aucune donnée", round(valeurs, 1))) %>% lapply(htmltools::HTML)
    
    leaflet(donnees_carte) %>% addProviderTiles("OpenStreetMap.France") %>% 
      addPolygons(fillColor = ~pal(valeurs), weight = 1, color = "white", fillOpacity = 0.8,
                  highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 1, bringToFront = TRUE),
                  label = infobulles_carte) %>%
      addLegend("bottomright", pal = pal, values = valeurs, title = "Légende")
  })
  
  output$graphique_barres <- renderPlotly({
    df_top <- donnees_filtrees() %>% arrange(desc(.data[[input$choix_variable]])) %>% head(input$nb_pays)
    p <- ggplot(df_top, aes(x = reorder(Pays, .data[[input$choix_variable]]), y = .data[[input$choix_variable]], fill = .data[[input$choix_variable]],
                            text = paste0("Pays : ", Pays, "<br>Valeur : ", round(.data[[input$choix_variable]], 1)))) +
      geom_col() + coord_flip() + scale_fill_viridis_c() + theme_minimal() + labs(x = "", y = "") + theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })
  
  output$graphique_nuage <- renderPlotly({
    p <- ggplot(donnees_filtrees(), aes(x = .data[[input$choix_var_x]], y = .data[[input$choix_variable]], color = Continent,
                                        text = paste0("Pays : ", Pays, "<br>X : ", round(.data[[input$choix_var_x]], 1), "<br>Y : ", round(.data[[input$choix_variable]], 1)))) +
      geom_point(alpha = 0.7, size = 3) + theme_minimal() + labs(x = labels_variables[[input$choix_var_x]], y = labels_variables[[input$choix_variable]]) + theme(legend.position = "bottom")
    
    if (input$choix_log_x) {
      p <- p + scale_x_log10()
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  output$graphique_boxplot <- renderPlotly({
    df <- donnees_filtrees() %>% filter(!is.na(Continent))
    type_y <- if (input$choix_variable %in% c("Emissions_CO2", "PIB_Habitant")) "log" else "linear"
    
    if(type_y == "log") {
      df <- df %>% filter(.data[[input$choix_variable]] > 0)
    }
    
    plot_ly(df, x = ~Continent, y = ~.data[[input$choix_variable]], type = "box",
            color = ~Continent, boxpoints = "outliers",
            hovertemplate = "<b>%{x}</b><br>%{y:.2f}<extra></extra>") %>%
      layout(showlegend = FALSE, 
             xaxis = list(title = ""),
             yaxis = list(title = labels_variables[input$choix_variable], type = type_y),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })
  
  donnees_comparaison <- reactive({
    data_energie %>% filter(Pays %in% c(input$pays_comp_1, input$pays_comp_2), Annee >= input$annee_comp[1], Annee <= input$annee_comp[2])
  })
  
  output$titre_pays_1 <- renderText({ paste(labels_variables[input$var_comp], "—", input$pays_comp_1) })
  output$titre_pays_2 <- renderText({ paste(labels_variables[input$var_comp], "—", input$pays_comp_2) })
  
  output$valeur_pays_1 <- renderText({ round(tail(donnees_comparaison() %>% filter(Pays == input$pays_comp_1) %>% pull(input$var_comp), 1), 1) })
  output$valeur_pays_2 <- renderText({ round(tail(donnees_comparaison() %>% filter(Pays == input$pays_comp_2) %>% pull(input$var_comp), 1), 1) })
  
  output$graphique_comparaison <- renderPlotly({
    p <- ggplot(donnees_comparaison(), aes(x = Annee, y = .data[[input$var_comp]], color = Pays, group = Pays,
                                           text = paste0(Pays, " (", Annee, ") : ", round(.data[[input$var_comp]], 1)))) +
      geom_line(linewidth = 1) + geom_point(size = 2) + theme_minimal() + labs(x = "", y = "") +
      theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text")
  })
  
  output$tableau_comparaison <- renderDT({
    donnees_comparaison() %>% 
      arrange(Pays, Annee) %>% 
      select(Pays, Annee, Acces_Elec, Part_Renouvelable, Emissions_CO2, PIB_Habitant, Continent) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, class = "cell-border stripe")
  })
  
  donnees_base_filtrees <- reactive({
    if ("Tous" %in% input$filtre_pays_data || is.null(input$filtre_pays_data)) data_energie else data_energie %>% filter(Pays %in% input$filtre_pays_data)
  })
  
  output$tableau_donnees <- renderDT({ 
    datatable(
      donnees_base_filtrees(), 
      options = list(
        pageLength = 50,                    
        scrollY = "calc(100vh - 300px)",    
        scrollX = TRUE,                     
        dom = 'lfrtip'                      
      ), 
      rownames = FALSE, 
      class = "cell-border stripe"
    ) 
  })
  
  output$download_csv <- downloadHandler(filename = function() { paste0("data_", Sys.Date(), ".csv") }, content = function(file) { write.csv(donnees_base_filtrees(), file, row.names = FALSE) })
  output$download_txt <- downloadHandler(filename = function() { paste0("data_", Sys.Date(), ".txt") }, content = function(file) { write.table(donnees_base_filtrees(), file, row.names = FALSE, sep = "\t", quote = FALSE) })
  
  donnees_pays_focus <- reactive({ data_energie %>% filter(Pays == input$pays_focus) %>% arrange(Annee) })
  
  generer_badge_ui <- function(df, nom_var) {
    val <- round(tail(df[[nom_var]], 1), 1)
    croissance <- calculer_croissance(df, nom_var)
    html_badge <- ""
    if (!is.na(croissance)) {
      classe_css <- if (croissance >= 0) "badge-hausse" else "badge-baisse"
      fleche <- if (croissance >= 0) "▲" else "▼"
      html_badge <- paste0("<div class='kpi-badge ", classe_css, "'>", fleche, " ", abs(round(croissance, 1)), "% vs an préc.</div>")
    }
    HTML(paste0("<div style='font-size: 1.5rem;'>", val, "</div>", html_badge))
  }
  
  output$vb_elec   <- renderUI({ generer_badge_ui(donnees_pays_focus(), "Acces_Elec") })
  output$vb_renouv <- renderUI({ generer_badge_ui(donnees_pays_focus(), "Part_Renouvelable") })
  output$vb_co2    <- renderUI({ generer_badge_ui(donnees_pays_focus(), "Emissions_CO2") })
  output$vb_pib    <- renderUI({ generer_badge_ui(donnees_pays_focus(), "PIB_Habitant") })
  
  output$graphique_evolution <- renderPlotly({
    noms_clairs <- c(
      "Acces_Elec" = "Accès à l'électricité (%)",
      "Part_Renouvelable" = "Part d'énergies renouvelables (%)",
      "Emissions_CO2" = "Émissions de CO2 (kt)",
      "PIB_Habitant" = "PIB par habitant ($)"
    )
    
    df_long <- donnees_pays_focus() %>% 
      select(Annee, all_of(input$variables_focus)) %>% 
      pivot_longer(-Annee) %>%
      mutate(name = noms_clairs[name])
    
    p <- ggplot(df_long, aes(x = Annee, y = value, color = name)) + 
      geom_line(linewidth = 1) + 
      facet_wrap(~name, scales = "free_y") + 
      theme_minimal() + 
      labs(x = "Années", y = "") + 
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$graphique_camembert <- renderPlotly({
    df <- donnees_pays_focus() %>% arrange(Annee) %>% tail(1)
    vals <- c(
      max(0, df$Elec_Fossile, na.rm = TRUE), 
      max(0, df$Elec_Renouvelable, na.rm = TRUE), 
      max(0, df$Elec_Nucleaire, na.rm = TRUE)
    )
    etiquettes <- c("Fossile", "Renouvelable", "Nucléaire")
    
    if(sum(vals) == 0) {
      return(plot_ly() %>% layout(title = "Pas de données sur le mix électrique", paper_bgcolor="transparent"))
    }
    
    plot_ly(labels = etiquettes, values = vals, type = 'pie', hole = 0.5,
            marker = list(colors = c("#e74c3c", "#27ae60", "#f39c12"),
                          line = list(color = "#ffffff", width = 2)),
            textinfo = "label+percent",
            hovertemplate = "<b>%{label}</b><br>%{value:.1f} TWh<extra></extra>") %>%
      layout(showlegend = TRUE, legend = list(orientation = "h", x = 0.1, y = -0.1),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })
  
  output$graphique_croissance <- renderPlotly({
    var <- input$var_croissance
    df <- donnees_pays_focus() %>% arrange(Annee) %>% drop_na(all_of(var))
    
    if(nrow(df) < 2) return(plot_ly() %>% layout(title = list(text = "Pas assez de données"), paper_bgcolor="transparent"))
    
    df_croiss <- df %>% mutate(Croissance = (.data[[var]] - lag(.data[[var]])) / abs(lag(.data[[var]])) * 100) %>% drop_na(Croissance)
    
    plot_ly(df_croiss, x = ~Annee, y = ~Croissance, type = "bar",
            marker = list(color = ifelse(df_croiss$Croissance >= 0, "#27ae60", "#e74c3c")),
            hovertemplate = "Année: %{x}<br>Croissance: %{y:.2f}%<extra></extra>") %>%
      layout(xaxis = list(title = "", showgrid = FALSE),
             yaxis = list(title = "Croissance (%)", gridcolor = "#e8ecf0",
                          zeroline = TRUE, zerolinecolor = "#333", zerolinewidth = 1.5),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })
}

shinyApp(ui, server)
library(rsconnect)
rsconnect::deployApp('path/to/your/app')