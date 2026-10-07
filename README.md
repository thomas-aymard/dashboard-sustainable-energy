# 🌍 Tableau de Bord - Transition Énergétique Mondiale

Application web interactive développée en **R (Shiny / Bslib)** dans le cadre du BUT Science des Données (IUT Paris Rives de Seine). Ce projet analyse l'évolution de la transition écologique à travers le monde sur plusieurs décennies.

🔗 **Accès direct au Live Demo :** [Tester l'application en ligne](https://reportingr.shinyapps.io/dashboard-energy/)

---

### 📊 Fonctionnalités du Dashboard
* **Accueil & Contexte :** Présentation des sources de données et des enjeux de la transition énergétique.
* **Carte Mondiale Interactive (`Leaflet`) :** Visualisation géographique des indicateurs clés par pays et par région.
* **Analyses & Graphiques dynamiques (`Plotly`) :** Top des pays, nuages de points croisant le PIB et les émissions de CO2, et boxplots par continent.
* **Comparaison directe :** Outil de benchmark comparant l'évolution de deux pays côte à côte.
* **Suivi par pays & Mix électrique :** Analyse détaillée de l'évolution temporelle, taux de croissance annuel et parts de production (fossile, renouvelable, nucléaire).
* **Exploration & Export :** Tableau de données complet filtrable avec options de téléchargement direct (`CSV` et `TXT`).

---

### 🛠️ Technologies utilisées
* **Langage :** R
* **Framework Web :** Shiny, Bslib (Thème Bootstrap 5 / Zephyr)
* **Manipulation de données :** Tidyverse (`dplyr`, `tidyr`)
* **Visualisation & Cartographie :** Plotly, Leaflet, SF, RNaturalEarth
* **Outils d'aide :** Countrycode
