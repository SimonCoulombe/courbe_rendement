# fait ctrl-shift-O pour afficher une genre de table des matères à droite
# les commentaires sont indiqués avec un # au début des la ligne
# les lignes commentées se terminant par ----- apparaissent dans la tables des matières.


# installation des packages (faire seulement une fois) -----
# install.packages("here")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("janitor")
# install.packages("readr")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("remotes")
# remotes::install_github("mountainmath/canbank")
# remotes::install_github("mountainmath/cansim")


# chargement des libraries en mémoire -----
library(here)     # pour facilement référrer au répertoire "racine" du projet avec here::here()
library(readr)   # pour lire les csv en mémoire avec readr::read_csv()
library(janitor) # pour donner des noms corrects aux colonnes avec janitor::clean_names()  
library(dplyr)   # pour manipuler les datas (créer une nouvelle colonne avec mutate(), filtrer() les lignes avec filter, choisir les colonnes qu'on veut garder avec select() , générer des données aggrégées avec group_by() puis summarise() ou mutate()
library(stringr) # pour manipuler les caractères
library(lubridate) # pour manipuler les dates  
library(tidyr)   # pour remplir les données vers le bas avec tidyr::fill() et pour créer une table longue avec tidyr::pivot_longer()
library(ggplot2)  # pour faire des graphiques 
library(canbank)  # pour facilement télécharger des séries disponibles sur l'API de la banque du canada.   canbank::list_boc_series()  retourne la description de toutes les séries disponibles.
library(cansim)   # pour facilement télécharger les vecteurs de cansim.    cansim::list_cansim_cubes()  retourne la description de toutes les tables disponibles.

# quelques options pour mes graphiques parce que je suis coquet   -----

ggplot2::theme_set(theme_minimal()) # this ggplot2 theme uses roboto condensed font, which works well with the font used for the whole document.
options(ggplot2.discrete.fill  = function() scale_fill_viridis_d() )
options(ggplot2.continuous.fill  = function() scale_fill_viridis_c())
options(ggplot2.discrete.colour = function() scale_color_viridis_d())
options(ggplot2.continuous.colour = function() scale_color_viridis_c())

# charger yield curve en mémoire et les nettoyer ----
#https://www.bankofcanada.ca/rates/interest-rates/bond-yield-curves/
data_yield_curve <- read_csv("yield_curves.csv") %>%  # lire le csv  
  janitor::clean_names()  %>% # nettoyer noms de colonnes   
  select(date, starts_with("zc"))%>%  # garder seulement les colonnes "dates" ainsi que celles qui commencent par zc  
  mutate(across(starts_with("zc"), as.numeric))  %>%  # convertir toutes les variables qui commencent par zc en numérique   
  pivot_longer(cols = starts_with("zc"), names_to = "duration", values_to = "yield") %>%  # "allonger la table"    
  mutate(duration = as.numeric(str_replace_all(duration, "[^0-9]", ""))/100) %>%  # créer une variable duration numérique en gardant seulement les chiffres dans les mots ZCxxxYR" 
  group_by(date) %>%   # pour chaque date 
  mutate(at_least_1_not_na = max(!is.na(yield))) %>% # je regarde si j'ai au moins une valeur pas manquante  pour une des 120 durations
  ungroup() %>%
  filter(at_least_1_not_na == 1) %>% # je garde juste les dates pour lesquelles j'Ai au moins une valeur pas manquante 
  select(-at_least_1_not_na)  # jeter la colonne at_least_1_not_na, j'en ai plus besoin

readr::write_csv(data_yield_curve, "data_yield_curve.csv")    # domber le data dans un csv pour usage futur.

# alternativement, chargé le yield data pré-nettoyé   -----
#data_yield_curve <- read_csv("clean_yield_data.csv")


# plot yield curve données les plus récentes
data_yield_curve %>% 
  filter(date == max(date)) %>% 
  ggplot(aes(x=duration, y = yield)) + 
  geom_point() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(
    title = glue::glue("Yield curve for {max(clean_yield_data$date)}"),
    x = "Duration (years)",
    y = "Yield (%)")

# charger  target rate  Bank of canada ----
# Je vais les chercher dans la table "10-10-0139"
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1010013901
# on pourrait aller chercher le csv là-bas, le télécharger et ensuite le charger en mémoire avec read_csv()
# mais à la place j'utilise le package {cansim} pour me connecter directement à statcan et pomper la data


# note j'ai essayé le package canbank pour pomper des données de la banque du canada, mais quand je cherche le mot target dans les séries disponibles il n'Y a rien de prometteur.
# sauf peut être STATIC_ATABLE_V39079               Overnight rate target (end of month) Overnight rate target (end of month)  
#canbank::list_boc_series() %>% filter(stringr::str_detect(description, "target"))



data_taux_directeur <- cansim::get_cansim("10-10-0139") %>%  # on pompe le data de statcan pour le tableau 10-10-0139
  janitor::clean_names() %>%  # on nettoie les noms de colonnes
  dplyr::filter(financial_market_statistics == "Target rate") %>%  # on ne garde que les lignes pour target rate (il y a plein d'autres indicateurs que je ne veux pas)
  tidyr::fill(value, .direction = "down") %>%   # on remplace les valeurs manquantes par la date précédente.  parce que les jours fériés ne sont pas populés on dirait
  dplyr::filter(!is.na(value)) %>%   # les premières dates n'ont pas de valeur car le taux directeur d'existait pas dans ces dates là.. on jette donc les lignes qui n'ont toujours pas de valeur
  dplyr::mutate(date = lubridate::ymd(ref_date),  # je crée une variable de classe date plutôt que charactère 
                pct = value / 100)   # je convertie les chiffre en pourcentage.

# better plot taux directeur
data_taux_directeur %>% 
  ggplot(aes(x= date, y = pct)) + 
  geom_line()  + 
  theme_minimal() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(
    title = "taux directeur canada",
    subtitle = "subtitle",
    caption = "made with love by Simon using the mountainmath/cansim package ",
    y = "Taux directeur (%)",
    x =  "Date"
  )

