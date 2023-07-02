############################################################################
###                   Universitatea Alexandru Ioan Cuza, Iași           ###
###               Facultatea de Economie și Administrarea Afacerii      ###
###                        Informatică Economică                        ###
############################################################################
###
############################################################################
###      Lucrare de finalizare a studiilor de licență, iulie 2023       ###
############################################################################
###
############################################################################
###  Analiza exploratorie a datelor privind producțiile cinematografice ###
############################################################################
###
############################################################################
###            Detalii privind analiza datelor                          ###
############################################################################
#      Setul de date utilizat în cadrul analizei: 
#     https://www.kaggle.com/datasets/komalkhetlani/imdb-dataset
############################################################################


#######################################################################
###	                              Agenda                           ###	
#######################################################################
###	  I. EDA utilizând pachetul `tidyverse`                         ###	
###	  II. EDA utilizând pachetul `DataExplorer`                     ###	
###	  III. EDA utilizând pachetul `inspectdf`                       ###	
#######################################################################



# install.packages("ggplot2")  
# install.packages("purrr")
# install.packages("dplyr")
# install.packages("ggstatsplot")


library(dplyr)
library(purrr)
library(ggplot2)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tibble)
library(odbc)
library(DBI)
library(patchwork)
library(ggstatsplot)
library(scales)


###  Setează directorul de lucru curent în R 
###  Încarcă în mediu datele salvate intr-un fisier de tip R 
setwd('D:/LucrareLicenta/BD/Final_DB/Kaggle_IMDB_Data')
load('kaggle.RData')

###  Tabela utilizată în procesul de analiză
complete_movies <- inner_join(movie_actors, movies_actor_titles, by = c("nconst" = "nconst")) %>%
  inner_join(movies_basics, by = c("movie_id" = "tconst")) %>%
  inner_join(movies_directors, by = c("movie_id" = "tconst"), relationship = "many-to-many") %>%
  inner_join(movies_genres, by = c("movie_id" = "tconst"), relationship = "many-to-many")%>%
  inner_join(movies_ratings, by = c("movie_id"= "tconst")) 

###  Reducerea setului de date la 10.000 de înregistrări 
complete_movies_sample <- complete_movies %>%
  sample_n(10000)


### Salvarea setului de date în directorul specificat
save(data, file = "complete_movies_sample.Rdata")
setwd('D:/LucrareLicenta/BD/Final_DB/Kaggle_IMDB_Data')
load('complete_movies_sample.Rdata')



#######################################################################
###	          I. EDA utilizând pachetul `tidyverse`                 ###	
#######################################################################


#################################################################
## Pregătirea datelor pentru analiză ## 
#################################################################

## Comasare date Versiunea 1 pentru genurile de filme
sample_movies <- complete_movies_sample %>%
  mutate(genre = if_else(genre == 'Music' | genre == 'Musical', 'Music', genre),
         genre = if_else(genre == 'Reality-TV' | genre == 'Talk-Show', 'Reality-TV', genre),
         genre = if_else(genre == 'Biography' | genre == 'Documentary', 'Documentary', genre),
         genre = if_else(genre == 'Crime' | genre == 'Horror', 'Horror', genre),
         genre = ifelse(tolower(genre) == 'sci-fi', 'SF', genre),
         genre = if_else(genre == 'Fantasy', 'SF', genre),
         genre = if_else(genre == 'Crime', 'Horror', genre),
         genre = if_else(genre == 'War'| genre == 'Western', 'History', genre),
         genre = if_else(genre == 'Film-Noir', 'Drama', genre),
         genre = if_else(genre == 'Adult' | genre =='Game-Show' | genre=='Reality-TV' | genre=='Short' | genre=='Sport', 'Other-genre', genre),
         titleType=if_else(titleType=='Short'| titleType=='tvMiniSeries' | titleType=='tvShort', 'tvMiniSeries', titleType ),
         titleType=if_else(titleType=='tvMovie', 'movie', titleType ),
         titleType=if_else(titleType=='videoGame', 'video', titleType ))

##Comasare date Versiunea 2
df <- complete_movies_sample %>%
  mutate(genre = case_when(
    genre %in% c('Music', 'Musical') ~ 'Music',
    genre %in% c('Reality-TV', 'Talk-Show', 'News') ~ 'Reality-TV',
    genre %in% c('Biography', 'Documentary') ~ 'Documentary',
    genre %in% c('Fantasy', 'Sci-fi') ~ 'SF',
    genre %in% c('Crime', 'Horror') ~ 'Horror',
    genre %in% c('War', 'Western', 'History') ~ 'History',
    genre %in% c('Film-Noir', 'Drama') ~ 'Drama',
    TRUE ~ genre
  ))


#################################################################
## Cerința 1: Afișați numărul de valori lipsă pentru fiecare variabilă ## 
#################################################################
## Identificarea valorilor nule 

missing_vals <- sample_movies %>%
  map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
  tibble(variable = names(.), n_missing = .) %>%
  mutate (percent_missing = round(n_missing * 100 / 
                                    nrow(sample_movies), 2))

## Reprezentare grafică
ggplot(missing_vals, 
       aes (x = variable, y = n_missing, fill = variable)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(percent_missing, '%'), 
                hjust = if_else(percent_missing > 3, 1.02, -0.03), 
                vjust = 0.5), size = 4 ) +
  theme(legend.position="none") + # this will remove the legend
  scale_y_continuous(limits = c(0,170), breaks = seq(0, 150, 10)) 


#################################################################
## Cerința 2: Afișați frecvența (și procentul) valorilor pentru 
##                fiecare variabilă caracter/factor  ##
#################################################################

glimpse(sample_movies)

## Se calculează frecvențele pentru fiecare variabilă și valoare categorică
eda_factors <- sample_movies %>%
  select(-nconst, -primaryName, -knownForTitles, -movie_id, director_id)%>%
  mutate_if(is.factor, as.character) %>%
  select_if(., is.character ) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(sample_movies),2)) %>%
  arrange(variable, value)
View(eda_factors)

glimpse(eda_factors)

test <- eda_factors %>%
  filter (is.na(value))

test <- eda_factors %>%
  filter (value == 'N/A')


## Reprezentare grafică numai pentru factorii cu mai puțin de 50 de valori distincte
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (n_of_values <= 50) %>%    
  ungroup() %>%
  select (variable) %>%
  inner_join(eda_factors) %>%
  ggplot(., aes(x = value, y = n_value, fill = value)) +
  geom_col() +
  geom_text (aes(label = paste0(round(percent,0), '%'),
                 vjust = if_else(n_value > 500, 1.5, -0.5)), size = 3) +
  facet_wrap(~ variable, scale = "free") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(strip.text.x = element_text(size = 13)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')

#################################################################
## Cerința 3: Afișarea distribuției (sub formă de histograme,         ##
## diagrame de densitate și boxplot) ale fiecărei variabile numerice      ##
#################################################################
glimpse(sample_movies)

num_variables <- sample_movies %>%
  mutate(
    numVotes = suppressWarnings(as.numeric(numVotes)),
    startYear = suppressWarnings(as.numeric(startYear)),
    endYear = suppressWarnings(as.numeric(endYear)),
    runtimeMinutes = suppressWarnings(as.numeric(runtimeMinutes)),
    birthYear = suppressWarnings(as.numeric(birthYear))
  ) %>%
  select(where(is.numeric) & !isAdult) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value")
View(num_variables)

## histograme separate pentru fiecare valoare numerică; scară liberă %>%
  ggplot(., aes(x = value, fill = variable)) +
  geom_histogram() +
  facet_wrap(~ variable, scale = "free") +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size = 9)) +
  theme(strip.text.x = element_text(size = 12)) +
  xlab("") + ylab("frequency") 


## boxplot (scara comună) 
num_variables %>%
  filter(variable=='numVotes')%>%
  drop_na(value) %>%
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, nrow = 1) +
  theme(legend.position = 'none') +
  xlab("") +
  ylab("value") +
  theme(axis.text.x = element_blank()) +
  scale_y_continuous(breaks = seq(0, 10000000, 100000))

##
num_variables %>%
    filter(variable!='numVotes')%>%
  # select(-numVotes, -deathYear, -endYear)%>%
   drop_na(value) %>%
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, nrow = 1, scales ="free") +
  theme(legend.position = 'none') +
  xlab("") +
  ylab("value") +
  theme(axis.text.x = element_blank()) +
    scale_y_continuous(breaks = seq(0, 100000, 100))
## 
num_variables %>%
  select(-numVotes, -deathYear, -endYear, -startYear) %>%
  drop_na() %>%
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, nrow = 1, scales = "free") +
  theme(legend.position = 'none') +
  xlab("") +
  ylab("value") +
  theme(axis.text.x = element_blank()) +
  scale_y_continuous(breaks = seq(0, 100000, 100))



#################################################################
## Cerința 4: Afișarea distribuției (sub formă de histograme,         ##
## diagrame de densitate și boxplot)                                  ##
#################################################################
glimpse(sample_movies)

df <- sample_movies %>%
  mutate(
    numVotes = suppressWarnings(as.numeric(numVotes)),
    startYear = suppressWarnings(as.numeric(startYear)),
    endYear = suppressWarnings(as.numeric(endYear)),
    runtimeMinutes = suppressWarnings(as.numeric(runtimeMinutes)),
    birthYear = suppressWarnings(as.numeric(birthYear))
  ) %>%
  transmute (averageRating, 
             genre) %>%
  mutate (id = row_number()) %>%
  pivot_longer(c(-id, -genre),
               names_to = "variable", values_to = "value" ) %>%
  arrange(desc(value))
View(df)


## histogramă separată pentru fiecare valoare numerică; scară liberă
df %>%
  ggplot(., aes(x = value, fill = variable)) +
  geom_histogram() +
  facet_wrap(. ~ variable + genre, ncol = 4) +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size = 8)) +
  theme(strip.text.x = element_text(size = 8)) +
  xlab("") + ylab("frequency")+
  ylim(c(0, 15))

## histogramă fără valori de tipul NA
df %>%
  drop_na(value) %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(. ~ variable + genre, ncol = 10) +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size = 8)) +
  theme(strip.text.x = element_text(size = 8)) +
  xlab("") +
  ylab("frequency")+
ylim(c(0, 15))


## # trasează curbele de densitate suprapuse
df %>%
  ggplot(., aes(x = value, fill = genre, color = genre)) +
  geom_density(aes(alpha = 0.1)) +
  facet_wrap(. ~ variable, scales = "free") +
  theme(axis.text.x = element_text(size = 8)) +
  theme(strip.text.x = element_text(size = 12)) +
  xlab("") + ylab("frequency") 

#################################################################
## Cerința 5: Afișează corelația dintre variabilele numerice ##
## cu pachetul `corrr` 
#################################################################


## afișare corelații
temp <- sample_movies %>%
  select_if(is.numeric) %>%
  corrr::correlate()  
View(temp)


## o privire mai bună a corelațiilor
temp <- sample_movies %>%
  select_if(is.numeric) %>%
  corrr::correlate() %>%
  corrr::focus(averageRating, numVotes, mirror = TRUE) %>%  
  corrr::rearrange() %>%  # rearanjați după corelații
  corrr::shave() 
View(temp)


## diagrama de corelare
sample_movies %>%
  select_if(is.numeric) %>%
  corrr::correlate() %>%
  corrr::rplot()


## network plot
sample_movies %>%
  select_if(is.numeric) %>%
  corrr::correlate() %>%
  network_plot(min_cor = .2)




#######################################################################
###	          II. EDA utilizând pachetul `DataExplorer`            ###	
#######################################################################

 install.packages('DataExplorer') 

# If this does not work (in December 2020) `DataExplorer` package was 
# not available  on CRAN, so it could be installed only with:
if (!require(devtools)) 
  install.packages("devtools")
#devtools::install_github("boxuancui/DataExplorer", force = TRUE)
# install.packages('DataExplorer')
library(DataExplorer)

#######################################################################
### NUMERIC DATA
sample_movies <- na.omit(sample_movies) 

sample_movies <- sample_movies %>%
  mutate(
    numVotes = as.numeric(numVotes),
    startYear = as.numeric(startYear),
    endYear = as.numeric(endYear),
    runtimeMinutes = as.numeric(runtimeMinutes),
    birthYear = as.numeric(birthYear)
  )

##Informatii de baza in legatura cu tipul de date si valorile lipsa

# ...ca text...
temp <- DataExplorer::introduce(sample_movies)
View(temp)

# ... și grafic
plot_intro(temp)


## Valori lipsă 
plot_missing(sample_movies)


#  Grafică informații despre variabilele categoriale/factoriale
DataExplorer::plot_bar(sample_movies)

sample_movies %>%
  select (genre, numVotes) %>%
  plot_bar()



# Trasează histograma pentru toate variabilele numerice
DataExplorer::plot_histogram(sample_movies)


# Trasează curbele de densitate pentru toate variabilele numerice
DataExplorer::plot_density(sample_movies)


## Plot the boxplots of variables `cty_l100km`, 
#  `hwy_l100km`, `Air Pollution Score` and  `Greenhouse Gas Score`,
#  #   grouped on `Veh Class` 

## Trasează diagramele cu casete ale variabilelor `genre`,
# `genre`, `runtimeMinutes` și `averageRating`,
## grupate pe „genre”

sample_movies %>%
  transmute (genre, runtimeMinutes, 
             averageRating ) %>%
  DataExplorer::plot_boxplot(., by = "genre")





## Graficul corelațiilor dintre variabilele numerice
glimpse(sample_movies)
sample_movies

sample_movies %>%
  # mutate (Displ = as.numeric(Displ), Cyl = as.integer(Cyl)) %>%
  select_if(., is.numeric ) %>%
  na.omit(.) %>%
  DataExplorer::plot_correlation()


#Crearea unui Raport de tipul report.html 
config <- configure_report(
  add_plot_str = TRUE,
  add_plot_qq = TRUE,
  add_plot_prcomp = TRUE
)

getwd()
create_report(sample_movies %>% na.omit(.), config = config)



#######################################################################
###	              III. EDA utilizând pachetul `inspectdf`           ###	
#######################################################################
library(inspectdf)


##  Afișare tipuri de date ca text....
temp <- inspectdf::inspect_types(sample_movies)

#... și ca diagramă
inspect_types(sample_movies) %>%
  show_plot ()


## Afișați dimensiunea fiecărei coloane
inspect_mem(sample_movies) %>%
  show_plot ()


## Valori lipsă
inspect_na(sample_movies) %>%
  show_plot ()


# Grafică informații despre variabilele categoriale/factoriale,
# afișând doar valoarea predominantă
inspect_imb(sample_movies) %>%
  show_plot ()


# Grafică informații despre variabilele categoriale/factoriale,
# afișează informații detaliate
inspect_cat(sample_movies) %>%
  show_plot ()



# Trasează histograma pentru toate variabilele numerice
inspect_num(sample_movies) %>%
  show_plot ()


# Graficul corelațiilor dintre variabilele numerice
sample_movies %>%
  # mutate (Displ = as.numeric(Displ), Cyl = as.integer(Cyl)) %>%
  select_if(., is.numeric ) %>%
  na.omit(.) %>%
  inspect_cor(.) %>%
  show_plot ()


