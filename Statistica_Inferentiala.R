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
### Statistica inferențială a datelor privind producțiile cinematografice ###
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
###	  I. Corelația intre rating și buget                           ###	
###	  II. Relatie dintre scorul filmului si genul filmului         ###	
#######################################################################



setwd("D:/LucrareLicenta/1.ShinyApp/1.IMDb-final-app/")
getwd()
movies_EDA <- read_csv("data/movies.csv")
glimpse(movies_EDA)

#################################################################
## Pregătirea datelor pentru analiză ## 
#################################################################

## Comasare date Versiunea 1 pentru genurile de filme
movies_EDA <- movies_EDA %>%
  mutate(genre = if_else(genre == 'Music' | genre == 'Musical', 'Music', genre),
         genre = if_else(genre == 'Reality-TV' | genre == 'Talk-Show', 'Reality-TV', genre),
         genre = if_else(genre == 'Biography' | genre == 'Documentary', 'Documentary', genre),
         genre = if_else(genre == 'Crime' | genre == 'Horror', 'Horror', genre),
         genre = ifelse(tolower(genre) == 'sci-fi', 'SF', genre),
         genre = if_else(genre == 'Fantasy', 'SF', genre),
         genre = if_else(genre == 'Crime', 'Horror', genre),
         genre = if_else(genre == 'War'| genre == 'Western', 'History', genre),
         genre = if_else(genre == 'Film-Noir', 'Drama', genre),
         genre = if_else(genre == 'Adult' | genre =='Game-Show' | genre=='Reality-TV' | genre=='Short' | genre=='Sport', 'Other-genre', genre))


#######################################################################
###       Problema de cercetare nr 1: 
###       Exista vreo relatie intre rating si buget? 
#######################################################################

##  Pentru a verifica semnificatia statistica a corelatiei dintre cele doua variabile vom folosi
##  un test de corelatie
##  Testul de corelatie are ca ipoteza nula (H0) ca cele doua variabile nu sunt corelate
##  in functie de distributia variabilelor, testul poate fi parametric sau neparametric

## mai intai testam daca distributia variabilei rating este normala 
## ipoteza nula H0 a testului de normalitate este ca distributia este normala

shapiro.test(sample( movies_EDA$score, 5000))
##Intrucat p-value < 2.2e-16 este mult mai mic decat pragul de semnificatie de 0.05,
##Ipoteza nula este respinsa, Astfel spus, distributia variabilei nu este una normala
## In consecinta, testul de corelatie folosit va fi neparametric


# coeficient de corelaţie neparametric
ggscatterstats(
  data = movies_EDA,
  x = budget,
  y = score,
  type = "np"
)

## Intrucat p-value = 0.92, o valoare cu mult peste pragul de semnificatie de 0,05, rezulta ca
## ipoteza nula H0 a testului de corelatie nu poate fi respinsa. Prin urmare, scorul filmului nu este asociat
## cu bugetul



#######################################################################
### Problema de cercetare nr 2:  
### Exista vreo relatie intre scorul filmului si genul filmului? 
#######################################################################

## Intrucat am vazut ca scorul filmului nu are o distributie normala, vom folosi un test neparametric
## Ipoteza nula a testului Kruskal-Wallis (H0) este ca scorul filmului nu prezinta 
## diferente semnificative in functie de genul filmului (variabilele scor si gen sunt independente)

ggbetweenstats(
  data = movies_EDA,
  x = genre,
  y = score,
  plot.type = "boxviolin",
  type = "np"
)

## p-value 2.81*10^-166. Aceasta valoare este mult mai mica decat pragul de semnificatie de 0.05
## de aceea ipoteza nula este respinsa. Cu alte cuvinte, scorul este asociat cu genul filmului
## marimea efectului este (epsilon^2) 0.11 ceea ce inseamna un efect slab spre mediu
## In concluzie, asocierea dintre gen si scor este semnificativa dpdv statistic, insa relativ slaba

#####################
lm1<- lm(score ~ budget+runtime + genre + factor(year),data=movies_EDA)
summary(lm1)