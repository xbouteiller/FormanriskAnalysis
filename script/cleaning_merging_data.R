# script 2
# Creating a clean data frame 

cat(
"
This script is for creating nice and cleaned data containing all informations : p50, pop info, climatic informations
"
)

## set data directory
setwd("~/Documents/research/FORMANRISK/data/data_formanrisk")

## library
library('tidyverse')
library('broom')
library('lme4')
library('lmerTest')
library('gapminder')
library('CARS')
library("dismo")
library('nlme')
library('lme4')
library("lmerTest")

## Data frame

env = read.table("varenv.csv",  h= T, sep = ';')
str(env)

individual = read.table("individual.csv", h=T, sep = ';')
str(individual)

pop = read.table("pop_result.csv", h=T, sep = ';')
str(pop)

## cleaning pop name 
env2 = env  %>% 
  mutate(site = as.factor(as.character(recode(site, "biscarosse"="biscarrosse", 
                                              "la_teste"="la teste", 
                                              "lm"="lit et mixe", 
                                              "mz"="mimizan", 
                                              "spain continental dune"="spain dune", 
                                              "spain branas"="branas", 
                                              "orz"="orzaduero", 
                                              "pinarest llanos"="llanos",
                                              "Pierroton"="pierroton", 
                                              "Duna Continental"="spain dune", 
                                              "Orzaduero"="orzaduero", 
                                              "PinaresLlanos"="llanos", 
                                              "Mingorría"="", 
                                              "Perpignan - Mas Palegrí"="perpignan", 
                                              "Brañas Verdes"="branas", 
                                              "Seirós (Ribeira de Pena) "="ribeira", 
                                              "Leiria (Marinha Grande) "="leiria", 
                                              "Seirós (Ribeira de Pena)"="ribeira", 
                                              "Leiria (Marinha Grande)​"="leiria", 
                                              "Oín"="oin", 
                                              "Barroqueiras-San Vicente do Mar"="san vicente", 
                                              "Biscarrosse"="biscarrosse", 
                                              "Lit_et_Mixe"="lit et mixe", 
                                              "Mimizan"="mimizan",
                                              'Forêt domaniale de Cerbère'='cerbere',
                                              'Céret (Le Palau)'= 'ceret',
                                              'oin_es'='oin_es',
                                              'oin_fr'='oin_fr',
                                              'oin_es_leiria'= 'oin_P','
                                              hourtin' = 'hourtin',
                                              'la_teste' = 'la_teste'
  ) )))


indiv2 = individual %>% 
  mutate(site = as.factor(as.character(recode(site, "biscarosse"="biscarrosse", 
                                              "la_teste"="la teste", 
                                              "lm"="lit et mixe", 
                                              "mz"="mimizan", 
                                              "spain continental dune"="spain dune", 
                                              "spain branas"="branas", 
                                              "orz"="orzaduero", 
                                              "pinarest llanos"="llanos",
                                              "Pierroton"="pierroton", 
                                              "Duna Continental"="spain dune", 
                                              "Orzaduero"="orzaduero", 
                                              "PinaresLlanos"="llanos", 
                                              "Mingorría"="", 
                                              "Perpignan - Mas Palegrí"="perpignan", 
                                              "Brañas Verdes"="branas", 
                                              "Seirós (Ribeira de Pena) "="ribeira", 
                                              "Leiria (Marinha Grande) "="leiria", 
                                              "Seirós (Ribeira de Pena)"="ribeira", 
                                              "Leiria (Marinha Grande)​"="leiria", 
                                              "Oín"="oin", 
                                              "Barroqueiras-San Vicente do Mar"="san vicente", 
                                              "Biscarrosse"="biscarrosse", 
                                              "Lit_et_Mixe"="lit et mixe", 
                                              "Mimizan"="mimizan",
                                              'Forêt domaniale de Cerbère'='cerbere',
                                              'Céret (Le Palau)'= 'ceret',
                                              'oin_es'='oin_es',
                                              'oin_fr'='oin_fr',
                                              'oin_es_leiria'= 'oin_P',
                                              'hourtin' = 'hourtin',
                                              'la_teste' = 'la_teste'
  ))))


pop2 = pop %>% 
  mutate(site = as.factor(as.character(recode(site, "biscarosse"="biscarrosse", 
                                              "la_teste"="la teste", 
                                              "lm"="lit et mixe", 
                                              "mz"="mimizan", 
                                              "spain continental dune"="spain dune", 
                                              "spain branas"="branas", 
                                              "orz"="orzaduero", 
                                              "pinarest llanos"="llanos",
                                              "Pierroton"="pierroton", 
                                              "Duna Continental"="spain dune", 
                                              "Orzaduero"="orzaduero", 
                                              "PinaresLlanos"="llanos", 
                                              "Mingorría"="", 
                                              "Perpignan - Mas Palegrí"="perpignan", 
                                              "Brañas Verdes"="branas", 
                                              "Seirós (Ribeira de Pena) "="ribeira", 
                                              "Leiria (Marinha Grande) "="leiria", 
                                              "Seirós (Ribeira de Pena)"="ribeira", 
                                              "Leiria (Marinha Grande)​"="leiria", 
                                              "Oín"="oin", 
                                              "Barroqueiras-San Vicente do Mar"="san vicente", 
                                              "Biscarrosse"="biscarrosse", 
                                              "Lit_et_Mixe"="lit et mixe", 
                                              "Mimizan"="mimizan",
                                              'Forêt domaniale de Cerbère'='cerbere',
                                              'Céret (Le Palau)'= 'ceret',
                                              'oin_es'='oin_es',
                                              'oin_fr'='oin_fr',
                                              'oin_es_leiria'= 'oin_P',
                                              'hourtin' = 'hourtin',
                                              'la_teste' = 'la_teste'
  ))))

## test

### test
if(all(levels(env2$site) %in% levels(indiv2$site))){
  print('ok all pops are here')
}else{
  print('error')
  print(levels(env2$site)[!(levels(env2$site) %in% levels(indiv2$site))])
}

### test
if(all(levels(env2$site) %in% levels(pop2$site))){
  print('ok all pops are here')
}else{
  print('error')
  print(levels(env2$site)[!(levels(env2$site) %in% levels(indiv2$site))])
}



## join 
ind_join = indiv2 %>% full_join(env2, by='site')
pop_join = pop2 %>% full_join(env2, by='site')
### test
if(all(levels(ind_join$site) %in% levels(indiv2$site))){
  print('ok all pops are here')
}else{
  print('error')
  print(levels(env2$site)[!(levels(env2$site) %in% levels(indiv2$site))])
}

## save table
write.table(ind_join, 'individual_join.csv', sep=";", row.names = FALSE)
write.table(pop_join, 'pop_join.csv', sep=";", row.names = FALSE)
