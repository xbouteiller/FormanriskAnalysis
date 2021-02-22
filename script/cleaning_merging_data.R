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

raw = read.table("rawdata.csv", h=T, sep = ';')
str(raw)

sampl = read.table("sample.csv", h=T, sep = ';')
str(sampl)

################################################################################################################################
#### New pop extracted by Sylvain
site_new_i= read.table("new_pop_info.csv",  h= T, sep = ';')

site_new_v= read.table("new_pop_value.csv",  h= T, sep = ',')
site_new_v$site = as.character(site_new_v$site)


#### rename pop from number to character
for(i in seq(1,nrow(site_new_i))){
  num = site_new_i[i,'pop_number']
  popname = site_new_i[i,'site']
  site_new_v[site_new_v$site == num, 'site'] = as.character(popname)
}
site_new_v

str(site_new_v)
site_new_v$P50 = -1*site_new_v$P50
#### check col names
colnames(site_new_v) %in% colnames(sampl)

#### merge
sampl = merge(sampl, site_new_v, by = colnames(site_new_v), all = TRUE)
################################################################################################################################

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






raw2 = raw %>% 
  mutate(site = as.factor(as.character(recode(Sampling_location, "biscarosse"="biscarrosse", 
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

sampl2 = sampl %>% 
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



### Add REP

REP = c()
for(i in str_split(sampl2$code, " ")){ 
  REP = append(REP, as.integer(tail(i,1)))
  }
print(REP)
sampl2$REP = REP
sampl2$REP[is.na(sampl2$REP)] = 1
print(sampl2$REP)

### test
if(all(levels(env2$site) %in% levels(sampl2$site))){
  print('ok all pops are here')
}else{
  print('error')
  print(levels(env2$site)[!(levels(env2$site) %in% levels(sampl2$site))])
}

### test
if(all(levels(env2$site) %in% levels(pop2$site))){
  print('ok all pops are here')
}else{
  print('error')
  print(levels(env2$site)[!(levels(env2$site) %in% levels(sampl2$site))])
}



## join 
ind_join = sampl2 %>% full_join(env2, by='site')
pop_join = pop2 %>% full_join(env2, by='site')

### test
if(all(levels(ind_join$site) %in% levels(sampl2$site))){
  print('ok all pops are here')
}else{
  print('error')
  print(levels(env2$site)[!(levels(env2$site) %in% levels(sampl2$site))])
}

## save table
if( any('mimizan2' %in% ind_join$site)){
write.table(ind_join, 'individual_join_add.csv', sep=";", row.names = FALSE)
}else{
write.table(ind_join, 'individual_join.csv', sep=";", row.names = FALSE)
write.table(pop_join, 'pop_join.csv', sep=";", row.names = FALSE)
}