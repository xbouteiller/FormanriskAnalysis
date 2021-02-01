# install.packages('raster')
# install.packages('sp')
# install.packages('RgoogleMaps')
# install.packages('broom')
# install.packages('lme4')
# install.packages('lmerTest')
# install.packages('gapminder')
# install.packages('CARS')
# install.packages('dismo')

# Packages
library('raster')
library('sp')
library('tidyverse')
library('RgoogleMaps')
library('broom')
library('lme4')
library('lmerTest')
library('gapminder')
library('CARS')
library("dismo")
library('nlme')
library('lme4')
library("lmerTest")
?biovars

setwd("~/Documents/research/FORMANRISK/data/plotetdata")


# Data Preparation
r <- getData(name="worldclim", var="bio", res=0.5, lat=44.73696, lon=-0.77265)

treat = read.table("treat.csv", h=T, sep = ';')
str(treat)

sample = read.table("sample.csv", h=T, sep = ';')
str(sample)

individual = read.table("individual.csv", h=T, sep = ';')
str(individual)

site = read.table("Formanrisk sampling - site information.csv",  h= T, sep = ',')
str(site)

env = read.table("varenv.csv",  h= T, sep = ';')
str(env)

# Map

lat <- c(min(site$Y)-0.5,max(site$Y)+0.5) #define our map's ylim
lon <- c(min(site$X)-0.5,max(site$X)+0.5) #define our map's xlim
center = c(mean(lat), mean(lon))  #tell what point to center on
zoom <- 5  #zoom: 1 = furthest out (entire globe), larger numbers = closer in
site$size <- "tiny"  #create a column indicating size of marker
site$col <- "black"   #create a column indicating color of marker
site$char <- ""   #normal Google Maps pinpoints will be drawn

mymarkers <- cbind.data.frame(site$Y, site$X, site$size, 
                              site$col, site$char)   #create the data frame by binding my data columns of GPS coordinates with the newly created columns
names(mymarkers) <- c("lat", "lon", "size", "col", "char")  #assign column headings
str(mymarkers)

bb = qbbox(lat=mymarkers$lat, lon=mymarkers$lon)

terrain_close <- GetMap.bbox(bb$lonR, bb$latR,  destfile= "terrclose.png",
                             markers= mymarkers,  maptype = "satellite")


# explicitly set device to png with type='cairo-png'
png('MyTile1.png',type='cairo-png')     
# add points            
tmp <- PlotOnStaticMap(terrain_close, lat = mymarkers$lat, lon = mymarkers$lon, cex=3,pch=20,col=c('red'));
dev.off()



# resultats


# Histogramme P50
individual %>% filter( Species == 'pinus pinaster') %>% 
  mutate(site = recode(site,la_teste = 'la teste', lm='lit et mixe',mz = 'mimizan', 'spain continental dune'='spain dune')) %>% 
  ggplot( aes(x=P50, fill = P50)) + geom_histogram(color='black', fill=rgb(0,0,1,0.5)) + theme_classic() +
  theme(axis.text.x = element_text( size = 25),axis.text.y = element_text( size = 25),
        axis.title.x = element_text(color="black", size=25, face="bold"),
        axis.title.y = element_text(color="black", size=25, face="bold")) +
  scale_fill_manual(values=c(rgb(0,0,1,0.5)))

individual %>% filter( Species == 'pinus pinaster') %>% 
  mutate(site = recode(site,la_teste = 'la teste', lm='lit et mixe',mz = 'mimizan', 'spain continental dune'='spain dune')) %>% 
  summarise(mean = mean(P50), sd = sd(P50))


# Boxplot P50 / Treatment
individual %>% filter( Species == 'pinus pinaster') %>% 
  mutate(site = recode(site,la_teste = 'la teste', lm='lit et mixe',mz = 'mimizan', 'spain continental dune'='spain dune')) %>% 
  ggplot( aes(x=Treatment, y=P50, fill=Treatment)) + geom_boxplot() + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 25),axis.text.y = element_text( size = 25),
                                   axis.title.x = element_text(color="black", size=25, face="bold"),
                                   axis.title.y = element_text(color="black", size=25, face="bold"),
                                    legend.title = element_text( size = 20),
                                    legend.text = element_text(size = 20) )+
  scale_fill_manual(values=c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)))




# Boxplot P50 / pop / treatment
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
                                              'oin_es_leiria'= 'oin_P'
                                               ) )))

individual %>% filter( Species == 'pinus pinaster') %>% 
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
                                                 'oin_es_leiria'= 'oin_P'
                                                 )))) %>% 
  inner_join(env2, by='site') %>% 
  ggplot( aes(x=reorder(site, Y), y=P50, fill=Treatment)) + geom_boxplot() + theme_classic() + xlab('Site') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 25),axis.text.y = element_text( size = 25),
        axis.title.x = element_text(color="black", size=25, face="bold"),
        axis.title.y = element_text(color="black", size=25, face="bold"),
        legend.title = element_text( size = 20),
        legend.text = element_text(size = 20) )+
        scale_fill_manual(values=c(rgb(0,0,1,0.5), rgb(1,0,0,0.5))) + 
        annotate("segment", x = 3, xend = 11, y = -5., yend = -5., colour = "darkgrey",
                 size=3, alpha=1, arrow=arrow(), lineend = c('butt')) + 
        annotate("text", x = c(7.0), y = c(-4.85),
           label = c("Latitude") , color="darkgrey",
           size=10 , angle=0, fontface="bold")




 annotate("text", x = c(2,4.5), y = c(20,25),
          label = c("label 1", "label 2") , color="orange",
          size=7 , angle=45, fontface="bold")

+
  geom_segment(aes(
    x = -.3,
    xend = -.2,
    y = min(.$x) * .95,
    yend = 0
  ),
  size = 1,
  arrow = arrow()
  )
env2[order(env2$Y),c('Y','site')]





individual %>% filter( Species == 'pinus pinaster') %>% 
  mutate(site = recode(site,la_teste = 'la teste', lm='lit et mixe',mz = 'mimizan', 'spain continental dune'='spain dune')) %>% 
  group_by(Treatment)%>% 
  summarise(mean = mean(P50), sd = sd(P50))

individual %>% filter( Species == 'pinus pinaster') %>% 
  mutate(site = recode(site,la_teste = 'la teste', lm='lit et mixe',mz = 'mimizan', 'spain continental dune'='spain dune')) %>% 
  group_by(site, Treatment)%>% 
  summarise(mean = mean(P50), sd = sd(P50))


individual %>% filter( Species == 'pinus pinaster') %>% lm(P50 ~ Treatment, .) %>% anova()
individual %>% filter( Species == 'pinus pinaster') %>% lm(P50 ~ site+Treatment, .) %>% anova()
individual %>% filter( Species == 'pinus pinaster') %>%   group_by(site) %>% do(tidy(lm(P50 ~ Treatment, .)))
individual %>% filter( Species == 'pinus pinaster') %>%   group_by(site) %>% lm(P50 ~ Treatment, .) 


# Corr with env

treat$site
cat(unique(as.character(treat$site)), sep = '"="", \n"')
cat(as.character(env$site), sep = '"="", \n"')



individual2 = individual %>% filter( Species == 'pinus pinaster') %>% 
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
                                              'oin_es_leiria'= 'oin_P'
                                              ) )))

treat2 = treat  %>% filter( Species == 'pinus pinaster') %>% 
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
                                              "Barroqueiras-San Vicente do Mar"="san vicente", 
                                              "Biscarrosse"="biscarrosse", 
                                              "Lit_et_Mixe"="lit et mixe", 
                                              "Mimizan"="mimizan",
                                              'Forêt domaniale de Cerbère'='cerbere',
                                              'Céret (Le Palau)'= 'ceret',
                                              'oin_es'='oin_es',
                                              'oin_fr'='oin_fr',
                                              'oin_es_leiria'= 'oin_P'
                                              ) )))


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
                       'oin_es_leiria'= 'oin_P'
                       ) )))



treat2$site
env2$site
levels(env2$site)
levels(treat2$site)
levels(individual2$site)

str(treat)
str(env)

pop_join = treat2 %>% inner_join(env2, by='site')
ind_join = individual2 %>% full_join(env2, by='site')

ind_join  %>% filter( Species == 'pinus pinaster') %>% 
  ggplot(aes(x=bio5_15,y=P50, col = Country)) + geom_point() + theme_classic() 

pop_join  %>% filter( Species == 'pinus pinaster') %>% group_by(site) %>% 
  mutate(P50_mean = mean(P50)) %>% 
  mutate(bio5_15=bio5_15/10) %>% 
  ggplot(aes(x=bio5_15,y=P50_mean)) +geom_point(size=2, shape=20) + 
  geom_smooth(method = "glm", formula = y ~ log(x)) + xlab("Max Temperature of Warmest Month") + ylab('P50') + theme_classic() +
  theme(axis.text.x = element_text( size = 25),axis.text.y = element_text( size = 25),
        axis.title.x = element_text(color="black", size=25, face="bold"),
        axis.title.y = element_text(color="black", size=25, face="bold")) 

pop_join  %>% filter( Species == 'pinus pinaster') %>% group_by(site) %>% 
  mutate(P50_mean = mean(P50)) %>% 
  mutate(bio10_15=bio10_15/10) %>% 
  ggplot(aes(x=bio10_15,y=P50_mean)) +geom_point(size=2, shape=20) + 
  geom_smooth(method = "glm", formula = y ~ log(x)) + xlab("Mean Temperature of Warmest Quarter") + ylab('P50 mean') + theme_classic() +
  theme(axis.text.x = element_text( size = 14), 
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))

test = pop_join  %>% filter( Species == 'pinus pinaster') %>% group_by(site) %>% 
  mutate(P50_mean = mean(P50))

test %>%  select(site,bio5_15, Treatment,P50,P50_mean)

summary(lm(P50_mean ~ bio5_15, test))
anova(lm(P50_mean ~ bio5_15, test))

plot((lm(log(P50) ~ log(bio5_15), pop_join)))

ind_join  %>% filter( Species == 'pinus pinaster') %>% 
  ggplot(aes(x=bio10_15,y=P50, col = Country)) + geom_point() + theme_classic()
pop_join  %>% filter(Species == 'pinus pinaster') %>% 
  ggplot(aes(x=bio10_15,y=P50, col = Country)) + geom_point() + theme_classic()

# BIO1 = Annual Mean Temperature
# 
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# 
# BIO3 = Isothermality (BIO2/BIO7) (×100)
# 
# BIO4 = Temperature Seasonality (standard deviation ×100)
# 
# BIO5 = Max Temperature of Warmest Month
# 
# BIO6 = Min Temperature of Coldest Month
# 
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# 
# BIO8 = Mean Temperature of Wettest Quarter
# 
# BIO9 = Mean Temperature of Driest Quarter
# 
# BIO10 = Mean Temperature of Warmest Quarter
# 
# BIO11 = Mean Temperature of Coldest Quarter
# 
# BIO12 = Annual Precipitation
# 
# BIO13 = Precipitation of Wettest Month
# 
# BIO14 = Precipitation of Driest Month
# 
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# 
# BIO16 = Precipitation of Wettest Quarter
# 
# BIO17 = Precipitation of Driest Quarter
# 
# BIO18 = Precipitation of Warmest Quarter
# 
# BIO19 = Precipitation of Coldest Quarter


# Model


dfpp = individual %>% filter( Species == 'pinus pinaster') %>% as.data.frame()


model = lm(P50 ~ site*Treatment,
             data=dfpp)
model = gls(P50 ~ site*Treatment,
           data=dfpp)
model = lmer(P50 ~ Treatment + (1|site),
             data=dfpp,
             REML=TRUE)
model = lmer(P50 ~ Treatment + (1|site:Treatment),
             data=dfpp,
             REML=TRUE)
model = lmer(P50 ~ Treatment + (1|site/Treatment),
             data=dfpp,
             REML=TRUE)
model = lmer(P50 ~  Treatment + (Treatment|site),
             data=dfpp,
             REML=TRUE)
model
summary(model)
anova(model)
fitted(model)


M.mixed <- lme(P50~Treatment, random=~1|site,method="REML", data=dfpp)
M.mixed2 <- lme(P50~Treatment, random=~1+Treatment|site,method="REML", data=dfpp)
M.gls<- gls(P50~Treatment, method="REML",correlation=corCompSymm(form=~1|site),data=dfpp)
M.gls0<- gls(P50~Treatment, method="REML",data=dfpp)
AIC(M.mixed, M.mixed2, M.gls)
AIC(M.mixed,  M.gls, M.gls0)
anova(M.gls)

anova(M.mixed)
?lme


install.packages('nlme')
library('nlme')


(mm_plot <- ggplot(dfpp, aes(x = Treatment, y = P50, colour = site)) +
    facet_wrap(~site, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_point(data = cbind(dfpp, pred = fitted(model)), aes(y = pred), col = 'red') +  # adding predicted line from mixed model
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)


library(multcomp)
posthoc = glht(model,
               Treatment = mcp(Tech="Tukey"))
mcs = summary(posthoc,
              test=adjusted("single-step"))


# install.packages('glmmTMB')
library(sjPlot)
# Visualise random effects 
(re.effects <- plot_model(model, type = "re", show.values = TRUE))
# show summary
summary(model)


individual %>% filter( Species == 'pinus pinaster') %>% 
  mutate(site = recode(site,la_teste = 'la teste', lm='lit et mixe',mz = 'mimizan', 'spain continental dune'='spain dune')) %>% 
  mutate(fitted = fitted(model)) %>% 
  ggplot( aes(x=site, y=fitted, col=Treatment)) + geom_point() + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 14), 
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))


rand(model)


by_country <- individual %>% filter( Species == 'pinus pinaster') %>%  
  group_by(site) %>% 
  nest()
country_model <- function(df) {
  lm(P50 ~ Treatment, data = df)
}
by_country <- by_country %>% 
  mutate(model = map(data, country_model))  %>% 
  mutate(glance = map(model, broom::glance), 
        rsq = glance %>% map_dbl("r.squared"),
        # tidy gets summary variables of the model coefficients
        tidy = map(model, broom::tidy),
        # augment gets per observation variables (e.g., residuals)
        augment = map(model, broom::augment))
by_country %>% unnest(glance)



# 

coords <- data.frame(x=site$X,y=site$Y)
points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)
df <- cbind.data.frame(coordinates(points),values)

