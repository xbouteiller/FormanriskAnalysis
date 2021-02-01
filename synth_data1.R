df = read.table("FORMANIRSK total modif xb 2.csv", header = TRUE, sep=",")
str(df)
table(df[,'Sample_ref_2'],df[,'Sampling_location'])


library(tidyverse)

df %>% group_by(Sampling_location, Sample_ref_1) %>% count()


df %>%
  group_by(Sampling_location) %>%
  summarize(n_unique = n_distinct(Sample_ref_1)) %>%
  mutate(sum = sum(n_unique))

df %>%
  summarize(n_unique = n_distinct(Sampling_location)) %>%
  mutate(sum = sum(n_unique))



df %>% distinct(Sample_ref_1) %>%
  group_by(Sampling_location) %>%
  summarise(n_distinct())

dta <- data.frame(id = rep(1:500, 30),
                  sex = rep (c("M", "F"), 750),
                  child = rep(c(1, 0, 0, 1), 375))

dta %>% distinct(id) %>% count(sex)
