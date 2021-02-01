df = read.table("/home/xavier/Documents/research/FORMANRISK/data/FORMANIRSK total modif xb 2.csv", header = TRUE, sep=",")
str(df)
table(df[,'Sample_ref_2'],df[,'Sampling_location'])


library(tidyverse)


df2=df %>%
  group_by(Sampling_location) %>%
  summarize(n_rep = n_distinct(REP), max_plc = max(PLC)) %>%
  filter(n_rep != 1) %>% 
  mutate(sum = sum(n_rep))

print(df2)

df3=df %>%
  group_by(Sampling_location, Sample_ref_2) %>%
  summarize(n_rep = n_distinct(REP), max_plc = max(PLC)) %>% 
  filter(max_plc < 75) %>% 
  mutate(sum = sum(n_rep))

print(df3)
