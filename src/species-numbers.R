library(tidyverse)

read.csv("data/spatial-survey-species.csv") %>% pull(Taxon) %>% unique %>% length 
read.csv("data/temporal-survey-matrices.csv") %>% pull(Taxon) %>% unique %>% length 

read.csv("data/spatial-survey-species.csv") %>% select(Taxon) %>%
  filter(! Taxon %in% (read.csv("data/temporal-survey-matrices.csv") %>% pull(Taxon) %>% unique)) %>%
  pull(Taxon) %>% unique %>% length 

read.csv("data/temporal-survey-matrices.csv") %>%
  separate(Plot, into = c("Plot", "Year")) %>%
  select(Taxon, Year) %>%
  unique %>%
  group_by(Year) %>% 
  tally()


read.csv("data/temporal-survey-matrices.csv") %>%
  separate(Plot, into = c("Plot", "Year")) %>%
  group_by(Taxon, Year) %>%
  tally %>%
  unique %>%
  spread(Year, n) %>%
  filter(is.na(`19`))

read.csv("data/temporal-survey-matrices.csv") %>%
  separate(Plot, into = c("Plot", "Year")) %>%
  group_by(Taxon, Year) %>%
  tally %>%
  unique %>%
  spread(Year, n) %>%
  filter(is.na(`09`))

read.csv("data/spatial-survey-species.csv") %>% pull(Plot) %>% unique %>% length

rbind(read.csv("data/spatial-survey-species.csv"), read.csv("data/temporal-survey-species.csv")) %>%
  select(Taxon, Plot) %>%
  unique %>%
  group_by(Taxon) %>% tally %>% arrange(-n)


rbind(read.csv("data/temporal-survey-matrices.csv") %>%
  select(Plot, Taxon) %>%
  unique,
read.csv("data/spatial-survey-species.csv") %>%
  select(Plot, Taxon) %>%
  unique) %>%
  group_by(Plot) %>% tally %>%
  group_by() %>% summarise(m = mean(n), x = max(n), i =min(n))
