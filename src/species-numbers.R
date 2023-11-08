library(tidyverse)

read.csv("data/spatial-survey-species.csv") %>% pull(Taxon) %>% unique %>% length 
read.csv("data/temporal-survey-species.csv") %>% pull(Taxon) %>% unique %>% length 

read.csv("data/spatial-survey-species.csv") %>% select(Plot, Taxon) %>%
  unique %>%
  group_by(Plot) %>%
  summarise(n = length(Taxon)) %>%
  pull(n) %>% max

read.csv("data/spatial-survey-species.csv") %>% select(Plot, Taxon) %>%
  unique %>%
  group_by(Plot) %>%
  summarise(n = length(Taxon)) %>%
  pull(n) %>% min

read.csv("data/spatial-survey-species.csv") %>% select(Plot, Taxon) %>%
  unique %>%
  group_by(Plot) %>%
  summarise(n = length(Taxon)) %>%
  pull(n) %>% mean

read.csv("data/temporal-survey-species.csv") %>% select(Plot, Taxon) %>%
  unique %>%
  group_by(Plot) %>%
  summarise(n = length(Taxon)) %>%
  pull(n) %>% max

read.csv("data/temporal-survey-species.csv") %>% select(Plot, Taxon) %>%
  unique %>%
  group_by(Plot) %>%
  summarise(n = length(Taxon)) %>%
  pull(n) %>% min

read.csv("data/temporal-survey-species.csv") %>% select(Plot, Taxon) %>%
  unique %>%
  group_by(Plot) %>%
  summarise(n = length(Taxon)) %>%
  pull(n) %>% mean


read.csv("data/spatial-survey-species.csv") %>% select(Taxon) %>%
  filter(! Taxon %in% (read.csv("data/temporal-survey-species.csv") %>% pull(Taxon) %>% unique)) %>%
  pull(Taxon) %>% unique %>% length 

read.csv("data/temporal-survey-species.csv") %>%
  separate(Plot, into = c("Plot", "Year")) %>%
  select(Taxon, Year) %>%
  unique %>%
  group_by(Year) %>% 
  tally()


read.csv("data/temporal-survey-species.csv") %>%
  separate(Plot, into = c("Plot", "Year")) %>%
  select(Taxon, Year) %>%
  unique %>%
  mutate(Value = 1) %>%
  spread(Year, Value) %>%
  filter(is.na(`19`))

read.csv("data/temporal-survey-species.csv") %>%
  separate(Plot, into = c("Plot", "Year")) %>%
  select(Taxon, Year) %>%
  unique %>%
  mutate(Value = 1) %>%
  spread(Year, Value) %>%
  filter(is.na(`09`))

read.csv("data/spatial-survey-species.csv") %>% pull(Plot) %>% unique %>% length

rbind(read.csv("data/spatial-survey-species.csv"), read.csv("data/temporal-survey-species.csv")) %>%
  separate(Plot, into = c("Plot", "Survey"), sep = "-") %>%
  select(Taxon, Plot) %>%
  unique %>%
  group_by(Taxon) %>% tally %>% arrange(-n)

### clara
read.csv("data/temporal-survey-species.csv") %>%
  group_by(Plot)%>%
  tally() %>%
  summarise(mean=mean(n))
  
