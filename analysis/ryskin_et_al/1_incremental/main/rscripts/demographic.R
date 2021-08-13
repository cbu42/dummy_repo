library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")
setwd('../data')
theme_set(theme_bw())

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

d = read.csv("ryskin-subject_information.csv", header = TRUE)

# look at comments
unique(d$comments,d$workerid)

# look at problems
unique(d$problems,d$workerid)

# fair price
ggplot(d, aes(x=fairprice)) +
  geom_histogram(stat="count")

# overall assessment
ggplot(d, aes(x=asses)) +
  geom_histogram(stat="count")

# enjoyment (3 levels)
ggplot(d, aes(x=enjoyment)) +
  geom_histogram(stat="count")

# age
ggplot(d, aes(x=age)) +
  geom_histogram(stat="count")

# gender
ggplot(d, aes(x=gender)) +
  geom_histogram(stat="count")

# education
ggplot(d, aes(x=education)) +
  geom_histogram(stat="count")

# language
ggplot(d, aes(x=language)) +
  geom_histogram(stat="count") +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

# average time 
df = read_csv("ryskin-time_in_minutes.csv")

times = df %>%
  select(workerid,time_in_minutes) %>%
  unique()

ggplot(times, aes(x=time_in_minutes)) +
  geom_histogram()

ggplot(times, aes(x=age, y=Answer.time_in_minutes)) +
  geom_point()+
  geom_smooth(method="lm")

mean(times$time_in_minutes)
