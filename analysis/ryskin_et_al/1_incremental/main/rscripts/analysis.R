library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("helpers.R")
setwd('../data')
theme_set(theme_bw())

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

df = read.csv("trials_merged.csv", header = TRUE)
demo = read.csv("subject_info_merged.csv", header = TRUE)

#formatting
df$response = gsub(" ","",df$response)
df$response = gsub("\\[","",df$response)
df$response = gsub("\\]","",df$response)
df$response = gsub("\\'","",df$response)
df$response = gsub("AOI","",df$response)

df = df %>%
  group_by(workerid)%>%
  mutate(trial_number = seq(1:n())) %>%
  ungroup() %>%
  mutate(trial_group = ifelse(trial_number<31,"first_half","second_half"))

df = separate(df,response,into=c("click1","click2","click3","click4"),sep=",")

# run 2 models: 
# 1. like Sun & Breheny, fit linear models individually to each time window: "We constructed separate linear mixed- effects models for each time window predicting target preference scores from fixed effects of Determiner (all, some or number), Target size (small or big), Time and their interactions, including maximal random effects structure supported by the data." -- TODO
# 2. do the more principled mixed effects logistic regression on each window

# get just experimental trials and wrangle data
dmodel =  df %>%
  filter(ExpFiller=="Exp") %>%
  select(workerid,condition,size,click1,click2,click3,click4,target1,target2,competitor1,competitor2,instruction3) %>%
  mutate(ID = row_number()) %>%
  gather(click_number,location,click1:click4) %>%
  mutate(target=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitor=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0)))

# condition dataset on just target and competitor clicks
ddet = dmodel %>%
  mutate(TorC=target == 1  | competitor == 1) %>%
  filter(TorC == TRUE) %>%
  mutate(target = as.factor(as.character(target))) %>%
  mutate(condition=fct_relevel(condition,"num","all")) %>% 
  mutate(item=word(as.character(instruction3), -1)) %>% 
  droplevels() 

d_baseline = ddet %>% 
  filter(click_number == "click1") %>% 
  droplevels()
d_gender = ddet %>% 
  filter(click_number == "click2") %>% 
  droplevels()
d_determiner = ddet %>% 
  filter(click_number == "click3") %>% 
  droplevels()
d_noun = ddet %>% 
  filter(click_number == "click4") %>% 
  droplevels()

nrow(d_baseline) # 2160
nrow(d_gender) # 3869
nrow(d_determiner) # 3978
nrow(d_noun) # 4043

# no effect, as expected in prior window
dc_baseline = cbind(d_baseline,myCenter(d_baseline[,c("size","condition")]))
m.baseline = glmer(target ~ condition*csize + (1+condition+csize|workerid) + (1|item),family="binomial",data=dc_baseline)
summary(m.baseline)

# no effect, as expected in gender window
dc_gender = cbind(d_gender,myCenter(d_gender[,c("size","condition")]))
m.gender = glmer(target ~ condition*csize + (1+condition+size|workerid) + (1|item),family="binomial",data=dc_gender)
summary(m.gender)

# the crucial window: (still work on convergence issues)
contrasts(d_determiner$size) # big reference level
dc_determiner = cbind(d_determiner,myCenter(d_determiner[,c("size","condition")]))
m.determiner = glmer(target ~ condition*csize + (1+condition+size|workerid) + (1|item),family="binomial",data=dc_determiner)
summary(m.determiner)

dc_noun = cbind(d_noun,myCenter(d_noun[,c("size","condition")]))
m.noun = glmer(target ~ condition*csize + (1+condition+size|workerid) + (1|item),family="binomial",data=dc_noun)
summary(m.noun)

# simple effects analysis to probe interaction in determiner window
m_determiner.simple = glmer(target ~ condition*size-size + (1+condition+size|workerid) + (1|item),family="binomial",data=dc_determiner)
summary(m_determiner.simple)
