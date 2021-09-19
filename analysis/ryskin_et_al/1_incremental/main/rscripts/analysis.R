library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("helpers.R")
setwd('../data')
theme_set(theme_bw())

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

# df = read_csv("trials_merged.csv", header = TRUE)
# demo = read.csv("subject_info_merged.csv", header = TRUE)
df = read_csv("ryskin-trials.csv")
demo = read.csv("ryskin-subject_information.csv", header = TRUE)

#formatting

#remove error column
df$error <- NULL
df$proliferate.condition <- NULL
`%notin%` <- Negate(`%in%`)
#remove pilot participants
df <- df[df$workerid %notin% c(17, 13, 16, 19, 14, 18),]
length(unique(df$workerid)) #63 participants
#trial numbers have to be reduced by 2
df$trial_number <- df$trial_number-2

#removing periods from the loc ids
df$loc_big_filler <- gsub("\\.", "", df$loc_big_filler)
df$loc_contrast = gsub("\\.","",df$loc_contrast)
df$loc_small_filler = gsub("\\.","",df$loc_small_filler)
df$loc_target_pic = gsub("\\.","",df$loc_target_pic)

#formation of experiment halves
df = df %>%
  mutate(trial_group = ifelse(trial_number<151,"first_half","second_half"))

#separation of clicks
df = df %>% 
  separate(response,into = c("click_prior", "click2", "click3"), sep=",")
#clean click values
df$click_prior <- gsub("\\[", "", df$click_prior)
df$click_prior <- gsub("\\'", "", df$click_prior)
df$click2 <- gsub("\\]", "", df$click2)
df$click2 <- gsub("\\'", "", df$click2)
df$click2 <- gsub(" ", "", df$click2)
df$click3 <- gsub("\\]", "", df$click3)
df$click3 <- gsub("\\'", "", df$click3)
df$click3 <- gsub(" ", "", df$click3)
# naming clicks 2 and 3:
df = df %>% 
  mutate(click_noun = case_when(is.na(click3) ~ click2,
                                TRUE ~ click3)) %>% 
  mutate(click_adj = case_when(is.na(click3) ~ "NA",
                               TRUE ~ click2)) %>% 
  select(-click2, -click3)
#gets the location of the competitor 
df = df %>% 
  separate(target_pic, into = c("noun","feature"), sep="[_.]", remove=F, extra="drop") %>% 
  mutate(loc_competitor_pic = case_when(feature == "small" ~ loc_small_filler,
                                        feature == "big" ~ loc_big_filler,
                                        TRUE ~ "NA"))

view(df[1:50,])
names(df)
?gather

# run 2 models: 
# 1. like Sun & Breheny, fit linear models individually to each time window: "We constructed separate linear mixed- effects models for each time window predicting target preference scores from fixed effects of Determiner (all, some or number), Target size (small or big), Time and their interactions, including maximal random effects structure supported by the data." -- TODO
# 2. do the more principled mixed effects logistic regression on each window

# get just experimental trials and wrangle data
dmodel =  df %>%
  select(workerid,pragContext,cond,click_prior,click_adj,click_noun,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction) %>%
  pivot_longer(names_to = "window", values_to = "selection",cols=click_prior:click_noun) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(distractor = case_when(loc_target_pic!=selection & loc_competitor_pic !=selection ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(window=fct_recode(window,prior="click_prior",adjective="click_adj",noun="click_noun")) %>% 
  mutate(window = fct_relevel(window,"prior","adjective"))


# condition dataset on just target and competitor clicks
ddet = dmodel %>%
  mutate(TorC=target == 1  | competitor == 1) %>%
  filter(TorC == TRUE) %>%
  mutate(target = as.factor(as.character(target))) %>%
  droplevels()

# be sure to accommodate new click names
d_prior = ddet %>% 
  filter(window == "prior") %>% 
  droplevels()
d_adj = ddet %>% 
  filter(window == "adjective") %>% 
  droplevels()
d_noun = ddet %>% 
  filter(window == "noun") %>% 
  droplevels()

nrow(d_prior) # 8850
nrow(d_adj) # 7639
nrow(d_noun) # 15660

# no effect, as expected in prior window
# dc_baseline = cbind(d_baseline,myCenter(d_baseline[,c("size","condition")]))
# m.baseline = glmer(target ~ condition*csize + (1+condition+csize|workerid) + (1|noun),family="binomial",data=dc_baseline)
# summary(m.baseline)
# 
# # no effect, as expected in gender window
# dc_gender = cbind(d_gender,myCenter(d_gender[,c("size","condition")]))
# m.gender = glmer(target ~ condition*csize + (1+condition+size|workerid) + (1|noun),family="binomial",data=dc_gender)
# summary(m.gender)
# 
# # the crucial window: (still work on convergence issues)
# contrasts(d_determiner$size) # big reference level
# dc_determiner = cbind(d_determiner,myCenter(d_determiner[,c("size","condition")]))
# m.determiner = glmer(target ~ condition*csize + (1+condition+size|workerid) + (1|noun),family="binomial",data=dc_determiner)
# summary(m.determiner)
# 
# dc_noun = cbind(d_noun,myCenter(d_noun[,c("size","condition")]))
# m.noun = glmer(target ~ condition*csize + (1+condition+size|workerid) + (1|noun),family="binomial",data=dc_noun)
# summary(m.noun)
# 
# # simple effects analysis to probe interaction in determiner window
# m_determiner.simple = glmer(target ~ condition*size-size + (1+condition+size|workerid) + (1|noun),family="binomial",data=dc_determiner)
# summary(m_determiner.simple)
