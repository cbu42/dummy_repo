library(tidyverse)
library(lme4)
library(stringr)
library(stringi)
library(lmerTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")
setwd('../data')
theme_set(theme_bw())

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
# ryskin fig 2: dark purple, light purple, dark green, light green
ryskinPalette <- c("#831CBF", "#cf97EF", "#00C19b", "#90D1C4")

# df = read.csv("trials_merged.csv", header = TRUE)
# demo = read.csv("subject_info_merged.csv", header = TRUE)
df = read_csv("ryskin-trials.csv")
demo = read.csv("ryskin-subject_information.csv", header = TRUE)

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

df = df %>% 
  mutate(click_noun = case_when(is.na(click3) ~ click2,
                                TRUE ~ click3)) %>% 
  mutate(click_adj = case_when(is.na(click3) ~ "NA",
                               TRUE ~ click2)) %>% 
  select(-click2, -click3)

df = df %>% 
  separate(target_pic, into = c("noun","feature"), sep="[_.]", remove=F, extra="drop") %>% 
  mutate(loc_competitor_pic = case_when(feature == "small" ~ loc_small_filler,
                                        feature == "big" ~ loc_big_filler,
                                        TRUE ~ "NA"))

### PULL RESPONSE TIMES
df$response_temp = NA
df$response_temp <- gsub("\\[", "", df$response_times)
df$response_temp <- gsub("\\]", "", df$response_temp)
# response times (hence t) for each window
df = df %>% 
  separate(response_temp,into = c("t_prior", "t2", "t3"), sep=", ")
# accounting for modified vs unmodified cases
df = df %>% 
  mutate(t_noun = case_when(is.na(t3) ~ t2,
                                TRUE ~ t3)) %>% 
  mutate(t_adj = case_when(is.na(t3) ~ "NA",
                               TRUE ~ t2)) %>% 
  select(-t2, -t3)
#char to int
df$t_prior <- as.integer(df$t_prior)
df$t_adj <- as.integer(df$t_adj)
df$t_noun <- as.integer(df$t_noun)


### EXCLUSIONS

df = df %>% 
  mutate(selection_correct = click_noun == loc_target_pic)

table(df$selection_correct) # 267 incorrect responses for pilot, 3431 for main

table(df$trialType)

# exclude anyone with < 90% correct selections on all trials where correct selection was possible from linguistic signal
# UPDATE THIS CODE

# accuracy <- df %>%
#   filter(trialType == "test") %>%
#   group_by(workerid, pragContext) %>%
#   tally(selection_correct) %>%
#   mutate(correct=n/40) #40 test trials total

# NOTE: This take on "accuracy" differs from the above in that "correct" reflects
# the percent of all correct selections where correct selection was possible from linguistic signal.
# The "accuracy" commented out above counts only test trials in calculating "correct" 
bad_accuracy <- df %>%
  subset(df$pragContext == "bad") %>%
  subset(cond != 'contrast_control' & cond != 'semantic_control') %>%
  subset(!(cond =='no_contrast' & trialType == "train"))  %>% #removes train
  group_by(workerid,pragContext) %>%
  tally(selection_correct) %>%
  mutate(correct=n/140)

good_accuracy <- df %>%
  subset(df$pragContext == "good") %>%
  group_by(workerid,pragContext) %>%
  tally(selection_correct) %>%
  mutate(correct=n/300)

accuracy <- rbind(good_accuracy,bad_accuracy)
View(accuracy %>% arrange(correct))

toexclude = accuracy %>%
  filter(correct <.9)

length(toexclude$workerid) # 4 for pilot, 4 for main
length(toexclude$workerid)/length(accuracy$workerid) #0.06349206 data loss

df = df %>% 
  filter(!workerid %in% toexclude$workerid)

#unique(demo$language) # no exlusions (some people left it blank?)

# # remove trials with incorrect selections
df = df %>%
  filter(selection_correct==1)

nrow(df) # 936 for pilot, 14745 for main

# MAKE SURE THE ABOVE EXCLUSION CODE DOES THE RIGHT THING
# get only experimental trials (no fillers) for further analysis
# as a reminder we can use all trial types for testing our linking assumption
d_test = df %>% 
  filter(trialType == "test") %>%
  droplevels()

### PART I: PLOT DATA FROM REPLICATION TASK
# plot proportion of selections by condition
toplot =  d_test %>%
  select(workerid,pragContext,cond,click_prior,click_adj,click_noun,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number,trial) %>%
  pivot_longer(names_to = "window", values_to = "selection",cols=click_prior:click_noun) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(distractor = case_when(loc_target_pic!=selection & loc_competitor_pic !=selection ~ 1,
                                TRUE ~ 0)) %>% 
  group_by(cond,pragContext,window) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor), m_distractor=mean(distractor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor), ci_low_distractor=ci.low(distractor),ci_high_distractor=ci.high(distractor)) %>%
  pivot_longer(names_to="location",values_to="Mean",cols=m_target:m_distractor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,ifelse(location=="m_distractor",ci_low_distractor,0)))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,ifelse(location=="m_distractor",ci_high_distractor,0)))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target", "distractor"="m_distractor")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>% 
  mutate(window=fct_recode(window,prior="click_prior",adjective="click_adj",noun="click_noun")) %>% 
  mutate(window = fct_relevel(window,"prior","adjective"))

proportions = ggplot(toplot, aes(x=window, y=Mean, group=Region)) +
  geom_line(aes(color=Region),size=1.3) +
  geom_point(aes(color=Region),size=2.5,shape="square") +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.3) +
  facet_grid(cond ~ pragContext ) + 
  scale_color_manual(values=c("darkgreen","blue", "orange")) +
  xlab("Window") +
  ylab("Proportion of selections") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))

proportions
ggsave(proportions, file="../graphs/proportions.pdf",width=9,height=4.5)

# recreate Fig 13 from Sun & Breheny 2020
toplot =  df %>%
  filter(cond %in% c('contrast','no_contrast')) %>% 
  select(workerid,pragContext,cond,click_prior,click_adj,click_noun,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number,trial) %>%
  pivot_longer(names_to = "window", values_to = "selection",cols=click_prior:click_noun) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(distractor = case_when(loc_target_pic!=selection & loc_competitor_pic !=selection ~ 1,
                                TRUE ~ 0)) %>%
  # when target == 0 and comp == 0, turn target 0 into .5? or exclude? excluding for time being (leads to exclusion of 3230 data points)
  filter(target == 1 | competitor == 1) %>% 
  group_by(pragContext,cond,window,workerid) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor)+.00000001) %>% 
  ungroup() %>% 
  mutate(prop=(m_target/m_competitor)+.00000001,targetadvantage=log(prop)) %>% 
  group_by(pragContext,cond,window) %>%
  summarize(target=mean(targetadvantage),ci_low_target=ci.low(targetadvantage),ci_high_target=ci.high(targetadvantage)) %>%
  ungroup() %>% 
  mutate(YMin=target-ci_low_target,YMax=target+ci_high_target) %>% 
  mutate(cond=fct_relevel(cond,"contrast", "no_contrast")) %>%
  mutate(window=fct_recode(window,prior="click_prior",adjective="click_adj",noun="click_noun")) %>% 
  mutate(window = fct_relevel(window,"prior","adjective"))
dodge=position_dodge(0)

fig13 <- ggplot(toplot, aes(x=window, y=target, color=cond, linetype=pragContext,group=interaction(cond,pragContext))) +
  geom_line(size=1.3,position=dodge) +
  geom_point(size=2.5,shape="square",position=dodge) +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.7,  linetype="solid",position=dodge) +
  # facet_grid(size ~condition ) + 
  scale_color_manual(values=c(cbPalette[2],cbPalette[6])) +
  scale_x_discrete(breaks=c("click_prior","click_adj","click_noun"),
                   labels=c("Prior", "Adj", "Noun")) +
  xlab("Window") +
  ylab("log(P(Target)/P(Competitor))") #+
# theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
fig13
ggsave("../graphs/results-idt.pdf",width=4.5,height=2.5)

#ryskin fig 2
#line type as contrast? and alpha as target/competitor?
toplot =  d_test %>%
  select(workerid,pragContext,cond,click_prior,click_adj,click_noun,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number,trial) %>%
  pivot_longer(names_to = "window", values_to = "selection",cols=click_prior:click_noun) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>% 
  # mutate(distractor = case_when(loc_target_pic!=selection & loc_competitor_pic !=selection ~ 1,
  #                               TRUE ~ 0)) %>% 
  group_by(cond,pragContext,window) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor)) %>%
  pivot_longer(names_to="location",values_to="Mean",cols=m_target:m_competitor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  #exclude distractor to better resemble ryskin fig. 2
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>% 
  mutate(window=fct_recode(window,prior="click_prior",adjective="click_adj",noun="click_noun")) %>% 
  mutate(window = fct_relevel(window,"prior","adjective")) %>% 
  mutate(pragContext=fct_recode(pragContext,reliable="good",unreliable="bad")) %>% 
  mutate(pragContext = fct_relevel(pragContext,"reliable","unreliable")) %>% 
  mutate(cond = fct_relevel(cond,"no_contrast","contrast"))

#reverse dark and light
#consider switching contrast and no contrast positions/roles
ryskin_f2 = ggplot(toplot, aes(x=window, y=Mean, color=pragContext, alpha = Region, group=interaction(pragContext, cond, Region))) +
  geom_line(aes(color=pragContext, linetype= cond),size=1.3) +
  geom_point(aes(color=pragContext),size=2.5,shape="square") +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.3) +
  labs(color="Pragmatic context", alpha = "Region") +
  #scale_alpha_discrete(range=c(.5,1))+
  scale_alpha_discrete(limits = c("target", "competitor"),range=c(1,.3))+
  #facet_grid(cond ~ pragContext ) + 
  scale_color_manual(values=c(ryskinPalette[1], ryskinPalette[3])) +
  xlab("Window") +
  ylab("Proportion of selections") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
ryskin_f2
ggsave(ryskin_f2, file="../graphs/ryskin_fig2_rep.pdf",width=9,height=4.5)

# Does the bad pragContext make us increasingly less likely to draw a contrastive inference?
# Subset to Adjective window bc that's where contrast effect shows up. 
# Plot contrast v no contrast and bad s good just in the adj window. X axis is trial number (over time analysis)
#defunct
toplot =  d_test %>%
  select(workerid,response_times, pragContext,cond,click_adj,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number) %>%  
  mutate(selection = click_adj) %>% 
  mutate(response_adj = str_extract(response_times, '\\s(.*?),')) %>% 
  mutate(response_adj = as.integer(gsub('\\s|,', "", response_adj))) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>%
  group_by(cond,pragContext, trial_number) %>%
  mutate(m_target=mean(target),m_competitor=mean(competitor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor)) %>%
  pivot_longer(names_to="location",values_to="Mean",cols=m_target:m_competitor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>% 
  mutate(pragContext=fct_recode(pragContext,reliable="good",unreliable="bad")) %>% 
  mutate(pragContext = fct_relevel(pragContext,"reliable","unreliable")) #%>% 
#mutate(cond = fct_relevel(cond,"no_contrast","contrast"))
view(toplot)
over_time = ggplot(toplot, aes(x=trial_number, y=response_adj, color=pragContext, linetype=cond, group=interaction(pragContext, cond))) +
  #geom_line(aes(color=pragContext, linetype= cond),size=1.3) +
  geom_smooth(method='lm')+
  labs(color="Pragmatic context", linetype = "Condition") +
  #scale_alpha_discrete(range=c(.5,1))+
  #facet_grid(cond ~ pragContext ) + 
  scale_color_manual(values=c(ryskinPalette[1], ryskinPalette[3])) +
  xlab("Trial number (sequentially)") +
  ylab("Reaction time (ms)") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
over_time
ggsave(over_time, file="../graphs/response-over_time.pdf",width=9,height=4.5)

toplot_target <- toplot %>% 
  filter(Region=='target')
contrastive_inf = ggplot(toplot_target, aes(x=trial_number, y=Mean, color=pragContext, linetype=cond, shape = cond, group=interaction(pragContext, cond))) +
  geom_smooth(method='lm')+
  geom_point() +
  labs(color="Pragmatic context", linetype = "Condition") +
  #scale_alpha_discrete(range=c(.33,.5,1))+
  #facet_grid(cond ~ pragContext ) + 
  scale_color_manual(values=c(ryskinPalette[1], ryskinPalette[3])) +
  xlab("Trial number (sequentially)") +
  ylab("Proportion of selections") +
  labs(title = "Target selections during adjective window") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
contrastive_inf
ggsave(contrastive_inf, file="../graphs/contrastive_inf_over_time.pdf",width=9,height=4.5)

# TODO:
# create a binary variable: TRUE if participant has seen filler trial before test trials?
# create a continuous var: how many filler trials have they seen before the first test trial?
# then, recreate the above plot with those ^^ vars as an additional variable. facet the plot using the above. 
d_test2 = df %>% 
  filter(trialType != "train") %>%
  droplevels()
toplot_trialType =  d_test2 %>%
  select(workerid,response_times, pragContext,cond,click_adj,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number, trialType) %>%  
  mutate(selection = click_adj) %>% 
  mutate(response_adj = str_extract(response_times, '\\s(.*?),')) %>% 
  mutate(response_adj = as.integer(gsub('\\s|,', "", response_adj))) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>%
  group_by(cond,pragContext, trial_number) %>%
  mutate(m_target=mean(target),m_competitor=mean(competitor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor)) %>%
  pivot_longer(names_to="location",values_to="Mean",cols=m_target:m_competitor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>% 
  mutate(pragContext=fct_recode(pragContext,reliable="good",unreliable="bad")) %>% 
  mutate(pragContext = fct_relevel(pragContext,"reliable","unreliable"))

#group_by: workerid
#df_f: filter for fillers
#df_t: filter for tests
# toplot_trialType$filler_first = NA # creates a new variable filled with NAs
# ind_f <- d_test2 %>% 
#   ???group_by(workerid) %>% 
#   filter(trialType=='filler') %>% 
#   min(trial_number)
ind  <-  toplot_trialType$trial_number = 1 & toplot_trialType$trialType == 'filler'
toplot_trialType$filler_first[ind] = v1


view(toplot_target)
contrastive_inf = ggplot(toplot_target, aes(x=trial_number, y=Mean, color=pragContext, linetype=cond, shape = cond, group=interaction(pragContext, cond))) +
  geom_smooth(method='lm')+
  geom_point() +
  labs(color="Pragmatic context", linetype = "Condition") +
  #scale_alpha_discrete(range=c(.33,.5,1))+
  #facet_grid(cond ~ pragContext ) + 
  scale_color_manual(values=c(ryskinPalette[1], ryskinPalette[3])) +
  xlab("Trial number (sequentially)") +
  ylab("Proportion of selections") +
  labs(title = "Target selections during adjective window") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
contrastive_inf
ggsave(contrastive_inf, file="../graphs/contrastive_inf_over_time.pdf",width=9,height=4.5)

### PART II: PLOT CATEGORICAL DATA AGAINST EYE MOVEMENT DATA

# load eye-tracking data from Ryskin 2019
# adj_tar <- read.delim("ryskin_eyetracking/PragTrain3_adj_window_for_tardur_tarDURdatastruct.txt")
# adjn_GLMM <- read.delim("ryskin_eyetracking/PragTrain3_adjnoun_for_GLMM_timesummLongForm.txt")
# adjn_tar <- read.delim("ryskin_eyetracking/PragTrain3_adjnoun_window_for_tardur_tarDURdatastruct.txt")
# noun_GLMM <- read.delim("ryskin_eyetracking/PragTrain3_noun_for_GLMM_timesummLongForm.txt")
# noun_tar <- read.delim("ryskin_eyetracking/PragTrain3_noun_window_for_tardur_tarDURdatastruct.txt")
# baseline = read.csv("sb_eyetracking/exp200ms_beselinedata.csv", header = TRUE)

# prior window
prior_df = read_tsv(paste0(getwd(),"/ryskin_eyetracking/PragTrain3_baseline_window_timesummLongForm.txt"),
                      col_names=c('target_dur','contrast_dur','compet_dur','other_dur','currsubj','subject','trialnum','order','timebin','trialID','condWith','condBet','targ_loc','contrast_loc','compet_loc','list','trialType','accuracy','target_fix','target_AR1','compet_fix','compet_AR1')) %>% 
  mutate(contrast_cond = case_when(
    condWith == 0 ~ 'filler', 
    condWith == 1 ~ 'no_contrast',
    condWith == 2 ~ 'contrast'),
    prag_context_cond = case_when(
      condBet == 10 ~ 'reliable',
      condBet == 9 ~ 'unreliable',
      condBet == 0 ~ 'filler'),
    trial_type = case_when(
      trialType == 1 ~ 'test',
      trialType == 99 ~ 'train',
      trialType == 999 ~ 'filler'),
    uniqueID = str_c('l',as.character(list),'_',trial_type,trialID),
    total_dur = (target_dur + contrast_dur + compet_dur + other_dur),
    target_prop = target_dur/total_dur,
    contrast_prop = contrast_dur/total_dur,
    compet_prop = compet_dur/total_dur,
    other_prop = other_dur/total_dur,
    timebin_rel = timebin/10 - 17)
#lots of zeros (and NaN and NAs)
view(prior_df[1:100,])
table(prior_df$total_dur)
table(prior_df$target_dur)
table(prior_df$compet_fix)
nrow(prior_df)

pragtrain3_list1 = read_tsv(paste0(getwd(),'/ryskin_eyetracking/experiments_2-3_trials_list_1.txt'))
pragtrain3_list2 = read_tsv(paste0(getwd(),'/ryskin_eyetracking/experiments_2-3_trials_list_2.txt'))
# pragtrain3_list1 = read_tsv('../data/ryskin_eyetracking/experiments_2-3_trials_list_1.txt')
# pragtrain3_list2 = read_tsv('../data/ryskin_eyetracking/experiments_2-3_trials_list_2.txt')
pragtrain3_lists = bind_rows(pragtrain3_list1,pragtrain3_list2) %>% 
  mutate(unique_ID = str_c(as.character(counterbalance),'_',trialID))
prior_df = prior_df %>% 
  left_join(pragtrain3_lists, by = c('uniqueID'='unique_ID'))
rm(pragtrain3_list1, pragtrain3_list2)

# expt2 reading in adj+noun window data for glmm
# reads in a large file - can be downloaded locally from Ryskin 2019's OSF page
pragtrain3_data = read_tsv('C:/Users/cb476/OneDrive/Desktop/ALPS Lab/PragTrain3_adjnoun_for_GLMM_timesummLongForm.txt',
# pragtrain3_data = read_tsv("../data/PragTrain3_adjnoun_for_GLMM_timesummLongForm.txt",
                           col_names=c('target_dur','contrast_dur','compet_dur','other_dur','currsubj','subject','trialnum','order','timebin','trialID','condWith','condBet','targ_loc','contrast_loc','compet_loc','list','trialType','accuracy','target_fix','target_AR1','compet_fix','compet_AR1')) %>% 
  mutate(contrast_cond = case_when(
    condWith == 0 ~ 'filler', 
    condWith == 1 ~ 'no_contrast',
    condWith == 2 ~ 'contrast'),
    prag_context_cond = case_when(
      condBet == 10 ~ 'reliable',
      condBet == 9 ~ 'unreliable',
      condBet == 0 ~ 'filler'),
    trial_type = case_when(
      trialType == 1 ~ 'test',
      trialType == 99 ~ 'train',
      trialType == 999 ~ 'filler'),
    uniqueID = str_c('l',as.character(list),'_',trial_type,trialID),
    total_dur = (target_dur + contrast_dur + compet_dur + other_dur),
    target_prop = target_dur/total_dur,
    contrast_prop = contrast_dur/total_dur,
    compet_prop = compet_dur/total_dur,
    other_prop = other_dur/total_dur,
    timebin_rel = timebin/10 - 17)

pragtrain3_data = pragtrain3_data %>% 
  left_join(pragtrain3_lists, by = c('uniqueID'='unique_ID'))
rm(pragtrain3_lists)

#adds prior window to pragtrain3_data
pragtrain3_data <- rbind(prior_df, pragtrain3_data)

pragtrain3_crit = pragtrain3_data %>% 
  group_by(subject,trialnum) %>% 
  mutate(target_AR1_corrected = lag(target_fix),
         compet_AR1_corrected = lag(compet_fix)) %>% 
  filter(trial_type == 'test'& target_AR1 != 999) %>% 
  ungroup()

# expt2 setting up factors and contrasts
pragtrain3_crit$timebin_rel_c = scale(pragtrain3_crit$timebin_rel,scale=F, center = T)
pragtrain3_crit$prag_context_cond = factor(pragtrain3_crit$prag_context_cond)
pragtrain3_crit$contrast_cond = factor(pragtrain3_crit$contrast_cond)

contrasts(pragtrain3_crit$prag_context_cond)<-c(-0.5,0.5)
contrasts(pragtrain3_crit$contrast_cond)<-c(0.5,-0.5)

contrasts(pragtrain3_crit$prag_context_cond)
contrasts(pragtrain3_crit$contrast_cond)

# expt2 adding half contrasts
pragtrain3_crit2 = pragtrain3_crit %>% 
  mutate(half= if_else(trialnum<151,"first","second"))

pragtrain3_crit2$trialorder_c = scale(pragtrain3_crit2$trialnum,scale=F,center=T)
pragtrain3_crit2$half<-factor(pragtrain3_crit2$half)
contrasts(pragtrain3_crit2$half)<-c(-0.5,0.5)
contrasts(pragtrain3_crit2$half)


# LINKING FUNCTION QUESTION: PLOT
# plot eye movement proportions against selection proportions by window
toplot_eye = pragtrain3_crit %>% 
  separate(audio,into=c("noun","size","wav")) %>% 
  # cat_small.
  # select(contrast_cond,prag_context_cond,timebin,noun,target_fix,compet_fix) %>% 
  select(contrast_cond,prag_context_cond,timebin,target_fix,compet_fix) %>% 
  filter(!(target_fix == 1 & compet_fix == 1)) %>% 
  mutate(other_fix = ifelse(target_fix + compet_fix == 0, 1, 0),
         window = ifelse(timebin <=200, "prior", ifelse(timebin <= 830, "adjective","noun"))) %>% 
  pivot_longer(names_to = "Region", values_to = "fixation",cols=target_fix:other_fix) %>%
  mutate(Region = fct_recode(Region,"target" = "target_fix","competitor"="compet_fix","other"="other_fix")) %>% 
  # group_by(contrast_cond,prag_context_cond,window,Region,noun) %>% 
  group_by(contrast_cond,prag_context_cond,window,Region) %>% 
  #summarize(prop_looks=mean(fixation),ci_low_looks=ci.low(fixation),ci_high_looks=ci.high(fixation)) %>%
  summarize(prop_looks=mean(fixation)) %>% 
  ungroup() %>% 
  #mutate(ymin_looks=prop_looks-ci_low_looks,ymax_looks=prop_looks+ci_high_looks) %>% 
  rename(pragContext = prag_context_cond, cond = contrast_cond)


#ryskin had this plot
#plot by timebin and plot fixations over time without plotting against selections

toplot_select =  d_test %>%
  # select(workerid,pragContext,cond,click_prior,click_adj,click_noun,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number,trial,noun) %>%
  select(workerid,pragContext,cond,click_prior,click_adj,click_noun,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number,trial) %>%
  pivot_longer(names_to = "window", values_to = "selection",cols=click_prior:click_noun) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(other = case_when(target + competitor == 0 ~ 1,
                                TRUE ~ 0)) %>%
  # group_by(cond,pragContext,window,noun) %>%
  group_by(cond,pragContext,window) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor),m_other=mean(other),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor),ci_low_other=ci.low(other),ci_high_other=ci.high(other)) %>%
  ungroup() %>% 
  mutate(info_target = paste(m_target, m_target-ci_low_target, m_target+ci_high_target), info_competitor=paste(info_competitor = paste(m_competitor, m_competitor-ci_low_competitor, m_competitor+ci_high_competitor)),info_other = paste(m_other, m_other-ci_low_other, m_other+ci_high_other)) %>% 
  select(cond,pragContext,window,info_target,info_competitor,info_other) %>% 
  pivot_longer(names_to="location",values_to="info_selections",cols=info_target:info_other) %>%
  separate(info_selections,into=c("prop_selections","ymin_selections","ymax_selections"),sep=" ") %>% 
  # mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  # mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  # mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  #exclude distractor to better resemble ryskin fig. 2
  mutate(Region=fct_recode(location,"competitor"="info_competitor","target"="info_target","other"="info_other")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>% 
  mutate(window=fct_recode(window,prior="click_prior",adjective="click_adj",noun="click_noun")) %>% 
  mutate(pragContext=fct_recode(pragContext,reliable="good",unreliable="bad")) %>% 
  mutate(pragContext = fct_relevel(pragContext,"reliable","unreliable")) %>% 
  mutate(cond = fct_relevel(cond,"no_contrast","contrast")) %>% 
  # select(cond, pragContext, window, Region, prop_selections,noun)
  select(cond, pragContext, window, Region, prop_selections, ymin_selections, ymax_selections)

# toplot_select %>% 
#   arrange(noun,cond,pragContext,window) %>% 
#   view()
# 
# toplot_eye %>% 
#   arrange(noun,cond,pragContext,window) %>% 
#   view()

# toplot = left_join(toplot_select, toplot_eye, by=c("cond","noun","pragContext","window","Region"))
toplot = left_join(toplot_select, toplot_eye, by=c("cond","pragContext","window","Region")) %>% 
  mutate(window = fct_relevel(window,"prior","adjective")) %>% 
  mutate(ymin_selections = as.numeric(ymin_selections),ymax_selections = as.numeric(ymax_selections),prop_selections = as.numeric(prop_selections))
nrow(toplot)
toplot

ggplot(toplot, aes(x=prop_selections,y=prop_looks,color=Region,shape=cond)) +
  geom_point() +
  #geom_errorbar(aes(ymin=ymin_looks,ymax=ymax_looks),width=0) +
  #geom_errorbarh(aes(xmin=ymin_selections,xmax=ymax_selections),height=0) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    # color="Window",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(pragContext~window)
ggsave("../graphs/corr_faceted_pragcond_prior_included.pdf")

ggplot(toplot, aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext)) +
  geom_point() +
  #geom_errorbar(aes(ymin=ymin_looks,ymax=ymax_looks),width=0) +
  #geom_errorbarh(aes(xmin=ymin_selections,xmax=ymax_selections),height=0) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    # color="Window",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(cond~window)

ggsave("../graphs/corr_faceted_contrastcond.pdf")

ggplot(toplot, aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(method="lm") +
  geom_point() +
  #geom_errorbar(aes(ymin=ymin_looks,ymax=ymax_looks),width=0) +
  #geom_errorbarh(aes(xmin=ymin_selections,xmax=ymax_selections),height=0) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    # color="Window",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) 
ggsave("../graphs/corr_overall.pdf",width=5.5,height=3.5)

#TODOL report these correlations (w/ facets, report the correlations with it)
#facet by window
#also include by-item for unreliable
#selections on x axis, corresponding eye movements on y axis
# overall correlation between eye movement and decision task data
cor.test(toplot$prop_selections,toplot$prop_looks) # .88. df=22, p<.0001

# correlation between eye movement and decision task data separately by time window
cors_window = toplot %>% 
  group_by(window) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window
cors_window # .38 (in adjective window, ns); .99 in noun window, p<.0001

# correlation between eye movement and decision task data separately by pragmatic reliability condition
cors_prag = toplot %>% 
  filter(window != "prior") %>% 
  group_by(pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_prag # .38 (in adjective window, ns); .99 in noun window, p<.0001



# model summary: 
# - main effect of explicit beliefs in predicted direction
# - effect of explicit beliefs varies by window: bigger in noun than adj window
# effect of explicit beliefs varies by region: beliefs have less explanatory power for distractor looks (but no diff between target and competitor) --> overall, the correlation plots show greater "other" looks across the board than predicted by selection proportions

# contrasts(toplot$window)
# contrasts(toplot$window) = cbind("det.to.baseline"=c(1,0,0,0),"det.to.gender"=c(0,1,0,0),"det.to.noun"=c(0,0,0,1))
tomodel = cbind(toplot,myCenter(toplot[,c("prop_selections","pragContext","cond")]))

#cogsci 2020 paper model:
m = lm(prop_looks ~ cprop_selections + cprop_selections:window + cprop_selections:Region, data=tomodel)
summary(m)

# adding interactions with experimental conditions of interest also doesn't change anything
m = lm(prop_looks ~ cprop_selections + cprop_selections:window + cprop_selections:Region+ cprop_selections:cpragContext + cprop_selections:ccond, data=tomodel)
summary(m)

# model for only targets (because full model violates assumption of independence of samples)
targ = toplot %>% 
  filter(Region == "target") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

m.targ = lm(prop_looks ~ cprop_selections + cprop_selections:window, data=targ)
summary(m.targ)



# submodels for each window -- NOT YET RUN
d_baseline = toplot %>% 
  filter(window == "baseline") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

m.baseline = lm(prop_looks ~ cprop_selections*Region, data=d_baseline)
summary(m.baseline)

d_gender = toplot %>% 
  filter(window == "gender") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

m.gender = lm(prop_looks ~ cprop_selections*Region, data=d_gender)
summary(m.gender)

d_det = toplot %>% 
  filter(window == "determiner+name") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

m.det = lm(prop_looks ~ cprop_selections*Region, data=d_det)
summary(m.det)

d_noun = toplot %>% 
  filter(window == "noun") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

m.noun = lm(prop_looks ~ cprop_selections*Region, data=d_noun)
summary(m.noun)

