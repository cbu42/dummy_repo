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
df$response_times

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

# remove trials with incorrect selections
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

# Begin: Ryskin script
## Experiment 2 

### E2 GLMM: Adj + Noun 

# expt2 reading in adj+noun window data for glmm
# reads in a large file - can be downloaded locally from Ryskin 2019's OSF page
pragtrain3_data = read_tsv('C:/Users/cb476/OneDrive/Desktop/ALPS Lab/PragTrain3_adjnoun_for_GLMM_timesummLongForm.txt', col_names=c('target_dur','contrast_dur','compet_dur','other_dur','currsubj','subject','trialnum','order','timebin','trialID','condWith','condBet','targ_loc','contrast_loc','compet_loc','list','trialType','accuracy','target_fix','target_AR1','compet_fix','compet_AR1')) %>% 
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

pragtrain3_list1 = read_tsv('../data/ryskin_eyetracking/experiments_2-3_trials_list_1.txt')
pragtrain3_list2 = read_tsv('../data/ryskin_eyetracking/experiments_2-3_trials_list_2.txt')
pragtrain3_lists = bind_rows(pragtrain3_list1,pragtrain3_list2) %>% 
  mutate(unique_ID = str_c(as.character(counterbalance),'_',trialID))
pragtrain3_data = pragtrain3_data %>% 
  left_join(pragtrain3_lists, by = c('uniqueID'='unique_ID'))
#rm(pragtrain3_list1, pragtrain3_list2, pragtrain3_lists)

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

# expt2 adj+noun glmm
m_pragtrain3_adjnoun_glmm = glmer(target_fix ~ contrast_cond*prag_context_cond + target_AR1_corrected + 
                                    (1+contrast_cond|subject)+(1+prag_context_cond|audio),
                                  data = pragtrain3_crit ,
                                  family = binomial(link = 'logit'),
                                  glmerControl(optimizer = 'bobyqa',
                                               optCtrl=list(maxfun=2e5)))

#corresponds to table 4 on page 18 of ryskin et al. (possibly fixed effects only??)
summary(m_pragtrain3_adjnoun_glmm)
#contrast_cond1:prag... the neg estimate means the effect of contrast is smaller for unreliable speakers compared to reliable condition
#TODO: look at adj window as well!
#needed: df where each row corresponds to combination of time window to a particular region in particular condition
#outcome var is proportion of selections
#change_cb
summary(pragtrain3_crit$target_fix)
view(pragtrain3_crit[1:50,])
table(pragtrain3_crit$target_fix)
table(pragtrain3_crit$compet_fix)
names(pragtrain3_crit)
#1/0: viewing target or not in 10 ms bins

# see if the paper makes mention of target_AR1_corrected - it might encode what the participants' previous fixation was
# note to self about fixations: curr usually predicted by previous fixation
table(pragtrain3_crit$target_AR1_corrected)

### E2 GLMM Adj + Noun by experiment halves

# expt2 adding half contrasts
pragtrain3_crit2 = pragtrain3_crit %>% 
  mutate(half= if_else(trialnum<151,"first","second"))

pragtrain3_crit2$trialorder_c = scale(pragtrain3_crit2$trialnum,scale=F,center=T)
pragtrain3_crit2$half<-factor(pragtrain3_crit2$half)
contrasts(pragtrain3_crit2$half)<-c(-0.5,0.5)
contrasts(pragtrain3_crit2$half)
# expt2 adjnoun glmm by halves
m_pragtrain3_adjnoun_glmm.halves.3 = glmer(
  target_fix ~ contrast_cond*prag_context_cond*half + target_AR1_corrected + (1+contrast_cond|subject)+(1+prag_context_cond|audio),
  data = pragtrain3_crit2 ,
  family = binomial(link = 'logit'),
  glmerControl(optimizer = 'bobyqa',
               optCtrl=list(maxfun=2e5)))

summary(m_pragtrain3_adjnoun_glmm.halves.3)

### E2 GLMM Adj + Noun by trial order 

# expt2 adjnoun glmm by trialorder
names(pragtrain3_crit2)
names(pragtrain3_crit)

#change_cb: parameter target_AR1_c from original does not exist
# m_pragtrain3_adjnoun_glmm.order3 = glm(target_fix ~ contrast_cond*prag_context_cond*trialorder_corrected + target_AR1_c ,
#                                        data = pragtrain3_crit2 )

#original
m_pragtrain3_adjnoun_glmm.order3 = glm(target_fix ~ contrast_cond*prag_context_cond*trialorder_c + target_AR1_c ,
                                       data = pragtrain3_crit2 )
summary(m_pragtrain3_adjnoun_glmm.order3)

### E2 GLMM: Adj window

# expt2 adj glmm
m_pragtrain3_adj_glmm = glmer(target_fix ~ contrast_cond*prag_context_cond + target_AR1_corrected +       
                                (1+contrast_cond+target_AR1_corrected|subject)+(1+contrast_cond+target_AR1_corrected|audio),
                              data = pragtrain3_crit %>% filter(timebin <= 830),
                              family = binomial(link = 'logit'),
                              glmerControl(optimizer = 'bobyqa',
                                           optCtrl=list(maxfun=2e5)))

summary(m_pragtrain3_adj_glmm)

### E2 GLMM: Noun window


# expt2 reading in noun window data for GLMM
pragtrain3_noun_data = read_tsv('C:/Users/cb476/OneDrive/Desktop/ALPS Lab/PragTrain3_noun_for_GLMM_timesummLongForm.txt', col_names=c('target_dur','contrast_dur','compet_dur','other_dur','currsubj','subject','trialnum','order','timebin','trialID','condWith','condBet','targ_loc','contrast_loc','compet_loc','list','trialType','accuracy','target_fix','target_AR1','compet_fix','compet_AR1')) %>%
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

pragtrain3_noun_data = pragtrain3_noun_data %>% 
  left_join(pragtrain3_lists, by = c('uniqueID'='unique_ID'))

pragtrain3_noun_crit = pragtrain3_noun_data %>% 
  group_by(subject,trialnum) %>% 
  mutate(target_AR1_corrected = lag(target_fix),
         compet_AR1_corrected = lag(compet_fix)) %>% 
  filter(trial_type == 'test'& target_AR1 != 999) %>% 
  ungroup()

# expt2 noun window setting up factors and contrasts
pragtrain3_noun_crit$timebin_rel_c = scale(pragtrain3_noun_crit$timebin_rel,scale=F, center = T)
pragtrain3_noun_crit$prag_context_cond = factor(pragtrain3_noun_crit$prag_context_cond)
pragtrain3_noun_crit$contrast_cond = factor(pragtrain3_noun_crit$contrast_cond)
contrasts(pragtrain3_noun_crit$prag_context_cond)<-c(-0.5,0.5)
contrasts(pragtrain3_noun_crit$contrast_cond)<-c(0.5,-0.5)

contrasts(pragtrain3_noun_crit$prag_context_cond)
contrasts(pragtrain3_noun_crit$contrast_cond)

# expt2 noun window glmm 
m_pragtrain3_noun_glmm.1 = glmer(target_fix ~ contrast_cond*prag_context_cond + target_AR1_corrected + 
                                   (1+target_AR1_corrected|subject)+(1+contrast_cond+target_AR1_corrected|audio),
                                 data = pragtrain3_noun_crit,
                                 family = binomial(link = 'logit'),
                                 glmerControl(optimizer = 'bobyqa',
                                              optCtrl=list(maxfun=2e5)))

summary(m_pragtrain3_noun_glmm.1)

### E2 Average proportions: Adj+ Noun window

# expt2 reading in adj+noun window data for target proportion
pragtrain3_adjnoun_tarprop_data = read_tsv('expt2/PragTrain3_adjnoun_window_for_tardur_tarDURdatastruct.txt', col_names=c('target_dur','contrast_dur','compet_dur','other_dur','currsubj','order','item','targ_loc','condWith','condBet','test','subject')) %>% # item = [list,trialID]; keep = 1 if trialType == 'test', 0 otherwise
  mutate(contrast_cond = case_when(
    condWith == 0 ~ 'filler', 
    condWith == 1 ~ 'no_contrast',
    condWith == 2 ~ 'contrast'),
    prag_context_cond = case_when(
      condBet == 10 ~ 'reliable',
      condBet == 9 ~ 'unreliable',
      condBet == 0 ~ 'filler'),
    list = str_c('l',stri_sub(item,1,1)),
    trialID = if_else(test == 1,str_c('test',stri_sub(item,2,-1)),'other'),
    uniqueID = str_c(list,'_',trialID),
    total_dur = (target_dur + contrast_dur + compet_dur + other_dur),
    target_prop = target_dur/total_dur,
    contrast_prop = contrast_dur/total_dur,
    compet_prop = compet_dur/total_dur,
    other_prop = other_dur/total_dur,
    target_adv = target_prop - compet_prop,
    elog_target_prop = log((target_prop+.5)/(1-target_prop+.5)))

pragtrain3_adjnoun_tarprop_data = pragtrain3_adjnoun_tarprop_data %>% 
  left_join(pragtrain3_lists, by = c('uniqueID'='unique_ID'))

pragtrain3_adjnoun_tarprop_crit = pragtrain3_adjnoun_tarprop_data %>% 
  filter(test == 1) 

# expt2 adjnoun tarprop making factors and contrasts
pragtrain3_adjnoun_tarprop_crit$prag_context_cond  = factor(pragtrain3_adjnoun_tarprop_crit$prag_context_cond)
pragtrain3_adjnoun_tarprop_crit$contrast_cond = factor(pragtrain3_adjnoun_tarprop_crit$contrast_cond)

contrasts(pragtrain3_adjnoun_tarprop_crit$prag_context_cond)<-c(-0.5,0.5)
contrasts(pragtrain3_adjnoun_tarprop_crit$contrast_cond)<-c(0.5,-0.5)

contrasts(pragtrain3_adjnoun_tarprop_crit$prag_context_cond)
contrasts(pragtrain3_adjnoun_tarprop_crit$contrast_cond)

# expt2 model adjnoun tarprop
pt3_model_adjnoun_tarprop.2 = lmer(elog_target_prop ~ prag_context_cond*contrast_cond + (1+contrast_cond|subject)+ (1+prag_context_cond |audio), data=pragtrain3_adjnoun_tarprop_crit)

summary(pt3_model_adjnoun_tarprop.2)

# expt2 model adjnoun tarprop follow-up
pragtrain3_adjnoun_tarprop_crit$prag_context_cond<-relevel(pragtrain3_adjnoun_tarprop_crit$prag_context_cond, ref="reliable")

pt3_model_adjnoun_tarprop.2.b = lmer(elog_target_prop ~ prag_context_cond*contrast_cond + (1+contrast_cond|subject)+ (1+prag_context_cond |audio), data=pragtrain3_adjnoun_tarprop_crit)

summary(pt3_model_adjnoun_tarprop.2.b)

### E2 Average Proportions Adj + Noun by experiment halves

# expt2 adding half contrasts to tarprop
pragtrain3_adjnoun_tarprop_crit2 = pragtrain3_adjnoun_tarprop_crit %>% 
  mutate(half= if_else(order<151,"first","second"))

pragtrain3_adjnoun_tarprop_crit2$trialorder_c = scale(pragtrain3_adjnoun_tarprop_crit2$order,scale=F,center=T)
pragtrain3_adjnoun_tarprop_crit2$half<-factor(pragtrain3_adjnoun_tarprop_crit2$half)
contrasts(pragtrain3_adjnoun_tarprop_crit2$half)<-c(-0.5,0.5)
contrasts(pragtrain3_adjnoun_tarprop_crit2$half)

# expt2 model adjnoun tarprop by half
pt3_model_adjnoun_tarprop.half2 = lmer(elog_target_prop ~ prag_context_cond*contrast_cond*half + (1+contrast_cond+half|subject)+ (1+prag_context_cond*half |audio), data=pragtrain3_adjnoun_tarprop_crit2)

summary(pt3_model_adjnoun_tarprop.half2)

### E2 Average Proportions Adj + Noun by trial order

# expt2 model adjnoun tarprop by order
pt3_model_adjnoun_tarprop.order4 = lmer(elog_target_prop ~ prag_context_cond*contrast_cond*trialorder_c + (1+contrast_cond|subject)+ (1+prag_context_cond |audio), data=pragtrain3_adjnoun_tarprop_crit2,control = lmerControl(optCtrl = list(maxfun=2e5)))

summary(pt3_model_adjnoun_tarprop.order4)

### E2 Average proportions: Adj window

# expt2 reading in adj window data for target proportion
pragtrain3_adj_tarprop_data = read_tsv('expt2/PragTrain3_adj_window_for_tardur_tarDURdatastruct.txt', col_names=c('target_dur','contrast_dur','compet_dur','other_dur','currsubj','order','item','targ_loc','condWith','condBet','test','subject')) %>% # item = [list,trialID]; keep = 1 if trialType == 'test', 0 otherwise
  mutate(contrast_cond = case_when(
    condWith == 0 ~ 'filler', 
    condWith == 1 ~ 'no_contrast',
    condWith == 2 ~ 'contrast'),
    prag_context_cond = case_when(
      condBet == 10 ~ 'reliable',
      condBet == 9 ~ 'unreliable',
      condBet == 0 ~ 'filler'),
    list = str_c('l',stri_sub(item,1,1)),
    trialID = if_else(test == 1,str_c('test',stri_sub(item,2,-1)),'other'),
    uniqueID = str_c(list,'_',trialID),
    total_dur = (target_dur + contrast_dur + compet_dur + other_dur),
    target_prop = target_dur/total_dur,
    contrast_prop = contrast_dur/total_dur,
    compet_prop = compet_dur/total_dur,
    other_prop = other_dur/total_dur,
    target_adv = target_prop - compet_prop,
    elog_target_prop = log((target_prop+.5)/(1-target_prop+.5)))

pragtrain3_adj_tarprop_data = pragtrain3_adj_tarprop_data %>% 
  left_join(pragtrain3_lists, by = c('uniqueID'='unique_ID'))

pragtrain3_adj_tarprop_crit = pragtrain3_adj_tarprop_data %>% 
  filter(test == 1) 

# expt2 adj tarprop making factors and contrasts
pragtrain3_adj_tarprop_crit$prag_context_cond  = factor(pragtrain3_adj_tarprop_crit$prag_context_cond)
pragtrain3_adj_tarprop_crit$contrast_cond = factor(pragtrain3_adj_tarprop_crit$contrast_cond)

contrasts(pragtrain3_adj_tarprop_crit$prag_context_cond)<-c(-0.5,0.5)
contrasts(pragtrain3_adj_tarprop_crit$contrast_cond)<-c(0.5,-0.5)

contrasts(pragtrain3_adj_tarprop_crit$prag_context_cond)
contrasts(pragtrain3_adj_tarprop_crit$contrast_cond)

#r expt2 model adj tarprop
pt3_model_adj_tarprop.2 = lmer(elog_target_prop ~ prag_context_cond*contrast_cond + (1|subject)+ (1 |audio), data=pragtrain3_adj_tarprop_crit)

summary(pt3_model_adj_tarprop.2)

### E2 Average proportions: Noun window 

# expt2 reading in noun window data for target proportion 
pragtrain3_noun_tarprop_data = read_tsv('expt2/PragTrain3_noun_window_for_tardur_tarDURdatastruct.txt', col_names=c('target_dur','contrast_dur','compet_dur','other_dur','currsubj','order','item','targ_loc','condWith','condBet','test','subject')) %>% # item = [list,trialID]; keep = 1 if trialType == 'test', 0 otherwise
  mutate(contrast_cond = case_when(
    condWith == 0 ~ 'filler', 
    condWith == 1 ~ 'no_contrast',
    condWith == 2 ~ 'contrast'),
    prag_context_cond = case_when(
      condBet == 10 ~ 'reliable',
      condBet == 9 ~ 'unreliable',
      condBet == 0 ~ 'filler'),
    list = str_c('l',stri_sub(item,1,1)),
    trialID = if_else(test == 1,str_c('test',stri_sub(item,2,-1)),'other'),
    uniqueID = str_c(list,'_',trialID),
    total_dur = (target_dur + contrast_dur + compet_dur + other_dur),
    target_prop = target_dur/total_dur,
    contrast_prop = contrast_dur/total_dur,
    compet_prop = compet_dur/total_dur,
    other_prop = other_dur/total_dur,
    target_adv = target_prop - compet_prop,
    elog_target_prop = log((target_prop+.5)/(1-target_prop+.5)))

pragtrain3_noun_tarprop_data = pragtrain3_noun_tarprop_data %>% 
  left_join(pragtrain3_lists, by = c('uniqueID'='unique_ID'))

pragtrain3_noun_tarprop_crit = pragtrain3_noun_tarprop_data %>% 
  filter(test == 1) 

# expt2 noun tarprop making factors and contrasts
pragtrain3_noun_tarprop_crit$prag_context_cond  = factor(pragtrain3_noun_tarprop_crit$prag_context_cond)
pragtrain3_noun_tarprop_crit$contrast_cond = factor(pragtrain3_noun_tarprop_crit$contrast_cond)

contrasts(pragtrain3_noun_tarprop_crit$prag_context_cond)<-c(-0.5,0.5)
contrasts(pragtrain3_noun_tarprop_crit$contrast_cond)<-c(0.5,-0.5)

contrasts(pragtrain3_noun_tarprop_crit$prag_context_cond)
contrasts(pragtrain3_noun_tarprop_crit$contrast_cond)

# expt2 model noun tarprop
pt3_model_noun_tarprop.0 = lmer(elog_target_prop ~ prag_context_cond*contrast_cond + (1+contrast_cond|subject)+ (1+ prag_context_cond |audio), data=pragtrain3_noun_tarprop_crit)

summary(pt3_model_noun_tarprop.0)

# End: Ryskin script


# load eye-tracking data from Ryskin 2019
# adj_tar <- read.delim("ryskin_eyetracking/PragTrain3_adj_window_for_tardur_tarDURdatastruct.txt")
# adjn_GLMM <- read.delim("ryskin_eyetracking/PragTrain3_adjnoun_for_GLMM_timesummLongForm.txt")
# adjn_tar <- read.delim("ryskin_eyetracking/PragTrain3_adjnoun_window_for_tardur_tarDURdatastruct.txt")
# noun_GLMM <- read.delim("ryskin_eyetracking/PragTrain3_noun_for_GLMM_timesummLongForm.txt")
# noun_tar <- read.delim("ryskin_eyetracking/PragTrain3_noun_window_for_tardur_tarDURdatastruct.txt")
# baseline = read.csv("sb_eyetracking/exp200ms_beselinedata.csv", header = TRUE)

# order should be: baseline / gender / determiner + name / noun ---> will ignore  "preview" since there's no corresponding window in the incremental decision experiment

g = rbind(baseline,gender,determiner,name,end)  %>% 
  mutate(item=word(as.character(instruction), -1))

# re-load incremental decision data 
s = read.csv("trials_merged.csv", header = TRUE)  %>% 
  mutate(item=word(as.character(instruction3), -1))

s$response = gsub(" ","",s$response)
s$response = gsub("\\[","",s$response)
s$response = gsub("\\]","",s$response)
s$response = gsub("\\'","",s$response)
s$response = gsub("AOI","",s$response)


selection = s %>%
  filter(ExpFiller=="Exp") %>%
  separate(response,into=c("baseline","gender","determiner+name","noun"),sep=",") %>%
  gather(window,location,baseline:noun) %>%
  select(workerid,Prime,condition,determiner,size,window,location,target1,target2,competitor1,competitor2,item) %>%
  mutate(targetclick=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitorclick=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0))) %>%
  mutate(distractorclick=ifelse(targetclick=="1",0,ifelse(competitorclick=="1",0,1))) %>%
  group_by(determiner,size,window,item) %>%
  summarize(Mean_target_selection=mean(targetclick),Mean_competitor_selection=mean(competitorclick),Mean_distractor_selection=mean(distractorclick))

gaze =  g %>%
  filter(TrackLoss=="FALSE") %>%
  select(Prime,condition,determiner,size,targetlook,competitorlook,residuelook,whichword,item) %>%
  mutate(distractorlook=ifelse(targetlook=="1",0,ifelse(competitorlook=="1",0,ifelse(residuelook=="1",0,1)))) %>%
  mutate(targetdistractorlook = ifelse(targetlook=="1",1,ifelse(distractorlook=="1",1,0))) %>%
  mutate(competitordistractorlook = ifelse(competitorlook=="1",1,ifelse(distractorlook=="1",1,0))) %>%
  mutate(window=as.character(whichword)) %>%
  mutate(window = ifelse(whichword =="determiner","determiner+name", ifelse(whichword=="name","determiner+name",ifelse(whichword=="end","noun",window)))) %>%
  group_by(determiner,size,window,item) %>%
  summarize(Mean_target_look=mean(targetlook),Mean_competitor_look=mean(competitorlook),Mean_distractor_look=mean(distractorlook),Mean_targetdistractor_look=mean(targetdistractorlook),Mean_competitordistractor_look=mean(competitordistractorlook))

df = merge(selection, gaze, by=c("determiner","size","window","item"))
df$window_re<- factor(df$window, levels = c("baseline","gender","determiner+name","noun"))

# CORRELATIONAL ANALYSES

# compute and visualize overall correlation
longer_selections = df %>% 
  select(-Mean_target_look,-Mean_competitor_look,-Mean_distractor_look,-Mean_targetdistractor_look,-Mean_competitordistractor_look,-window_re) %>% 
  pivot_longer(cols=c("Mean_target_selection","Mean_competitor_selection","Mean_distractor_selection"),names_to=c("delete_this","Region","remove_this"),names_sep=c("_"),values_to="prop_selections") %>% 
  select(-delete_this,-remove_this)

longer_looks = df %>% 
  select(-Mean_target_selection,-Mean_competitor_selection,-Mean_distractor_selection,-Mean_targetdistractor_look,-Mean_competitordistractor_look,-window_re) %>% 
  pivot_longer(cols=c("Mean_target_look","Mean_competitor_look","Mean_distractor_look"),names_to=c("delete_this","Region","remove_this"),names_sep=c("_"),values_to="prop_looks") %>% 
  select(-delete_this,-remove_this)

toplot = longer_looks %>% 
  left_join(longer_selections,by=c("determiner","size","window","Region","item")) %>% 
  mutate(determiner=fct_recode(determiner,"number"="two","number"="three")) %>% 
  mutate(Region=fct_relevel(Region,"target","competitor"),window=fct_relevel(window,"baseline","gender")) %>% 
  droplevels()

#selections on x axis, corresponding eye movements on y axis
# overall correlation between eye movement and decision task data
cor.test(toplot$prop_looks,toplot$prop_selections) # .87

# correlation between eye movement and decision task data separately by time window
cors_window = toplot %>% 
  group_by(window) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window # .57, .81, .91, .96

# how many unique items?
toplot %>% 
  filter(window == "baseline" & Region == "target") %>% 
  select(determiner,size,item) %>% 
  unique() %>% 
  nrow()

ggplot(toplot, aes(x=prop_selections, y=prop_looks)) +
  geom_point(size=2,aes(color=Region),alpha=.6) +
  geom_smooth(method='lm',size=1,color="grey26",group=1) +
  # geom_smooth(method='lm',size=1,aes(color=Region)) +
  geom_abline(slope=1,linetype="dotted",color="gray40") +
  geom_text(data=cors_window, aes(label=paste("r=",Correlation)), x=.5,y=.9) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    # shape="Window",
    # color="Window",
    x="Proportion of selections (Exp. 1)",
    y="Proportion of looks (S. & B., 2020)") +
  xlim(0,1) +
  ylim(0,1) +
  # coord_fixed() +
  #facets by window. for each window it recreates the same plot
  facet_wrap(~window,nrow=1) 
ggsave("../graphs/corr-window.pdf",width=10,height=2.5)


# collapsing across items
agr = toplot %>% 
  group_by(window,Region,determiner,size) %>% 
  summarize(prop_selections = mean(prop_selections),prop_looks=mean(prop_looks))

cors_window_it = agr %>% 
  group_by(window) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_it # .87, .98, .97, 1

ggplot(agr, aes(x=prop_selections, y=prop_looks, group=1)) +
  geom_point(size=2,aes(color=window)) +
  geom_smooth(method='lm',size=1,color="grey26") +
  geom_abline(slope=1,linetype="dotted",color="gray40") +
  geom_text(data=cors_window_it, aes(label=paste("r=",Correlation)), x=.5,y=.9) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    # shape="Window",
    # color="Window",
    x="Proportion of selections (Exp. 1)",
    y="Proportion of looks (S. & B., 2020)") +
  xlim(0,1) +
  ylim(0,1) +
  # coord_fixed() +
  facet_wrap(~window) +
  theme(legend.position="top")
ggsave("../graphs/corr-window-coll.pdf",width=6,height=3)



# correlation between eye movement and decision task data separately by condition within determiner window
cors_determiner = toplot %>% 
  filter(window == "determiner+name") %>% 
  group_by(determiner, size) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_determiner

# determiner size  Correlation     P
# <fct>      <chr>       <dbl> <dbl>
# 1 all        big          0.94     0
# 2 all        small        0.87     0
# 3 some       big          0.88     0
# 4 some       small        0.82     0
# 5 number     big          0.95     0
# 6 number     small        0.96     0

ggplot(toplot %>% filter(window == "determiner+name"), aes(x=prop_selections, y=prop_looks)) +
  geom_point(size=2,aes(color=Region,alpha=size)) +
  geom_smooth(method='lm',size=1,color="grey26",group=1) +
  # geom_smooth(method='lm',size=1) +
  geom_abline(slope=1,linetype="dotted",color="gray40") +
  geom_text(data=cors_determiner, aes(label=paste("r=",Correlation),alpha=size), x=c(.3,.7,.3,.7,.3,.7),y=.9, show.legend = FALSE) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  scale_alpha_manual(values=c(.9,.3)) +
  labs(
    size="Set size",
    color="Region",
    x="Proportion of selections (Exp. 1)",
    y="Proportion of looks (S. & B., 2020)") +
  xlim(0,1) +
  ylim(0,1) +
  # coord_fixed() +
  facet_wrap(~determiner,nrow=1) +
  theme(legend.position="top",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10),legend.spacing.y = unit(0.001, 'cm'))
ggsave("../graphs/corr-determiner.pdf",width=6,height=2.5)


# collapsing across items
agr = toplot %>% 
  filter(window == "determiner+name") %>% 
  group_by(window,Region,determiner,size) %>% 
  summarize(prop_selections = mean(prop_selections),prop_looks=mean(prop_looks))

cors_determiner_it = agr %>% 
  group_by(determiner, size) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_determiner_it # all close to 1 but p > .05 (too few data points)



# correlation between eye movement and decision task data separately by region
cors_reg = toplot %>% 
  group_by(Region) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_reg # .9, .68, .73 

ggplot(toplot, aes(x=prop_selections, y=prop_looks, group=1)) +
  geom_point(size=2,aes(color=Region,shape=window)) +
  geom_smooth(method='lm',size=1,color="grey26") +
  geom_abline(slope=1,linetype="dotted",color="gray40") +
  geom_text(data=cors_reg, aes(label=paste("r=",Correlation)), x=.5,y=.9) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(shape="Window",
       color="Region",
       x="Proportion of selections (Exp. 1)",
       y="Proportion of looks (S. & B., 2020)") +
  xlim(0,1) +
  ylim(0,1) +
  # coord_fixed() +
  facet_wrap(~Region) +
  guides(color = FALSE) +
  theme(legend.position="top",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10),legend.spacing.y = unit(0.001, 'cm'))
ggsave("../graphs/corr-region.pdf",width=6,height=2.5)

# other plots: full by-condition plot
ggplot(toplot, aes(x=prop_selections, y=prop_looks, group=1)) +
  geom_point(size=2,aes(color=window,shape=Region),alpha=.7) +
  geom_smooth(method='lm',size=1,color="grey26") +
  geom_abline(slope=1,linetype="dotted",color="gray40") +
  # geom_text(data=cors, aes(label=paste("r=",Correlation)), x=.2,y=.9) +
  scale_color_manual(values=c(cbPalette[4],cbPalette[1],cbPalette[5],cbPalette[7])) +
  labs(shape="Region",
       color="Window",
       x="Proportion of selections (Exp. 1)",
       y="Proportion of looks (S. & B., 2020)") +
  xlim(0,1) +
  ylim(0,1) +
  # coord_fixed() +
  facet_grid(size~determiner) +
  theme(legend.direction = "horizontal", legend.box = "vertical") +
  theme(legend.position="top",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10),legend.spacing.y = unit(0.001, 'cm'))#,legend.box.spacing = unit(0.01, 'cm'),) 
# guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("../graphs/correlations-bycondition.pdf",width=6,height=4)

toplot$cprop_selections = toplot$prop_selections - mean(toplot$prop_selections)


# model summary: 
# - big main effect of explicit beliefs
# - effect of explicit beliefs doesn't vary by window or region, except: beliefs have much smaller explanatory power for distractor looks
# NOT IMPORTANT: - overall fewer looks to distractor in all windows except baseline (interactions with window); overall fewer looks to competitor in noun window
contrasts(toplot$window)
contrasts(toplot$window) = cbind("det.to.baseline"=c(1,0,0,0),"det.to.gender"=c(0,1,0,0),"det.to.noun"=c(0,0,0,1))

m = lmer(prop_looks ~ cprop_selections*window*Region + (1+cprop_selections*window*Region|item), data=toplot)
summary(m)

#cogsci paper model:
m = lm(prop_looks ~ cprop_selections + cprop_selections:window + cprop_selections:Region, data=toplot)
summary(m)

# adding interactions with experimental conditions of interest also doesn't change anything
m = lm(prop_looks ~ cprop_selections + cprop_selections:window + cprop_selections:Region+ cprop_selections:determiner + cprop_selections:size, data=toplot)
summary(m)

# model for only targets (because full model violates assumption of independence of samples)
targ = toplot %>% 
  filter(Region == "target") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

m.targ = lm(prop_looks ~ cprop_selections + cprop_selections:window, data=targ)
summary(m.targ)



# submodels for each window
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


# PLOT PROPORTIONS OF LOOKS

# proportion of looks to target, competitor, and residue
gazer =  g %>%
  filter(TrackLoss=="FALSE" & time < 300) %>%
  select(Prime,condition,determiner,size,targetlook,competitorlook,residuelook,whichword,item,time) %>%
  mutate(distractorlook=ifelse(targetlook=="1",0,ifelse(competitorlook=="1",0,ifelse(residuelook=="1",0,1)))) %>%
  # mutate(window=as.character(whichword)) %>%
  # mutate(window = ifelse(whichword =="determiner","determiner+name", ifelse(whichword=="name","determiner+name",ifelse(whichword=="end","noun",window)))) %>%
  filter(targetlook == 1 | competitorlook == 1 | residuelook== 1) %>% #CHANGE
  #filter(targetlook == 1 | competitorlook == 1) %>% #CHANGE
  group_by(time,condition,size) %>%
  summarize(Mean_target_look=mean(targetlook),Mean_competitor_look=mean(competitorlook),Mean_residue_look=mean(residuelook),target_ci_low=ci.low(targetlook),target_ci_high=ci.high(targetlook),competitor_ci_low=ci.low(competitorlook),competitor_ci_high=ci.high(competitorlook),residue_ci_low=ci.low(residuelook),residue_ci_high=ci.high(residuelook)) %>%
  ungroup() %>%
  mutate(YMin_target=Mean_target_look-target_ci_low,YMax_target=Mean_target_look+target_ci_high,YMin_competitor=Mean_competitor_look-competitor_ci_low,YMax_competitor=Mean_competitor_look+competitor_ci_high,YMin_residue=Mean_residue_look-residue_ci_low,YMax_residue=Mean_residue_look+residue_ci_high)

# prepare data for plotting
long_props = gazer %>% 
  select(condition,size,time,Mean_target_look,Mean_residue_look,Mean_competitor_look) %>%  #,other_prop) %>% 
  pivot_longer(cols = Mean_target_look:Mean_competitor_look,names_to=c("region"),values_to=c("proportion")) %>% 
  separate(region,c(NA,"region",NA))

long_ymin = gazer %>% 
  select(condition,size,time,YMin_target,YMin_residue,YMin_competitor) %>%  #,other_prop) %>% 
  pivot_longer(cols = YMin_target:YMin_competitor,names_to=c("region"),values_to=c("ymin")) %>% 
  separate(region,c(NA,"region"))

long_ymax = gazer %>% 
  select(condition,size,time,YMax_target,YMax_residue,YMax_competitor) %>%  #,other_prop) %>% 
  pivot_longer(cols = YMax_target:YMax_competitor,names_to=c("region"),values_to=c("ymax")) %>% 
  separate(region,c(NA,"region"))

toplot = long_props %>%
  left_join(long_ymin,by=c("condition","size","time","region")) %>%
  left_join(long_ymax,by=c("condition","size","time","region")) %>%
  mutate(region = fct_relevel(region,"target","competitor"),determiner = fct_relevel(as.factor(condition),"all","some"))

offset = 71
windowsize = 15.9 #ms
onsets = g %>% 
  summarize(gender=mean(gender_onset)*windowsize-1000,determiner=mean(determiner_onset)*windowsize-1000,name=mean(name_onset)*windowsize-1000,noun=mean(noun_onset)*windowsize-1000)

windows = tibble(window=c("baseline","gender","determiner","name","noun"),x=c(24+offset,70+offset,118+offset,159+offset,188+offset)*windowsize-1000)
vlinesize=.5

toplot$ttime = (toplot$time*windowsize)-1000

ggplot(toplot, aes(x=ttime, y=proportion)) +
  geom_line(size=1, aes(color=determiner,linetype=size)) +
  geom_ribbon(aes(ymin=ymin,ymax=ymax,fill=determiner,group=interaction(determiner,size)),alpha=.3) +
  scale_fill_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  scale_color_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  geom_vline(aes(xintercept=onsets$gender),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$determiner),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$name),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$noun),size=vlinesize) +
  geom_text(data=windows,aes(label=window,x=x),y=.9,size=2) +
  xlab("Time in ms relative to audio onset") +
  ylab("Proportion of looks to target") +  
  scale_x_continuous(breaks=seq(0,4000,by=400),minor_breaks = seq(200,3800,by=400)) +
  facet_wrap(~region)
# theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
ggsave("../graphs/proportions_condsize_withresidue.pdf",width=9,height=3)

# only target and residue looks
ttoplot = toplot %>% 
  filter(region == "target" | region == "residue") %>% 
  droplevels()

ggplot(ttoplot, aes(x=ttime, y=proportion)) +
  geom_line(size=1, aes(color=determiner,linetype=size)) +
  geom_ribbon(aes(ymin=ymin,ymax=ymax,fill=determiner,group=interaction(determiner,size)),alpha=.3) +
  scale_fill_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  scale_color_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  geom_vline(aes(xintercept=onsets$gender),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$determiner),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$name),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$noun),size=vlinesize) +
  geom_text(data=windows,aes(label=window,x=x),y=.95,size=2.5) +
  xlab("Time in ms relative to audio onset") +
  ylab("Proportion of looks to region") +  
  scale_x_continuous(breaks=seq(0,4000,by=400),minor_breaks = seq(200,3800,by=400)) +
  facet_wrap(~region)
# theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
ggsave("../graphs/proportions_condsize_withresidue_tr.pdf",width=9,height=3)

# only target looks
ttoplot = toplot %>% 
  filter(region == "target") %>% 
  droplevels()

ggplot(ttoplot, aes(x=ttime, y=proportion)) +
  geom_line(size=1, aes(color=determiner,linetype=size)) +
  geom_ribbon(aes(ymin=ymin,ymax=ymax,fill=determiner,group=interaction(determiner,size)),alpha=.3) +
  scale_fill_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  scale_color_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  geom_vline(aes(xintercept=onsets$gender),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$determiner),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$name),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$noun),size=vlinesize) +
  geom_text(data=windows,aes(label=window,x=x),y=.9,size=2) +
  xlab("Time in ms relative to audio onset") +
  ylab("Proportion of looks to target") +  
  scale_x_continuous(breaks=seq(0,4000,by=400),minor_breaks = seq(200,3800,by=400)) 
# theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
ggsave("../graphs/proportions_condsize_withresidue_target.pdf",width=5,height=3)



# > g %>% group_by(whichword) %>% summarize(mintime=min(time),meantime=mean(time),mediantime=median(time),maxtime=max(time))
# # A tibble: 5 x 5
# whichword  mintime meantime mediantime maxtime
# * <chr>        <int>    <dbl>      <dbl>   <int>
#   1 baseline        72      93         93      114
# 2 determiner     171     191.       191      212
# 3 end            244     287.       274      868
# 4 gender         115     142.       142.     170
# 5 name           212     228.       227      244

# proportion of looks to target and competitor
gaze =  g %>%
  filter(TrackLoss=="FALSE" & time < 300) %>%
  select(Prime,condition,determiner,size,targetlook,competitorlook,residuelook,whichword,item,time) %>%
  mutate(distractorlook=ifelse(targetlook=="1",0,ifelse(competitorlook=="1",0,ifelse(residuelook=="1",0,1)))) %>%
  # mutate(window=as.character(whichword)) %>%
  # mutate(window = ifelse(whichword =="determiner","determiner+name", ifelse(whichword=="name","determiner+name",ifelse(whichword=="end","noun",window)))) %>%
  filter(targetlook == 1 | competitorlook == 1) %>% #CHANGE
  #filter(targetlook == 1 | competitorlook == 1) %>% #CHANGE
  group_by(time,condition,size) %>%
  summarize(Mean_target_look=mean(targetlook),Mean_competitor_look=mean(competitorlook),target_ci_low=ci.low(targetlook),target_ci_high=ci.high(targetlook),competitor_ci_low=ci.low(competitorlook),competitor_ci_high=ci.high(competitorlook)) %>%
  ungroup() %>%
  mutate(YMin_target=Mean_target_look-target_ci_low,YMax_target=Mean_target_look+target_ci_high,YMin_competitor=Mean_competitor_look-competitor_ci_low,YMax_competitor=Mean_competitor_look+competitor_ci_high)

# prepare data for plotting
long_props = gaze %>% 
  select(condition,size,time,Mean_target_look,Mean_competitor_look) %>%  #,other_prop) %>% 
  pivot_longer(cols = Mean_target_look:Mean_competitor_look,names_to=c("region"),values_to=c("proportion")) %>% 
  separate(region,c(NA,"region",NA))

long_ymin = gaze %>% 
  select(condition,size,time,YMin_target,YMin_competitor) %>%  #,other_prop) %>% 
  pivot_longer(cols = YMin_target:YMin_competitor,names_to=c("region"),values_to=c("ymin")) %>% 
  separate(region,c(NA,"region"))

long_ymax = gaze %>% 
  select(condition,size,time,YMax_target,YMax_competitor) %>%  #,other_prop) %>% 
  pivot_longer(cols = YMax_target:YMax_competitor,names_to=c("region"),values_to=c("ymax")) %>% 
  separate(region,c(NA,"region"))

toplot = long_props %>%
  left_join(long_ymin,by=c("condition","size","time","region")) %>%
  left_join(long_ymax,by=c("condition","size","time","region")) %>%
  mutate(region = fct_relevel(region,"target","competitor"),determiner = fct_relevel(as.factor(condition),"all","some"))

# dodge=position_dodge(.9)
offset = 71
windowsize = 15.9 #ms
onsets = g %>% 
  summarize(gender=mean(gender_onset)*windowsize-1000,determiner=mean(determiner_onset)*windowsize-1000,name=mean(name_onset)*windowsize-1000,noun=mean(noun_onset)*windowsize-1000)

windows = tibble(window=c("baseline","gender","determiner","name","noun"),x=c(24+offset,70+offset,118+offset,159+offset,188+offset)*windowsize-1000)
vlinesize=.5

toplot$ttime = (toplot$time*windowsize)-1000

ggplot(toplot, aes(x=ttime, y=proportion)) +
  geom_line(size=1, aes(color=determiner,linetype=size)) +
  geom_ribbon(aes(ymin=ymin,ymax=ymax,fill=determiner,group=interaction(determiner,size)),alpha=.3) +
  scale_fill_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  scale_color_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  geom_vline(aes(xintercept=onsets$gender),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$determiner),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$name),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$noun),size=vlinesize) +
  geom_text(data=windows,aes(label=window,x=x),y=.9,size=2) +
  xlab("Time in ms relative to audio onset") +
  ylab("Proportion of looks to target") +  
  scale_x_continuous(breaks=seq(0,4000,by=400),minor_breaks = seq(200,3800,by=400)) +
  facet_wrap(~region)
# theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
ggsave("../graphs/proportions_condsize.pdf",width=7,height=3)

# only target looks
ttoplot = toplot %>% 
  filter(region == "target") %>% 
  droplevels()

ggplot(ttoplot, aes(x=ttime, y=proportion)) +
  geom_line(size=1, aes(color=determiner,linetype=size)) +
  geom_ribbon(aes(ymin=ymin,ymax=ymax,fill=determiner,group=interaction(determiner,size)),alpha=.3) +
  scale_fill_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  scale_color_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  geom_vline(aes(xintercept=onsets$gender),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$determiner),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$name),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$noun),size=vlinesize) +
  geom_text(data=windows,aes(label=window,x=x),y=.9,size=2) +
  xlab("Time in ms relative to audio onset") +
  ylab("Proportion of looks to target") +  
  scale_x_continuous(breaks=seq(0,4000,by=400),minor_breaks = seq(200,3800,by=400)) 
# theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
ggsave("../graphs/proportions_condsize_target.pdf",width=5,height=3)

# MODELS
tomodel = g %>%
  select(Prime,Subject,item,condition,determiner,size,time,targetlook,competitorlook,whichword) %>%
  filter(targetlook == 1 | competitorlook == 1) 

determiner_window = tomodel %>%
  filter(whichword=="determiner") %>%
  mutate(targetlook=as.factor(targetlook),size=as.factor(size),time=as.factor(time)) %>%
  mutate(csize=as.numeric(size)-mean(as.numeric(size))) %>%
  mutate(ctime=as.numeric(time)-mean(as.numeric(time)))

name_window = tomodel %>%
  filter(whichword=="name") %>%
  mutate(targetlook=as.factor(targetlook),size=as.factor(size),time=as.factor(time)) %>%
  mutate(csize=as.numeric(size)-mean(as.numeric(size))) %>%
  mutate(ctime=as.numeric(time)-mean(as.numeric(time)))

m.determiner = glmer(targetlook ~ determiner*csize*ctime + (determiner*csize*ctime|Subject) + (1+determiner*csize*ctime|item), family="binomial",data=determiner_window)
summary(m.determiner)

m.name = glmer(targetlook ~ determiner*csize*ctime + (determiner*csize*ctime|Subject) + (1+determiner*csize*ctime|item), family="binomial",data=name_window)
summary(m.name)

