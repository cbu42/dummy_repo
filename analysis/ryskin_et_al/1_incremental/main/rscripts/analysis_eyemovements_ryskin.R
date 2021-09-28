library(tidyverse)
library(lme4)
library(stringr)
library(stringi)
library(lmerTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")
setwd('../data')
theme_set(theme_bw())
getwd()

## Experiment 2 

### E2 GLMM: Prior
# imports the prior window of eye movement data, namely 
prior_data = read_tsv(paste0(getwd(),"/ryskin_eyetracking/PragTrain3_baseline_window_timesummLongForm.txt"),
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
#view(prior_data[1:50,])




### E2 GLMM: Adj + Noun 

# expt2 reading in adj+noun window data for glmm
# reads in a large file - can be downloaded locally from Ryskin 2019's OSF page
pragtrain3_data = read_tsv('C:/Users/cb476/OneDrive/Desktop/ALPS Lab/PragTrain3_adjnoun_for_GLMM_timesummLongForm.txt', 
#pragtrain3_data = read_tsv("../data/PragTrain3_adjnoun_for_GLMM_timesummLongForm.txt",
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

# expt2 adj glmm -- added filtering of trials that were neither fixations to target nor to competitor
# with filter, only main effect of contrast and previous trial fixation are significant predictors!
m_pragtrain3_adj_glmm = glmer(target_fix ~ contrast_cond*prag_context_cond + target_AR1_corrected +       
                                (1+contrast_cond+target_AR1_corrected|subject)+(1+contrast_cond+target_AR1_corrected|audio),
                              data = pragtrain3_crit %>% filter(timebin <= 830 & (target_fix == 1 | compet_fix == 1)),
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