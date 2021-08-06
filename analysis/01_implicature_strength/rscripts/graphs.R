library(tidyverse)
library(lme4)

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
  mutate(trial_group = ifelse(trial_number<31,"first_half","second_half")) %>% 
  mutate(item=word(as.character(instruction3), -1))

df = separate(df,response,into=c("click1","click2","click3","click4"),sep=",")

### EXCLUSIONS
df = df %>%
  mutate(selection_correct = ifelse(as.numeric(click4) == as.numeric(target1),1,ifelse(as.numeric(click4) == as.numeric(target2),1,0)))

table(df$selection_correct) # 665 incorrect responses

# exclude anyone with < 95% correct selections
accuracy = df %>% 
  filter(ExpFiller != "Prac") %>% 
  group_by(workerid) %>% 
  tally(selection_correct) %>% 
  mutate(correct=n/48) 

View(accuracy %>% arrange(n))

toexclude = accuracy %>% 
  filter(correct < .95)

length(toexclude$workerid) # exclude 29 subjects
length(toexclude$workerid)/length(accuracy$workerid) # exclude 24% of subjects

df = df %>% 
  filter(!workerid %in% toexclude$workerid)

unique(demo$language) # no exlusions (some people left it blank?)

# trials with incorrect selections
df = df %>% 
  filter(selection_correct==1)

nrow(df) # 4348

# get only experimental trials (no fillers) for further analysis
df = df %>% 
  filter(ExpFiller=="Exp") %>%
  droplevels()

### PART I: PLOT DATA FROM REPLICATION TASK

# plot proportion of selections by condition
toplot =  df %>%
  filter(ExpFiller=="Exp") %>%
  select(workerid,condition,size,click1,click2,click3,click4,target1,target2,competitor1,competitor2,instruction3) %>%
  mutate(ID = row_number()) %>%
  gather(click_number,location,click1:click4) %>%
  mutate(target=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitor=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0))) %>%
  group_by(condition,size,click_number) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor)) %>%
  gather(location,Mean,m_target:m_competitor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>%
  mutate(click_number=fct_recode(click_number,prior="click1",gender="click2",determiner="click3",noun="click4"))

proportions = ggplot(toplot, aes(x=click_number, y=Mean, group=Region)) +
  geom_line(aes(color=Region),size=1.3) +
  geom_point(aes(color=Region),size=2.5,shape="square") +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.3) +
  facet_grid(size ~condition ) + 
  scale_color_manual(values=c("darkgreen","orange")) +
  xlab("Window") +
  ylab("Proportion of selections") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))

proportions

ggsave(proportions, file="../graphs/proportions.pdf",width=9,height=4.5)

# plot proportion of selections by condition and experiment half
toplot =  df %>%
  filter(ExpFiller=="Exp") %>%
  select(workerid,condition,size,click1,click2,click3,click4,target1,target2,competitor1,competitor2,instruction3,trial_group) %>%
  mutate(ID = row_number()) %>%
  gather(click_number,location,click1:click4) %>%
  mutate(target=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitor=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0))) %>%
  group_by(condition,size,click_number,trial_group) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor)) %>%
  gather(location,Mean,m_target:m_competitor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>%
  mutate(click_number=fct_recode(click_number,prior="click1",gender="click2",determiner="click3",noun="click4"))

proportions = ggplot(toplot, aes(x=click_number, y=Mean, group=Region)) +
  geom_line(aes(color=Region),size=1.3) +
  geom_point(aes(color=Region),size=2.5,shape="square") +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.3) +
  facet_grid(trial_group + size ~condition ) + 
  scale_color_manual(values=c("darkgreen","orange")) +
  xlab("Window") +
  ylab("Proportion of selections") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))

proportions

ggsave(proportions, file="../graphs/proportions_order.pdf",width=9,height=9)

# recreate Fig 13 from Sun & Breheny 2020
# compute and then plot target preference scores in each window
toplot =  df %>%
  filter(ExpFiller=="Exp") %>%
  select(workerid,condition,size,click1,click2,click3,click4,target1,target2,competitor1,competitor2,instruction3) %>%
  mutate(ID = row_number()) %>%
  gather(click_number,location,click1:click4) %>%
  mutate(target=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitor=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0))) %>%
  # when target == 0 and comp == 0, turn target 0 into .5? or exclude? excluding for time being (leads to exclusion of 3230 data points)
  filter(target == 1 | competitor == 1) %>% 
  group_by(condition,size,click_number,workerid) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor)+.00000001) %>% 
  ungroup() %>% 
  mutate(prop=(m_target/m_competitor)+.00000001,targetadvantage=log(prop)) %>% 
  group_by(condition,size,click_number) %>%
  summarize(target=mean(targetadvantage),ci_low_target=ci.low(targetadvantage),ci_high_target=ci.high(targetadvantage)) %>%
  ungroup() %>% 
  mutate(YMin=target-ci_low_target,YMax=target+ci_high_target) %>% 
  mutate(Condition=fct_relevel(condition,"all","some"),Size=size) %>%
  mutate(Condition=fct_recode(Condition,"number"="num"))
dodge=position_dodge(0)

ggplot(toplot, aes(x=click_number, y=target, color=Condition, linetype=Size,group=interaction(Condition,Size))) +
  geom_line(size=1.3,position=dodge) +
  geom_point(size=2.5,shape="square",position=dodge) +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.7,  linetype="solid",position=dodge) +
  # facet_grid(size ~condition ) + 
  scale_color_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  scale_x_discrete(breaks=c("click1","click2","click3","click4"),
                   labels=c("Baseline", "Gender", "Determiner", "Noun")) +
  xlab("Window") +
  ylab("log(P(Target)/P(Competitor))") #+
  # theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
ggsave("../graphs/results-idt.pdf",width=4.5,height=2.5)

#Leyla new plots
toplot =  df %>%
  filter(ExpFiller=="Exp") %>%
  select(workerid,condition,size,click1,click2,click3,click4,target1,target2,competitor1,competitor2,instruction3) %>%
  mutate(ID = row_number()) %>%
  gather(click_number,location,click1:click4) %>%
  mutate(target=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitor=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0))) %>%
  filter(target == 1 | competitor == 1) %>% #CHANGE
  group_by(condition,size,click_number) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor),target_ci_low=ci.low(target),target_ci_high=ci.high(target),competitor_ci_low=ci.low(competitor),competitor_ci_high=ci.high(competitor)) %>% 
  ungroup() %>% 
  mutate(YMin=m_target-target_ci_low,YMax=m_target+target_ci_high) %>% 
  #mutate(YMin=m_competitor-competitor_ci_low,YMax=m_competitor+competitor_ci_high) %>% 
  mutate(Condition=fct_relevel(condition,"all","some"),Size=size) %>%
  mutate(Condition=fct_recode(Condition,"number"="num"))
dodge=position_dodge(0)

ggplot(toplot, aes(x=click_number, y=m_target, color=Condition, linetype=Size,group=interaction(Condition,Size))) +
  geom_line(size=1.3,position=dodge) +
  geom_point(size=2.5,shape="square",position=dodge) +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.7,  linetype="solid",position=dodge) +
  # facet_grid(size ~condition ) + 
  scale_color_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  scale_x_discrete(breaks=c("click1","click2","click3","click4"),
                   labels=c("baseline", "gender", "determiner", "noun")) +
  xlab("Window") +
  ylab("Proportion of target selections") #+
# theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
ggsave("../graphs/results-target.pdf",width=4.5,height=2.5)


### PART II: PLOT CATEGORICAL DATA AGAINST EYE MOVEMENT DATA

# load eye-tracking data from Sun&Breheny ---> not sure about these variables: TrialId, mean, subject, unique,TETTime, RTTime, time
baseline = read.csv("sb_eyetracking/exp200ms_beselinedata.csv", header = TRUE)
gender = read.csv("sb_eyetracking/exp200ms_genderdata.csv", header = TRUE)
determiner = read.csv("sb_eyetracking/exp200ms_determiner.csv", header = TRUE)
name = read.csv("sb_eyetracking/exp200ms_namedata.csv", header = TRUE)
end = read.csv("sb_eyetracking/exp200ms_enddata.csv", header = TRUE)
preview = read.csv("sb_eyetracking/exp200ms_previewdata.csv", header = TRUE)

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

