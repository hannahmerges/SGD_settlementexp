## prelim test of settlement patterns in SGD treatments 

#### Load libraries ####
library(tidyverse)
library(here)
library(car)
library(PNWColors)
library(lme4)
library(lmerTest)
library(performance)
library(agricolae)
library(patchwork)
library(broom)




###########################
#### Load data ####
###########################
SettlementCounts <- read_csv(here("Data","Jan24_SettlementCounts.csv"))

###########################
## tidy data 
###########################

SettlementCounts <- SettlementCounts %>% 
  mutate(dead=20-(swimming+plastic+mesh+top+bottom+side)) %>% 
  mutate(settled=(plastic+mesh+top+bottom+side)) %>% 
  mutate(treatment_level=as.factor(treatment_level), 
         spawning_day=as.factor(spawning_day)) %>% 
  mutate(dead = ifelse(dead < 0, 0, dead))

###############################################
#### quick visualizations to plot patterns of settlement ####
###############################################
# first need to tidy and pivot the df to make count data and sampleID 

settlement_counts_sum <- SettlementCounts %>%
  pivot_longer(cols = swimming:side, names_to = "settlement_surface" , values_to = "count")
##list which columns you want, colon includes "a" through "z", if just want one or two can use "|" 
#to pivot, then rename the new column and the values that you are trying to get as the output

# group by treatment type and then summarize counts from there 
#settlement_counts_sum <- settlement_counts_sum %>% 
# group_by(treatment_level) %>% 
#  summarize(sum_total=sum(count)) 

##calculating standard deviation 
#settlement_counts_sum <- settlement_counts_sum %>% 
# sd(count)

## not separated by day 
settlement_plot <- settlement_counts_sum %>% 
  ggplot(aes(x=treatment_level,  
             y=count,
             fill=settlement_surface)) + 
  geom_col() + 
  # facet_wrap(~spawning_day) + 
  theme_bw() + 
  scale_fill_manual(values=pnw_palette("Sailboat")) + 
  scale_x_discrete(limits=c("Ambient","Low","Medium", "High"))  
#geom_errorbar(aes(x=treatment_level, ymin=count-sd, ymax=count+sd), width=0.4)

## separated by day to see if there are any differences across day 
settlement_plot2 <- settlement_counts_sum %>% 
  ggplot(aes(x=treatment_level,  
             y=count,
             fill=settlement_surface)) + 
  geom_col() + 
  facet_wrap(~spawning_day) + 
  theme_bw() + 
  scale_fill_manual(values=pnw_palette("Sailboat")) + 
  scale_x_discrete(limits=c("Ambient","Low","Medium", "High"))

# group by swimming vs any settled 
settlement_counts_mutated <- SettlementCounts %>% 
  mutate(settled = plastic+mesh+top+bottom+side) %>%
  pivot_longer(cols = swimming|settled, names_to = "swim_noswim" , values_to = "count")

settlement_plot3 <- settlement_counts_mutated %>% 
  ggplot(aes(x=treatment_level,  
             y=count,
             fill=swim_noswim)) + 
  geom_col() + 
  #facet_wrap(~treatment_level) + 
  theme_bw() + 
  scale_fill_manual(values=pnw_palette("Sailboat")) + 
  scale_x_discrete(limits=c("Ambient","Low","Medium", "High"))

settlement_plot4 <- settlement_counts_mutated %>% 
  ggplot(aes(x=treatment_level,  
             y=count,
             fill=swim_noswim)) + 
  geom_col() + 
  facet_wrap(~spawning_day) + 
  theme_bw() + 
  scale_fill_manual(values=pnw_palette("Sailboat")) + 
  scale_x_discrete(limits=c("Ambient","Low","Medium", "High"))


## THINGS TO DO BEFORE SHOWING PETE AND NYSSA 

# add error bars 
# load env data 
# include random effect of tank and maybe table?
# make plots for salinity and count data 
# figure out Tris calibrations to get pH calculations 
# make plots for pH and count data 
# add days 4 and 5 and hopefully 6 to see if there is a stronger pattern 
# think about hourly effect of day 1-3 and also day of larval release 
# plug in hypothetical, approximate nutrient values from respo data to see if pattern with nutrients 



#### try making an ANOVA ####

SettlementCounts1 <- SettlementCounts %>% 
  filter(spawning_day==2)

SettlementCounts2 <- SettlementCounts %>% 
  filter(spawning_day==3)

anova_settlement <- aov(settled ~ treatment_level, data=SettlementCounts)

summary(anova_settlement) 

### if do lmer, use anova rather than summary 

## try to isolate difference between just ambient and low or med and high treatments for day 3 



SettlementCounts %>%
  ggplot(aes(x=treatment_level, y=settled, color=treatmentID_rep)) +
  geom_point() + 
  scale_x_discrete(limits=c("Ambient","Low","Medium", "High"))  

SettlementCounts %>% 
  group_by(spawning_day, treatment_level) %>% 
  summarize(mean_settled = mean(settled, na.rm=TRUE), 
            mean_dead = mean(dead, na.rm=TRUE), 
            mean_swimming = mean(swimming, na.rm=TRUE), 
            se_settled = sd(settled, na.rm=TRUE)/sqrt(n()), 
            se_dead = sd(dead, na.rm=TRUE)/sqrt(n()), 
            se_swimming = sd(swimming, na.rm=TRUE)/sqrt(n())) %>% 
  ungroup() %>% 
  ggplot(aes(x=spawning_day, y=mean_settled, color=treatment_level)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=mean_settled-se_settled, ymax=mean_settled+se_settled), width=0.1) + 
  scale_color_discrete(limits=c("Ambient","Low","Medium", "High"))  + 
  facet_wrap(~treatment_level)

SettlementCounts %>% 
  filter(treatment_level== "Ambient") %>% 
  group_by(spawning_day) %>% 
  summarize(mean_settled = mean(settled, na.rm=TRUE), 
            mean_dead = mean(dead, na.rm=TRUE), 
            mean_swimming = mean(swimming, na.rm=TRUE), 
            se_settled = sd(settled, na.rm=TRUE)/sqrt(n()), 
            se_dead = sd(dead, na.rm=TRUE)/sqrt(n()), 
            se_swimming = sd(swimming, na.rm=TRUE)/sqrt(n())) %>% 
  ungroup() %>% 
  ggplot(aes(x=spawning_day, y=mean_settled)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=mean_settled-se_settled, ymax=mean_settled+se_settled), width=0.1)  
# scale_color_discrete(limits=c("Ambient","Low","Medium", "High"))
#facet_wrap(~treatment_level)

## errors and percent averages across replicates 
## box plot of total settlers 
settlement_counts_mutated <- SettlementCounts %>% 
  mutate(settled = plastic+mesh+top+bottom+side) %>%
  mutate(dead=20-(swimming+settled)) %>% 
  pivot_longer(cols = swimming|dead|settled, names_to = "swim_settled_dead" , values_to = "count")

settlement_counts_mutated %>%
  ggplot(aes(x=treatment_level, y=count, fill=swim_settled_dead)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  ggtitle("Settlers vs Swimmers vs Presumed Dead") + 
  facet_wrap(swim_settled_dead~spawning_day, scales ="free_y") + 
  scale_x_discrete(limits=c("Ambient","Low","Medium", "High"))  

# just the settlers 
settlement_counts_mutated2 <- SettlementCounts %>% 
  mutate(settled = plastic+mesh+top+bottom+side) %>% 
  pivot_longer(cols = settled, names_to = "settled" , values_to = "count")

settlement_counts_mutated2 %>%
  ggplot(aes(x=treatment_level, y=count, fill=settled)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  ggtitle("Settlers") + 
  scale_x_discrete(limits=c("Ambient","Low","Medium", "High"))

settlement_by_day <- settlement_counts_mutated2 %>%
  ggplot(aes(x=treatment_level, y=count, fill=settled)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  facet_wrap(~spawning_day) +
  ggtitle("Settlers") + 
  scale_x_discrete(limits=c("Ambient","Low","Medium", "High"))


