##Load in Libararies
library(readxl)
library(tidyverse)
library(broom)
library(rstatix)
library(ggpubr)
library(RColorBrewer)
library(ggsci)

#Funcitons
zero_data <- function(data) {
  ave <- data %>%
    slice(1) %>%
    summarise(across(starts_with("w"), mean, na.rm = TRUE)) %>%
    pull()
  
  result <- data %>%
    mutate(across(starts_with("w"), ~ . - ave))%>%
    mutate(across(where(is.numeric), ~ replace(., . < 0, 0)))
}

##Import Data
raw1 <- read_excel('', 
                   col_names = TRUE,
                   skip = 1)# File path of data
setwd('') #Set working directory to send files

##Name Triplicates
##    Use shift+command+c to # out any unused code
triplicate_names <- as_labeller(c(
  'T1'="PNP",
  'T2'="Ac-PNP",
  'T3'="Prop-PNP",
  'T4'="But-PNP",
  'T5'='Isobut-PNP',
  'T6'='Piv-PNP',
  'T7'= 'Pent-PNP',
  'T8'= 'Hex-PNP',
  'T9'= 'Hept-PNP',
  'T10'= 'Bz-PNP',
  'T11'= 'Veh'))
  # 'T12'= '1 µM Sat-Hex'))

#Chose colors for the graph
palette_names <- 'Paired'
#Get colors from chosen pallet
colors <- brewer.pal(palette_names, n=12)

##Remove Temp
raw2 <- raw1 %>%
  # select(-'T° 600') %>%
  mutate(Time = seq(0, (n() - 1) * 1/15, 1/15))%>%
  drop_na()

##Row A
T1 <- raw2 %>%
  select(Time, A1:A3)%>%
  add_column('T1')%>%
  rename('time' = 'Time', 
         'w1' = 'A1',
         'w2' = 'A2',
         'w3' = 'A3',
         'group' =  "\"T1\"")%>%
  zero_data()

T2 <- raw2 %>%
  select(Time, A4:A6)%>%
  add_column('T2')%>%
  rename('time' = 'Time', 
         'w1' = 'A4',
         'w2' = 'A5',
         'w3' = 'A6',
         'group' =  "\"T2\"")%>%
  zero_data()

T3 <- raw2 %>%
  select(Time, A7:A9)%>%
  add_column('T3')%>%
  rename('time' = 'Time', 
         'w1' = 'A7',
         'w2' = 'A8',
         'w3' = 'A9',
         'group' =  "\"T3\"")%>%
  zero_data()

T4 <- raw2 %>%
  select(Time, A10:A12)%>%
  add_column('T4')%>%
  rename('time' = 'Time', 
         'w1' = 'A10',
         'w2' = 'A11',
         'w3' = 'A12',
         'group' =  "\"T4\"")%>%
  zero_data()

##Row_B
T5 <- raw2 %>%
  select(Time, B1:B3)%>%
  add_column('T5')%>%
  rename('time' = 'Time',
         'w1' = 'B1',
         'w2' = 'B2',
         'w3' = 'B3',
         'group' =  "\"T5\"")%>%
  zero_data()

T6 <- raw2 %>%
  select(Time, B4:B6)%>%
  add_column('T6')%>%
  rename('time' = 'Time',
         'w1' = 'B4',
         'w2' = 'B5',
         'w3' = 'B6',
         'group' =  "\"T6\"")%>%
  zero_data()

T7 <- raw2 %>%
  select(Time, B7:B9)%>%
  add_column('T7')%>%
  rename('time' = 'Time',
         'w1' = 'B7',
         'w2' = 'B8',
         'w3' = 'B9',
         'group' =  "\"T7\"")%>%
  zero_data()

T8 <- raw2 %>%
  select(Time, B10:B12)%>%
  add_column('T8')%>%
  rename('time' = 'Time',
         'w1' = 'B10',
         'w2' = 'B11',
         'w3' = 'B12',
         'group' =  "\"T8\"")%>%
  zero_data()

##Row C

T9 <- raw2 %>%
  select(Time, C1:C3)%>%
  add_column('T9')%>%
  rename('time' = 'Time',
         'w1' = 'C1',
         'w2' = 'C2',
         'w3' = 'C3',
         'group' =  "\"T9\"")%>%
  zero_data()

T10 <- raw2 %>%
  select(Time, C4:C6)%>%
  add_column('T10')%>%
  rename('time' = 'Time',
         'w1' = 'C4',
         'w2' = 'C5',
         'w3' = 'C6',
         'group' =  "\"T10\"")%>%
  zero_data()

T11 <- raw2 %>%
  select(Time, C7:C9)%>%
  add_column('T11')%>%
  rename('time' = 'Time',
         'w1' = 'C7',
         'w2' = 'C8',
         'w3' = 'C9',
         'group' =  "\"T11\"")%>%
  zero_data()

T12 <- raw2 %>%
  select(Time, C10:C12)%>%
  add_column('T12')%>%
  rename('time' = 'Time',
         'w1' = 'C10',
         'w2' = 'C11',
         'w3' = 'C12',
         'group' =  "\"T12\"")%>%
  zero_data()

##Combine data and add in mean, stdev
Comb_Dat <- bind_rows(T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)%>%
  group_by(time,group)%>%
  mutate(mean = mean(c(w1,w2,w3)))%>%
  mutate(stdev = sd(c(w1,w2,w3)))%>%
  mutate(ODmax = mean+stdev)%>%
  mutate(ODmin = mean-stdev)%>%
  filter(time <=10)

##Scatter plot of growth curve
ggplot()+
  geom_point(Comb_Dat, mapping = aes(x=time, y= w1, color = group), size = 0.1,show.legend = FALSE)+
  geom_point(Comb_Dat, mapping = aes(x=time, y= w2, color = group), size = 0.1,show.legend = FALSE)+
  geom_point(Comb_Dat, mapping = aes(x=time, y= w3, color = group), size = 0.1,show.legend = FALSE)+
  geom_smooth(Comb_Dat, mapping = aes(x=time, y= mean, color = group),method = 'loess',show.legend = TRUE)+
  labs(x='Time (h)', y='OD 400 nm', color='Group', aes(size=10))+
  scale_color_manual(
    values = colors,
    breaks = c('T1', 'T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'T8', 'T9', 'T10', 'T11', 'T12'),
    labels = triplicate_names
  )+
  scale_y_continuous(limits = c(0, 1.3))+
  # facet_wrap(~factor(group,c('T1','T2','T3','T4','T5','T6','T7','T8','T9','T10'
  #                             ,'T11', 'T12')),
  #            labeller = triplicate_names)+
  theme_classic()+
  theme(legend.position = 'bottom') 

ggsave('DPM-F-53_PNP-LiveCellAssay_ecoli_no-Control_16Aug2023.pdf', width = 6, height = 4)
ggsave('DPM-F-53_PNP-LiveCellAssay_ecoli_no-Control_16Aug2023.png', width = 6, height = 4)
  
##Graph of OD600 vs Conc at Defined Time####

conc_labels <- list(
  'T1'= 0,
  'T2'= 0,
  'T3'= 75,
  'T4'= 50,
  'T5'= 40,
  'T6'= 30,
  'T7'= 20,
  'T8'= 10,
  'T9'= 5,
  'T10'= 1,
  'T11'= 0.5,
  'T12'= 0.1)

timepoint10h <- Comb_Dat%>%
  filter(time == '8')%>%
  ungroup()%>%
  add_column(conc = conc_labels)%>%
  mutate(concentration = as.numeric(conc_labels))%>%
  select(-conc)%>%
  arrange(concentration)%>%
  slice(2:12)

ggplot(timepoint10h, mapping = aes(x=concentration, y=mean, label = round(mean, digits = 2)))+
  geom_point(show.legend = FALSE)+
  # geom_label(hjust = 0, nudge_y = -0.05, nudge_x = 0.5)+
  geom_errorbar(aes(ymin=ODmin, ymax=ODmax))+
  geom_smooth(color='black',method = 'lm',show.legend = FALSE)+
  labs(x='Concentration (µM)', y='OD 600 nm @ 8 h', aes(size=5))+
  theme_classic()
  
ggsave('', width = 6, height = 4)
ggsave('', width = 6, height = 4)
