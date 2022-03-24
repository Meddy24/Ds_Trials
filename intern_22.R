## Load libraries 
library(tidyverse)
library(readxl)
library(sysfonts)
library(extrafont)
library(bbplot)

showtext::showtext_auto()
showtext::showtext_opts(dpi=320)
font_families_google()
font_add_google(name="Overpass",family="overpass")

## Import Data Set 
intern_22_23 <- read_excel("intern - 22-23.xlsx")

## Pick major Universities 
uganda_univer <- c("MUK","KING CEASOR","MUST","KABALE","IUIU","AGA KHAN","BUSITEMA","GULU","KIU",
                   "MUNI","UCU","LIRA") 
## Filter 
data_intern <- intern_22_23 %>% select(c(3,4,5,7)) %>% 
  filter(UNIVERSITY %in% uganda_univer)

  ## order the universities 
data_intern$UNIVERSITY <- factor(data_intern$UNIVERSITY, levels = c("KIU", "MUK", "MUST","GULU","LIRA","AGA KHAN","KABALE","IUIU","MUNI","KING CEASOR","UCU"))

## Plot absolute sex counts 
p1 <- ggplot(data_intern,aes(UNIVERSITY , fill = SEX)) +
  geom_bar(position = "fill")+ 
  scale_fill_manual(values = c("#B30baa","#287a9c")) +
  coord_flip()+
  bbc_style()+
  geom_hline(yintercept = 0.5,color="black",linetype=2)+
  labs(title = " Medical Interns 22-23 | Female - Male (Proportions)")

## Plot sex per course
p4 <- ggplot(data_intern,aes(QUALIFICATION , fill = SEX)) +
  geom_bar(position="fill") +
  scale_fill_manual(values = c("#B30baa","#287a9c")) +
  coord_flip()+
  bbc_style()+
  geom_hline(yintercept = 0.5,color="black",linetype=2)+
  labs(title = " Medical Interns 22-23 | Female - Male (Proportions)")

## PLot sex university proportions 
p2 <- ggplot(data_intern,aes(UNIVERSITY , fill = SEX)) +
  geom_bar() +
  scale_fill_manual(values = c("#B30baa","#287a9c")) +
  coord_flip()+
  bbc_style()+
  labs(title = " Medical Interns 22-23 | Female - Male ")

  ## University and Qualifications 

p3 <- ggplot(data_intern,aes(UNIVERSITY,fill = QUALIFICATION)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
   bbc_style()+
  labs(title = " Medical Interns 22-23 | Course")

## Arrange the maps
map <- grid.arrange(p2,p1,p4,p3)

## save the plot 
ggsave("intern.jpeg", plot = map, width = 28 , height =  15,dpi=700)




  
