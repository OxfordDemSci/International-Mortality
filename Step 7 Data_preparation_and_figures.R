#Data preparation and figures for all causes of death except alcohol and drug-related mortality (these are in Step 8 file)
# 0. Init ----------------------------------------------------------------------
# library
library(haven); library(glue); library(tidyverse); library(zoo); library(lubridate)

# define labels
country_label <- c('Canada', 'USA', 'Japan', 'Austria', 'Belgium', 'Bulgaria','Czech Republic', 'Denmark', 
                   'Finland', 'France', 'Germany', 'Hungary', 'Italy', 
                   'Netherlands', 'Norway', 'Poland','Portugal', 'Romania', 'Slovakia', 'Slovenia',
                   'Spain', 'Sweden','Switzerland','UK', 'Australia')

sex_label <- c('Male', 'Female')
age_label <- c('25-44', '45-54', '55-64')

cause_label <- c('Infectious and Parasitic Diseases', 'HIV/AIDS', 'Respiratory Diseases', 'Trachea/Bronchus, Lung Cancers',
                 'All Other Cancers', 'Nervous System Diseases', 'Metabolic Diseases',
                 'Cardiovascular Disease', 'Suicide', 'Homicide', 'Transport Accidents', 'Other External Causes', 'All Other Causes', 'All Causes')


country_category_label <- c("HI", "CEE", "USA", "UK")

# 1. Import data ---------------------------------------------------------------
wd <- "~/Desktop/STATA Code/Raw data"
raw <- read_dta(glue('{wd}/NEW_dat_icd910.dta'))
# import 2013 European Standard Population (ESP) file
esp <- read_dta(glue('{wd}/esp.dta'))

dup <- function(dat){
  dat_duplicated<-dat %>%
    group_by_all() %>%
    filter(n()>1) %>%
    ungroup()
  return(!nrow(dat_duplicated)==0)}

dup(raw)

# 2. Prepare data for plots ----------------------------------------------------
# cleaning variables before collapsing
dat_dup <- raw %>% 
  filter(age>=7, age<=14, cause<=99, sex!=9, year<2020) %>%
  arrange(country, year, sex, cause, age) %>%
  # merge in 2013 ESP
  left_join(esp, by = 'age') %>%
  mutate(
    age = case_when(
      age>=7 & age<=10 ~ 1,  
      age>=11 & age<=12 ~ 2,
      age>=13 & age<=14 ~ 3
    ) %>% factor(labels = age_label),
    cause = factor(cause, labels = cause_label),
    sex = factor(sex, labels = sex_label), 
    country_cat = case_when(
      country %in% c(4030, 4045, 4150, 4230, 4270, 4274, 4276) ~ 2, #Bulgaria, Czech Republic, Hungary, Poland, Romania, Slovakia, Slovenia
      country==2450 ~ 3, #United States of America
      country==4308 ~ 4, #United Kingdom
      TRUE ~ 1, # other countries belong to HI
    ) %>% factor(labels = country_category_label),
    country = factor(country, labels = country_label)
  )

# get rid of duplicates due to missing values
dat <- dat_dup[!duplicated(dat_dup), ]

# country-specific mortality (moving average)
midlife1 <- dat %>%
  #calculate age-specific mortality rate
  mutate(mort = deaths/pop) %>%
  group_by(country, year, sex, cause, age) %>%  
  summarise( 
    # weight mortality rates using 2013 ESP (age standardization)
    mort = (sum(mort*w/sum(w)))*100000
  ) %>%
  ungroup()%>%
  group_by(country, sex, cause, age) %>%
  # arrange years in a chronological order before averaging
  arrange(year) %>%
  # 3-year moving average
  mutate(mort_ma = rollmean(mort, 3, fill=NA)) %>%
  group_modify(~{                                 
    mort_ma <- arrange(.x, year)
    mort_ma[1, 'mort_ma'] <- 1/2*(mort_ma[1, 'mort'] + mort_ma[2, 'mort'])
    mort_ma[nrow(mort_ma), 'mort_ma'] <-
      1/2*(mort_ma[nrow(mort_ma)-1, 'mort'] + mort_ma[nrow(mort_ma), 'mort'])
    return(mort_ma)
  }) 

# HI & CEE mean mortality (moving average)
midlife2 <- midlife1 %>%
  left_join(dat %>% distinct(country, country_cat), by = 'country') %>%
  group_by(country_cat, year, sex, cause, age) %>%
  summarise(mort_ma = mean(mort_ma, na.rm = TRUE),        
            mort = mean(mort, na.rm = TRUE)) %>%
  filter(country_cat=="HI" | country_cat=="CEE") %>%
  mutate(icd10_year=as.double('NA')) %>%
  rename(country = country_cat)

# combine country-specific and HI- & CEE- mean mortality
midlife_tot <- bind_rows(midlife1, midlife2) %>%
  group_by(country, sex, cause, age)%>%
  mutate(baseline_mort=first(mort[mort!=0]))%>% #condition what it is the first non-0 value
  mutate(mort_change=100*((mort-baseline_mort)/baseline_mort),
         mort_change_ma=rollmean(mort_change, 3, fill=NA)) %>%
  group_modify(~{                                  
    mort_change_ma <- arrange(.x, year)
    mort_change_ma[1, 'mort_change_ma']<-0
    mort_change_ma[nrow(mort_change_ma), 'mort_change_ma'] <-
      1/2*(mort_change_ma[nrow(mort_change_ma)-1, 'mort_change'] + mort_change_ma[nrow(mort_change_ma), 'mort_change'])
    return(mort_change_ma)
  }) 

midlife<-midlife_tot %>%  group_by(age) %>%
  group_split() %>%
  set_names(age_label) %>% map(~{
    .x <- .x %>% 
      group_by(cause) %>%
      group_split() %>%
      set_names(cause_label) %>%
      map(~{
        .x <- .x %>%
          group_by(sex) %>%
          group_split() %>%
          set_names(sex_label)
      })
  })


#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
######################################### panel figures by cause ################################################
############################# version with fixed scales for selected external causes ############################
#############################       mortality rates, 3-year rolling average        #############################
#################################################################################################################
#################################################################################################################
plot_list <- midlife %>% map(~{
  .x <- .x %>% map(~{
    .x <- .x %>% map(~{
      .x <- NULL
    })
  })
})



for (i in 1:3) {         
  for (t in 1:14) {
    for (s in 1:2) {
      USA=midlife[[i]][[t]][[s]] %>%filter(country== "USA")
      UK=midlife[[i]][[t]][[s]] %>%filter(country== "UK")
      HI=midlife[[i]][[t]][[s]] %>%filter(country== "HI")
      CEE=midlife[[i]][[t]][[s]] %>%filter(country== "CEE")
      myplot <- midlife[[i]][[t]][[s]] %>% 
        ggplot(aes(year, mort_ma, color=country, label=country, alpha=country, linetype=country))+
        geom_line(aes(size=country))+
        geom_line(data=HI, aes(size=country))+
        geom_line(data=CEE, aes(size=country))+
        geom_line(data=UK, aes(size=country))+
        geom_line(data=USA, aes(size=country))+
        scale_color_manual(values=c("#88CCEE","#CC6677","#88CCEE","#88CCEE","#88CCEE","#DDCC77","#DDCC77","#88CCEE","#88CCEE","#88CCEE",                     #basic colour scheme 
                                             "#88CCEE","#DDCC77","#88CCEE","#88CCEE","#88CCEE","#DDCC77","#88CCEE","#DDCC77",
                                             "#DDCC77","#88CCEE","#88CCEE","#88CCEE","#88CCEE", "#117733","#88CCEE","#332288","#999933"),
                                             
                           labels = c('', '', '', '', '','','',
                                      '', '', '', '', '', '', '',
                                      '', '', '','', '', '',
                                      '', 'USA', 'UK', 'Peer', 'CEE', 'Peer mean','CEE mean'))+
         scale_size_manual(values=c(0.7, 1.5, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7,
                                    0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7,0.7, 1.5, 0.7, 1.5, 1.5),guide = FALSE)+
        scale_alpha_manual(values = c(0.6, 2, 0.6, 0.6, 0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,
                                      0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,1,0.6, 1,1), guide = "none")+
        scale_linetype_manual(values=c(rep('solid',27)), guide = "none")+
        guides(color = guide_legend(title='', override.aes = list(
          colour = c('NA', 'NA', 'NA', 'NA', 'NA','NA',            
                     'NA', 'NA', 'NA', 'NA', 'NA',
                     'NA', 'NA', 'NA', 'NA',
                     'NA', 'NA', 'NA', 'NA', 'NA',
                     'NA', '#CC6677', "#117733", '#88CCEE', '#DDCC77',
                     "#332288","#999933"),
          linetype = c(rep(0,21),rep(1,4),rep(8,2)), 
          linewidth = c(0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        0, 2, 2, 2, 2,
                        2,2))))+
        theme_classic()+
        theme(legend.position="none")+
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              legend.text=element_text(size=16), legend.position = 'right',
              legend.justification="right",
              legend.margin=margin(0,10,40,10),
              legend.box.margin=margin(10,10,40,40),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 20))+
        coord_cartesian(expand=TRUE, xlim = c(NA, 2019))+
        ggtitle(paste0('Ages ', age_label[[i]],', ',sex_label[[s]]))+
        scale_x_continuous(breaks = seq(1990, 2019, by = 5), lim = c(1990,2019), expand = c(0, 0))
        plot_list[[i]][[t]][[s]] <- myplot
      
      
      #Suicide
      if(midlife[[i]][[t]][[s]]$sex[1] == 'Female' & midlife[[i]][[t]][[s]]$cause[1] == 'Suicide'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(0, 30))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      if(midlife[[i]][[t]][[s]]$sex[1] == 'Male' & midlife[[i]][[t]][[s]]$cause[1] == 'Suicide'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(0, 100))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      
      #Homicide
      if(midlife[[i]][[t]][[s]]$sex[1] == 'Female' & midlife[[i]][[t]][[s]]$cause[1] == 'Homicide'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(0, 6.5))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      if(midlife[[i]][[t]][[s]]$sex[1] == 'Male' & midlife[[i]][[t]][[s]]$cause[1] == 'Homicide'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(0, 30))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      
      #Transport Accidents
      if(midlife[[i]][[t]][[s]]$sex[1] == 'Female' & midlife[[i]][[t]][[s]]$cause[1] == 'Transport Accidents'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(0, 16.5))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      if(midlife[[i]][[t]][[s]]$sex[1] == 'Male' & midlife[[i]][[t]][[s]]$cause[1] == 'Transport Accidents'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(0, 60))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      # #Other External Causes
      # if(midlife[[i]][[t]][[s]]$sex[1] == 'Female' & midlife[[i]][[t]][[s]]$cause[1] == 'Other External Causes'){
      #   myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(0, 30))
      #   plot_list[[i]][[t]][[s]] <- myplot
      # }
      # 
      # if(midlife[[i]][[t]][[s]]$sex[1] == 'Male' & midlife[[i]][[t]][[s]]$cause[1] == 'Other External Causes'){
      #   myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(0, 150))
      #   plot_list[[i]][[t]][[s]] <- myplot
      # }
      
      
    }
  }
}


plot_list[[1]][[3]][[2]]

##########creating mortality panels by cause


plot_list2<-list()

for (i in 1:length(cause_label)){
  
  m1<-plot_list[[1]][[i]][[1]]
  m2<-plot_list[[2]][[i]][[1]]
  m3<-plot_list[[3]][[i]][[1]]
  
  f1<-plot_list[[1]][[i]][[2]]
  f2<-plot_list[[2]][[i]][[2]]
  f3<-plot_list[[3]][[i]][[2]]
  
  library('ggpubr')
  
  figure_both<-ggarrange(m1, m2, m3,f1, f2, f3, ncol = 3, nrow = 2, 
                         common.legend = TRUE, legend="right")
  
  
  g<-annotate_figure(figure_both,
                     top = text_grob(paste0("Age-Standardized Mortality from ", cause_label[[i]], ", Years 1990-2019\n  "), color = "black", face = "bold", size = 25),
                     bottom = text_grob(~bold("Year"), color = "black", size = 20),
                     left = text_grob(~bold("Deaths per 100,000"), color = "black", rot = 90, size = 20),
                     right = text_grob("", color = "black", rot = 90))
  
  
  plot_list2[[i]] = g
  
}

# # # Export --------------------------------------------------------------------

# plot_list2[[14]]
# ggsave(file="all-cause.jpg", width = 20, height = 10)
# plot_list2[[11]]
# ggsave(file="transport.jpg", width = 20, height = 10)
# plot_list2[[10]]
# ggsave(file="homicide.jpg", width = 20, height = 10)
# plot_list2[[9]]
# ggsave(file="suicide.jpg", width = 20, height = 10)
# plot_list2[[8]]
# ggsave(file="CVD.jpg", width = 20, height = 10)
# plot_list2[[7]]
# ggsave(file="metabolic.jpg", width = 20, height = 10)
# plot_list2[[4]]
# ggsave(file="lung.jpg", width = 20, height = 10)
# plot_list2[[5]]
# ggsave(file="cancers.jpg", width = 20, height = 10)
# 
# wd <- "~/Desktop/STATA Code"
# pdf('panels_by_cause_color_blind_friendly.pdf')
# pdf.options(width=20, height=10)
# for (i in c(14,11,10,9,8,7,4,5,1,2,3,6,12,13)) {
# print(plot_list2[[i]])
#      }
# dev.off()

#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
######################################### panel figures by cause ################################################
################################# % change from the baseline year ###############################################
######################## version with individual lines for CEE and HI countries excluded ########################
#################################################################################################################
#################################################################################################################

for (i in 1:3) {
  for (t in 1:14) {
    for (s in 1:2) {
      USA=midlife[[i]][[t]][[s]] %>%filter(country== "USA")
      UK=midlife[[i]][[t]][[s]] %>%filter(country== "UK")
      HI=midlife[[i]][[t]][[s]] %>%filter(country== "HI")
      CEE=midlife[[i]][[t]][[s]] %>%filter(country== "CEE")
      myplot<- midlife[[i]][[t]][[s]] %>%
        ggplot(aes(year, mort_change_ma, color=country, label=country, alpha=country, linetype=country))+
        geom_line(data=CEE, aes(size=country))+
        geom_line(data=HI, aes(size=country))+
        geom_line(data=UK, aes(size=country))+
        geom_line(data=USA, aes(size=country))+
        scale_color_manual(values=c("#999933", "#332288", "#117733", "#CC6677"),
                           labels = c('USA', 'UK', 'Peer mean','CEE mean'))+
        scale_size_manual(values=c(1.5, 1.5, 1.5, 1.5),guide = FALSE)+
        scale_alpha_manual(values = c(2,2,2,2), guide = "none")+
        scale_linetype_manual(values = c(1,1,1,1), guide = "none")+
        guides(color = guide_legend(title='', override.aes = list(linetype = c(1, 1,
                                                                               1,1),
                                                                  colour = c('#CC6677', "#117733","#332288" ,"#999933"),
                                                                  linewidth = c(2, 2, 2,2))))+
        theme_classic()+
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              legend.text=element_text(size=16), legend.position = 'right',
              legend.justification="right",
              legend.margin=margin(0,10,40,10),
              legend.box.margin=margin(10,10,40,40),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              plot.title=element_text(size=20))+
        coord_cartesian(expand=TRUE, xlim = c(NA, 2019))+
        ggtitle(paste0('Ages ', age_label[[i]],', ',sex_label[[s]]))+
        scale_x_continuous(breaks = seq(1990, 2019, by = 5), lim = c(1990,2019), expand = c(0, 0))+
        geom_hline(yintercept = 0, colour='black')+
        coord_cartesian(expand=TRUE, ylim = c(-100, 100))
      
      plot_list[[i]][[t]][[s]] <- myplot
      
      
      #'Infectious and Parasitic Diseases'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'Infectious and Parasitic Diseases'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-60, 100))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      
      #'HIV/AIDS'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'HIV/AIDS'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-100, 2000))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      
      #'Respiratory Diseases'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'Respiratory Diseases'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-60, 30))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      #'Trachea/Bronchus, Lung Cancers'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'Trachea/Bronchus, Lung Cancers'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-80, 100))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      #'All Other Cancers'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'All Other Cancers'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-50, 10))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      #'Nervous System Diseases'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'Nervous System Diseases'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-55, 80))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      #'Metabolic Diseases'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'Metabolic Diseases'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-60, 75))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      #'Cardiovascular Disease'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'Cardiovascular Disease'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-70, 20))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      #'Suicide'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'Suicide'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-60, 50))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      #'Homicide'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'Homicide'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-90, 70))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      #'Transport Accidents'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'Transport Accidents'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-80, 3))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      #'Other External Causes'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'Other External Causes'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-70, 30))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      #'All Other Causes'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'All Other Causes'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-60, 70))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
      #'All Causes'
      if(midlife[[i]][[t]][[s]]$cause[1] == 'All Causes'){
        myplot <- myplot + coord_cartesian(expand=TRUE, ylim = c(-50, 10))
        plot_list[[i]][[t]][[s]] <- myplot
      }
      
    }
  }
}


##########creating mortality panels by cause

plot_list2<-list()

for (i in 1:length(cause_label)){
  
  m1<-plot_list[[1]][[i]][[1]]
  m2<-plot_list[[2]][[i]][[1]]
  m3<-plot_list[[3]][[i]][[1]]
  
  f1<-plot_list[[1]][[i]][[2]]
  f2<-plot_list[[2]][[i]][[2]]
  f3<-plot_list[[3]][[i]][[2]]
  
  library('ggpubr')
  
  figure_both<-ggarrange(m1, m2, m3,f1, f2, f3, ncol = 3, nrow = 2, 
                         common.legend = TRUE, legend="right")
  
  
  g<-annotate_figure(figure_both,
                     top = text_grob(paste0("Percent Change in Age-Standardized Mortality from the Baseline Year (1990), ", cause_label[[i]]), color = "black", face = "bold", size = 25),
                     bottom = text_grob(~bold("Year"), color = "black", size = 20),
                     left = text_grob(~bold("Percent Change in  Deaths per 100,000"), color = "black", rot = 90, size = 20),
                     right = text_grob("", color = "black", rot = 90))
  
  
  plot_list2[[i]] = g
  
}

# Export --------------------------------------------------------------------

# plot_list2[[14]]
# ggsave(file="all-cause_change.jpg", width = 20, height = 10)
# plot_list2[[8]]
# ggsave(file="CVD_change.jpg", width = 20, height = 10)
# 
# pdf('plots change from the baseline.pdf')
# pdf.options(width=20, height=10)
# for (i in c(14,11,10,9,8,7,4,5,1,2,3,6,12,13)) {
#   print(plot_list2[[i]])
# }

dev.off()


#################################################################################################################
#################################################################################################################
#################################################################################################################
######################################### SUPPLEMENTARY MATERIALS -- FIGURES ####################################
######################################### panel figures by cause and country ####################################
#################################################################################################################
#################################################################################################################

for (i in 1:3) {         
  for (t in 1:14) {
    for (s in 1:2) {
      plot_list[[i]][[t]][[s]] <- midlife[[i]][[t]][[s]] %>% 
        ggplot(aes(year, mort_ma))+
        facet_wrap(facets = vars(country), scales='free') +
        geom_point(color='red') +
        geom_line(color='grey') +
        theme_classic()+
        theme(legend.position="none")+
        labs( x = 'Year', y = 'Deaths per 100,000') +
        scale_x_continuous(breaks = seq(1990, 2020, 5)) +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        ggtitle(paste0(
          'Three-Year Moving Average of ', sex_label[[s]],' Mortality from ', 
          cause_label[[t]], ' at Ages ', age_label[[i]]))+
        theme(axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),
              axis.title.x = element_text(size=25),
              axis.title.y = element_text(size=25),
              title = element_text(size=25))
      
    }
  }
}


# 4. Export --------------------------------------------------------------------

  # pdf('panels_by_cause_and_country_p1.pdf')
  # #pdf(paste0(glue('{wd}'), age_label[[i]], '.pdf'))
  # pdf.options(width = 20, height = 10)
  # 
  # for (i in 1:3) {
  # for (t in c(14,11,10,9,8,7,4,5,1,2,3,6,12,13)) {
  #   for (s in 1:2) {
  #     print(plot_list[[i]][[t]][[s]])
  #     
  #   }
  # }
  # }
  # dev.off()

  #################################################################################################################
  #################################################################################################################
  #################################################################################################################
  #################################################################################################################
  ###################################### changes in all cause mortality between 1990 and 2019 #####################
  ####################################################### by country ##############################################
  #################################################################################################################
  #################################################################################################################
  #################################################################################################################
  library(latex2exp)
  age_label <- c('25-44', '45-54', '55-64')
  
  midlife_change1990_2019<-midlife_tot%>%  
    select(country, sex, cause, age, year, mort)%>%
    filter(cause=='All Causes')%>%
    group_by(country, sex, age)%>%
    mutate(mort_start=first(mort),
           mort_end=last(mort),
           mort_change_from_1990=100*((mort_end - mort_start)/mort_start))%>%
    #summarize(mort_change_from_1990=mean(mort_change_from_1990))%>%
    #mutate(mort_change_from_1990=round(mort_change_from_1990,1))%>%
    ungroup()%>%
    mutate(color=case_when(mort_change_from_1990 < 0 ~1,
                           mort_change_from_1990 > 0 ~ 0))%>%
    filter(country=='USA' & age=='55-64')
  
  
  dat1<-midlife_change1990_2019 %>% filter(sex=="Female" & age=='25-44')
  
  F1<-ggplot(dat1, aes(x=reorder(country,mort_change_from_1990), y=mort_change_from_1990, fill=as.factor(color)))+
    geom_bar(stat='identity')+
    scale_fill_manual(values=c("#b2182b",rep("#4393c3", 26)))+
    #coord_flip()+
    theme_classic()+
    geom_text(aes(label = mort_change_from_1990,
                  hjust = ifelse(mort_change_from_1990 < 0, 1.5, -1),
                  vjust = 0.5,
                  size = 2))+ 
    scale_y_continuous(limits = c(-65, 6))+
    #coord_cartesian(ylim = c(-70, 6))+
    coord_flip()+
    labs(title=~bold("Percent Change in All-Cause Mortality Between 1990 and 2019/Females, Ages 25-44"), 
         x=~bold("Country"), 
         y = TeX(r'(Percent Change $= 100 * \frac{Mortality_{2019} - Mortality_{1990}}{Mortality_{1990}}$)'))+
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.position = 'no')
  
  
  dat2<-midlife_change1990_2019 %>% filter(sex=="Female" & age=='45-54')
  
  F2<-ggplot(dat2, aes(x=reorder(country,mort_change_from_1990), y=mort_change_from_1990, fill=as.factor(color)))+
    geom_bar(stat='identity')+
    scale_fill_manual(values=c(rep("#4393c3", 27)))+
    #coord_flip()+
    theme_classic()+
    geom_text(aes(label = mort_change_from_1990,
                  hjust = ifelse(mort_change_from_1990 < 0, 1.5, -1),
                  vjust = 0.5,
                  size = 2))+ 
    scale_y_continuous(limits = c(-55, 0))+
    #coord_cartesian(ylim = c(-70, 6))+
    coord_flip()+
    labs(title=~bold("Percent Change in All-Cause Mortality Between 1990 and 2019/Females, Ages 45-54"), 
         x=~bold("Country"), 
         y = TeX(r'(Percent Change $= 100 * \frac{Mortality_{2019} - Mortality_{1990}}{Mortality_{1990}}$)'))+
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.position = 'no')
  
  
  dat3<-midlife_change1990_2019 %>% filter(sex=="Female" & age=='55-64')
  
  F3<-ggplot(dat3, aes(x=reorder(country,mort_change_from_1990), y=mort_change_from_1990, fill=as.factor(color)))+
    geom_bar(stat='identity')+
    scale_fill_manual(values=c(rep("#4393c3", 27)))+
    #coord_flip()+
    theme_classic()+
    geom_text(aes(label = mort_change_from_1990,
                  hjust = ifelse(mort_change_from_1990 < 0, 1.5, -1),
                  vjust = 0.5,
                  size = 2))+ 
    scale_y_continuous(limits = c(-47, 0))+
    #coord_cartesian(ylim = c(-70, 6))+
    coord_flip()+
    labs(title=~bold("Percent Change in All-Cause Mortality Between 1990 and 2019/Females, Ages 55-64"), 
         x=~bold("Country"), 
         y = TeX(r'(Percent Change $= 100 * \frac{Mortality_{2019} - Mortality_{1990}}{Mortality_{1990}}$)'))+
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.position = 'no')
  
  
  dat4<-midlife_change1990_2019 %>% filter(sex=="Male" & age=='25-44')
  
  M1<-ggplot(dat4, aes(x=reorder(country,mort_change_from_1990), y=mort_change_from_1990, fill=as.factor(color)))+
    geom_bar(stat='identity')+
    scale_fill_manual(values=c(rep("#4393c3", 27)))+
    #coord_flip()+
    theme_classic()+
    geom_text(aes(label = mort_change_from_1990,
                  hjust = ifelse(mort_change_from_1990 < 0, 1.5, -1),
                  vjust = 0.5,
                  size = 2))+ 
    scale_y_continuous(limits = c(-70, 0))+
    #coord_cartesian(ylim = c(-70, 6))+
    coord_flip()+
    labs(title=~bold("Percent Change in All-Cause Mortality Between 1990 and 2019/Males, Ages 25-44"), 
         x=~bold("Country"), 
         y = TeX(r'(Percent Change $= 100 * \frac{Mortality_{2019} - Mortality_{1990}}{Mortality_{1990}}$)'))+
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.position = 'no')
  
  
  dat5<-midlife_change1990_2019 %>% filter(sex=="Male" & age=='45-54')
  
  M2<-ggplot(dat5, aes(x=reorder(country,mort_change_from_1990), y=mort_change_from_1990, fill=as.factor(color)))+
    geom_bar(stat='identity')+
    scale_fill_manual(values=c(rep("#4393c3", 27)))+
    #coord_flip()+
    theme_classic()+
    geom_text(aes(label = mort_change_from_1990,
                  hjust = ifelse(mort_change_from_1990 < 0, 1.5, -1),
                  vjust = 0.5,
                  size = 2))+ 
    scale_y_continuous(limits = c(-65, 0))+
    #coord_cartesian(ylim = c(-70, 6))+
    coord_flip()+
    labs(title=~bold("Percent Change in All-Cause Mortality Between 1990 and 2019/Males, Ages 45-54"), 
         x=~bold("Country"), 
         y = TeX(r'(Percent Change $= 100 * \frac{Mortality_{2019} - Mortality_{1990}}{Mortality_{1990}}$)'))+
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.position = 'no')
  
  
  dat6<-midlife_change1990_2019 %>% filter(sex=="Male" & age=='55-64')
  
  M3<-ggplot(dat6, aes(x=reorder(country,mort_change_from_1990), y=mort_change_from_1990, fill=as.factor(color)))+
    geom_bar(stat='identity')+
    scale_fill_manual(values=c(rep("#4393c3", 27)))+
    #coord_flip()+
    theme_classic()+
    geom_text(aes(label = mort_change_from_1990,
                  hjust = ifelse(mort_change_from_1990 < 0, 1.5, -1),
                  vjust = 0.5,
                  size = 2))+ 
    scale_y_continuous(limits = c(-58, 5))+
    #coord_cartesian(ylim = c(-70, 6))+
    coord_flip()+
    labs(title=~bold("Percent Change in All-Cause Mortality Between 1990 and 2019/Males, Ages 55-64"), 
         x=~bold("Country"), 
         y = TeX(r'(Percent Change $= 100 * \frac{Mortality_{2019} - Mortality_{1990}}{Mortality_{1990}}$)'))+
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.position = 'no')
  
  
  # pdf('percent_change_1990_2019_all_cause_mort.pdf')
  # pdf.options(width=20, height=10)
  # print(M1) 
  # print(M2) 
  # print(M3) 
  # print(F1) 
  # print(F2) 
  # print(F3) 
  # dev.off()
  # getwd()
  


