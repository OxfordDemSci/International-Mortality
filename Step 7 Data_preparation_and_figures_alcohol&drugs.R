#Data preparation and figures for alcohol and drug-related mortality figures
# 0. Init ----------------------------------------------------------------------
# library
library(haven); library(glue); library(tidyverse); library(zoo); library(lubridate)

# define labels
country_label <- c('Canada', 'USA', 'Japan', 'Austria', 'Belgium', 'Bulgaria','Chech Republic', 'Denmark', 
                   'Finland', 'France', 'Germany', 'Hungary', 'Italy', 
                   'Netherlands', 'Norway', 'Poland','Portugal', 'Romania', 'Slovakia', 'Slovenia',
                   'Spain', 'Sweden','Switzerland','UK', 'Australia')
sex_label <- c('Male', 'Female')
age_label <- c('25-44', '45-54', '55-64')
cause_label <- c('Drug-Related Causes', 'Alcohol-Related Causes')
country_category_label <- c("HI", "CEE", "USA", "UK")

# 1. Import data ---------------------------------------------------------------
wd <- "~/Desktop/STATA Code/Raw data"
raw <- read_dta(glue('{wd}/NEWW_dat_icd10.dta'))
# import 2013 European Standard Population (ESP)
esp <- read_dta(glue('{wd}/esp.dta'))

# 2. Prepare data for plots ----------------------------------------------------
# cleaning variables before collapsing
dat <- raw %>% 
  filter(age>=7, age<=14, cause==10 | cause==11, sex!=9, year<2020 & year>1999) %>%
  arrange(country, year, sex, cause, age) %>%
  # Luyin: merge in 2013 ESP
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
      country %in% c(4030, 4045, 4150, 4230, 4270, 4274, 4276) ~ 2, #Bulgaria, Chech Republic, Hungary, Poland, Romania, Slovakia, Slovenia
      country==2450 ~ 3, #United States of America
      country==4308 ~ 4, #United Kingdom
      TRUE ~ 1, # other countries belong to HI
    ) %>% factor(labels = country_category_label),
    country = factor(country, labels = country_label)
  )

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


midlife_tot <- bind_rows(midlife1, midlife2) %>%
  group_by(country, sex, cause, age)%>%
  mutate(baseline_mort=first(mort[mort!=0]))%>% #include condition what it is the first non-0 value
  mutate(mort_change=100*(mort-baseline_mort)/baseline_mort,
         mort_change_ma=rollmean(mort_change, 3, fill=NA)) %>%
  group_modify(~{                                  
    mort_change_ma <- arrange(.x, year)
    mort_change_ma[1, 'mort_change_ma']<-0
    mort_change_ma[nrow(mort_change_ma), 'mort_change_ma'] <-
      1/2*(mort_change_ma[nrow(mort_change_ma)-1, 'mort_change'] + mort_change_ma[nrow(mort_change_ma), 'mort_change'])
    return(mort_change_ma)
  }) 


midlife_da<-midlife_tot %>%  group_by(age) %>%
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
plot_list_da <- midlife_da %>% map(~{
  .x <- .x %>% map(~{
    .x <- .x %>% map(~{
      .x <- NULL
    })
  })
})

for (i in 1:3) {         
  for (t in 1:2) {
    for (s in 1:2) {
      USA=midlife_da[[i]][[t]][[s]] %>%filter(country== "USA")
      UK=midlife_da[[i]][[t]][[s]] %>%filter(country== "UK")
      HI=midlife_da[[i]][[t]][[s]] %>%filter(country== "HI")
      CEE=midlife_da[[i]][[t]][[s]] %>%filter(country== "CEE")
      myplot_da <- midlife_da[[i]][[t]][[s]] %>% 
        ggplot(aes(year, mort_ma, color=country, label=country, alpha=country)) + 
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
        labs(x ='Year', y ='Deaths per 100,000') +
        guides(color = guide_legend(title='', override.aes = list(linetype = c(0, 0, 0, 0, 0, 0,
                                                                               0, 0, 0, 0, 0,
                                                                               0, 0, 0, 0,
                                                                               0, 0, 0, 0, 0,
                                                                               0, 1, 1, 1, 1,
                                                                               1,1),
                                                                  
                                                                  colour = c('NA', 'NA', 'NA', 'NA', 'NA','NA',   
                                                                             'NA', 'NA', 'NA', 'NA', 'NA',
                                                                             'NA', 'NA', 'NA', 'NA',
                                                                             'NA', 'NA', 'NA', 'NA', 'NA',
                                                                             'NA', '#CC6677', "#117733", '#88CCEE', '#DDCC77',
                                                                             "#332288","#999933"),
                                                                             
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
        coord_cartesian(expand=TRUE, xlim = c(2000, 2019))+
        ggtitle(paste0('Ages ', age_label[[i]],', ',sex_label[[s]]))+
        scale_x_continuous(breaks = seq(2000, 2019, by = 5), lim = c(2000,2019), expand = c(0, 0))
      
      plot_list_da[[i]][[t]][[s]] <- myplot_da
      
      
      #Drug-Related Causes
      if(midlife_da[[i]][[t]][[s]]$sex[1] == 'Female' & midlife_da[[i]][[t]][[s]]$cause[1] == 'Drug-Related Causes'){
        myplot_da <- myplot_da + coord_cartesian(expand=TRUE, ylim = c(0, 25))
        plot_list_da[[i]][[t]][[s]] <- myplot_da
      }

      if(midlife_da[[i]][[t]][[s]]$sex[1] == 'Male' & midlife_da[[i]][[t]][[s]]$cause[1] == 'Drug-Related Causes'){
        myplot_da <- myplot_da + coord_cartesian(expand=TRUE, ylim = c(0, 52))
        plot_list_da[[i]][[t]][[s]] <- myplot_da
      }

      
      
    }
  }
}

############creating panels by cause 

plot_list2_da<-list()

for (i in 1:length(cause_label)){
  
  m1<-plot_list_da[[1]][[i]][[1]]
  m2<-plot_list_da[[2]][[i]][[1]]
  m3<-plot_list_da[[3]][[i]][[1]]
  
  f1<-plot_list_da[[1]][[i]][[2]]
  f2<-plot_list_da[[2]][[i]][[2]]
  f3<-plot_list_da[[3]][[i]][[2]]
  
  library('ggpubr')
  
  figure_both<-ggarrange(m1, m2, m3,f1, f2, f3, ncol = 3, nrow = 2, 
                         common.legend = TRUE, legend="right")
  
  
  g<-annotate_figure(figure_both,
                     top = text_grob(paste0("Age-Standardized Mortality from ", cause_label[[i]], ", Years 2000-2019\n  "), color = "black", face = "bold", size = 25),
                     bottom = text_grob(~bold("Year"), color = "black", size = 20),
                     left = text_grob(~bold("Deaths per 100,000"), color = "black", rot = 90, size = 20),
                     right = text_grob("", color = "black", rot = 90))
  
  
  plot_list2_da[[i]] = g
  
}

# # Export --------------------------------------------------------------------

# plot_list2_da[[1]]
# ggsave(file="drugs.jpg", width = 20, height = 10)
# plot_list2_da[[2]]
# ggsave(file="alcohol.jpg", width = 20, height = 10)


# pdf('plots drugs and alcohol.pdf')
# pdf.options(width=20, height=10)
# for (i in 1:2) {
#   print(plot_list2_da[[i]])
#  }
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
  for (t in 1:2) {
    for (s in 1:2) {
      USA=midlife_da[[i]][[t]][[s]] %>%filter(country== "USA")
      UK=midlife_da[[i]][[t]][[s]] %>%filter(country== "UK")
      HI=midlife_da[[i]][[t]][[s]] %>%filter(country== "HI")
      CEE=midlife_da[[i]][[t]][[s]] %>%filter(country== "CEE")
      myplot_da<- midlife_da[[i]][[t]][[s]] %>%
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
        theme(legend.position="none")+
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              legend.text=element_text(size=16), legend.position = 'right',
              legend.justification="right",
              legend.margin=margin(0,10,40,10),
              legend.box.margin=margin(10,10,40,40),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              plot.title = element_text(size=20))+
        coord_cartesian(expand=TRUE, xlim = c(NA, 2019))+
        ggtitle(paste0('Ages ', age_label[[i]],', ',sex_label[[s]]))+
        scale_x_continuous(breaks = seq(2000, 2019, by = 5), lim = c(2000,2019), expand = c(0, 0))+
        geom_hline(yintercept = 1, colour='black')+
        coord_cartesian(expand=TRUE, ylim = c(-100, 100))
      
      plot_list_da[[i]][[t]][[s]] <- myplot_da
      
      
      #'Drug-Related Causes'
      if(midlife_da[[i]][[t]][[s]]$cause[1] == 'Drug-Related Causes'){
        myplot_da <- myplot_da + coord_cartesian(expand=TRUE, ylim = c(-50, 1000))
        plot_list_da[[i]][[t]][[s]] <- myplot_da
      }
      
      
      #'Alcohol-Related Causes'
      if(midlife_da[[i]][[t]][[s]]$cause[1] == 'Alcohol-Related Causes'){
        myplot_da <- myplot_da + coord_cartesian(expand=TRUE, ylim = c(-60, 60))
        plot_list_da[[i]][[t]][[s]] <- myplot_da
      }
      
      
      
      
    }
  }
}

############creating panels by cause 


plot_list2_da<-list()

for (i in 1:length(cause_label)){
  
  m1<-plot_list_da[[1]][[i]][[1]]
  m2<-plot_list_da[[2]][[i]][[1]]
  m3<-plot_list_da[[3]][[i]][[1]]
  
  f1<-plot_list_da[[1]][[i]][[2]]
  f2<-plot_list_da[[2]][[i]][[2]]
  f3<-plot_list_da[[3]][[i]][[2]]
  
  library('ggpubr')
  
  figure_both<-ggarrange(m1, m2, m3,f1, f2, f3, ncol = 3, nrow = 2, 
                         common.legend = TRUE, legend="right")
  
  
  g<-annotate_figure(figure_both,
                     top = text_grob(paste0("Percent Change in Age-Standardized Mortality from the Baseline Year (2000), ", cause_label[[i]]), color = "black", face = "bold", size = 25),
                     bottom = text_grob(~bold("Year"), color = "black", size = 20),
                     left = text_grob(~bold("Percent Change in  Deaths per 100,000"), color = "black", rot = 90, size = 20),
                     right = text_grob("", color = "black", rot = 90))
  
  
  plot_list2_da[[i]] = g
  
}

# Export --------------------------------------------------------------------

# pdf('drugs and alcohol plots change from the baseline.pdf')
# pdf.options(width=20, height=10)
# for (i in c(1:2)) {
#   print(plot_list2_da[[i]])
#  }
# 
#  dev.off()


