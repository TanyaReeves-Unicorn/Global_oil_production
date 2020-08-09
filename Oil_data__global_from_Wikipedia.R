
# AIM: This is a list of countries by oil production, as compiled 
# from the U.S. Energy Information Administration database for calendar year 2019
# CODING AIM: CREATE A MAP OF TOP PRODUCERS, data from Wikipedia
# Acknowledgements: many thanks for help and inspiration from R_Bloggers, R_London

#=====================

# EXPLAIN KEY TERMS

#=====================

# bbl = barrels
# prod_per_capita = production per capita

#==============
# LOAD PACKAGES
#==============

library(tidyverse)
library(sf)
library(rvest)
library(stringr)
library(scales)
library(viridis)
library(maps)

#============
# SCRAPE DATA
#============

df.oil <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_oil_production") %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()


head(df.oil)

#====================
# CHANGE COLUMN NAMES 
#====================

colnames(df.oil) <- c('rank', 'country', 'oil_bbl_per_day', 'prod_per_capita')

#=============================
# WRANGLE VARIABLES INTO SHAPE
#=============================

#----------------------------------
# COERCE 'rank' VARIABLE TO INTEGER
# bbl_per_day into integer
# prod_per_capita into integer
#----------------------------------

df.oil <- df.oil %>% mutate(rank = as.integer(rank)) 

df.oil <- df.oil %>% mutate(oil_bbl_per_day = oil_bbl_per_day %>% str_replace_all(',','') %>% as.integer())

df.oil <- df.oil %>% mutate(prod_per_capita = prod_per_capita %>% str_replace_all(',','') %>% as.integer())

df.oil %>% glimpse()

#===========================
#CREATE VARIABLE: 'opec_ind'
# 1 = OPEC country, 0 = not
#===========================

df.oil <- df.oil %>% mutate(opec_ind = if_else(str_detect(country, 'OPEC'), 1, 0))

#=========================================================
# CLEAN UP 'country'
# - some country names are tagged as being OPEC countries
#   and this information is in the country name
# - we will strip this information out
#=========================================================

df.oil <- df.oil %>% mutate(country = country %>% str_replace(' \\(OPEC\\)', '') %>% str_replace('\\s{2,}',' '))

#------------------------------------------
# EXAMINE OPEC COUNTRIES
# - here, we'll just visually inspect
#   to make sure that the names are correct
#------------------------------------------

df.oil %>% 
  filter(opec_ind == 1) %>%
  select(country)
#                  country
#1            Saudi Arabia
#2                    Iraq
#3                    Iran
#4    United Arab Emirates
#5                  Kuwait
#6               Venezuela
#7                 Nigeria
#8                  Angola
#9                 Algeria
#10                  Libya
#11                Ecuador
#12 Congo, Republic of the
#13      Equatorial Guinea
#14                  Gabon

#==================
# REORDER VARIABLES
#==================

df.oil <- df.oil %>% select(rank, country, opec_ind, oil_bbl_per_day)

df.oil %>% glimpse()

#========
# GET MAP
#========

map.world <- map_data('world')

df.oil

#==========================
# CHECK FOR JOIN MISMATCHES
#==========================

anti_join(df.oil, map.world, by = c('country' = 'region'))
#rank                           country opec_ind oil_bbl_per_day
#1   NA                  World production        0        80622000
#2    1                  United States[6]        0        15043000
#3   21                    United Kingdom        0          939760
#4   31            Congo, Republic of the        1          308363
#5   35             Sudan and South Sudan        0          255000
#6   47               Trinidad and Tobago        0           60090
#7   67 Congo, Democratic Republic of the        0           20000

#=====================
# RECODE COUNTRY NAMES
#=====================

map.world %>%
  group_by(region) %>%
  summarise() %>%
  print(n = Inf)
# 252 diferent countries and regions

#=========================================
# MAKE EVERYTHING MORE READABLE
#=========================================

#-----------------------
# JOIN DATASETS TOGETHER
#-----------------------

map.oil <- left_join(map.world, df.oil, by = c('region' = 'country')) 

#=====
# PLOT
#=====

# BASIC (this is a first draft)

ggplot(map.oil, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = oil_bbl_per_day))

#=======================
# FINAL, FORMATTED DRAFT
#=======================


df.oil %>% filter(oil_bbl_per_day > 822675) %>% summarise(mean(oil_bbl_per_day))
# 6518342

df.oil %>% filter(oil_bbl_per_day < 822675) %>% summarise(mean(oil_bbl_per_day))
# 96581.08

ggplot(map.oil, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = oil_bbl_per_day))

ggplot(map.oil, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = oil_bbl_per_day)) +
  scale_fill_gradientn(colours = c('#461863','#404E88','#2A8A8C','#7FD157','#F9E53F')
                       ,values = scales::rescale(c(100,96581,822675,3190373,10000000))
                       #,labels = comma
                       ,breaks = c(100,96581,822675,3190373,10000000)
  ) +
  guides(fill = guide_legend(reverse = T)) +
  labs(fill = 'bbl/day'
       ,title = 'Oil Production by Country'
       ,subtitle = 'Barrels per day, 2019'
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(family = 'Gill Sans', color = '#EEEEEE')
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 14)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#333333')
        ,plot.background = element_rect(fill = '#333333')
        ,legend.position = c(.18,.36)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) +
  annotate(geom = 'text'
           ,label = 'Source: U.S. Energy Information Administration\nhttps://en.wikipedia.org/wiki/List_of_countries_by_oil_production'
           ,x = 18, y = -55
           ,size = 3
           ,family = 'Gill Sans'
           ,color = '#CCCCCC'
           ,hjust = 'left'
  )

