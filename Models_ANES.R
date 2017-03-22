#======================
# Problem Set 05
# Author: Hyunjoo Oh
#======================

# make the environment empty
rm(list=ls())
# set the working directory:
setwd('/Users/hyunjoooh/Desktop/2017_Stat_Prog/PS5')

##### 1-A #####
# Foreign makes it easier to read .dta into R 
library(foreign)
# Load the data
ANES <- read.dta('anes_timeseries_2012_stata12.dta')

# randomly subset the data into two partitions

# Make a vector of the name of variables of interest
myVar <- c('ft_dpc', 'ft_rpc', 
           'econ_ecnow', 'econ_ecnext', 'econ_ecpast',
           'interest_whovote2008',
           'trust_social',
           'mediapo_tv', 'mediapo_radio', 'mediapo_nwsprev', 'mediapo_net'
)

# Subset the data which only include the variables of interest:
my.anes <- ANES[,is.element(colnames(ANES), myVar)]

# recode variables
# feeling themometer score for democratic candidate, Pr.Obama 
# and that for republican candidate
summary(my.anes$ft_dpc)
summary(my.anes$ft_rpc)

# whom they vote for in 2008 elections
summary(my.anes$interest_whovote2008)

# how people concive the state of national economy:
summary(my.anes$econ_ecnow) # now
summary(my.anes$econ_ecpast) # last year
summary(my.anes$econ_ecnext) # next year

# in terms of campaigns, how much they are exposed to different types of media 
summary(my.anes$mediapo_tv) # Watch campaign programs on TV
summary(my.anes$mediapo_radio) # Hear radio speeches/discussn about Pres campaign
summary(my.anes$mediapo_nwsprev) # Read about Presidential campaign in newspaper
summary(my.anes$mediapo_net) # View/hear internet information abt Pres campaign

