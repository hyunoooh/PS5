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
library(AER)
library(stats)
require(car)
require(ggplot2)
require(GGally)
require(VGAM)

# Load the data
ANES <- read.dta('anes_timeseries_2012_stata12.dta')

# randomly subset the data into two partitions

# Make a vector of the name of variables of interest
myVar <- c('ft_dpc', 'ft_rpc', 
           'econ_ecnow', 'econ_ecnext', 'econ_ecpast',
           'interest_whovote2008',
           'mediapo_tv', 'mediapo_radio', 'mediapo_nwsprev', 'mediapo_net'
)

# Subset the data which only include the variables of interest:
my.anes <- ANES[,is.element(colnames(ANES), myVar)]


### Recode variables

# feeling themometer score for Democratic candidate, Pr.Obama 
# and that for Republican candidate
unique(my.anes$ft_dpc) # we need to delete the missing values, the negative values
my.anes$ft_dpc <- ifelse(my.anes$ft_dpc>=0, my.anes$ft_dpc, NA)
summary(my.anes$ft_dpc) # Democratic candidate, Pr.Obama 

unique(my.anes$ft_rpc) # we need to delete the missing values, the negative values
my.anes$ft_rpc <- ifelse(my.anes$ft_rpc>=0, my.anes$ft_rpc, NA)
summary(my.anes$ft_rpc) # Republican candidate


# whom they vote for in 2008 elections
unique(my.anes$interest_whovote2008)  # we need to delete the missing values
# voted for Obama = 1, otherwise(John mccain) = 0
my.anes$interest_whovote2008 <- ifelse(my.anes$interest_whovote2008 == "-1. Inapplicable" |
                                      my.anes$interest_whovote2008 == "-9. Refused" |
                                      my.anes$interest_whovote2008 == "-8. Don't know" |
                                      my.anes$interest_whovote2008 == "5. Other {SPECIFY}", 
                                      NA, 
                                      ifelse(my.anes$interest_whovote2008 == "1. Barack obama", 1, 0
))

# how people concieve the state of national economy:
# Very good = 5, Good = 4, Neither good nor bad = 3, Bad=2, Very bad=1
summary(my.anes$econ_ecnow) # now
my.anes$econ_ecnow <- ifelse(my.anes$econ_ecnow == "-9. Refused" |
                            my.anes$econ_ecnow == "-8. Don't know", NA,
                            ifelse(my.anes$econ_ecnow == "1. Very good", 5,
                                   ifelse(my.anes$econ_ecnow == "2. Good", 4,
                                          ifelse(my.anes$econ_ecnow == "3. Neither good nor bad", 3,
                                                   ifelse(my.anes$econ_ecnow == "4. Bad", 2, 1)))))

# Gotten better = 3, Stayed about the same = 2, Gotten worse = 1
summary(my.anes$econ_ecpast) # last year
my.anes$econ_ecpast <- ifelse(my.anes$econ_ecpast == "-9. Refused" |
                               my.anes$econ_ecpast == "-8. Don't know", NA,
                                ifelse(my.anes$econ_ecpast == "1. Gotten better", 3,
                                    ifelse(my.anes$econ_ecpast == "2. Stayed about the same", 2, 1)))

# Get better = 3, Stay about the same = 2, Get worse = 1
summary(my.anes$econ_ecnext) # next year
my.anes$econ_ecnext <- ifelse(my.anes$econ_ecnext == "-9. Refused" |
                                my.anes$econ_ecnext == "-8. Don't know", NA,
                                  ifelse(my.anes$econ_ecnext == "1. Get better", 3,
                                     ifelse(my.anes$econ_ecnext == "2. Stay about the same", 2, 1)))


# in terms of campaigns, how much they are exposed to different types of media 
# Yes = 1, No = 0
summary(my.anes$mediapo_tv) # Watch campaign programs on TV
my.anes$mediapo_tv <- ifelse(my.anes$mediapo_tv == "-9. Refused" |
                               my.anes$mediapo_tv == "-8. Don't know" |
                               my.anes$mediapo_tv == "-7. Deleted due to partial (post-election) interview" |
                               my.anes$mediapo_tv == "-6. Not asked, unit nonresponse (no post-election interview)", NA,
                               ifelse(my.anes$mediapo_tv == "1. Yes", 1, 0))
summary(my.anes$mediapo_radio) # Hear radio speeches/discussn about Pres campaign
my.anes$mediapo_radio <- ifelse(my.anes$mediapo_radio == "-9. Refused" |
                               my.anes$mediapo_radio == "-8. Don't know" |
                               my.anes$mediapo_radio == "-7. Deleted due to partial (post-election) interview" |
                               my.anes$mediapo_radio == "-6. Not asked, unit nonresponse (no post-election interview)", NA,
                             ifelse(my.anes$mediapo_radio == "1. Yes", 1, 0))
summary(my.anes$mediapo_nwsprev) # Read about Presidential campaign in newspaper
my.anes$mediapo_nwsprev <- ifelse(my.anes$mediapo_nwsprev == "-9. Refused" |
                               my.anes$mediapo_nwsprev == "-8. Don't know" |
                               my.anes$mediapo_nwsprev == "-7. Deleted due to partial (post-election) interview" |
                               my.anes$mediapo_nwsprev == "-6. Not asked, unit nonresponse (no post-election interview)", NA,
                             ifelse(my.anes$mediapo_nwsprev == "1. Yes", 1, 0))
summary(my.anes$mediapo_net) # View/hear internet information abt Pres campaign
my.anes$mediapo_net <- ifelse(my.anes$mediapo_net == "-9. Refused" |
                               my.anes$mediapo_net == "-8. Don't know" |
                               my.anes$mediapo_net == "-7. Deleted due to partial (post-election) interview" |
                               my.anes$mediapo_net == "-6. Not asked, unit nonresponse (no post-election interview)", NA,
                             ifelse(my.anes$mediapo_net == "1. Yes", 1, 0))
# check if variables are coded in a right way:
unique(my.anes$mediapo_net)
unique(my.anes$mediapo_nwsprev)
unique(my.anes$mediapo_radio)
unique(my.anes$mediapo_tv)


### Randomly subset the data into two partitions:
training.set <- my.anes[sample(1:nrow(my.anes), nrow(my.anes)/2, replace = FALSE),]

### model.1: OLS model
###          how does the feeling thermometer for republican candidate 
###          and how they voted in 2008 election relate to 
###          how much they like the democratic candidate, Pr.Obama 
### Y = feeling thermometer score for Pr.Obama
### X1 = feeling thermometer score for McCain
### X2 = whether they voted for Pr.Obama in the 2008 election
### X1*X2 = interaction term

model.1 <- lm(ft_dpc ~ ft_rpc*interest_whovote2008, data = training.set)
summary(model.1)

### model.2: OLS model
###          how does the perception of national economy 
###          affect the feeling themometer for Pr.Obama
### Y = feeling thermometer score for Pr.Obama
### X1 = perception of national economy (Past)
### X2 = perception of national economy (Now)
### X3 = perception of national economy (Future)

model.2 <- lm(ft_dpc ~ econ_ecpast+econ_ecnow+econ_ecnext, data = training.set)
summary(model.2)

### model.3: Tobit model
###          depending on the type of media exposure 
###          how feeling themometer for Pr.Obama differs
### Y = feeling thermometer score for Pr.Obama
### X1 = media exposure: TV
### X2 = media exposure: radio
### X3 = media exposure: newspaper
### X4 = media exposure: internet

ggpairs(training.set[, c("mediapo_tv", "mediapo_radio", "mediapo_nwsprev", "mediapo_net")])
model.3 <- tobit(ft_dpc ~ mediapo_tv+mediapo_radio+mediapo_nwsprev+mediapo_net,
                 data=taining.set, left = 0, right = 100)
summary(model.3)


### Make predictions for the other partition of the data:
# model.1:
predict.model.1 <- predict(model.1, newdata = my.anes[!taining.set,], 
                           type = 'response')
# model.2:
predict.model.2 <- predict(model.2, newdata = my.anes[!taining.set,], 
                           type = 'response')
# model.3:
predict.model.3 <- predict(model.3, newdata = my.anes[!taining.set,], 
                           type = 'response')

### Generate a test set:
test.set <- matrix(c(predict.model.1, predict.model.2, predict.model.3), ncol = 3)
colnames(test.set) <- c("OLS.interaction", "OLS", "Tobit")



### Use our package: 
