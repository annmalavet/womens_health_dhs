library(tidyverse)
library(haven)
library(elasticnet)
library(janitor)
library(ggplot2)
setwd("/Users/~/Documents/w_h")
allData <- read_dta("data/raw/ZMIR61FL.DTA")
childData <- read_dta("data/raw/child.DTA")
options(max.print=1000000) 

##AGE OF MOTHER @ FIRST BIRTH
##
##
age_at_birth0 <-allData$v212

# unique(age_at_birth0)
# [1] 19 NA 20 18 23 16 21 17 22 15 13 14 28 25 26 24 27 29 11 30 32 12 34 31 42 33 37 35 36 38 40
##
##VIOLENCE INDEX
##weight for domestic violence responders
##needs to be of a 6 decimal place
##divide by 1000000
unique(allData$d005)
allData$violenceIndex <- allData$d005 / 1000000
allData$violenceIndex
##
##
##EDUCATION LEVEL
education_level <- allData$v106
###maked education variable. a binary value - higher education and greater is 1 else 0
###
allData$binaryEdu <- as.numeric(allData$v106 > 2)
unique(allData$binaryEdu)
##
##
##REGION
region <- allData$v024
unique(region)
##
##
#NUMBER OF ANTENATAL VISITS FOR PREGNANCY - FIRST PREGNANCY
number_antenatal <- allData$m14_1
#[1]  3 NA  2  4  1  5  6  7 98 11  0  8 10  9 14 12 ##remove NA and 98
unique(number_antenatal)
##
##
#whether house has electricity
##0 is NO, 1 is YES, 7 is not a daily resident
##Exclude 7 or make NA

has_electricity <- allData$v119

###1  0  7 NA --7 is not a resident - NA 
allData %>% filter(!is.na(has_electricity))
ncol(allData)
unique(has_electricity)

## remove records with no weight values for first birthweight
something <- allData %>%
  ##drop NA for violence index
  drop_na(violenceIndex) %>%
  filter(violenceIndex < 2)  %>% 
  ##drop NA for binary EDU
  drop_na(binaryEdu) %>%
  ##antenatal - remove 98 and NA
  drop_na(m14_1) %>%
  filter(m14_1 !=98) %>%
  ##drop NA for first birth weight
  drop_na(m19_1) %>%
  ##removing 'no response' from 'birth weights'
  filter(m19_1 != 9996)  %>% 
  filter(m19_1 != 9998)
  ##removing "not a resident from 'has electricity'"
  filter(v119 != 7)  %>% 
  ##removing NA from age at first birth
  drop_na(v212) %>%

#allData[!is.na(allData$m19_1),]
unique(something$m19_1)
unique(something$m14_1)
unique(something$binaryEdu)
unique(something$violenceIndex)
#age of mother at birth
unique(something$v212)
#nrow(something)

count(something, v024==10)

######
#
# New Subset with birthweight, regions, has electricity, educational level
# antenatal visits frequency, age of mother at first birth
#
######

  
REGION1 <- filter(something, v024 == 1)

REGION2 <- filter(something, v024 == 2)
REGION3 <- filter(something, v024 == 3)
REGION4 <- filter(something, v024 == 4)
REGION5 <- filter(something, v024 == 5)
REGION6 <- filter(something, v024 == 6)
REGION7 <- filter(something, v024 == 7)
REGION8 <- filter(something, v024 == 8)
REGION9 <- filter(something, v024 == 9)
REGION10 <- filter(something, v024 == 10)


##By Region Linear regression for binary edu
##edu was binarized
reg1 <-lm( m19_1 ~ binaryEdu m19_1 , data=REGION1)
summary(reg1)
#p-value: 0.8696

reg2 <-lm( m19_1 ~  binaryEdu , data=REGION2)
summary(reg2)
#p-value: 0.3246

reg3 <-lm( m19_1 ~ binaryEdu  , data=REGION3)
summary(reg3)
#p-value:0.8492

reg4 <-lm( m19_1 ~ binaryEdu  , data=REGION4)
summary(reg4)
#p-value: 0.1125

reg5 <-lm( m19_1 ~  binaryEdu  , data=REGION5)
summary(reg5)
#p-value: 0.1361

reg6 <-lm( m19_1 ~ binaryEdu  , data=REGION6)
summary(reg6)
#p-value: 0.7048

reg7 <-lm( m19_1 ~ binaryEdu  , data=REGION7)
summary(reg7)
#p-value: 0.009314

reg8 <-lm( m19_1 ~ binaryEdu , data=REGION8)
summary(reg8)
#p-value: 0.3486

reg9 <-lm( m19_1 ~ binaryEdu , data=REGION9)
summary(reg9)
#p-value: 0.5174

reg10 <-lm(m19_1~ binaryEdu  , data=REGION10)
summary(reg10)
#p-value: 0.6077

### STEP TWO BY REGION - VIOLENCE INDEX
###
###
reg1 <-lm(m19_1~ binaryEdu + violenceIndex  , data=REGION1)
summary(reg1)
ggplot(reg1)
#p-value: 0.4705

reg2 <-lm(m19_1~ binaryEdu + violenceIndex , data=REGION2)
summary(reg2)
#p-value: 0.02172

reg3 <-lm(m19_1~ binaryEdu + violenceIndex  , data=REGION3)
summary(reg3)
#p-value: 0.9432

reg4 <-lm(m19_1~ binaryEdu + violenceIndex  , data=REGION4)
summary(reg4)
#p-value: 0.1222

reg5 <-lm(m19_1~ binaryEdu+ violenceIndex   , data=REGION5)
summary(reg5)
#p-value: 0.1967

#p-value: 0.1403

reg6 <-lm(m19_1~ binaryEdu+ violenceIndex   , data=REGION6)
summary(reg6)
#p-value: 0.1403

reg7 <-lm(m19_1~ binaryEdu+ violenceIndex  , data=REGION7)
summary(reg7)
#p-value: 0.006326

##reg 8 without edu
reg8e <-lm(m19_1~  violenceIndex  , data=REGION8)
summary(reg8e)
#p-value: 0.1748

reg8 <-lm(m19_1~ binaryEdu+ violenceIndex  , data=REGION8)
summary(reg8)
#p-value: 0.266

reg9 <-lm(m19_1~ binaryEdu+ violenceIndex  , data=REGION9)
summary(reg9)
#p-value: 0.7059

reg10 <-lm(m19_1~ binaryEdu+ violenceIndex  , data=REGION10)
summary(reg10)
#p-value: 0.6513

###STEP 3 ANTENATAL VISIT AMOUNT
##only for .5 and above regions

##reg 8 without edu or violence
reg8e <-lm(m19_1~  violenceIndex + m14_1 , data=REGION8)
summary(reg8e)
#p-value: 0.3295

##removing violence index region six
reg6v <-lm(m19_1~ binaryEdu+ m14_1   , data=REGION6)
summary(reg6v)

reg1ANTE <-lm(m19_1~ binaryEdu + violenceIndex + m14_1  , data=REGION1)
summary(reg1ANTE)

reg3ANTE <-lm(m19_1~ binaryEdu + violenceIndex + m14_1  , data=REGION3)
summary(reg3ANTE)
#p-value: 0.9286

reg8ANTE <-lm(m19_1~ binaryEdu+ violenceIndex + m14_1  , data=REGION8)
summary(reg8ANTE)
#p-value: 0.3522

reg9ANTE <-lm(m19_1~ binaryEdu+ violenceIndex + m14_1 , data=REGION9)
summary(reg9ANTE)

reg10ANTE <-lm(m19_1~ binaryEdu+ violenceIndex + m14_1 , data=REGION10)
summary(reg10ANTE)
#p-value: 0.2053

####STEP 4 
#### age of mother at birth v212
###
###
reg8e <-lm(m19_1~   v212 , data=REGION8)
summary(reg8e)
#p-value: 0.03291

##removing violence index region six
reg6v <-lm(m19_1~ binaryEdu+ m14_1 + v212   , data=REGION6)
summary(reg6v)

reg1birthage <-lm(m19_1~ binaryEdu + violenceIndex + v212 + m14_1   , data=REGION1)
summary(reg1birthage)

reg3 <-lm(m19_1~ binaryEdu + violenceIndex + v212 + m14_1   , data=REGION3)
summary(reg3)
#p-value: 0.9981

reg8 <-lm(m19_1~ binaryEdu+ violenceIndex + v212 + m14_1  , data=REGION8)
summary(reg8)
#p-value:0.03153

##without antenatal visits
reg9 <-lm(m19_1~ binaryEdu+ violenceIndex + v212  , data=REGION9)
summary(reg9)
#p-value:  0.7241

reg10 <-lm(m19_1~ binaryEdu+ violenceIndex + v212 + m14_1  , data=REGION10)
summary(reg10)
#p-value:0.7724

###STEP 5
###only for p values .5 and above for ELECTRICITY v119

##reegion 8
##removing violence index region six
##region 8 only v119
reg8e <-lm(m19_1~   v119 , data=REGION8)
summary(reg8e)
#p-value: 0.01338

reg6v <-lm(m19_1~ binaryEdu+ m14_1 + v212 +v119   , data=REGION6)
summary(reg6v)

reg1EL <-lm(m19_1~ binaryEdu+ violenceIndex + v212 + v119 + m14_1 , data=REGION1)
summary(reg1EL)
#p-value: 0.554

reg2EL <-lm(m19_1~ binaryEdu+ violenceIndex + v212 + v119 + m14_1 , data=REGION2)
summary(reg2EL)
#p-value: 0.1606

reg3EL <-lm(m19_1~ binaryEdu+ violenceIndex + v212 + v119 + m14_1 , data=REGION3)
summary(reg3EL)
#p-value: 0.8494

reg4EL <-lm(m19_1~ binaryEdu+ violenceIndex + v212 + v119 + m14_1 , data=REGION4)
summary(reg4EL)
#p-value: 0.2648

reg5EL <-lm(m19_1~ binaryEdu+ violenceIndex + v212 + v119  + m14_1, data=REGION5)
summary(reg5EL)
#p-value: 0.02194

reg6EL <-lm(m19_1~ binaryEdu+ violenceIndex + v212 + v119 + m14_1 , data=REGION6)
summary(reg6EL)
# p-value: 0.3929

reg7EL <-lm(m19_1~ binaryEdu+ violenceIndex + v212 + v119 + m14_1 , data=REGION7)
summary(reg7EL)
#p-value: 0.008767

reg8EL <-lm(m19_1~ binaryEdu+ violenceIndex + v212 + v119  + m14_1, data=REGION8)
summary(reg8EL)
#p-value: .01134

##REGION9 ( WITHOUT antenatal)
reg9EL <-lm(m19_1~ binaryEdu+ violenceIndex + v212 + v119  data=REGION9)
summary(reg9EL)
#p-value: 0.8333

reg10EL <-lm(m19_1~ binaryEdu+ violenceIndex + v212 + v119  + m14_1, data=REGION10)
summary(reg10EL)
#p-value: 0.7434

##
##

