


###set up

library(readr)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(grid)
library(scales)
library(reshape2)
library(hexbin)
library(ellipse)
library(corrplot)
library(lattice)
library(MASS)
library(car)
library(splines)
library(corrgram)

### read dataset 
sdd<- read.csv("Untitled_Message/Speed Dating Data.csv", header = T, stringsAsFactors = F)
dim(sdd)
str(sdd)
sum(is.na(sdd))

###check missing data
missing.data<- c("NA", "")
missing.data<-0
sdd[is.na(sdd)]<-0
sum(is.na(sdd))
View(sdd)
colnames(sdd)


##### bar plots analysis #####

###Events
qplot(data=sdd, wave, binwidth=1, colour=I("black"), fill=I("coral"), 
      xlab="Events", ylab="numbers")

###ages
str(sdd$age)
mean(sdd$age)
qplot(data=sdd, age, binwidth=1.5, colour=I("black"), fill=I("coral"))

###races
sddr<-sdd %>% mutate(race1=ifelse(race_o %in% 1,"Black/African American",
                           ifelse(race_o %in% 2, "European/Caucasian-American",
                           ifelse(race_o %in% 3,"Latino/Hispanic American", 
                           ifelse(race_o %in% 4, "Asian/Pacific Islander/Asian-American",
                           ifelse(race_o %in% 5, "Native American", 
                           "other"))))))
qplot(data=sddr, x=race1, geom = "bar", colour=I("black"), fill=I("coral"))

###fields
sdd$field_cd=factor(sdd$field_cd)
sddf<-sdd %>% mutate(field1=ifelse(field_cd %in% 1,"Business/Law",
                                 ifelse(field_cd %in% 2, "Math/Statistics",
                                        ifelse(field_cd %in% 3,"Psychology/Sociology", 
                                               ifelse(field_cd %in% 4, "Medicine",
                                                      ifelse(field_cd %in% 5, "Engineering", 
                                                             ifelse(field_cd %in% 6,"Classic/Writing",
                                                                    ifelse(field_cd %in% 7, "Philosophy/Religion",
                                                                           ifelse(field_cd %in% 8,"Economics/Business/Finance", 
                                                                                  ifelse(field_cd %in% 9, "Education",
                                                                                         ifelse(field_cd %in% 10, "Chemistry", 
                                                                                                ifelse(field_cd %in% 11,"Social Work",
                                                                                                       ifelse(field_cd %in% 12, "Undergrad - GS",
                                                                                                              ifelse(field_cd %in% 13,"Political/International Affair", 
                                                                                                                     ifelse(field_cd %in% 14, "Film",
                                                                                                                            ifelse(field_cd %in% 15, "Art/Theater", 
                                                                                                                                   ifelse(field_cd %in% 16, "Language",
                                                                                                                                          ifelse(field_cd %in% 17,"Architecture", 
                                                                                                                                                 ifelse(field_cd %in% 18,"GSAS","Others")))))))))))))))))))

qplot(data=sddf,  x=field1, geom = "bar",colour=I("black") , fill=I("coral"))

###goal

sddg<-sdd %>% mutate(goal1=ifelse(goal %in% 1,"Seemed like a fun night out",
                                   ifelse(goal %in% 2, "To meet new people",
                                          ifelse(goal %in% 3,"To get a date", 
                                                 ifelse(goal %in% 4, "Looking for a serious relationship",
                                                        ifelse(goal %in% 5, "To say I did it", 
                                                               "others"))))))

qplot(data=sddg,  x=goal1, geom = "bar",colour=I("black") , fill=I("coral"))

###match
barplot(
  table(sdd$match),xlab = '0 = not match, 1 = match', ylab = 'numbers',
  main = "Match?",
  col = c("darkred","deepskyblue4"))

##### Relationship #####  

### gender and fields

sdd[sdd$gender == 0,]$gender <- "W"
sdd[sdd$gender == 1,]$gender <- "M"
field2 <- sdd[!is.na(sdd$field_cd),] %>%
  group_by(gender, field_cd) %>%
  summarise(n = n())

  
field3<-c("Business/Law","Math/Statistics","Psychology/Sociology","Medicine",
          "Engineering","Classic/Writing","Philosophy/Religion","Economics/Business/Finance",
          "Education","Chemistry","Social Work","Undergrad - GS","Political/International Affair",
          "Film","Art/Theater","Language","Architecture","GSAS","Others")

ggplot(field2, aes(x = field_cd, y=n, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Gender") +
  xlab("Field") + ylab("Count") + ggtitle("Gender & fields") +
  scale_x_discrete(labels = field3, breaks = 1:19) +
  coord_flip()+
  scale_fill_manual(values=c("darkcyan","darkgoldenrod2"))

###gender and age

race2 <- c(
  "Black/African American",
  "European/Caucasian-American",
  "Latino/Hispanic American",
  "Asian/Pacific Islander/Asian-American",
  "Native American",
  "Other"
)

race3 <- sdd[!is.na(sdd$race),] %>%
  group_by(gender, race) %>%
  summarise(
    n = n()
  )

ggplot(race3, aes(x = race, y = n, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Gender") +
  xlab("Race") + ylab("Count") + ggtitle("Race & Gender") +
  scale_x_continuous(labels = race2, breaks = 1:6) +
  coord_flip()+
  scale_fill_manual(values=c("darkcyan","darkgoldenrod2"))

### order and match
om<-sdd %>%
  group_by(order) %>%
  summarise(average=mean(match, na.rm=TRUE))
qplot(data=om, x=order, y=average, xlab="Order of dating", ylab="Match") + geom_smooth(method=loess,color=I("blue"))

###points of attributes
sdd <- sdd %>%
  mutate(sum1_1=attr1_1+sinc1_1+intel1_1+fun1_1+amb1_1+shar1_1) %>%
  mutate(attr1_1n=attr1_1/(sum1_1/100)) %>%
  mutate(sinc1_1n=sinc1_1/(sum1_1/100)) %>%
  mutate(intel1_1n=intel1_1/(sum1_1/100)) %>%
  mutate(amb1_1n=amb1_1/(sum1_1/100)) %>%
  mutate(shar1_1n=shar1_1/(sum1_1/100))

boxplot(sdd[,65:70], col="red", 
        xlab='Attributes',ylab='Score',main='Importance of Attributes')


##### decision correlation (6 attributes) #####

attr1 <- sdd %>% group_by(iid, age, gender, income, exphappy, goal, go_out, date) %>% 
  summarise(dec1= mean(dec_o, na.rm = T),
            attr = mean(attr_o, na.rm = T), 
            sinc = mean(sinc_o, na.rm = T), 
            intel = mean(intel_o, na.rm = T), 
            fun = mean(fun_o, na.rm = T), 
            amb = mean(amb_o, na.rm = T), 
            shar = mean(shar_o, na.rm = T))
  

quartz()
corrgram(attr1, 
         upper.panel=panel.cor, text.panel=panel.txt,
         main="Correlation of Attractiveness")

##### Linear Regression #####

###test 1.1: decision~attractive
lm.attr11 = lm(dec1~attr,data=attr1)
summary(lm.attr11)

###test 1.2: decision~fun
lm.attr12 = lm(dec1~fun,data=attr1)
summary(lm.attr12)


###test 1.3: decision~sincere
lm.attr13 = lm(dec1~sinc,data=attr1)
summary(lm.attr13)

###test 1.4: decision~intelligece
lm.attr14 = lm(dec1~intel,data=attr1)
summary(lm.attr14)

###test 1.5: decision~ambitious
lm.attr15 = lm(dec1~amb,data=attr1)
summary(lm.attr15)

###test 1.6: decision~share interests
lm.attr16 = lm(dec1~shar,data=attr1)
summary(lm.attr16)

##### Choose 3 strong variables #####
### attractive,fun,shar(R-squared > 0.3)

### test 2.1
lm.attr21 = lm(dec1~attr+fun+shar,data=attr1)
summary(lm.attr21)
plot(lm.attr21)

### test 2.2
lm.attr22 = lm(dec1~attr*fun*shar,data=attr1)
summary(lm.attr22)
plot(lm.attr22)

### test 2.3
lm.attr23 = lm(dec1~attr+fun:shar,data=attr1)
summary(lm.attr23)
plot(lm.attr23)
