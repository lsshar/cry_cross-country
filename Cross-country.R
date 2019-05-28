## Author: Leah S. Sharman
## Date Created: 15.02.2019
## Desc: Data analysis for cross-cultural study

############    DATA WRANGLING    ##############
##Created BACS subscales
#*BACS helpful
#*BACS unhelpful social
#*BACS unhelpful individual

##Created combined gender endorsement variable with z-scores used from all items of 'GES' and 'TMF'

##Created combined cry-intensity variable with z-scores used from 'intense_tear' and 'intense_time'

##Imputed scores for estimated crying frequency where a range was provided

###############   DATA ANALYSIS   ###############


#read the cross-cultural data set using the name 'data'
data <- read.csv(file = "x-cultural_data.csv")






###########################    Descriptives     ###########################

#Install packages for analyses
install.packages("psych")
library("psych")

#total N participants 

#mean age
mean(data$age)

#describe by gender
describe.by(data$age, group = data$gender)
describeBy(data$age, group = data$live)
describeBy(data$gender, group = data$live)

install.packages("jmv")
library("jmv")

install.packages("dplyr")
library("dplyr")

##Exploratory descriptives and t-tests by country and gender
descriptives(data=data, vars = c("Freq", "mood_1", "Cry_intensity","BACS_Help","BACS_social","BACS_Ind"), split = c("live", "gender"), n = FALSE, missing = FALSE, min = FALSE, max = FALSE, median = FALSE)

##Exploratory descriptives and t-tests by gender
descriptives(data=data, vars = c("Freq", "mood_1", "Cry_intensity","BACS_Help","BACS_social","BACS_Ind"), split = "gender", sd = TRUE, n = FALSE, missing = FALSE, min = FALSE, max = FALSE, median = FALSE)


## filter for gender
gender.2 <- filter(data, gender == "1" | gender == "2")

#t-test for frequency on gender
ttestIS(formula = Freq ~ gender, data = gender.2, effectSize = TRUE)
#Levenes test sig - Accoung for with Welchs
ttestIS(formula = Freq ~ gender, data = gender.2, effectSize = TRUE, welchs = TRUE)

#####Cry-intensity
ttestIS(formula = Cry_intensity ~ gender, data = gender.2, effectSize = TRUE)
#Levenes test sig - Account for with Welchs
ttestIS(formula = Cry_intensity ~ gender, data = gender.2, effectSize = TRUE, welchs = TRUE)

#####t-test for mood following crying on gender
ttestIS(formula = mood_1 ~ gender, data = gender.2, effectSize = TRUE)

#####t-test for BACS Helpful on gender
ttestIS(formula = BACS_Help ~ gender, data = gender.2, effectSize = TRUE)
#Levenes test sig - Account for with Welchs
ttestIS(formula = BACS_Help ~ gender, data = gender.2, effectSize = TRUE, welchs = TRUE)

#####t-test for BACS Social on gender
ttestIS(formula = BACS_social ~ gender, data = gender.2, effectSize = TRUE)
#Levenes test sig - Account for with Welchs
ttestIS(formula = BACS_social ~ gender, data = gender.2, effectSize = TRUE, welchs = TRUE)

#####t-test for BACS Individual on gender
ttestIS(formula = BACS_Ind ~ gender, data = gender.2, effectSize = TRUE)

###########   ANOVA for between countries and gender    ##########
##Filter men
men <- filter(data, gender == "1")

ANOVA(formula = live ~ Freq, data = men, effectSize = "eta",
      emmPlots = TRUE,          
      emmTables = TRUE)  

ANOVA(formula = live ~ Cry_intensity, data = men, effectSize = "eta",
      emmPlots = TRUE,          
      emmTables = TRUE)  

ANOVA(formula = live ~ BACS_Help, data = men, effectSize = "eta",
      emmPlots = TRUE,          
      emmTables = TRUE)  

ANOVA(formula = live ~ BACS_social, data = men, effectSize = "eta",
      emmPlots = TRUE,          
      emmTables = TRUE)  

ANOVA(formula = live ~ BACS_Ind, data = men, effectSize = "eta",
      emmPlots = TRUE,          
      emmTables = TRUE)  
##Filter women
women <- filter(data, gender == "2")

ANOVA(formula = live ~ Freq, data = women, effectSize = "eta",
      emmPlots = TRUE,          
      emmTables = TRUE)  

ANOVA(formula = live ~ Cry_intensity, data = women, effectSize = "eta",
      emmPlots = TRUE,          
      emmTables = TRUE)  

ANOVA(formula = live ~ BACS_Help, data = women, effectSize = "eta",
      emmPlots = TRUE,          
      emmTables = TRUE)  

ANOVA(formula = live ~ BACS_social, data = women, effectSize = "eta",
      emmPlots = TRUE,          
      emmTables = TRUE)  

ANOVA(formula = live ~ BACS_Ind, data = women, effectSize = "eta",
      emmPlots = TRUE,          
      emmTables = TRUE)  





######################    Correlation analysis     ######################

#Install packages for analyses
install.packages("Hmisc")
library("Hmisc")
install.packages("corrplot")
library("corrplot")

#### Correlations between Gender, Gender role identity, BACS_Help, Cry)intensity, mood_1

#correlation function using *spearmans rho*, 
#column no's are 63= gender, 74= GRE, 71= BACS_Help, 75= Cry_intensity, 24= mood_1
mycor <- rcorr(as.matrix(data[,c(63,74,71,75,24)]), type = "spearman")

#ask for output:
mycor$n ##number of observations
mycor$r ##spearmans correlations
mycor$P ##p-values





#######################    Mediation analysis     #######################

#Install packages for analyses
install.packages("medmod")
library("medmod")

install.packages("car")
library("car")

##check distribution of last crying experience
descriptives(data=data, vars = Cry_last, freq = TRUE)

#Adequate distribution to use minimum crying recency at '2' or cried in the last week.

##create dichotomised variable in data for Cry_last with 1,2 == 1; 3,4,5,6 == 2
data$last.cry <- car::recode(data$Cry_last, "1:2 = '1'; 3:6 = '2'") # 1=recent; 2 = not recent


myModel <- '
Cry_intensity ~ b1 * BACS_Help + c1 * GRE + c2 * last.cry
BACS_Help ~ a1 * GRE + a2 * last.cry

#indirect effects
indirect1 := a1 * b1

# contrasts
con1 := a1 * b1
con2 := a2 * b1
con3 := (a1-a2) * b1

# total effect
total1 := c1 + (a1 * b1)
total2 := c2 + (a2 * b1)
'

mediation1 <- ' # direct effect
Cry_intensity ~ c*GRE + last.cry
# mediator
BACS_Help ~ a*GRE + last.cry
Cry_intensity ~ b*BACS_Help
# indirect effect (a*b)
ab := a*b
# total effect
total := c + (a*b)'
require("lavaan")
fit <- sem(mediation1, 
           data=data, 
           se = "bootstrap", 
           bootstrap = 1000)

summary(fit, fit.measures=TRUE, rsquare=TRUE, ci = TRUE)


######################              Mediation by country
library("dplyr")

## apply filter for AUSTRALIA
live.Aus <- filter(data, live == "1")
pull(live.Aus) ##check filter

fit.Aus <- sem(mediation1, 
               data=live.Aus, 
               se = "bootstrap", 
               bootstrap = 1000)
summary(fit.Aus, fit.measures=TRUE, rsquare=TRUE, ci = TRUE)


## apply filter for CROATIA
live.Croatia <- filter(data, live == "2")
pull(live.Croatia) ##check filter

fit.Croatia <- sem(mediation1, 
                   data=live.Croatia, 
                   se = "bootstrap", 
                   bootstrap = 1000)
summary(fit.Croatia, fit.measures=TRUE, rsquare=TRUE, ci = TRUE)


## apply filter for NETHERLANDS
live.NL <- filter(data, live == "3")
pull(live.NL) ##check filter

fit.NL <- sem(mediation1, 
              data=live.NL, 
              se = "bootstrap", 
              bootstrap = 1000)
summary(fit.NL, fit.measures=TRUE, rsquare=TRUE, ci = TRUE)


## apply filter for THAILAND
live.Thai <- filter(data, live == "4")
pull(live.Thai) ##check filter

fit.Thai <- sem(mediation1, 
                data=live.Thai, 
                se = "bootstrap", 
                bootstrap = 1000)
summary(fit.Thai, fit.measures=TRUE, rsquare=TRUE, ci = TRUE)

## apply filter for UK
live.UK <- filter(data, live == "5")
pull(live.UK) ##check filter

fit.UK <- sem(mediation1, 
              data=live.UK, 
              se = "bootstrap", 
              bootstrap = 1000)
summary(fit.UK, fit.measures=TRUE, rsquare=TRUE, ci = TRUE)






###################   *Social crying analyses*   ################### 

###################    Correlation analysis    ###################
#### Correlations between mood and number of people present

library("Hmisc")
#correlation function using *pearsons r*, 
#column no's are 24= mood_1, 17= number of people present
socialcor <- rcorr(as.matrix(data[,c(24, 17)]))

#ask for output:
socialcor$n ##number of observations
socialcor$r ##spearmans correlations
socialcor$P ##p-values

#################   Social crying by Country  ################# 
#Australia
socialcor.Aus <- rcorr(as.matrix(live.Aus[,c(24, 17)]))
#ask for output:
socialcor.Aus$n ##number of observations
socialcor.Aus$r ##spearmans correlations
socialcor.Aus$P ##p-values

#Croatia
socialcor.Croatia <- rcorr(as.matrix(live.Croatia[,c(24, 17)]))
#ask for output:
socialcor.Croatia$n ##number of observations
socialcor.Croatia$r ##spearmans correlations
socialcor.Croatia$P ##p-values

#Netherlands
socialcor.NL <- rcorr(as.matrix(live.NL[,c(24, 17)]))
#ask for output:
socialcor.NL$n ##number of observations
socialcor.NL$r ##spearmans correlations
socialcor.NL$P ##p-values

#Thai
socialcor.Thai <- rcorr(as.matrix(live.Thai[,c(24, 17)]))
#ask for output:
socialcor.Thai$n ##number of observations
socialcor.Thai$r ##spearmans correlations
socialcor.Thai$P ##p-values

#UK
socialcor.UK <- rcorr(as.matrix(live.UK[,c(24, 17)]))
#ask for output:
socialcor.UK$n ##number of observations
socialcor.UK$r ##spearmans correlations
socialcor.UK$P ##p-values


###################   t-tests between social prescence    ###################
pairwise <- pairwise.t.test(data$mood_1, data$Context, p.adj="none")
pairwise

#explore same as test by vingerhoets
data$numberhelp<- car::recode(data$Context, "1 = '0'; 2 = '1'; 3:5 = '2'") #0=alone, 1= 1 person, 2 = 2 or more
#Aus
live.Aus$numberhelp<- car::recode(live.Aus$Context, "1 = '0'; 2 = '1'; 3:5 = '2'") 
#Croatia
live.Croatia$numberhelp<- car::recode(live.Croatia$Context, "1 = '0'; 2 = '1'; 3:5 = '2'") 
#NL
live.NL$numberhelp<- car::recode(live.NL$Context, "1 = '0'; 2 = '1'; 3:5 = '2'") 
#Thai
live.Thai$numberhelp<- car::recode(live.Thai$Context, "1 = '0'; 2 = '1'; 3:5 = '2'") 
#UK
live.UK$numberhelp<- car::recode(live.UK$Context, "1 = '0'; 2 = '1'; 3:5 = '2'") 


pairwise2 <- pairwise.t.test(data$mood_1, data$numberhelp, p.adj="none")
pairwise2

##Explore by country
#Australia
pairwise.Aus <- pairwise.t.test(live.Aus$mood_1, live.Aus$numberhelp, p.adj="none")
pairwise.Aus
#Croatia
pairwise.Croatia <- pairwise.t.test(live.Croatia$mood_1, live.Croatia$numberhelp, p.adj="none")
pairwise.Croatia
#NL
pairwise.NL <- pairwise.t.test(live.NL$mood_1, live.NL$numberhelp, p.adj="none")
pairwise.NL
#Thai
pairwise.Thai <- pairwise.t.test(live.Thai$mood_1, live.Thai$numberhelp, p.adj="none")
pairwise.Thai
#UK
pairwise.UK <- pairwise.t.test(live.UK$mood_1, live.UK$numberhelp, p.adj="none")
pairwise.UK

####Recode variable into known persons when crying ("yes") verses unknown ("no")

##install packages for analyses
library("car")

##recode variables context2 and context 3
data$known <- car::recode(data$context3, "1 = 0; 2:4 = 1") # 0=no, 1=yes
data$helped <- car::recode(data$context2, "1 = 1; 3 = 0; 2 = ''") #recode were you helped - removing 'unsure' variable:0=no, 1=yes

#descriptives
library("plyr")
library("corrplot")
count(data$known)
count(data$helped)

##filter out NA responses from helped
install.packages("crunch")
library("crunch")
#filter data
library("dplyr")
data.help <- filter(data, helped == 1 | helped == 0)
pull(data.help) ##check filter

storage.mode(data.help$helped) <- "numeric"  ## change 'helped' to numeric from character



###################   Chi square test   ###################
chi <- chisq.test(data.help$known,data.help$helped)
chi
chi$observed
chi$expected
corrplot(chi$residuals, is.cor = FALSE)

##Exploratory chi squre on known helpers == acquaintance, family/friend
library("crunch")

#filter data
library("dplyr")
## apply filter
context3_filter2.4 <- filter(data.help, context3 == 2 | context3 == 4)
pull(context3_filter2.4) ##check filter

#### chi-square test using filtered data for acquantances and friends/family
chi_exp <- chisq.test(context3_filter2.4$context3, context3_filter2.4$helped)
chi_exp
chi_exp$observed
chi_exp$expected
corrplot(chi_exp$residuals, is.cor = FALSE)




###################   t-test to check if mood improved when help was recieved  ###################
library("jmv")
ttestIS(formula = mood_1 ~ known, data = data.help, effectSize = TRUE)
ttestIS(formula = mood_1 ~ helped, data = data.help, effectSize = TRUE)

#by contry
install.packages("afex")
library("afex")
anova_countryxhelp <-aov_ez(id = 'ResponseId', 
                    dv = 'mood_1', 
                    data = data.help, 
                    between = c('live', 'helped'), 
                    return = afex_options("return_aov"),
                    print.formula = TRUE)
nice(anova_countryxhelp) ##Results

library("emmeans")
##follow up pos-hoc contrasts for country (live) on emotion change (mood)
emmeans(anova_countryxhelp, "live", contr = "pairwise", adj = 'tuk')

#Visualize data
install.packages("jtools")
library("jtools")
plot1<- afex_plot(object = anova_countryxhelp, x = "helped", trace = "live", dodge = 0.3,
                  point_arg = list(size = 2.5),
                  data_alpha = 0,
                  mapping = c("shape", "color"),
                  factor_levels = list(helped = c("No help", "Helped"),
                                       live = c("Australia", "Croatia","Netherland", "Thailand", "UK")),
                  legend_title = "Country") + labs (
                    x = "Helped", ##X-axis label
                    y = "Mood")   ##y-axis label
plot1 + ggpubr::theme_pubr()


###################   correlation between mood following crying and BACS_social ###################
library("Hmisc")

##general correlation
corr.all <- rcorr(data$mood_1, data$BACS_social, type="pearson")
#ask for output:
corr.all$n ##number of observations
corr.all$r ##spearmans correlations
corr.all$P ##p-values

##Filter data for people who cried in presence of others
data.social <- filter(data, Context == "2" |Context == "3"| Context == "4"| Context == "5")

##Correaltion analysis for those exposed to others 
corr.social <- rcorr(data.social$mood_1, data.social$BACS_social, type="pearson")
#ask for output:
corr.social$n ##number of observations
corr.social$r ##spearmans correlations
corr.social$P ##p-values

##filter for people who cried alone
data.alone <- filter(data, Context == "1")

corr.alone <- rcorr(data.alone$mood_1, data.alone$BACS_social, type="pearson")
#ask for output:
corr.alone$n ##number of observations
corr.alone$r ##spearmans correlations
corr.alone$P ##p-values

##############   compare correlation coefficients  ################
### Do this using a Fishers r-to-z transformation
install.packages("cocor")
library("cocor")
cocor(~mood_1 + BACS_social | mood_1 + BACS_social, list(data.social, data.alone))





##  Check analysis by country

###   AUSTRALIA
##Correaltion analysis for those exposed to others 
liveAus.social <- filter(live.Aus, Context == "2" |Context == "3"| Context == "4"| Context == "5")
corr.Aussocial <- rcorr(liveAus.social$mood_1, liveAus.social $BACS_social, type="pearson")
#ask for output:
corr.Aussocial$n ##number of observations
corr.Aussocial$r ##spearmans correlations
corr.Aussocial$P ##p-values

liveAus.alone <- filter(live.Aus, Context == "1")
corr.Ausalone <- rcorr(liveAus.alone$mood_1, liveAus.alone $BACS_social, type="pearson")
#ask for output:
corr.Ausalone $n ##number of observations
corr.Ausalone $r ##spearmans correlations
corr.Ausalone $P ##p-values



###   CROATIA

##Correaltion analysis for those exposed to others 
liveCroatia.social <- filter(live.Croatia, Context == "2" |Context == "3"| Context == "4"| Context == "5")
corrCroatia.social <- rcorr(liveCroatia.social$mood_1, liveCroatia.social $BACS_social, type="pearson")
#ask for output:
corrCroatia.social$n ##number of observations
corrCroatia.social$r ##spearmans correlations
corrCroatia.social$P ##p-values

#Alone
liveCroatia.alone <- filter(live.Croatia, Context == "1")

corr.Croatiaalone <- rcorr(liveCroatia.alone$mood_1, liveCroatia.alone $BACS_social, type="pearson")
#ask for output:
corr.Croatiaalone $n ##number of observations
corr.Croatiaalone $r ##spearmans correlations
corr.Croatiaalone $P ##p-values



###   Netherlands

##Correaltion analysis for those exposed to others 
liveNL.social <- filter(live.NL, Context == "2" |Context == "3"| Context == "4"| Context == "5")
corr.NLsocial <- rcorr(liveNL.social$mood_1, liveNL.social $BACS_social, type="pearson")
#ask for output:
corr.NLsocial$n ##number of observations
corr.NLsocial$r ##spearmans correlations
corr.NLsocial$P ##p-values

##Alone
liveNL.alone <- filter(live.NL, Context == "1")

corr.NLalone <- rcorr(liveNL.alone$mood_1, liveNL.alone $BACS_social, type="pearson")
#ask for output:
corr.NLalone $n ##number of observations
corr.NLalone $r ##spearmans correlations
corr.NLalone $P ##p-values


###   Thailand

##Correaltion analysis for those exposed to others 
liveThai.social <- filter(live.Thai, Context == "2" |Context == "3"| Context == "4"| Context == "5")
corr.Thaisocial <- rcorr(liveThai.social$mood_1, liveThai.social $BACS_social, type="pearson")
#ask for output:
corr.Thaisocial$n ##number of observations
corr.Thaisocial$r ##spearmans correlations
corr.Thaisocial$P ##p-values

##Alone
liveThai.alone <- filter(live.Thai, Context == "1")

corr.Thaialone <- rcorr(liveThai.alone$mood_1, liveThai.alone $BACS_social, type="pearson")
#ask for output:
corr.Thaialone $n ##number of observations
corr.Thaialone $r ##spearmans correlations
corr.Thaialone $P ##p-values

##Fisher r-to-z transformation
cocor(~mood_1 + BACS_social | mood_1 + BACS_social, list(liveThai.social, liveThai.alone))


###   UK

##Correaltion analysis for those exposed to others 
liveUK.social <- filter(live.UK, Context == "2" |Context == "3"| Context == "4"| Context == "5")
corr.UKsocial <- rcorr(liveUK.social$mood_1, liveUK.social $BACS_social, type="pearson")
#ask for output:
corr.UKsocial$n ##number of observations
corr.UKsocial$r ##spearmans correlations
corr.UKsocial$P ##p-values

##Alone
liveUK.alone <- filter(live.UK, Context == "1")

corr.UKalone <- rcorr(liveUK.alone$mood_1, liveUK.alone $BACS_social, type="pearson")
#ask for output:
corr.UKalone $n ##number of observations
corr.UKalone $r ##spearmans correlations
corr.UKalone $P ##p-values




