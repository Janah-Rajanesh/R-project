#Changing directory
setwd('C:\\Users\\Rajani-janah\\Desktop\\Data Science\\R Studio\\R')
getwd()
# loading csv data to dataframe 
df<-read.csv("insurance.csv")
df[df=='']<-NA #converting Null to Na
Missing<-df[df=='NA']
Missing
##############################################################################################
#Getting familiar with data
##############################################################################################
#shape of data

colnames(df)
str(df)

#or
names(df)
# show classes of all columns
sapply

# Checking the head of the dataset
vsdata<-head(data.frame(df),10)


head(df)
head(df,10)

# Checking the tail of the dataset
tail(df)
tail(df,20)

#I saw that the last observations are Na so they are duplicated as well, I will drop them when I drop duplicated data 
#in data cleansing part.

# number of missing values:
colSums(is.na(df))

#Percentage of missing values:
round(colMeans(is.na(df))*100,2)#2:digit=2

#getting summary of data
summary(df)
# Always make a copy of your dataset after you manipulate NA and before you drop the duplicated observations
df_original<-df

##########################################################################################
#Duplicate Data
#########################################################################################
dup<-duplicated(df)# returns for you TRUE for observation that is duplicated otherwise returns FALSE
#Note: an observation considered duplicated if values of all features are exactly the same as another observation
dup


dim(df)

#How many duplicated data are there?
sum(duplicated((df)))

del<-which(duplicated(df))
df<-df[-del,]
del

head(df)
tail(df)
vsdata<-tail(data.frame(df),10)


dim(df)

# number of missing values:
colSums(is.na(df))
#Percentage of missing values:
round(colMeans(is.na(df))*100,2)#2:digit=2

tail(df)
# because all the features for last row are NA you can remove it
# df<-head(df,-1) In our features doesn't have a NA, we don't remote any observations.

#dropping features are only for identification and 
#we don't have any knowledge to extract meaningful features from them
#sometimes based in business knowledge you can extract date , address or postal code , branch number or etcetra from ID
# df[,c("Loan.ID","Customer.ID")]<-NULL

colnames(df)

# What is the target? it depends on business problem
# An insurance company wants to test the hypothesis
# that the mean charges of life insurance held by men smoker equals that held 
#by smoker women.

# Ho = the mean charges of life insurance held by men smoker equals that 
#held by smoker women
# Ha = the mean charges of life insurance held by men smoker is different that 
#held by smoker women

# Q1.what is the distribution of target(sex)?

# how many missing values we have for sex
sum(is.na(df$sex))

#we don't have a missing value in our target.
#If we have some  missing value for target, and we want to drop that observation.
#r1<-which(is.na(df$sex))
#r1
#df<-df[-r1,]


#--------------------Univariate analysis ----------------------------------------
# for Categorical variables : for summarization : table of frequency or percentage
#                             for visualization : pie chart or bar chart,...

#since target is categorical variable, in univariate Analysis for summarization
#I will find frequency and for visualization I plot: pie chart or barchart 
vsdata<-data.frame(table(df$sex))  #table function returns frequency of each level
names(vsdata)[1] <- "Sex"
names(vsdata)[2] <- "Freq"


levels(as.factor(df$sex)) #returns unique values


table(df$region)
levels(as.factor(df$region)) #returns unique values
table(df$smoker)
levels(as.factor(df$smoker)) #returns unique values


tbl<-table(df$sex)
tbl

# Pie Chart with Percentages
freq1<-table(df$sex)
freq1
lbls <- c("Female", "Male")
pct <- round(freq1/sum(freq1)*100,2)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(freq1,labels = lbls, col=c("red","yellow"),
    main="Pie Chart of Sex")

# Simple Bar Plot
counts <- table(df$sex)
counts
barplot(counts, main="sex",
        ylab="Number",
        labels = lbls,
        col=c("red","yellow"),horiz = FALSE)



#you can repeat this process for all categorical columns for example smoker
#----------------------------------------------------------------------------

# Q2.what is the distribution of Smoker?
#how many missing values we have for Smoker
sum(is.na(df$smoker))

#we don't have any missing value for smoker
#since Term is categorical variable, in univaraite Analysis for summarization
# I will find frequency and for visualization I plot: pie chart or barchart 

tbl<-table(df$smoker)
tbl

# Pie Chart with Percentages
vsdata<-data.frame(table(df$age))  #table function returns frequency of each level
names(vsdata)[1] <- "Smoker"
names(vsdata)[2] <- "Number"

freq1<-data.frame(df$smoker)
freq1

lbls <- c("NO", "YES")
pct <- round(freq1/sum(freq1)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(freq1,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Smoker")

# Simple Bar Plot
barplot(freq1, main="Smoker",
        ylab="Number",col=c("red","yellow"),horiz = FALSE)


#-----------------------------------------------------------
#Q3.what is the distribution of Region?
#how many missing values we have for Region

sum(is.na(df$region))

#we don't have  any missing value for Region
#since Term is categorical variable, in univaraite Analysis for summarization
# I will find frequency and for visualization I plot: pie chart or barchart 

tbl<-table(df$region)
tbl


vsdata<-data.frame(table(df$region))  #table function returns frequency of each level
names(vsdata)[1] <- "Region"
names(vsdata)[2] <- "numbers"



# Pie Chart with Percentages
freq1<-table(df$region)
freq1

lbls <- c("northeast","northwest","southeast","southwest")
pct <- round(freq1/sum(freq1)*100,2)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(freq1,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Region")

# Simple Bar Plot
barplot(freq1, main="Bar Chart by Region",
        ylab="Number", ylim=c(0,400), col=rainbow(length(lbls)),density=c(5,10,20,30),border="#69b3a2", angle=c(0,45,90,150), horiz = FALSE)


#------------------------ Here Star Numerical  Variables - Univariate Analysis (Age, BMI, Children, Charges)
# for Numerical variables   : for summarization : Central tendency(mean, median,mode), 
#                                                 five-number-summary(min, Q1, median, Q2,max),
#                                                 measure of variability (Range,standard deviation, variance,
#                                                 cv(coefficient of variation=standard deviation divided by mean),
#                             for visualization : histogram, density, box plot,...


#Univariate analysis
#for numeric columns returns Five-Number_summary and 
#mean  (min, max, mean, median, 1st & 3rd quartiles) and NAs

str(df)
vsdata<-data.frame(summary(df[,-c(2,5,6)],na.rm = T))               # No include Categorical Variable index 2 = Sex, 5=Smoker, 6=Region
sapply(df[,-c(2,5,6)],fivenum,na.rm=T) # Five Number (min, max, mean, median, 1st & 3rd quartile)
sapply(df[,-c(2,5,6)],sd,na.rm=T)      # Standard Deviation 
sapply(df[,-c(2,5,6)],var,na.rm=T)     # Variance 
sapply(df[,-c(2,5,6)],mean,na.rm=T)    # Mean
sapply(df[,-c(2,5,6)],median,na.rm=T)  # Median
sapply(df[,-c(2,5,6)],min,na.rm=T)     # Min
sapply(df[,-c(2,5,6)],range,na.rm=T)   # Range
sapply(df[,-c(2,5,6)],sum,na.rm=T)     # Sum
cv<-(sapply(df[,-c(2,5,6)],sd,na.rm=T)/sapply(df[,-c(2,5,6)],mean,na.rm=T)*100) # Coeficiente Of variance
cv

# Function to find mode of the Data Set for each numeric variable
getm <- function(x) {
  val <- unique(x)
  val[which.max(tabulate(match(x, val)))]
}

modeAge<-getm(df[,c(1)]) # Mode for Age
modeAge

modeBMI<-getm(df[,c(3)]) # Mode for BMI
modeBMI

modeChildren<-getm(df[,c(4)]) # Mode for Children
modeChildren

modeCharges<-getm(df[,c(7)]) # Mode for Charges for Insurance Life
modeCharges

# for visualization : histogram, density, box plot,...
sapply(df[,-c(2,5,6)],range,na.rm=T)   
hist(df$age,breaks=5, main="Histogram of Age",
     xlab="Age",
     col=rainbow(5))  

tbl<-aggregate(df$age,list(df$age),sum)

################



algo<-aggregate(~age, data = df, FUN = sum)
#histogram
sapply(df[,-c(2,5,6)],range,na.rm=T\) 
h<-hist(df$children,breaks=c(0,1,2,3,4,5), 
        main="Histogram by number of Children",
        xlab="Number of Children", col=rainbow(5),
        border="#69b3a2")
xfit<-seq(min(df$children),max(df$children),length=50) 
yfit<-dnorm(xfit,mean=mean(df$children),sd=sd(df$children)) 
yfit<-yfit*diff(h$mids[1:2])*length(df$children) 
lines(xfit, yfit, col="blue", lwd=3)
####boxplot by BMI
sapply(df[,-c(2,5,6)],range,na.rm=T)   
boxplot(df$bmi,data=df,
        main="Box Plot by BMI",
        xlab="Body Mass Index",
        col="blue") 
#
sapply(df[,-c(2,5,6)],range,na.rm=T)
sapply(df[,-c(2,5,6)],sd,na.rm=T)
#boxplot by charges
boxplot(df$charges,data=df,
        main="Box Plot by Charges",
        xlab="Charges",
        col="Yellow") 

#kernel density of charges
charges <- density(df$charges) # returns the density data
plot(charges, main="Kernel Density of Charges")
polygon(charges, col="red", border="blue") 


#Visualization for region, smoker vs. sex - 2 way Table

vsdata<-data.frame(table(df$region))  #table function returns frequency of each level
names(vsdata)[1] <- "Region"
names(vsdata)[2] <- "numbers"



vsdata<-data.frame(table(df$region, df$sex))
names(vsdata)[1] <- "Region"
names(vsdata)[2] <- "sex"

tbl_reg_sex<-table(df$region, df$sex)
tbl_reg_sex



vsdata<-data.frame(table(df$smoker, df$sex))
names(vsdata)[1] <- "smoker"
names(vsdata)[2] <- "sex"



tbl_smo_sex<-table(df$smoker, df$sex)
tbl_smo_sex


vsdata<-data.frame(table(df$region, df$smoker))
names(vsdata)[1] <- "region"
names(vsdata)[2] <- "smoker"

tbl_reg_smo<-table(df$region, df$smoker)
tbl_reg_smo

################ Visualization: Stacked bar chart, Group bar chart
# Categorical Vs. Categorical
# Stacked Bar Plot with Colors and Legend:# you need 2 rows
# default beside=False ==> Stacked Bar Plot
# beside=TRUE ==> Grouped Bar Plot 

par(mfrow=c(2, 2), mar=c(5, 5, 4, 8))
counts<-table(df$sex, df$region) #you need 2 rows
counts
barplot(counts, main="Sex Distribution by Region",
        ylab="Number Region", col=c("Yellow","Blue"),
        legend.text = TRUE,
        beside=T,
        args.legend=list(
          x=ncol(counts)+12,
          y=100, 
          bty = "n"
        ))


counts <- table(df$sex, df$smoker) #you need 2 rows
counts
barplot(counts, main="Sex Distribution by Smoker",
        ylab="Number Smoker", col=c("orange","green"),
        legend.text = TRUE,
        beside=F,
        args.legend=list(
          bty = "n"
        ))

counts <- table(df$smoker, df$region)
barplot(counts, main="Smoker Distribution by Region",
        ylab="Number Region", col=c("darkblue","red"),
        legend.text = TRUE,
        beside=T,
        args.legend=list(
          x=15,
          y=130,
          bty = "n"
        )) 


##########################################################################################
#Chi-squared Test of Independence(Chi-squared test )
##########################################################################################
#Chi-squared Test of Independence(Pearson's Chi-squared)
##########################################################################################

#The chi-square test of independence is used to analyze the frequency 
#table (i.e. contengency table) formed by two categorical variables. 
#The chi-square test evaluates whether there is a significant association between the
#categories of the two variables.In other word Chi-Square test is a statistical
#method which used to determine if two categorical variables have a significant 
#correlation between them.

#By running Chi-square test you will get p-value, if p-value < 0.05 you can say there
#is association between those two categorical columns at 5% significant level(Be care full 
#that you should check assumption of chi-square test)

# Assumption of chi-square test:
# The Chi-square test statistic can be used if the following conditions are satisfied:
#1. N, the total frequency, should be reasonably large, say greater than 50.
#2. The sample observations should be independent. This implies that no individual item should 
#   be included twice or more in the sample.
#3. No expected frequencies should be small. Small is a relative term. Preferably each expected 
#   frequencies should be larger than 10 but in any case not less than 5.

library(vcd)
library(MASS) # load the MASS package

# Problem:
# Test the hypothesis whether region is independent of the sex at .05 significance level.

# We apply the chisq.test function to the contingency table tbl, and find the p-value 
# the contingency table

tbl_reg_sex<-table(df$region, df$sex)
tbl_reg_sex
addmargins(xtabs(~ region+sex,data=df))
prop.table(xtabs(~ region+sex,data=df))

# Solution
# Null hypothesis region is independent of the sex
# if condition of chi-square are satisfied and p-value is greater than significant level(5% here) 
# we will not reject null hypotheses and get this conclusion that there not is relation ship between 
# them at 5% significant level

# Since all assumption are met and p-value is less than 5% we can say that:
# so the Loan.Status and Term are statistically significantly associated (p-value = 0)

# or Mosaic plots provide a way to visualize contingency tables.A mosaic plot is a visual representation of the association between two variables.
mosaicplot(tbl_reg_sex, 
           shade=TRUE,
           main = "Sex Mosaic Plot",
           sub = "Sex by Regions",
           xlab = "Regions",
           ylab = "Sex",
           border = "blue")

#or
#Association Plots
assoc(tbl_reg_sex, shade=TRUE)

chisq.test(tbl_reg_sex)

################## Result 
# Pearson's Chi-squared test
# data:  tbl_reg_sex
# X-squared = 0.47691, df = 3, p-value = 0.9239
# Critical Value = 7.815

#Answer:
# The chi-square statistic is 0.4769. The p-value is 0.923934. 
# The result is not significant at p<.05.
# If X-squared = 0.47691 < Critical Value 7.815 Do not reject Ho Therefore, Region and Sex are 
# independent variable 

#########################################
# Problem:
# 
# Test the hypothesis whether smoker is independent of the sex at .05 significance level.

# We apply the chisq.test function to the contingency table tbl, and find the p-value 
# the contingency table

tbl_smo_sex<-table(df$smoker, df$sex)
tbl_smo_sex
addmargins(xtabs(~ smoker+sex,data=df))
prop.table(xtabs(~ smoker+sex,data=df))
chisq.test(tbl_smo_sex)





# Solution
# Null hypothesis region is independent of the sex
# if condition of chi-square are satisfied and p-value is greater than significant level(5% here) 
# we will not reject null hypotheses and get this conclusion that there not is relation ship between 
# them at 5% significant level

# We apply the chisq.test function to the contingency table tbl, and find the p-value 
# the contingency table

# or Mosaic plots provide a way to visualize contingency tables.A mosaic plot is a visual
# representation of the association between two variables.

mosaicplot(tbl_smo_sex, 
           shade=TRUE,
           main = "Sex Mosaic Plot",
           sub = "Sex by Smoker",
           xlab = "smoker",
           ylab = "Sex",
           las = 0,
           border = "Skyblue")

#or
#Association Plots
assoc(tbl_smo_sex, shade=TRUE)
chisq.test(tbl_smo_sex)

# Solution
# Null hypotheses smoker variable is depend of the sex

# if condition of chi-square are satisfied and p-value is less than significant level(5% here) 
# we will reject null hypotheses and get this conclusion that there is relation ship between 
# them at 5% significant level


#Pearson's Chi-squared test with Yates' continuity correction
#data:  tbl_smo_sex
#X-squared = 7.4691, df = 1, p-value = 0.006277

#Answer:
# The chi-square statistic is 7.3929 The p-value is .006548 The result is significant at p<.05.
#Since all assumption are met and p-value is less than 5% we can say that:
#so the sex and smoker are statistically significantly associated (p-value = 0)




#############################################
# Problem:
# Test the hypothesis whether region is independent of the smoker at .05 significance level.

tbl_reg_smo<-table(df$region, df$smoker)
tbl_reg_smo
addmargins(xtabs(~ region+smoker,data=df))
prop.table(xtabs(~ region+smoker,data=df))


# Solution
# Null hypothesis region is independent of the smoker
# if condition of chi-square are satisfied and p-value is less than significant level(5% here) 
# 
# we will not reject null hypotheses and get this conclusion that there is not relation ship between 
# them at 5% significant level

# We apply the chisq.test function to the contingency table tbl, and find the p-value 
# the contingency table

mosaicplot(tbl_reg_smo, 
           shade=TRUE,
           legend=TRUE,
           main = "Sex Mosaic Plot",
           sub = "Sex by Region",
           xlab = "Region",
           ylab = "Sex",
           las = 1,
           border = "blue")

#or
#Association Plots
assoc(tbl_reg_smo, shade=TRUE)


chisq.test(tbl_reg_smo)

#Pearson's Chi-squared test with Yates' continuity correction

#Pearson's Chi-squared test
#data:  tbl_reg_smo
# X-squared = 7.4691, df = 3, p-value = 0.06355

#Answer:
# The chi-square statistic is 7.4691. The p-value is .06355 The result is not significant at p<.05.
# If X-squared = 7.4691 < Critical Value 7.815 Do not reject Ho Therefore, Region and Smoker are 
# independent variable 



###################################################################################
#Continuous Vs. Continuous   : For Visualization scatter plot,...
#                              For test of independence: pearson correlation or spearman or Kendall
##########################################################################################

par(mfrow=c(2, 2), mar=c(5, 5, 4, 8))
plot(x = df$age, y = df$bmi,
     pch = 19, 
     main = "Scatter Plot Age Vrs BMI",
     xlab = "Age", ylab = "Bmi", col = "Blue")

plot(x = df$age, y = df$charges,
     pch = 20, 
     main = "Scatter Plot Age Vrs Charges",
     xlab = "Age", ylab = "Charges", col = "Green")

plot(x = df$bmi, y = df$charges,
     pch = 21, 
     main = "Scatter Plot BMI Vrs Charges",
     xlab = "Bmi", ylab = "Charges", col = "Red")


# As you know children is discrete variable and has limited options.
# If you want to treat children like categorical variables you can convert it to factor

qplot(age, bmi, data = df,
      color = factor(children),  # Change the color by a discrete numeric variable
      main = "QPlot Age Vrs BMI a Children as Factor",
      geom=c("point", "smooth"))


# Q6. is these data normal distributed?
#install.packages("ggpubr")

library("dplyr")
library("ggpubr")

set.seed(1234)
dplyr::sample_n(df, 30)

library(ggpubr)
ggqqplot(df$age)


library("ggpubr")
ggdensity(df$bmi, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")

shapiro.test(df$bmi)

#Shapiro-Wilk normality test

#data:  df$bmi
#W = 0.99388, p-value = 2.575e-05

# From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different
# from normal distribution. In other words, we can assume the normality.


# is there a statistically significant relationship between age, as BMI, and Charges?
#------------------------------------------------------------
#correlation
#------------------------------------------------------------
#Methods for correlation analyses
#There are different methods to perform correlation analysis:

# Pearson correlation (r), which measures a linear dependence between two 
# variables (x and y). It is also known as a parametric correlation test because 
# it depends to the distribution of the data. 
# It can be used only when x and y are 
# from normal distribution. The plot of y = f(x) is named the linear regression 
# curve.
# be noticed that Pearson correlation shows only strength of linear relation 
# Kendall tau and Spearman rho, which are rank-based correlation 
# coefficients (non-parametric)

str(df)
cor(na.omit(df)) #Error: all variables must be numeric 
# and you must remove NAs
cor(na.omit(df[,-c(2,5,6)]))

# here iris doesn't have NA but in other situation you should remove NAs
df_cor<-na.omit(df)
cor(df$age,df$bmi)      #default method = "pearson"
cor(df$age,df$charges)  #default method = "pearson"


cor(df$age,df$bmi,      method = "spearman")  #default method = "pearson"
cor(df$age,df$charges,  method = "spearman")  #default method = "pearson"

cor(df$age,df$bmi,      method = "kendall")  
cor(df$age,df$charges,  method = "kendall")  


str(df)
panel.cor <- function(x, y, cex.cor = 1.3, method = "pearson", ...) {
  options(warn = -1)                   # Turn of warnings (e.g. tied ranks)
  usr <- par("usr"); on.exit(par(usr)) # Saves current "usr" and resets on exit
  par(usr = c(0, 1, 0, 1))             # Set plot size to 1 x 1
  r <- cor(x, y, method = method, use = "pair")               # correlation coef
  p <- cor.test(x, y, method = method)$p.val                  # p-value
  n <- sum(complete.cases(x, y))                              # How many data pairs
  txt <- format(r, digits = 3)                                # Format r-value
  txt1 <- format(p, digits = 3)                                 # Format p-value
  txt2 <- paste0("r= ", txt, '\n', "p_val= ", txt1, '\n', 'n= ', n) # Make panel text
  text(0.5, 0.5, txt2, cex = cex.cor, ...)                      # Place panel text
  options(warn = 0)                                             # Reset warning
}
## END

pairs(df[,-c(2,3,4,5,6)], lower.panel = panel.cor)#iris[,1:4]=iris[,-5]


#Continuous Vs. Categorical  : For sumarization : group by categorical column then
#                              aggregate for numerical column
#                              For visualization: Grouped box plot,Grouped histogram
#                              For test of independence :


library(doBy)
summaryBy(bmi~ sex, df,FUN= c(mean,sd,var,min,max),na.rm=T)
summaryBy(age~ sex, df,FUN= c(mean,sd,var,min,max),na.rm=T)
summaryBy(charges~ sex, df,FUN= c(mean,sd,var,min,max),na.rm=T)
summaryBy(charges~ sex+smoker, df,FUN= c(mean,sd,var,min,max),na.rm=T)
summaryBy(charges~ sex+smoker+region, df,FUN= c(mean,sd,var,min,max),na.rm=T)
summaryBy(children~ sex+smoker, df,FUN= c(mean,sd,var,min,max),na.rm=T)

#visualization
library(ggplot2)
#Group box plot with qplot( I will do that with ggplot as well)
qplot(sex, bmi, data = df, 
      geom="boxplot", fill = sex)

qplot(sex, charges, data = df, 
      geom="boxplot", fill = sex)


#group histogram
# Changing histogram plot fill colors by Loan.Status and usinging semi-transparent fill
p<-ggplot(df, aes(x=bmi, fill=sex, color=sex)) +
  geom_histogram(position="identity", alpha=0.5)
p

v<-ggplot(df, aes(x=charges, fill=sex, color=sex)) +
  geom_histogram(position="identity", alpha=0.5)
v


#The position of the bins is controlled with position argument, Alpha refers to the opacity of a geom. Values of alpha range from 0 to 1, with lower values corresponding to more transparent colors.
#https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html

# Add mean lines
library(plyr)
mu<-ddply(df,"sex", summarise, group.mean=mean(charges,na.rm=T))
mu
v<-v+geom_vline(data=mu, aes(xintercept=group.mean, color=sex),
                linetype="dashed")
v

#Add density
p<-ggplot(df, aes(x=charges, fill=sex, color=sex)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.5)+
  geom_density(alpha=0.1)

p

# Add mean lines and Change the legend position
p+geom_vline(data=mu, aes(xintercept=group.mean, color=sex),
             linetype="dashed")+ theme(legend.position="top")+labs(title="Charges histogram plot",x="Charges", y = "Density")

#Group box plot
p<-ggplot(df, aes(x=sex, y=charges, fill=sex)) +
  geom_boxplot()
p

# Box plot with mean points
p<-p + stat_summary(fun.y=mean, geom="point", shape=23, size=4)
p

#Group box plot
p<-ggplot(df, aes(x=sex, y=charges, fill=sex)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=1)
p

# Box plot with mean points
p<-p + stat_summary(fun.y=mean, geom="point", shape=23, size=4)
p

#test of independence
#-------------------------------------------
#t-test: 
#-------------------------------------------

#One of the most common tests in statistics is the t-test, used to determine whether the means of 
#two groups are equal to each other. It is useful to do Bivariate analysis for Continuous Vs. Categorical
#when you Categorical column has only two levels(i.e we have only two group)
#The assumption for t-test is that both groups are sampled from normal distributions with equal 
#variances

#The null hypothesis is that the two means are equal, and the alternative is that they are not.
#There is also a widely used modification of the t-test, known as Welch's t-test that adjusts the 
#number of degrees of freedom when the variances are thought not to be equal to each other.
#By default, R assumes that the variances of two groups are unequal, thus defaulting to Welch's test.
#You can change it by using the flag var.equal=TRUE.

# We are ready to test statistically whether these two samples have a different mean using the T-Test.
# To do so first, we have to define our Null and Alternate Hypothesis:
# Good:Charges, Bad: Charged Off
# Null Hypothesis: µMale = µFemeale (the means of both populations are equal)
# Alternate Hypothesis: µMan is not equal to µBad (the means of both populations are not equal)
t.test(charges ~ sex, data=df ) 
# default paired = FALSE, var.equal = FALSE
# But Before the results should be interpreted, the assumptions of the test should be checked.
# Assumption of independent sample t-test:
# 1.the two samples are independent(here this assumption is true because each costumer belongs 
# to only one group Charged Off or Fully Paid)
# 2.no significant outliers in the two groups
# 3.the two groups of samples (A and B), being compared, should be normally distributed.
# (Rely on the Central Limit Theorem if the sample size is large enough (sample size:n>30))
# 4.the variances of the two groups should not be significantly different. This assumption is made 
# only by the original Student's t-test. It is relaxed in the Welch's t-test.

#So because p-value<2.2 e-16=0.00000000000000022<0.05 , we reject null hypotheses and 
# get this conclusion that there is a statistically significant difference between mean of man and 
# mean of female groups at 5% significant level

#https://statisticsbyjim.com/hypothesis-testing/t-tests-1-sample-2-sample-paired-t-tests/
#https://statisticsbyjim.com/anova/f-tests-anova/

names(df)
# Multivaraite Analysis : Charges and bmi for each group of combination of 
# sex and age
summaryBy(cbind(charges,bmi) ~ Sex+age, df,FUN= c(mean,sd,var,min,max),na.rm=T)