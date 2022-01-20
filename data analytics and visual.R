library(plyr)
library(ggplot2)
library(corrplot)
library(outliers)
library(pastecs)
library(boot)
library(dplyr)
library(sp)
library(Hmisc)
library(ggthemes)
library(RColorBrewer)
library(Metrics)

theme_wsj(
  base_size = 12,
  color = "brown",
  base_family = "sans",
  title_family = "mono"
)

#setting the working directory
setwd('C:/Users/thila/Desktop/Dataand visual analytics')

#checking the working directory
getwd()

#Importing the data frame
spendalot=read.csv("k2124313.csv")

#checking the head of the data set
head(spendalot)

#checking the tail of the data set
tail(spendalot)

#To check whether there are any null/missing values in the data set.
is.null(spendalot)
####There aren't any missing values in the data set

#to check the data types the data is in
str(spendalot)

#to check whether there are any duplicated observations
spendalot[duplicated(spendalot)]
#no duplicated rows are found

#Getting a frequency table for pre covid and post covid counts
count(spendalot, 'covid')
##8000 each

#Getting a frequency table for pre covid and post covid counts
count(spendalot, 'deprivation')
##8000 each

#Getting a frequency table for pre covid and post covid counts
count(spendalot, 'logspend')
##8000 each

#getting statistics summary
spendalot_covid=spendalot %>% filter(covid==0)
spendalot_covid
summary(spendalot_covid)
stat.desc(spendalot_covid)

spendalot_lockd=spendalot %>% filter(covid==1)
spendalot_lockd
summary(spendalot_lockd)
stat.desc(spendalot_lockd)
#getting a histogram and density plots to check distribution
attach(spendalot)

#plotting histogram and density plot for deprivation
plot_deprivation=ggplot(spendalot, aes(x = deprivation)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")
plot_deprivation

#checking for further normality
qqnorm(deprivation)

#plotting histogram and density plot for spend
plot_spend=ggplot(spendalot, aes(x = spend)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")
plot_spend

#checking for further normality
qqnorm(spend)


#plotting histogram and density plot for logspend
plot_logspend=ggplot(spendalot, aes(x = logspend)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")
plot_logspend

#checking for further normality
qqnorm(logspend)

###log transformation is normal

#to further detect any outliers(upper boundary)
test_logspend_upper=grubbs.test(logspend)
test_logspend_upper

test_spend_upper=grubbs.test(spend)
test_spend_upper


test_deprevation_upper=grubbs.test(deprivation)
test_deprevation_upper


#to further detect any outliers(lower boundary)
test_logspend_lower=grubbs.test(logspend,opposite=TRUE)
test_logspend_lower

test_logspend_lower=grubbs.test(spend,opposite=TRUE)
test_logspend_lower

test_logspend_lower=grubbs.test(deprivation,opposite=TRUE)
test_logspend_lower
##The tests show that spend 51.11 is an outlier but since we are usiing the log value we are achiving normality plus 
##we are dealing with outliers as well


#getting lof value of spend for better normality and to check whether its the same as logspend
spendalot_copy=transform(spendalot, logspend_dummy=log(spend))


#plotting correlogram to find out the colinear variables
corr_spendalot=cor(spendalot)
corrplot(corr_spendalot,method='number')
COL1(sequential = c("Oranges", "Purples", "Reds", "Blues", "Greens", 
                    "Greys", "OrRd", "YlOrRd", "YlOrBr", "YlGn"), n = 200)
corrplot(corr_spendalot, method='number',col = COL2('RdBu', 10))
##The corelogram prooved that its indeed logspend is simply log of spend.
##For better normality using only logspend for further predictive modeling.



#___T-Test to check significance difference in precovid and lockdown spending patter.
test_results=t.test(spend ~ covid)
test_results

test_results=t.test(logspend ~ covid)
test_results


test_results=t.test(spendalot$spend ~ spendalot$covid,var.equal=TRUE)
test_results

test_results=t.test(logspend ~ covid,var.equal=TRUE)
test_results

#Processing the data to make a visualization of the change

spenalot_covid_chart=spendalot_copy[order(covid),]
spenalot_covid_chart$index <- 1:nrow(spenalot_covid_chart)
spenalot_covid_chart$group=as.numeric(cut(spenalot_covid_chart$index,160))
spenalot_covid_chart

spend_m=spenalot_covid_chart %>% group_by(group) %>% summarise(spending_mean= mean(spend),
                                    covid_status = max(covid))
spend_m
spend_m$status <- ifelse(spend_m$covid_status %in% 0, "Precovid", "Lockdown")
# spend_m$covid <- ifelse(spend_m$covid_status %in% 0, "Precovid", "Lockdown")

ggplot(spend_m, aes(x = group, y =spending_mean  , colour = status, group = 1)) +
  geom_line(size=1.3) +
  geom_smooth(span = 0.2)+
  annotate(geom = "text", x = 40+80* (0:1), y = 10, label = unique(spend_m$status), size = 6) +
  geom_vline(aes(xintercept=81), linetype="dashed", size=1)+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+
  scale_x_discrete(expand=c(0.01, 0.01))+ ggtitle("Spending Pattern Difference")+
  scale_colour_wsj("colors6", "") + theme_wsj()+
 xlab("PRE/POST COVID SEPERATION")+
  ylab("AVERAGE SPEND")+
  theme(axis.title=element_text(size=14))



#We did two tests to find a significance difference in spend during pre-covid and lock down.Welch t test and two sample test.
#Both the results showed that theres a significant change in the spending amounts in those two periods with 95% confidence


#Getting average values for covid deprivation groups
spend_mean_data=spendalot %>% group_by(covid,deprivation) %>% summarise(spending_mean= mean(spend),
                                                             log_spend_mean=mean(logspend),
                                                               covid_status = max(covid))
tail(spend_mean_data)

#checking the linearity of the new data
ggplot(spend_mean_data, aes(x = deprivation, y = spending_mean,colour = covid, group = 1)) +
  geom_point()+
  ggtitle("LOCK DOWN VS PRE COVID MODEL PREDICTION")+
  scale_colour_wsj("colors6", "") + 
  theme_wsj()+
  xlab("DEPRIVIATION")+
  ylab("AVERAGE SPENDING")+
  facet_wrap(~covid)+  theme(axis.title=element_text(size=14))


#changing the numeric variable into categorical(covid)
spend_mean_data$covid=ifelse(spend_mean_data$covid_status == 0, "pre covid", "lockdown")
spend_mean_data

#Training the model
model_linear=lm(log_spend_mean ~ covid + deprivation, data = spend_mean_data)
model_linear
summary(model_linear)

#Calculating the uncertainty of average spend
predictions <- model_linear %>% predict(spend_mean_data)
predictions
exp(predictions)
rmse(exp(predictions), spend_mean_data$spending_mean)

#getting model fit evaluations
AIC(model_linear)
BIC(model_linear)

#For graph purpose getting confidence and predticion intervals and converting them to against average spend
prediction=predict(model_linear, spend_mean_data, interval="confidence") 
prediction
prediction_2=predict(model_linear, spend_mean_data, interval="prediction")
prediction_2

prediction=transform(prediction, new_fit_conf=exp(fit))
prediction=transform(prediction, new_upper_conf=exp(upr))
prediction=transform(prediction, new_lower_conf=exp(lwr))
prediction
prediction_2=transform(prediction_2, new_fit_pred=exp(fit))
prediction_2=transform(prediction_2, new_upper_pred=exp(upr))
prediction_2=transform(prediction_2, new_lower_pred=exp(lwr))
prediction_2

datlm_1 = cbind(spend_mean_data, prediction)
datlm_1
datlm_2= cbind(datlm_1, prediction_2)
datlm_2


#Plotting the regression model fit lines
ggplot(data=datlm_2, aes(x=deprivation, y=spending_mean,color=covid, group = 1))+
  geom_point()+
  geom_line( aes(y=new_fit_conf),size = 1)+
  geom_ribbon(aes(ymin=new_upper_conf, ymax=new_lower_conf),alpha=0.15)+
  geom_ribbon(aes(ymin=new_upper_pred, ymax=new_lower_pred),alpha=0.15,linetype = "dashed")+
  ggtitle("LOCK DOWN VS PRE COVID MODEL PREDICTION")+
  scale_colour_wsj("colors6", "") + 
  theme_wsj()+
  xlab("DEPRIVIATION")+
  ylab("AVERAGE SPENDING")+
  facet_wrap(~covid)+  theme(axis.title=element_text(size=14))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)


#FOr bootstrap model checking purposes
model_coef <- function(spendalot, index){
  coef(lm(logspend ~ covid + deprivation, data = spendalot, subset = index))
}

model_boot=boot(spendalot,model_coef,100)
model_boot

n <- length(logspend)
# Extract fitted coefficients from model object
b0 <- model_linear$coefficients[1]
b1 <- model_linear$coefficients[2]
b2 <- model_linear$coefficients[3]

# Find SSE and MSE
sse <- sum((logspend - model_linear$fitted.values)^2)
mse <- sse / (n - 2)
sse
mse
