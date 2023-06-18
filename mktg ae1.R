
#Importing dataset
ridesharedata<-read.csv(file.choose())

#Importing packages
install.packages("pacman")
pacman::p_load(ggplot2, dplyr, reader, caret, psych, MASS, descr, boot)

# count of the dependent variable
summary(ridesharedata)
str(ridesharedata, give.attr = FALSE)
ggplot(ridesharedata, aes(x = usedrideshare)) + geom_histogram(stat = "count")

#encoding the dependent variable to factor
ridesharedata$usedrideshare <- factor(ridesharedata$usedrideshare,levels = c("No","Yes"),
                                    labels = c(0,1))

#changing the categorical labels to factors
ridesharedata$workingdaysperweek <- as.factor(ridesharedata$workingdaysperweek)
ridesharedata$gender <- factor(ridesharedata$gender,levels = c("Female","Male"),
                             labels = c(0,1))
ridesharedata$itwork <- as.factor(ridesharedata$itwork)
ridesharedata$workexp <- as.factor(ridesharedata$workexp)

#Building a full logistic regression model with all available predictors
logitModelFull <- glm(usedrideshare ~ workingdaysperweek+distanceinkmsforcommute+gender+salary+itwork+workexp, family = binomial,
                      data = ridesharedata)
#checking the pseudo R-Squared
LogRegR2(logitModelFull)

#verifying the model is still available
summary(logitModelFull)

#examining the coefficients correctly using the odds ratio
coefsexp <- coef(logitModelFull) %>% exp() %>% round(2)
coefsexp


logitModelNew <- stepAIC(logitModelFull,trace = 0)
summary(logitModelNew)

formulaLogit <- as.formula(summary(logitModelNew)$call)
formulaLogit

# Step 4 – Examine the odds ratios to check for changes
coefsexp2 <- coef(logitModelNew) %>% exp() %>% round(2)
coefsexp2