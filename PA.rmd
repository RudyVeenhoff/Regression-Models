---
title: "Does Transmission effect MPG?"
author: "Rudy Veenhoff"
date: "20 oktober 2016"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Executive Summary
Is this report we investigate whether transmission type (automatic or manual) has any influence in the number of miles a car can drive per gallon using the data set mtcars. We find that MPG can be sufficiently predicted by the weight of the car and the number of cylinders it has. In view of this, transmission type does not seem to significantly impact MPG.

## Exploratory analysis
```{r head}
head(mtcars)
```

From the help file we learn that the 'am' column stands for transmission (0 = automatic, 1 = manual).
```{r ggplot}
library(ggplot2)
mtcars$am <- as.factor(mtcars$am) ## just to get a nicer plot
g <- ggplot(data=mtcars,aes(x=am,y=mpg))
g <- g + geom_boxplot(fill=c("royalblue1","springgreen"))
g <- g + xlab("Transmisson") +ylab("Miles per Gallon")
g
```

Cars with an automatic transmission seem to have lower mpg compared to cars with manual transmission when viewed in a vacuum. However, this might be too simple an explanation. There might be other variables confounding the relation between mpg and transmission. A variable that might confound might be the weight. It seems reasonable to assume that heavier cars have a higher fuel consumption, hence they travel less miles per gallon.

## Model Selection
We wish to find a linear model for the outcome mpg. Which regressors should we choose? Let's find out how correlated the variables are. We chose to show our correlation matrix in the appendix as to to keep our report clean and less of a data dump (See Appendix, section Correlation Matrix).

Almost all variables seem to be moderately to strongly correlated with mpg, however, they also seem highly correlated with each other. The ANOVA method seems appropriate in this situation.

Starting from our base model $\text{mpg} = \beta_0 + \beta_1\text{am}$, we will create a sequence of nested models by linearly adding all the regressors. Note that the order in which we add correlated regressors is of importance. If a regressor turns out to be significant; its correlated regressors will likely be insignificant. We also check whether the model residuals are approximataly normal with the Shapiro-Wilk test.  

See Appendix, section ANOVA for the results.

With a p-value of 0.05, we decide to include the regressors cyl and wt. Our model is:
$$\text{mpg} = \beta_0 + \beta_1\text{am}+\beta_2\text{cyl}+\beta_3\text{wt}$$

## How well is the fit?
We will now check to see how well our model fits the data by looking at $R^2$ and the residual plots.

```{r model}
mdl<- lm(mpg~am+cyl+wt,data=mtcars)
summary(mdl)$r.squared
```
About 83% of the variance is explained by our model. A decent number considering we have a relatively small amount of predictors.

```{r residual plot}
par(mfrow=c(2,2))
plot(mdl)
```

Our residuals seem nicely random. 

```{r coefficients}
summary(mdl)$coef
```

Here's we notice something is wrong. Our estimated transmission coefficient turns out to confidently not reject the null hypothesis that it's different from 0. To make things worse; it seems to be centered around 0. This makes us believe that mpg isn't dependent on transmission at all. The other regressors are highly significant though.

## Automatic or Manual?
From the data we've fitted a linear model for the outcome mpg with the predictors cyl, wt, and am. Which regressors to include was decided by the ANOVA tests. For the estimated transmission coefficient we found value of 0.17 and standard error of 1.3. The test  with null hypothesis $\beta_1=0$ fails the reject convingly (p > 0.89). We conclude that for this model the transmission type is negligible for predicting the amount miles per gallon. Our quantification for the difference between mpg for automatic and manual transmission cars would be 0.

## Appendix

### Correlation matrix
```{r correlation}
mtcars$am<-as.numeric(mtcars$am) ## Changing it back again
cor(mtcars)
```

### ANOVA
```{r ANOVA}
fit1 <- lm(mpg~am,data=mtcars)
fit2 <- update(fit1,mpg~am+cyl,data=mtcars)
fit3 <- update(fit2,mpg~am+cyl+disp,data=mtcars)
fit4 <- update(fit3,mpg~am+cyl+disp+drat,data=mtcars)
fit5 <- update(fit4,mpg~am+cyl+disp+drat+wt,data=mtcars)
fit6 <- update(fit5,mpg~am+cyl+disp+drat+wt+qsec,data=mtcars)
fit7 <- update(fit6,mpg~am+cyl+disp+drat+wt+qsec+vs,data=mtcars)
fit8 <- update(fit7,mpg~am+cyl+disp+drat+wt+qsec+vs+gear,data=mtcars)
fit9 <- update(fit8,mpg~am+cyl+disp+drat+wt+qsec+vs+gear+carb,data=mtcars)
anova(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9)
```

```{r testing normality}
shapiro.test(fit2$residuals);shapiro.test(fit5$residuals)
```
The test gives no reason to reject normality.
