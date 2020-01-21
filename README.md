# IS IT WORTH TO ATTEND SUMMER RESEARCH CAMP? A PROPENSITY SCORE MATCHING APPROACH TO EDUCATIONAL ATTAINMENT DETERMINANTS"

Authors: Luis Eduardo Andrade Silva & Cristina Calvo López

In this repository you can find the codes from the paper "Is it worth to attend summer research camp? A propensity score matching approach to educational attainment determinants" by Andrade Silva, Luis Eduardo and Calvo López, Cristina. 




```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# 0. Pre.

```{r, echo=FALSE}
rm(list=ls())
library(haven)
data <- read_sav("/Users/Luisinho/Desktop/Proyecto 2/NEW PROJECT/summer2.sav")
attach(data)
```


Change name and class to numeric:

```{r}
data$sex<-as.numeric(data$SEX)
data$video<-as.numeric(data$video)
data$treat<-as.numeric(data$treat)
data$anx<-as.numeric(data$anx)
data$bor<-as.numeric(data$bor)
data$summer<-as.numeric(data$summer)
data$gpa<-as.numeric(data$HSGPA)
data$mat<-as.numeric(data$SATM)
data$income<-as.numeric(data$INCOME)
data$fg<-as.numeric(data$FIRSTGEN)
data$religion<-as.numeric(data$religion)
data$white<-as.numeric(data$white)
data$act<-as.numeric(data$ACTCOMP)

attach(data)
class(gpa)
```

## Vars.

Dependent variable: ACT composite (mean score at ACT test):

```{r}
summary(act)
summary(gpa)
```

The treatment here is going or not to a research camp during the summer:

```{r}
summary(summer)
```
Where 0=going and 1= not going.

Those students going to summer camp are assummed to be good students that perform above average in high school. This kind of camps are suppossed to be more than nice to improve student's knowlegde and performance.


# 1. Descriptive tables.
##. Values.
### Mean outcomes for treated/untreated population by bor and gender.

compute the means:

```{r}
mean<-mean(mat)

## Belong, male.
mean1<-mean(data.matrix(data[data$sex==1 & data$summer==1 & data$white==0,"mat"]))
mean2<-mean(data.matrix(data[data$sex==1 & data$summer==0 & data$white==0,"mat"]))
## Belong, female.
mean3<-mean(data.matrix(data[data$sex==2 & data$summer==1 & data$white==0,"mat"]))
mean4<-mean(data.matrix(data[data$sex==2 & data$summer==0 & data$white==0,"mat"]))
```

```{r}
## Belong, male.
mean5<-mean(data.matrix(data[data$sex==1 & data$summer==1 & data$white==1,"mat"]))
mean6<-mean(data.matrix(data[data$sex==1 & data$summer==0 & data$white==1,"mat"]))
## Belong, female
mean7<-mean(data.matrix(data[data$sex==2 & data$summer==1 & data$white==1,"mat"]))
mean8<-mean(data.matrix(data[data$sex==2 & data$summer==0 & data$white==1,"mat"]))

d1<-mean1-mean2
d2<-mean3-mean4
d3<-mean5-mean6
d4<-mean7-mean8
```

### Distrubution of treatment in population by FG and gender.

```{r}
p<-length(mat)

## First gen, male.
p1<-length(data.matrix(data[data$sex==1 & data$summer==1 & data$white==0,"mat"]))/p
p2<-length(data.matrix(data[data$sex==1 & data$summer==0 & data$white==0,"mat"]))/p
## First gen, female.
p3<-length(data.matrix(data[data$sex==2 & data$summer==1 & data$white==0,"mat"]))/p
p4<-length(data.matrix(data[data$sex==2 & data$summer==0 & data$white==0,"mat"]))/p

## No first gen, male.
p5<-length(data.matrix(data[data$sex==1 & data$summer==1 & data$white==1,"mat"]))/p
p6<-length(data.matrix(data[data$sex==1 & data$summer==0 & data$white==1,"mat"]))/p
## No first gen, female
p7<-length(data.matrix(data[data$sex==2 & data$summer==1 & data$white==1,"mat"]))/p
p8<-length(data.matrix(data[data$sex==2 & data$summer==0 & data$white==1,"mat"]))/p

t1<-sum(p1+p2)
t2<-sum(p3+p4)
t3<-sum(p5+p6)
t4<-sum(p7+p8)

sum(p1+p2+p3+p4+p5+p6+p7+p8)
```



## Tables.
### a. Mean outcomes.

Table 1. WHITE ACT by gender/treatment.

```{r}
FGmean<-matrix(c(mean1, mean2, d1, mean5, mean6, d2), ncol=3, byrow=T)

colnames(FGmean) <- c("Untreated","Treated","Difference")
rownames(FGmean) <- c("Male", "Female")
FGmean <- as.table(FGmean)
FGmean
```

Table 2. NON WHITE ACT by gender/treatment.
```{r}
NFmean<-matrix(c(mean5, mean6, d3, mean7, mean8, d4), ncol=3, byrow=T)

colnames(NFmean) <- c("Untreated","Treated","Total")
rownames(NFmean) <- c("Male", "Female")
NFmean <- as.table(NFmean)
NFmean
```



### b. Distribution of outcomes.

Table 1. WHITE ACT by gender/treatment.
```{r}
FGds<-matrix(c(p1, p2, t1, p5, p6, t3), ncol=3, byrow=T)

colnames(FGds) <- c("Treated","Non Treated","Difference")
rownames(FGds) <- c("Male", "Female")
FGds <- as.table(FGds)
FGds
```

Table 2. Non WHITE ACT by gender/treatment.
```{r}
NFds<-matrix(c(p5, p6, t3, p7, p8, t4), ncol=3, byrow=T)

colnames(NFds) <- c("Treated","Non Treated","Difference")
rownames(NFds) <- c("Male", "Female")
NFds <- as.table(NFds)
NFds
```


# 2. Difference in means: outcome variable.

First we standarise mat:

```{r}
smat<-(mat-mean(mat))/ sd(mat)
mean(smat)
sd(smat)
```

Independent variable is going to summer camp:


```{r}
summary(summer)
```

differences in means:

```{r}
library(dplyr)
data %>%
  group_by(summer)  %>%
  summarise (n_students=n(), 
             mean_smat= mean(smat),
             std_error=sd(smat)/ sqrt(n_students))
```

Non-std.


```{r}
data %>%
  mutate(test = (mat - mean(mat)) / sd(mat)) %>% #this is how the math score is standardized
  group_by(summer) %>%
  summarise(mean_mat = mean(test))
```

The difference-in-means is statistically significant at conventional levels of confidence (as is also evident from the small standard error above):

```{r}
with(data, t.test(smat ~ summer))
```

# 3. Difference in means: pre-treatment covariates.

Let’s calculate the mean for each covariate by the treatment status:

```{r}
ecls_cov <- c('sex', 'income', 'fg', 'religion', 'bor', 'white', 'gpa')
data %>%
  group_by(summer) %>%
  select(one_of(ecls_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))
```


# 4. Propensity score estimation.

We estimate the propensity score by running a logit model (probit also works) where the outcome variable is a binary variable indicating treatment status. What covariates should you include? For the matching to give you a causal estimate in the end, you need to include any covariate that is related to both the treatment assignment and potential outcomes. I choose just a few covariates below—they are unlikely to capture all covariates that should be included. You’ll be asked to come up with a potentially better model on your own later.


```{r}
ecls_cov
m_ps <- glm(summer ~ sex + income+fg+religion+bor+white+gpa,
            family = binomial(), data = data)
summary(m_ps)
```

```{r}
prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     summer = m_ps$model$summer)
head(prs_df)
```

# 5. Region of common support.

After estimating the propensity score, it is useful to plot histograms of the estimated propensity scores by treatment status:

```{r}
labs <- paste("Summer school:", c("YES", "NA"))
library(ggplot2)
prs_df %>%
  mutate(summer = ifelse(summer == 0, labs[0], labs[1])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~summer) +
  xlab("Probability attending summer camp") +
  theme_bw()
```

# 6. Matching algorithm

The method we use below is to find pairs of observations that have very similar propensity scores, but that differ in their treatment status. We use the package MatchIt for this. This package estimates the propensity score in the background and then matches observations based on the method of choice (“nearest” in this case).

```{r}
ecls_nomiss <- data %>%  # MatchIt does not allow missing values
  select(mat, summer, one_of(ecls_cov)) %>%
  na.omit()

library(MatchIt)
mod_match <- matchit(summer ~ sex + income+fg+religion+bor+white+gpa,
                     method = "nearest", data = ecls_nomiss)
```

To create a dataframe containing only the matched observations, use the match.data() function:

```{r}
dta_m <- match.data(mod_match)
dim(dta_m)
```

# 7. Examining covariate balance in the matched sample

```{r}
dta_m %>%
  group_by(summer) %>%
  select(one_of(ecls_cov)) %>%
  summarise_all(funs(mean))

```

You can test this more formally using t-tests. Ideally, we should not be able to reject the null hypothesis of no mean difference for each covariate:

```{r}
lapply(ecls_cov, function(v) {
    t.test(dta_m[, v] ~ dta_m$summer)
})
```

No difference of means now.




# 8. Estimating treatment effects

Estimating the treatment effect is simple once we have a matched sample that we are happy with. We can use a t-test:

```{r}
with(dta_m, t.test(mat ~ summer))
```

## 8.1. simple ols

Or we can use OLS with or without covariates:

```{r}
lm_treat1 <- lm(mat ~ summer, data = dta_m)
summary(lm_treat1)
```

Clear effect of tratment!

# 8.2. Multiple ols.

```{r}

lm_treat2 <- lm(mat ~ summer + sex + income+fg+religion+bor+white+gpa, data = dta_m)
summary(lm_treat2)
```


Conclusion: clar effect of going to a summer camp, even when comparing between those students with similar characteristics. 
