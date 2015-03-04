# Load in the relevant libraries
library(mosaic)

### 1: confidence intervals

beta0 = 3
beta1 = 1.5
sigma = 3
n = 50

# Try a single data set
x = runif(n, 0, 10)
epsilon = rnorm(n,0,sigma)
y = beta0 + beta1*x + epsilon
plot(x, y)

# Monte Carlo
num_mc_samples = 1000
boot_samples = 500
confidence_level = 0.9
mc1 = do(num_mc_samples)*{
  
  # 1) Simulate data (same x's each time)
  epsilon = rnorm(n,0,sigma)
  y = beta0 + beta1*x + epsilon
  my_data_frame = data.frame(x,y)
  
  # 2) Fit model and compute normal-theory interval
  lm1 = lm(y~x, data = my_data_frame)
  int1 = confint(lm1, level=confidence_level)
  
  # 3) Bootstrap and get interval
  boot = do(boot_samples)*{
    lm_boot = lm(y~x, data = resample(my_data_frame))
    coef(lm_boot)
  }
  int2 = confint(boot, level=confidence_level)
  
  # 4) Check whether we covered the true slope
  cover1 = (int1[2,1] < beta1) & (int1[2,2] > beta1)
  cover2 = (int2[2,2] < beta1) & (int2[2,3] > beta1)
  
  # Return the results
  c(cover1, cover2)
}

# Check results
summary(mc1)

# Is the bootstrap performance consistent with the FCP?
# Recognize the coverage event as a binomial distribution
rbinom(1, num_mc_samples, confidence_level)
hist(rbinom(10000, num_mc_samples, confidence_level))


### 2: shocks

# Part A
lm1 = lm(expensive~cheap, data=shocks)
summary(lm1)
# R-squared exceeds 90%

# Part B: follow newspapers walkthrough.

# Is beta1 = 1 inside the 95% confidence interval?
confint(lm1)

# Now for the prediction intervals
# Make a data frame for the new x's where you want to predict
new_shocks = data.frame(cheap = c(510, 550, 590))

# Are any of the prediction intervals wider than 33 units?
predict(lm1, new_shocks, interval='prediction', level = 0.95)

# Check the assumptions of the normal regression model
hist(resid(lm1))
plot(resid(lm1) ~ cheap, data=shocks)




### Problem 2: CIS and beauty ratings

# Plots: ratings seem higher for men, for non-minorities,
# and for pretty people
plot(eval ~ beauty, data=profs)
boxplot(eval ~ minority, data=profs)
plot(eval ~ age, data=profs)
boxplot(eval ~ gender, data=profs)

# Ratings are lower for upper-division courses
# higher for single-credit courses
# higher for non-tenure-track teachers
# and higher for native English speakers
boxplot(eval ~ division, data=profs)
plot(eval ~ log(students), data=profs)
boxplot(eval ~ credits, data=profs)
boxplot(eval ~ tenure, data=profs)
boxplot(eval ~ native, data=profs)


# Let's build a model that incorporates all these effects
lm1 = lm(eval ~ native + tenure + credits + log(students) + gender + minority + beauty, data=profs)
summary(lm1)
anova(lm1)

# It looks like the beauty variable is the single largest contributor to the predictive abilities of the model

# We should check whether this finding is robust.
# What happens if we drop some of the more marginal variables?

# Class size looked like it had a small effect
lm2 = lm(eval ~ native + tenure + credits + gender + minority + beauty, data=profs)
summary(lm2)
anova(lm2)

# The tenure-track variable also has a small effect
# Try dropping it
lm3 = lm(eval ~ native + credits + gender + minority + beauty, data=profs)
summary(lm3)
anova(lm3)
confint(lm3)

# In all these different models, the "beauty effect" looks present
