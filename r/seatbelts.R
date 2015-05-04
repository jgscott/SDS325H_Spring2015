# Load the mosaic library after installing
library(mosaic)
library(faraway)

# Now we read in the data (in .txt format)
# tab-delimited
seatbelts = read.csv("seatbelts.csv", header=TRUE)


###################################
# Fatalities vs. seat-belt usage rates 
###################################


summary(seatbelts)
seatbelts = na.omit(seatbelts)
seatbelts$fatalities = 100*seatbelts$fatalities

# The main relationship of interest
plot(fatalities ~ seatbelt, data=seatbelts)

# Some possible confounders
plot(fatalities~income, data=seatbelts)
plot(fatalities~log(income), data=seatbelts)
# The logit transformation is defined in the faraway library

plot(fatalities ~ state, data=seatbelts)

plot(fatalities ~ factor(year), data=seatbelts)
# Above, we need factor(year) to tell R that year is a category label

plot(fatalities~factor(drinkage), data=seatbelts)
plot(fatalities~age, data=seatbelts)


### Fatalities versus seat-belt usage

# Let try a mixed-effects model with random effects for state/year
library(lme4)
lm1 = lmer(fatalities ~ log(income) + drinkage + age + speed70 + seatbelt + (1 | state) + (1 | year), data=seatbelts)

summary(lm1)

r1 = ranef(lm1, condVar = TRUE, whichel = 'state')
dotplot(r1)

r2 = ranef(lm1, condVar = TRUE, whichel = 'year')
dotplot(r2)

# Diagnostics
hist(resid(lm1))
boxplot(resid(lm1) ~ year, data=seatbelts)
boxplot(resid(lm1) ~ state, data=seatbelts)




###################################
# Seat-belt usage rates vs policy
###################################

lm2 = lmer(seatbelt ~ log(income) + drinkage + age + speed70 + enforce + (1 | state) + (1 | year), data=seatbelts)
summary(lm2)


# Make predictions
actualTX = subset(seatbelts, state=='TX')
fictionalTX = actualTX
fictionalTX$enforce = "no"

# Looks like seatbelt usage would have been lower
predict(lm2, newdata=fictionalTX)
fictionalTX$seatbelt = predict(lm2, newdata=fictionalTX)
predict(lm1, newdata=fictionalTX)

# Compare predicted vs actual
cbind(predict(lm1, newdata=fictionalTX)*actualTX$miles/100, actualTX$fatalities*actualTX$miles/100)
