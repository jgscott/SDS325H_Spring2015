# Anything followed by the #-sign is a comment.
# It is ignored by R.

# First install mosaic (via Packages tab; see the video from the class website.)

# Once you've installed mosaic, you'll need to load it
library(mosaic)

# Load the data set on orchard sprays and honeybees
data(OrchardSprays)

# Because this data set comes pre-loaded with the mosaic package,
# it has a help file associated with it.
?OrchardSprays

# What the variable names in this data set?
names(OrchardSprays)

# Summarize the variables
summary(OrchardSprays)

# Note that R is case sensitive
# This should give you an "object not found" error:
summary(orchardsprays)

# You can get help on any command via ?command
?names
?summary

# Calculate the mean and standard deviation of the decrease.
# the $ picks out a specific variable (decrease) from a data set (OrchardSprays)
mean(OrchardSprays$decrease) 
sd(OrchardSprays$decrease)

# A histogram of the decrease variable
hist(OrchardSprays$decrease)

# Want more buckets than the default?
hist(OrchardSprays$decrease, breaks=20)

# Make a boxplot of decrease stratified by treatment
# Note the between-group and within-group differences
boxplot(decrease~treatment, data=OrchardSprays)

# Calculate the decrease stratified by treatment.
# You must load the mosaic package,
# otherwise this command will generate an error.
mean(decrease~treatment, data=OrchardSprays)

# You can store these group means in another variable.
# A single equal sign means "assign the right-hand side
# to the variable on the left-hand side."
GroupMeans = mean(decrease~treatment, data=OrchardSprays)

# To see a variable printed to the console, just type the variable name
GroupMeans


####
# More advanced commands
####

# Make a dot plot/strip chart showing the group-wise data
stripchart(decrease~treatment,
	data=OrchardSprays, vertical=TRUE,
	xlab="Concentration of Lime Sulphur (A = highest; H = none)",
	ylab="Decrease in volume of solution")

# Add the group means in a different color and label
# pch tells you the type of points; type in ?points to see the choices
# cex: character expansion.  Makes everything bigger (> 1) or smaller (< 1)
points(GroupMeans, pch=19, col='blue', cex=1.5)

# Fit a model and look at the differences between means
lm2 = lm(decrease~treatment, data=OrchardSprays)
coef(lm2)

# Compare these coefficients to the group means:
# How are they related?
# Hint: try adding the "intercept" to each coefficient
# and compare the result to the corresponding group mean!
rbind(GroupMeans, coef(lm2))
