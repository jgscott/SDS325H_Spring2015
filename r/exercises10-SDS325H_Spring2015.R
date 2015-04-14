library(mosaic)

brca = read.csv('~/Sites/SDS325H_Spring2015/data/brca.csv', header=TRUE)
summary(brca)

# Recast radiologist as a factor
brca$radiologist = factor(brca$radiologist)

## First build a model for recall status, given risk factors and radiologist

# Could name all the variables explicitly
glm1 = glm(recall ~ radiologist + age5059 + age6069 + age70plus + familyhistory + biopsurg + symptoms + premeno + postmenohormone + postmenounknown + previousmammogram + density1 + density3 + density4, data=brca, family=binomial)

# Or could name the ones we don't want
glm1 = glm(recall ~ radiologist + ( . - cancer), data=brca, family=binomial)

# Are there are radiologists who seem more conservative than others?
summary(glm1)
# Looks like 34 and 95 recall least frequently, adjusting for risk factors
# 66 and 89 recall more frequently, adjusting for risk factors
# 13 (the baseline) is in the middle, adjusting for risk factors


# Now fit a model for cancer, given recall status and radiologist
glm2 = glm(cancer ~ . , data=brca, family=binomial)

# Recall is a very important predictor
# Are there any risk factors that should be given
# more weight in the recall decision?
summary(glm2)

# density and age seem like they predict cancer status,
# even adjusting for radiologist's judgments

# Could do additional model exploration but I will just use the basic "all predictors" model


# Estimate probability of recall for average patient (all covariates 0)
# with radiologist 13 (the median recall rate).
# Check this with mean(recall~radiologist, data=brca)
p.recall = exp(-2.15)/{1+exp(-2.15)}

p.cancer.giv.norecall = exp(-4.82)/{1+exp(-4.82)}
p.cancer.giv.recall = exp(-4.82 + 2.33)/{1+exp(-4.82 + 2.33)}

# true negative
# Joint probability of no cancer and no recall
pTN = (1-p.recall) * (1-p.cancer.giv.norecall)

# True positive: cancer and recall
pTP = p.recall * p.cancer.giv.recall

# False positive: no cancer and recall
# = prob(recall) * {1- prob(cancer | recall)}
pFP = p.recall * (1-p.cancer.giv.recall)

# False negative: no recall and cancer
pFN = {1-p.recall} * p.cancer.giv.norecall

# Four cells of the table
pFN; pTN; pFP; pTP

# Looks like the screening generates lots of false positives
# for the sake of keeping the false-negative rate down
