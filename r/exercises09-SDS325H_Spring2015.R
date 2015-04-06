library(mosaic)


# Problem 1

spamfit = read.csv('spam-fit.csv', header=TRUE)
spamtest = read.csv('spam-test.csv', header=TRUE)

# Fit the model versus all predictors (. is shorthand for this)
glm1 = glm(y ~ ., data=spamfit, family='binomial')

# Make tables to calculate error rates
yhat_train = predict(glm1, newdata=spamfit, type='response')
xtabs(~{yhat_train>0.5} + spamfit$y)

yhat_test = predict(glm1, newdata=spamtest, type='response')
xtabs(~{yhat_test>0.5} + spamtest$y)



# Problem 3

w_true = 0.25
n1 = 57
n2 = 63

num_tests = 1
sim1 = do(1000)*{

	x1 = rbinom(num_tests, n1, w_true)
	x2 = rbinom(num_tests, n2, w_true)
	w1 = x1/n1
	w2 = x2/n2
	
	delta = w1 - w2
	se_delta = sqrt(w1*(1-w1)/n1 + w2*(1-w2)/n2)
	z_delta = delta/se_delta
	max_z = abs(max(z_delta))
	max_z
}

sum(sim1$result > 1.96)

