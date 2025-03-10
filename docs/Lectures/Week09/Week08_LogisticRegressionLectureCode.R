if (!require("car")) install.packages("car")

data('Mroz')

logistic_model1<- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc, family=binomial(logit), data=Mroz)

summary_log01 = summary(logistic_model1)
summary_log01$df.residual

## Confidence Interval
confint(logistic_model1, level = 0.95)
## makign prediction
logistic_model_03<- glm(lfp ~ k5 +wc, family=binomial(logit), data=Mroz)

summary(logistic_model_03)

## drop non-significant varaibles
logistic_model2<- glm(lfp ~ k5 +  age + wc +  lwg + inc, family=binomial(logit), data=Mroz)
summary(logistic_model2)
## ANOVA TEST
anova(logistic_model1, logistic_model2, test = "LRT")

## Likelihood ratio test
lkh = logLik(logistic_model2)
lh = logLik(logistic_model1)

LR = -2 * (lkh - lh)

pchisq(LR[1], df = 2, lower.tail = F)

##all effect plot
library(effects) 
plot(allEffects(logistic_model2), type="response", ylim=c(0,1), ask=FALSE)


par(mfrow = c(1, 1))
library(effects)

## conditional effect plot
# Compute effect for low probability scenario
low_prb <- effect("k5", logistic_model2, given.values = c(age = 40, wcyes = 0, inc = 20))

# Compute effect for high probability scenario
high_prb <- effect("k5", logistic_model2, given.values = c(age = 25, wcyes = 1, inc = 5))

# Extract data
low_data <- as.data.frame(low_prb)
high_data <- as.data.frame(high_prb)

# Plot the low probability case first
plot(low_data$k5, low_data$fit, type = "l", col = "blue", lwd = 2, ylim = c(0, 1),
     ylab = expression(Pr(Y[i] == "labor-force participation probability")), xlab = "number of children 5 years old or younger",
     main = "labor-force participation probability vs k5")

# Add shaded confidence intervals for low probability
polygon(c(low_data$k5, rev(low_data$k5)), 
        c(low_data$lower, rev(low_data$upper)), col = rgb(0, 0, 1, 0.2), border = NA)

# Add high probability scenario
lines(high_data$k5, high_data$fit, col = "red", lwd = 2)

# Add shaded confidence intervals for high probability
polygon(c(high_data$k5, rev(high_data$k5)), 
        c(high_data$lower, rev(high_data$upper)), col = rgb(1, 0, 0, 0.2), border = NA)

# Add legend
legend("topright", legend = c("Low Probability", "High Probability"), col = c("blue", "red"), lwd = 2)


