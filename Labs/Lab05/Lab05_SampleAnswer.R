library(sf)

# path = 'E:\\teaching_Clark\\GitRepo\\Spring2025\\geog-247-statistics\\docs\\Labs\\Lab05'
# setwd(path)
# list.files(path)

##Task 1.a
df = st_read('data_accident\\Accident_data.shp')

##Task 1.b
df$geometry

##Task 1.c
df$accident = as.factor(df$accident)
## Task 2.a
logistic_acc = glm(accident~hour+F_SYSTEM+intersect+NUM_LANES+lane_width+BelowFreez+Fog+Thunder+FrozenPrec,family = binomial(logit),data = df)
summary(logistic_acc)

confint(logistic_acc, level = 0.95)

## Task 2.b explaination

## Task 2.c 
library(effects) 
plot(allEffects(logistic_acc), type="response", ylim=c(0,1), ask=FALSE)

## Task 3.a
logistic_acc02 = glm(accident~hour+F_SYSTEM+intersect+NUM_LANES+Fog,family = binomial(logit),data = df)
summary(logistic_acc02)
anova(logistic_acc, logistic_acc02)

## Likelihood ratio test
lkh = logLik(logistic_acc02)
lh = logLik(logistic_acc)

LR = -2 * (lkh - lh)

pchisq(LR[1], df = 4, lower.tail = F)

## Task 4.a
par(mfrow = c(1, 1))
library(effects)

# Compute effect for low probability scenario
low_prb <- effect("NUM_LANES", logistic_acc02, given.values = c(hour = 3, F_SYSTEM = 6, intersectyes = 0))

# Compute effect for high probability scenario
high_prb <- effect("NUM_LANES", logistic_acc02, given.values = c(hour = 19, F_SYSTEM = 2, intersectyes = 1))

# Extract data
low_data <- as.data.frame(low_prb)
high_data <- as.data.frame(high_prb)

# Plot the low probability case first
plot(low_data$NUM_LANES, low_data$fit, type = "l", col = "blue", lwd = 2, ylim = c(0, 1),
     ylab = expression(Pr(Y[i] == "Accident probability")), xlab = "Number of Lanes",
     main = "Accident Probability vs. Number of Lanes")

# Add shaded confidence intervals for low probability
polygon(c(low_data$NUM_LANES, rev(low_data$NUM_LANES)), 
        c(low_data$lower, rev(low_data$upper)), col = rgb(0, 0, 1, 0.2), border = NA)

# Add high probability scenario
lines(high_data$NUM_LANES, high_data$fit, col = "red", lwd = 2)

# Add shaded confidence intervals for high probability
polygon(c(high_data$NUM_LANES, rev(high_data$NUM_LANES)), 
        c(high_data$lower, rev(high_data$upper)), col = rgb(1, 0, 0, 0.2), border = NA)

# Add legend
legend("bottomright", legend = c("Low Probability", "High Probability"), col = c("blue", "red"), lwd = 2)






