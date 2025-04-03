 library('car')
data("CPS1985",package="AER")

rownames(CPS1985) = c(1:nrow(CPS1985))
## ScatterplotMatrix
scatterplotMatrix(~wage+education+age+experience,
                  data=CPS1985,
                  smooth = list(span = 0.5, col.smooth = "green"),   # Add smooth line in blue
                  regLine = list(method = lm, col = "red"),
                  spread = FALSE, id.cex=1.5)

## Test Multicollineariy
model_withoutML <- lm(wage~education+age, data=CPS1985)
summary(model_withoutML)
vif(model_withoutML)

model_withML <- lm(wage~education+experience+age, data=CPS1985)
summary(model_withML)
vif(model_withML)

## Test if errors have constant variance
model <- lm(wage~education+age, data=CPS1985)
summary(model)

plot(model$residuals~model$fitted.values)

model_log <- lm(log(wage)~education+log(age), data=CPS1985)
summary(model_log)
vif(model_log)

## Test for normality of the residual
plot(model_log)

hist(model_log$residuals, main = 'Residuals', xlab = 'Residuals')

boxplot(model_log$residuals)

# model with quadratic term or not
full_model <- lm(log(wage)~education+age, data=CPS1985)
summary(full_model)
car::residualPlots(full_model, main="Full model")

model_quadratic <- lm(log(wage) ~ education + age + I(age^2), data = CPS1985)
summary(model_quadratic)
anova(full_model, model_quadratic)

# detect outlier 
car::influenceIndexPlot(model_quadratic, id.n=2)

CPS1985[c(108,171,200,63,486), c('wage', 'education', 'age')]


