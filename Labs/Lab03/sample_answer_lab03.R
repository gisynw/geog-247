library(carData);library(car);library(regclass);library(openintro);library(MASS)

evals = read.csv('evals.csv')

##Task 1
# a
plot(evals$bty_avg~evals$bty_f1lower)

#b
lm01 = lm(score ~bty_f1lower+bty_f1upper+bty_f2upper+ bty_m1lower+ bty_m1upper+ bty_m2upper+bty_avg, data = evals)
vif(lm01)

#c
m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)

#d 
hist(m_bty_gen$residuals, main = 'Histogram of the residual')

plot(x = m_bty_gen$residuals, y = m_bty_gen$fitted.values,
     xlab = 'residuals', ylab = 'fitted value')

#e
#provide equation corresponding to males

#f 
m_bty_rank = lm(score ~ bty_avg+rank, data=evals)
summary(m_bty_rank)

############################Task 02
#a
m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg
             + pic_outfit + pic_color, data = evals)

#b
summary(m_full)

#c
#provide interpretation for the coefficient associated with the ethnicity variable

#d
#d.backward model
backward_model <- stepAIC(m_full, direction = "backward")
summary(backward_model)

#d.forward model
# Start with a null model (intercept only)
null_model = lm(score ~ 1, data = evals)
forward_model <- stepAIC(null_model, direction = "forward", 
                         scope = list(upper = ~rank + ethnicity + gender + language + age + cls_perc_eval
                                      + cls_students + cls_level + cls_profs + cls_credits + bty_avg
                                      + pic_outfit + pic_color, lower = ~1))
summary(forward_model)

#d.both model
# Stepwise selection (both directions)
both_model <- stepAIC(m_full, direction = "both")
summary(both_model)

# Compare AIC of all models
AIC(m_full, backward_model, forward_model,both_model)

summary(backward_model)

