library(carData);library(car);library(regclass);library(openintro);library(MASS)

setwd('E:\\Clark\\Spring2025\\Github_Courses\\geog-247_Statistics\\docs\\Labs\\Lab03')

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
# d best subset model
best_subset <- regsubsets(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
                          + cls_students + cls_level + cls_profs + cls_credits + bty_avg
                          + pic_outfit + pic_color, data = evals, nbest = 1, nvmax = 13)
summary_best <- summary(best_subset)
best_model_index <- which.min(summary_best$cp)
best_variables <- names(which(summary_best$which[best_model_index,]))
cat("Best model has", best_model_index, "predictors with lowest C_P:", max(summary_best$cp), "\n")

#d.backward model
# Perform backward elimination using AIC
full_model = lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
                + cls_students + cls_level + cls_profs + cls_credits + bty_avg
                + pic_outfit + pic_color, data = evals)
backward_model <- step(full_model,direction = "backward")
summary_backward <- summary(backward_model)

#d.forward model
forward_model  = regsubsets(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
                        + cls_students + cls_level + cls_profs + cls_credits + bty_avg
                        + pic_outfit + pic_color, data = evals, nvmax = 13, method = 'forward')
AIC(forward_model)
summary_forward <- summary(forward_model)
best_model_index <- which.min(summary_forward$cp)
best_variables <- names(which(summary_forward$which[best_model_index,]))
cat("Best model has", best_model_index, "predictors with lowest C_P:", max(summary_forward$cp), "\n")

# final model
final_model = lm(score ~  ethnicity + gender + language + age + cls_perc_eval+  cls_credits + bty_avg+ pic_outfit + pic_color, data = evals)
summary(final_model)
