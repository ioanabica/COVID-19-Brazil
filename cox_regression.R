library(dplyr)
library(stringr)
library(finalfit)
library(ggplot2)
library(survival)
library(coxme)
library(broom)  
library(survivalAnalysis)


d_xy = read.csv(file='data/data_for_cox_regression_in_hospital_mortality.csv')


model <- coxme(Surv(Days_hospital_to_outcome , is_dead) ~
                 Sex_male +
                 Age_70 + Age_60_70 + Age_50_60 + Age_40_50 + 
                 Cardiovascular + Asthma + Diabetis + Pulmonary + Immunosuppresion + Obesity +
                 Neurologic + Renal + Liver + 
                 Parda + Preta + Amarela + 
                 (1|SG_UF_NOT),
               d_xy)

model

# fixed effects
coeff_main = fixef(model)
sd_main = sqrt(diag(vcov(model)))

df_fixed = data.frame(effect = names(coeff_main), log_hr = coeff_main, log_hr_sd = sd_main, row.names=NULL)
df_fixed = df_fixed %>% arrange(desc(log_hr))
df_fixed$effect <- factor(df_fixed$effect, levels = df_fixed$effect[order(df_fixed$log_hr)])

write.csv(df_fixed, file = 'results/fixed_effects.csv')


# random effects
coeff_rand = ranef(model)$SG_UF_NOT
sd_rand = sqrt(diag(model$variance)[1:length(coeff_rand)])


df_random = data.frame(effect = names(coeff_rand), log_hr = coeff_rand, log_hr_sd = sd_rand, row.names=NULL)
df_random = df_random %>% arrange(desc(log_hr))
df_random$effect <- factor(df_random$effect, levels = df_random$effect[order(df_random$log_hr)])
                          
write.csv(df_random, file = 'results/random_effects.csv')

