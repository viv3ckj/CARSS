###Equation & Statistics###
install.packages("CalibrationCurves")

#Transformations for the model equation
cars_complete$log_CRE = 1/sqrt(cars_complete$CRE)
cars_complete$log_WBC = log(cars_complete$WBC+1)
cars_complete$log_URE = log(cars_complete$URE)
cars_complete$log_POT = log(cars_complete$POT)
cars_complete$log_syst = log(cars_complete$sys)
cars_complete$log_dias = log(cars_complete$dias)
cars_complete$log_resp = log(cars_complete$resp+1)
cars_complete$log_pulse = log(cars_complete$pulse)
cars_complete$AKI1 = ifelse(cars_complete$AKI.Warning.Stage==1,1,0)
cars_complete$AKI2 = ifelse(cars_complete$AKI.Warning.Stage==2,1,0)
cars_complete$AKI3 = ifelse(cars_complete$AKI.Warning.Stage==3,1,0)
cars_complete$alert1 = ifelse(cars_complete$Alertness.y=="Pain",1,0)
cars_complete$alert2 = ifelse(cars_complete$Alertness.y=="Voice",1,0)
cars_complete$alert3 = ifelse(cars_complete$Alertness.y=="Unresponsive",1,0)
cars_complete$age_log_wbc <- cars_complete$age*cars_complete$log_WBC
cars_complete$log_cre_log_wbc <- cars_complete$log_CRE*cars_complete$log_WBC
cars_complete$aki3_log_cre <- cars_complete$AKI3*cars_complete$log_CRE
cars_complete$age <- as.numeric(cars_complete$age)
cars_complete$male <- as.numeric(cars_complete$male)
cars_complete$sup <- as.numeric(cars_complete$sup)

cars_calc <- cars_complete

cars_calc$y <- -3.21959630508242 + 0.140107489855065 * cars_calc$male + 0.0765103914093195 * 
  cars_calc$age - 0.104475345731493 * cars_calc$ALB + 9.88278918389683 * cars_calc$log_CRE + 
  0.0017582077284345 * cars_calc$HB - 0.0237652204925313 * cars_calc$log_POT - 
  0.0225974340779114 * cars_calc$SOD + 1.16725083382606 * cars_calc$log_WBC + 1.21092015428962 * cars_calc$log_URE + 0.13149776337431 * cars_calc$AKI1 + 0.443145544771415 * cars_calc$AKI2 -  0.388264311799849 * cars_calc$AKI3 + 0.0926330022658697 * cars_calc$NEWS + 0.569373052590953 *  cars_calc$log_resp - 0.144506668206767 * cars_calc$temp + 0.776830878843219 *  cars_calc$log_dias - 0.91926266885138 * cars_calc$log_syst + 0.511486122558965 *  cars_calc$log_pulse - 0.0164265269767544 * cars_calc$sat + 0.606283721107285 *  cars_calc$sup + 0.716318746265315 * cars_calc$alert1 + 
  0.394692523269426 * cars_calc$alert2 +  1.92504456359168 * cars_calc$alert3 - 0.0145988784768987 * cars_calc$age_log_wbc +  1.48096130770941 * cars_calc$log_cre_log_wbc + 15.5505084187461 * cars_calc$aki3_log_cre

#Generates calibration plot (pre-re-calibration)
cars_calc$predicted_risk = exp(cars_calc$y)/(1+exp(cars_calc$y))
cars_calc$logit_predicted_risk = log(cars_calc$predicted_risk/(1-cars_calc$predicted_risk))
val.prob.ci(cars_calc$predicted_risk, cars_calc$Died)
#Based on the intercept of the above plot (0.96), the following code is for recalibration of plot. 
cars_calc$intercept = cars_calc$logit_predicted_risk+0.96
cars_calc$intercept_p <- 1/(1+exp(-cars_calc$intercept))
val.prob.ci(cars_calc$intercept_p, cars_calc$Died)

# 95% CI for intercept PRE
intercept = summary(glm(cars_calc$Died~1, data=cars_calc,family="binomial", offset=cars_calc$logit_predicted_risk))$coefficients[1,1];intercept
se = summary(glm(cars_calc$Died~1,data=cars_calc,family="binomial", offset=cars_calc$logit_predicted_risk))$coefficients[1,2]
ll=intercept - 1.96*se;ll;ul=intercept + 1.96*se;ul

#95% CI for slope PRE
slope = summary(glm(cars_calc$Died~cars_calc$logit_predicted_risk,data=cars_calc,family="binomial"))$coefficients[2,1];slope
se = summary(glm(cars_calc$Died~cars_calc$logit_predicted_risk,data=cars_calc,family="binomial"))$coefficients[2,2]
ll=slope - 1.96*se;ll;ul=slope + 1.96*se;ul

# 95% CI for intercept POST
intercept = summary(glm(cars_calc$Died~1, data=cars_calc,family="binomial", offset=cars_calc$intercept))$coefficients[1,1];intercept
se = summary(glm(cars_calc$Died~1,data=cars_calc,family="binomial", offset=cars_calc$intercept))$coefficients[1,2]
ll=intercept - 1.96*se;ll;ul=intercept + 1.96*se;ul

#95% CI for slope PRE
slope = summary(glm(cars_calc$Died~cars_calc$intercept,data=cars_calc,family="binomial"))$coefficients[2,1];slope
se = summary(glm(cars_calc$Died~cars_calc$intercept,data=cars_calc,family="binomial"))$coefficients[2,2]
ll=slope - 1.96*se;ll;ul=slope + 1.96*se;ul

### boxplot ###
tiff("R:/boxplot.tiff", height = 10, width = 10, units ='in', res=300)
par(mfrow=c(4,4), mar=c(4,4,1.5,1.5), oma=c(1.5,2,1,1))
cars_calc$diedf <- factor(cars_calc$Died, labels= c("Alive", "Died"))
boxplot(cars_calc$age~cars_calc$diedf, xlab = "", ylab = "", outline=FALSE,ylim=c(0,110))
title("Age (years)")
boxplot(cars_calc$ALB~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("Albumin (g/L)")
boxplot(cars_calc$CRE~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("Creatinine (umol/L)")
boxplot(cars_calc$HB~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("Haemoglobin (g/L)")
boxplot(cars_calc$POT~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("Potassium (mmol/L)")
boxplot(cars_calc$SOD~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("Sodium (mmol/L)")
boxplot(cars_calc$URE~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("Urea (mmol/L)")
boxplot(cars_calc$NEWS~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("NEWS")
boxplot(cars_calc$temp~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("Temperature (celsius)")
boxplot(cars_calc$pulse~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("Pulse Rate (beats/min)")
boxplot(cars_calc$sat~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("Oxygen Saturation (%)")
boxplot(cars_calc$resp~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("Respiratory Rate (breaths/min)")
boxplot(cars_calc$WBC~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("White Cell Count (10^9 cells/L)")
boxplot(cars_calc$sys~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("Systolic Pressure (mmHg)")
boxplot(cars_calc$dias~cars_calc$diedf,outline=FALSE, xlab = "", ylab = "")
title("Diastolic Pressure (mmHg)")
dev.off()

#### line plot ###
tiff("R:/lineplot.tiff", height = 10, width = 10, units = 'in', res=300)
par(mfrow=c(4,4), mar=c(4,4,1.5,1.5), oma=c(1.5,2,1,1))
library(Hmisc)
x <- cut2(cars_calc$age, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Age (years)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$ALB, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Albumin (g/L)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$CRE, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Creatinine (umol/L)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$HB, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Haemoglobin (g/L)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$POT, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Potassium (mmol/L)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$SOD, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Sodium (mmol/L)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$URE, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Urea (mmol/L)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$NEWS, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="NEWS", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$temp, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Temperature (celsius)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$pulse, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Pulse Rate (beats/min)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$sat, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Oxygen Saturation (%)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$resp, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Respiratory Rate (breaths/min)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$WBC, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="White Cell Count (10^9 cells/L)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$sys, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Systolic Pressure (mmHg)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
x <- cut2(cars_calc$dias, levels.mean=TRUE, g=8)
y <- tapply(cars_calc$Died, x, mean)
plot(levels(x), y, main="Diastolic Pressure (mmHg)", xlab="", ylab="Proportion Died", type='b') #ylim=c(0,0.6))
minor.tick(nx=5, ny=0, tick.ratio=0.3)
dev.off()

install.packages("pROC")
ci.auc(cars_calc$Died, cars_calc$intercept_p)
