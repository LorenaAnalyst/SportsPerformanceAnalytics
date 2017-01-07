# Analyzing NFL Game Time Performance Data (R)

#########################################
##   NFL Gametime Performance R code   ##
#########################################

NFLafcnfc<-read.csv('/Users/Desktop/NFLafcnfc.csv')
summary(NFLafcnfc)
# Team          W                L         
# :12   Min.   : 3.000   Min.   : 0.000  
# * -Carolina Panthers   : 1   1st Qu.: 5.000   1st Qu.: 5.000  
# * -New England Patriots: 1   Median : 6.000   Median : 8.000  
# AFC=0 NFC=1            : 1   Mean   : 6.969   Mean   : 6.969  
# Atlanta Falcons        : 1   3rd Qu.: 9.000   3rd Qu.: 9.000  
# Baltimore Ravens       : 1   Max.   :14.000   Max.   :11.000  
# (Other)                :28   NA's   :13       NA's   :13      

# PCT               PF              PA          Conference 
# Min.   :0.2140   Min.   :202.0   Min.   :243.0   Min.   :0.0  
# 1st Qu.:0.3570   1st Qu.:279.5   1st Qu.:272.0   1st Qu.:0.0  
# Median :0.4290   Median :313.5   Median :334.0   Median :0.5  
# Mean   :0.4994   Mean   :319.9   Mean   :319.9   Mean   :0.5  
# 3rd Qu.:0.6430   3rd Qu.:351.5   3rd Qu.:358.2   3rd Qu.:1.0  
# Max.   :1.0000   Max.   :449.0   Max.   :397.0   Max.   :1.0  
# 3 NA's   :13       NA's   :13      NA's   :13      NA's   :13   

# X7WinsUp     
# Min.   :0.0000  
# 1st Qu.:0.0000  
# Median :0.0000  
# Mean   :0.4375  
# 3rd Qu.:1.0000  
# Max.   :1.0000  
# NA's   :13  

# Let us run a logistic regression to examine if the odds of winning
# seven or more games is dictated by the conference AFC or NFC
# Use the following code
ConferenceWinsModel <- glm(X7WinsUp ~ Conference,data=NFLafcnfc,family=binomial)

# Get the summary
summary(ConferenceWinsModel)
# This is the output
# Call:
# glm(formula = X7WinsUp ~ Conference, family = binomial, data = NFLafcnfc)
# Deviance Residuals: 
#    Min      1Q  Median      3Q     Max  
# -1.073  -1.073  -1.073   1.286   1.286  
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)
# (Intercept) -2.513e-01  5.039e-01  -0.499    0.618
# Conference   3.165e-16  7.127e-01   0.000    1.000
# AIC: 47.86
# Number of Fisher Scoring iterations: 3

# Let us run a linear regression model to examine PCT by conference
regression<-lm(NFLafcnfc$PCT~ NFLafcnfc$Conference)
summary(regression)
# Call:
# lm(formula = NFLafcnfc$PCT ~ NFLafcnfc$Conference)
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.27269 -0.13606 -0.05769  0.15631  0.48781 
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.48669    0.05141   9.466 1.62e-10 ***
# NFLafcnfc$Conference  0.02550    0.07271   0.351    0.728    
# ---
# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# Residual standard error: 0.2057 on 30 degrees of freedom
# Multiple R-squared:  0.004083,  Adjusted R-squared:  -0.02911 
# F-statistic: 0.123 on 1 and 30 DF,  p-value: 0.7283
# There is no statistical significance by Conference on PCT
# This is good as it means that they are equally challenged

# To further verify we can run an ANOVA model to determine
# if there are any group differences on PCT by AFC or NFC conference
PCTbyConferenceANOVAmodel<-aov(NFLafcnfc$PCT~NFLafcnfc$Conference)
summary(PCTbyConferenceANOVAmodel)
#                      Df Sum Sq Mean Sq F value Pr(>F)
# NFLafcnfc$Conference  1 0.0052 0.00520   0.123  0.728
# Residuals            30 1.2688 0.04229  
# The results confirm the previous findings.
