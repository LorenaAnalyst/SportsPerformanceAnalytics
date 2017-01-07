# MLB Batting Performance by Player Position (R)

###########################################################################
##   MLB Player Game Time  Performance Correlation and Regression R code ##
###########################################################################
# Reading in the data 
MLBdata<-read.csv('/Users/Desktop/MLBdata.csv')

# Let us examine some gametime performance variables with Rank
# Run a correlation using the code below
# Examine the relationship between Rank and Hits
cor.test(MLBdata$RK, MLBdata$H)
# The results of running a correlation yield the following output
# Pearson's product-moment correlation
# data:  MLBdata$RK and MLBdata$H
# t = -14.3143, df = 140, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.8301014 -0.6941799
# sample estimates:
# cor 
# -0.7707694 

# Now let us take a look at a scatterplot
plot(MLBdata$RK, MLBdata$H,
main="Correlation MLB Rank with Hits",
         xlab = "MLB Rank", ylab = "Hits")

# Since there was a strong correlation let us run a regression model
# To determine how much variance in Rank is accounted for by hits.
# Use the following code
RankbyHitslinearmodel<-lm(MLBdata$RK~MLBdata$H)
summary(RankbyHitslinearmodel)

# Call:
# lm(formula = MLBdata$RK ~ MLBdata$H)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -75.388 -16.708   2.953  20.246  57.880 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 282.67155   14.91804   18.95   <2e-16 ***
# MLBdata$H   -1.42265    0.09939  -14.31   <2e-16 ***
# ---
# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# Residual standard error: 26.3 on 140 degrees of freedom
# Multiple R-squared:  0.5941,  Adjusted R-squared:  0.5912 
# F-statistic: 204.9 on 1 and 140 DF,  p-value: < 2.2e-16
# The linear regression model revealed that there is a 
# statistically significant variance accounted for by number of hits.
# This can be interpreted as 59% of the variance in Rank is accounted for
# by number of hits.

# Let us examine the association between Rank and Home Runs.
cor.test(MLBdata$RK, MLBdata$HR)
# Pearson's product-moment correlation
# data:  MLBdata$RK and MLBdata$HR
# t = 0.0882, df = 140, p-value = 0.9299
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.1574690  0.1719677
# sample estimates:
# cor 
# 0.007451552 

# Let us get a better picture by plotting the data
plot(MLBdata$RK, MLBdata$HR,
main="Correlation MLB Rank with Home Runs",
         xlab = "MLB Rank", ylab = "Home Runs")

# Let us run a regression model
RankbyHomeRunslinearmodel<-lm(MLBdata$RK~MLBdata$HR)
summary(RankbyHomeRunslinearmodel)
# Call:
# lm(formula = MLBdata$RK ~ MLBdata$HR)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -70.467 -35.513  -0.053  35.172  70.296 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 70.93497    7.07458  10.027   <2e-16 ***
# MLBdata$HR  0.02958    0.33549   0.088     0.93    
# ---
# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# Residual standard error: 41.27 on 140 degrees of freedom
# Multiple R-squared:  5.553e-05,  Adjusted R-squared:  -0.007087 
# F-statistic: 0.007774 on 1 and 140 DF,  p-value: 0.9299
# The linear regression model was not statistically significant

# Now let us examine the relationship between Rank and RBIs.
cor.test(MLBdata$RK, MLBdata$RBI)
# Pearson's product-moment correlation
# data:  MLBdata$RK and MLBdata$RBI
# t = -2.2021, df = 140, p-value = 0.0293
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.33752164 -0.01880671
# sample estimates:
# cor 
# -0.1829672 

# Let us examine the plot
plot(MLBdata$RK, MLBdata$RBI,
     main="Correlation MLB Rank with RBIs",
     xlab = "MLB Rank", ylab = "RBIs")

# Run a regression model on Rank by RBIs
RankbyRBIslinearmodel<-lm(MLBdata$RK~MLBdata$RBI)
summary(RankbyRBIslinearmodel)

# Call:
# lm(formula = MLBdata$RK ~ MLBdata$RBI)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -78.949 -29.746   0.103  34.583  73.409 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   98.1805    12.5947   7.795 1.32e-12 ***
# MLBdata$RBI  -0.3746     0.1701  -2.202   0.0293 *  
# ---
# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# Residual standard error: 40.58 on 140 degrees of freedom
# Multiple R-squared:  0.03348,  Adjusted R-squared:  0.02657 
# F-statistic: 4.849 on 1 and 140 DF,  p-value: 0.0293
# The linear regression model revealed that there is a 
# statistically significant variance accounted for by RBIs.
# This can be interpreted as 3% of the variance in Rank is 
# accounted for by RBI.

# Let us Examine the association between Rank and OBP
cor.test(MLBdata$RK, MLBdata$OBP)
# Pearson's product-moment correlation
# data:  MLBdata$RK and MLBdata$OBP
# t = -11.5397, df = 140, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.7739211 -0.6028090
# sample estimates:
# cor 
# -0.6982052 

# Now let us make a plot
plot(MLBdata$RK, MLBdata$OBP,
     main="Correlation MLB Rank with OBP",
     xlab = "MLB Rank", ylab = "OBP")

# Run a linear regression model on Rank by OBP
RankbyOBPlinearmodel<-lm(MLBdata$RK~MLBdata$OBP)
summary(RankbyOBPlinearmodel)
# Call:
# lm(formula = MLBdata$RK ~ MLBdata$OBP)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -53.012 -21.558  -2.978  18.228  79.611 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    346.94      24.00   14.46   <2e-16 ***
# MLBdata$OBP  -822.27      71.26  -11.54   <2e-16 ***
# ---
# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# Residual standard error: 29.55 on 140 degrees of freedom
# Multiple R-squared:  0.4875,  Adjusted R-squared:  0.4838 
# F-statistic: 133.2 on 1 and 140 DF,  p-value: < 2.2e-16
# The linear regression model revealed that there is a 
# statistically significant variance accounted for by OBP on Rank.
# This can be interpreted as 49% of the variance in Rank is 
# accounted for by OBP.

# Examine the relationship between Rank and AVG.
cor.test(MLBdata$RK, MLBdata$AVG_)
# Pearson's product-moment correlation
# data:  MLBdata$RK and MLBdata$AVG_
# t = -61.8568, df = 140, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.9871974 -0.9752558
# sample estimates:
# cor 
# -0.9821926 
# This is a very strong correlation

# Plot the data
plot(MLBdata$RK, MLBdata$AVG_,
     main="Correlation MLB Rank with AVG",
     xlab = "MLB Rank", ylab = "AVG")
# This plot is almost perfect

# Run a regression model on Rank by AVG.
RankbyAVGlinearmodel<-lm(MLBdata$RK~MLBdata$AVG_)
summary(RankbyAVGlinearmodel)
# Call:
# lm(formula = MLBdata$RK ~ MLBdata$AVG_)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -27.773  -5.945  -1.770   5.792  34.342 
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     503.008      7.007   71.79   <2e-16 ***
# MLBdata$AVG_ -1586.834     25.653  -61.86   <2e-16 ***
# ---
# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# Residual standard error: 7.754 on 140 degrees of freedom
# Multiple R-squared:  0.9647,  Adjusted R-squared:  0.9645 
# F-statistic:  3826 on 1 and 140 DF,  p-value: < 2.2e-16
# As evident from the plot AVG is a great predictor of Rank.
# The linear regression model revealed that there is a 
# statistically significant variance accounted for by AVG on Rank.
# This can be interpreted as 96% of the variance in Rank 
# is accounted for by AVG.

# Run a correlation on Rank and OPS.
cor.test(MLBdata$RK, MLBdata$OPS)
# Pearson's product-moment correlation
# data:  MLBdata$RK and MLBdata$OPS
# t = -8.1836, df = 140, p-value = 1.521e-13
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.6707184 -0.4458943
# sample estimates:
# cor 
# -0.5688397 

# Plot Rank by OPS
plot(MLBdata$RK, MLBdata$OPS,
     main="Correlation MLB Rank with OPS",
     xlab = "MLB Rank", ylab = "OPS")

# Run a regression model on Rank by OPS
RankbyOPSlinearmodel<-lm(MLBdata$RK~MLBdata$OPS)
summary(RankbyOPSlinearmodel)
# Call:
# lm(formula = MLBdata$RK ~ MLBdata$OPS)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -71.644 -24.634  -1.681  23.856  80.445 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    274.99      25.03  10.986  < 2e-16 ***
# MLBdata$OPS  -263.35      32.18  -8.184 1.52e-13 ***
# ---
# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# Residual standard error: 33.94 on 140 degrees of freedom
# Multiple R-squared:  0.3236,  Adjusted R-squared:  0.3187 
# F-statistic: 66.97 on 1 and 140 DF,  p-value: 1.521e-13
# The linear regression model revealed that there is a 
# statistically significant variance accounted for by OPS on Rank.
# This can be interpreted as 32% of the variance in Rank 
# is accounted for by OPS.

# Examine Rank and SLG.
cor.test(MLBdata$RK, MLBdata$SLG)
# Pearson's product-moment correlation
# data:  MLBdata$RK and MLBdata$SLG
# t = -5.4978, df = 140, p-value = 1.768e-07
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.5480639 -0.2757957
# sample estimates:
# cor 
# -0.4213792 

# Make a plot 
plot(MLBdata$RK, MLBdata$SLG,
     main="Correlation MLB Rank with SLG",
     xlab = "MLB Rank", ylab = "SLG")

# Conduct a linear regression on Rank by SLG
RankbySLGlinearmodel<-lm(MLBdata$RK~MLBdata$SLG)
summary(RankbySLGlinearmodel)
# Call:
# lm(formula = MLBdata$RK ~ MLBdata$SLG)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -74.999 -26.424  -1.709  28.642  70.927 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    193.63      22.44   8.629 1.22e-14 ***
# MLBdata$SLG  -279.03      50.75  -5.498 1.77e-07 ***
# ---
# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# Residual standard error: 37.43 on 140 degrees of freedom
# Multiple R-squared:  0.1776,  Adjusted R-squared:  0.1717 
# F-statistic: 30.23 on 1 and 140 DF,  p-value: 1.768e-07
# The linear regression model revealed that there is a 
# statistically significant variance accounted for by SLG on Rank.
# This can be interpreted as 18% of the variance in Rank 
# is accounted for by SLG.

# Let us run a multiple linear regression using 
# all the predictor variables we examined using the following code
mlr<-lm(MLBdata$RK ~ MLBdata$H + MLBdata$HR + MLBdata$RBI + 
          MLBdata$OBP + MLBdata$AVG_ + MLBdata$OPS + MLBdata$SLG)
# Get the summary
summary(mlr)
# Call:
# lm(formula = MLBdata$RK ~ MLBdata$H + MLBdata$HR + MLBdata$RBI + 
#        MLBdata$OBP + MLBdata$AVG_ + MLBdata$OPS + MLBdata$SLG)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -29.502  -5.891  -1.577   5.220  33.215 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    5.028e+02  7.684e+00  65.431   <2e-16 ***
# MLBdata$H    -1.302e-02  6.162e-02  -0.211    0.833    
# MLBdata$HR    6.847e-02  2.887e-01   0.237    0.813    
# MLBdata$RBI  -5.205e-02  6.660e-02  -0.782    0.436    
# MLBdata$OBP   1.160e+03  1.366e+03   0.849    0.397    
# MLBdata$AVG_ -1.580e+03  9.130e+01 -17.304   <2e-16 ***
# MLBdata$OPS  -1.151e+03  1.363e+03  -0.844    0.400    
# MLBdata$SLG   1.150e+03  1.359e+03   0.846    0.399  
# ---
# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# Residual standard error: 7.871 on 134 degrees of freedom
# Multiple R-squared:  0.9652,  Adjusted R-squared:  0.9634 
# F-statistic: 530.8 on 7 and 134 DF,  p-value: < 2.2e-16
# The results of this multiple linear regression show that 
# AVG is still the most important variable for predicting Rank
# after controlling for the other performance variables.

# The results of all these analyses reveal that it is important
# to understand not only the variables given but the sport and 
# how the combination of several performance variables can modify your model.

