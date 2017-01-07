# Analyzing NBA Game Time Performance Data (R)

###########################################################
##   NBA Basketball Player Gametime Performance R code   ##
###########################################################

Basketball <- read.csv('/Users/Desktop/BasketballDatasetAnalysis.csv')

DRBmodel = aov(DRB ~ Position + Age , data = Basketball)
summary(DRBmodel)
# Results
#              Df Sum Sq Mean Sq F value Pr(>F)    
# Position      8   8185  1023.2   83.59 <2e-16 *** 
# Age           1      4     3.6    0.29   0.59    
# Residuals   332   4064    12.2                   

# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# Position is significant on DRB after we control for age
# Diagnosis of the ANCOVA model
# Check if the residuals follow normal distribution by ploting QQ plot
qqnorm(DRBmodel$residuals)    
# approximately follows a straight line

# Run a model on offensive rebounds by NBA player position
ORBmodel = aov(ORB ~ Position, data = Basketball)
summary(ORBmodel)
#               Df Sum Sq Mean Sq F value Pr(>F)    
# Position      8   3694   461.7   99.39 <2e-16 ***
# Residuals   333   1547     4.6                   

# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# There are significant differences by NBA player position on offensive rebounds

ASTmodel = aov(AST ~ Position, data = Basketball)
summary(ASTmodel)
#               Df Sum Sq Mean Sq F value Pr(>F)    
# Position      8  15884  1985.5   54.25 <2e-16 ***
# Residuals   333  12188    36.6                   

# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# There are significant differences by NBA player position on number of assists

STLmodel = aov(STL ~ Position, data = Basketball)
summary(STLmodel)
# Df Sum Sq Mean Sq F value  Pr(>F)    
# Position      8  22.11  2.7642   8.491 1.6e-10 ***
# Residuals   333 108.41  0.3255                    

# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# There are significant differences by NBA player position on number of steals

PERmodel = aov(PER ~ Position, data = Basketball)
summary(PERmodel)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# Position      8    564   70.47   4.405 4.42e-05 ***
# Residuals   333   5327   16.00                     

# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# There are significant differences by NBA player position on PER

Minutesmodel = lm(Minutes ~ Age, data = Basketball)
summary(Minutesmodel)
# Call:
# lm(formula = Minutes ~ Age, data = Basketball)

# Residuals:
# Min       1Q   Median       3Q      Max 
# -1394.30  -642.19     8.06   587.91  1484.41 

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1501.885    257.669   5.829  1.3e-08 ***
# Age            5.428      9.631   0.564    0.573    

# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# Residual standard error: 731.7 on 340 degrees of freedom
# Multiple R-squared:  0.0009333,  Adjusted R-squared:  -0.002005 
# F-statistic: 0.3176 on 1 and 340 DF,  p-value: 0.5734
# There are no significant differences by NBA player age on minutes played

TSmodel = aov(TS ~ Position, data = Basketball)
summary(TSmodel)
# Df Sum Sq  Mean Sq F value Pr(>F)  
# Position      8 0.0460 0.005745   2.557 0.0102 *
# Residuals   333 0.7482 0.002247                 

# Signif. codes: 0 *** 0.001 ** 0.01 * 0.05 . 0.1 1                  

# There are significant differences by NBA player position on TS%

# Plots
# Defensive Rebounds by NBA Player Position
pdf("plot_nba_defensive_rebounds.pdf", width = 9, height = 7)
par(las = 1, cex = 1.1, mar = c(5, 4, 4, 2) + 0.1)
boxplot (DRB ~ Position, data = Basketball, 
        xlab = "Basketball Player Position", ylab = "Defensive Rebounds")
dev.off()

# Assists by NBA Player Position
pdf("plot_nba_assists.pdf", width = 9, height = 7)
par(las = 1, cex = 1.1, mar = c(5, 4, 4, 2) + 0.1)
boxplot (AST ~ Position, data = Basketball, 
         xlab = "Basketball Player Position", ylab = "Assists")
dev.off()

# Offensive Rebounds by NBA Player Position
pdf("plot_nba_offensive_rebounds.pdf", width = 9, height = 7)
par(las = 1, cex = 1.1, mar = c(5, 4, 4, 2) + 0.1)
boxplot (ORB ~ Position, data = Basketball, 
         xlab = "Basketball Player Position", ylab = "Offensive Rebounds")
dev.off()

# Steals by NBA Player Position
pdf("plot_nba_steals.pdf", width = 9, height = 7)
par(las = 1, cex = 1.1, mar = c(5, 4, 4, 2) + 0.1)
boxplot (STL ~ Position, data = Basketball, 
         xlab = "Basketball Player Position", ylab = "Steals")
dev.off()

# Player Efficiency Rating by NBA Player Position
pdf("plot_nba_player_efficiency.pdf", width = 9, height = 7)
par(las = 1, cex = 1.1, mar = c(5, 4, 4, 2) + 0.1)
boxplot (PER ~ Position, data = Basketball, 
         xlab = "Basketball Player Position", 
         ylab = "Player Efficiency Rating")
dev.off()

# True Shooting Percentage by NBA Player Position
pdf("plot_nba_shooting_percentage.pdf", width = 9, height = 7)
par(las = 1, cex = 1.1, mar = c(5, 4, 4, 2) + 0.1)
boxplot (TS ~ Position, data = Basketball, 
         xlab = "Basketball Player Position", 
         ylab = "True Shooting Percentage")
dev.off()

# Although some models may not yield statistical significance, the findings
# are still very relevant to player position. The findings demonstrate the 
# importance of meaningful differences versus statistical significance.

