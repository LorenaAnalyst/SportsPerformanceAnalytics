# Analyzing NBA Draft Data (R)

####################################
##   Slam Dunk Analytics R code   ##
####################################

# Read in the file 
Basketball<-read.csv('/Users/Desktop/BasketballDatasetAnalysis.csv')
# Get a glimpse at what is in the dataset (use the function summary)
summary(Basketball)

# To get the names of the variables use the following function
names(Basketball)
# [1]  "Player"   "Position"  "Age"      "Team"    "Games"    "Minutes"  
# [7]  "PER"      "TS"        "ORB"      "DRB"     "TRB"      "AST"      
# [13] "STL"      "BLK"       "TOV"      "USG"     "ORtg"     "DRtg"     
# [19] "OWS"      "DWS"       "WS"     

# Let us examine differences in number of assists by player position 
# after controlling for age
# To do this we will use an ANCOVA model
ancovabyage<-aov(AST~Position + Age, data=Basketball)

# Then use summary to see the actual results of the model you ran
summary(ancovabyage)

# Example of the output obtained from running an ANCOVA
#             Df   Sum Sq Mean Sq F value              Pr(>F)    
# Position      8  15884  1985.5  54.789 <0.0000000000000002 ***
# Age           1    157   157.1   4.336              0.0381 *  
# Residuals   332  12031    36.2                                

# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Let us run a multiple linear regression and compare our findings 
lmbyage<-lm(AST~Position + Age, data=Basketball)
summary(lmbyage)

# Call:
# lm(formula = AST ~ Position + Age, data = Basketball)

# Residuals:
# Min      1Q  Median      3Q     Max 
# -18.301  -3.912  -1.304   2.936  22.528 

# Coefficients:
#                Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)     3.4080     2.2367   1.524             0.128539    
# PositionPF      1.2393     1.0451   1.186             0.236522    
# PositionPF-SF  -2.8003     6.0693  -0.461             0.644815    
# PositionPG     19.1661     1.0814  17.724 < 0.0000000000000002 ***
# PositionSF      2.6826     1.0511   2.552             0.011155 *  
# PositionSF-PF  -2.3684     6.0786  -0.390             0.697057    
# PositionSG      6.6190     1.0544   6.277        0.00000000108 ***
# PositionSG-PG  22.4997     6.0693   3.707             0.000246 ***
# PositionSG-SF   3.7350     6.1002   0.612             0.540780    
# Age             0.1664     0.0799   2.082             0.038076 *  

# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 6.02 on 332 degrees of freedom
# Multiple R-squared:  0.5714,	Adjusted R-squared:  0.5598 
# F-statistic: 49.18 on 9 and 332 DF,  p-value: < 0.00000000000000022

# To check your reference code put the name of the dataset 
# followed by a dollar sign followed by the name of the variable
# within the summary function

summary(Basketball$Position)
# C    PF PF-SF    PG    SF SF-PF    SG SG-PG SG-SF 
# 61    73     1    63    71     1    70     1     1
# The reference group is C abbreviation for Center
# Run a model without player position
temp =  lm(AST~Age, data=Basketball)
# Check and see if there are significant differences between the model 
# that includes the covariate age or not 
# use the following code
anova(temp, lmbyage)

# The following output will be displayed

# Analysis of Variance Table
# Model 1: AST ~ Age
# Model 2: AST ~ Position + Age
# Res.Df   RSS Df Sum of Sq      F                Pr(>F)    
# 1    340 28000                                              
# 2    332 12031  8     15969 55.082 < 0.00000000000000022 ***
  
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##################################################
##  NBA Draft Performance and Player Positions  ##
##################################################

nbadraft<-read.csv('/Users/Desktop/nbadraft.csv')
summary(nbadraft)

# position       lane          shuttle   threequartersprint
# PF     :11   Min.   :10.27          : 3   Min.   :3.020     
# PG     :10   1st Qu.:10.77   3.12   : 3   1st Qu.:3.220     
# SG     :10   Median :11.11   3.15   : 3   Median :3.280     
# SF-SG  : 4   Mean   :11.15   2.88   : 2   Mean   :3.308   
# PG-SG  : 3   3rd Qu.:11.47   2.93   : 2   3rd Qu.:3.380     
# SF     : 3   Max.   :12.26   3.05   : 2   Max.   :3.620     
# (Other): 8   NA's   :1       (Other):34   
                   
# verticalleap   maxverticalleap     bench       
# Min.   :22.50   Min.   :25.0    Min.   : 0.000  
# 1st Qu.:26.50   1st Qu.:32.0    1st Qu.: 5.000  
# Median :28.50   Median :34.5    Median : 8.000  
# Mean   :29.58   Mean   :35.3    Mean   : 8.362  
# 3rd Qu.:32.50   3rd Qu.:38.0    3rd Qu.:11.500  
# Max.   :38.00   Max.   :44.0    Max.   :19.000  
#                                 NA's   :2   

# Make simple boxplots to get a picture of the different 
# physical measures by basketball player position

pdf('AgilitybyPlayerPosition.pdf', 
    height = 7, width = 11)
plot(nbadraft$position, nbadraft$lane, 
     main="Lane Agility by Player Position", 
     xlab="Basketball Player Position", ylab="Time in Seconds", pch=16)
dev.off()

pdf('VerticalLeapAnaerobicPowerbyPlayerPosition.pdf', 
    height = 7, width = 11)
plot(nbadraft$position, nbadraft$verticalleap, 
     main="Standing Vertical Leap by Player Position", 
     xlab="Basketball Player Position", ylab="Height Reached (inches)", pch=16)
dev.off()

pdf('MaxVerticalLeapAnaerobicPowerbyPlayerPosition.pdf', 
    height = 7, width = 11)
plot(nbadraft$position, nbadraft$maxverticalleap, 
     main="Max Vertical Leap by Player Position", 
     xlab="Basketball Player Position", ylab="Height Reached (inches)", pch=16)
dev.off()

pdf('BenchPressMuscularEndurancebyPlayerPosition.pdf', 
    height = 7, width = 11)
plot(nbadraft$position, nbadraft$bench, 
     main="Bench Press by Player Position", 
     xlab="Basketball Player Position", ylab="Number of Repetitions", pch=16)
dev.off()

pdf('ThreeQuarterSprintSpeedbyPlayerPosition.pdf', 
    height = 7, width = 11)
plot(nbadraft$position, nbadraft$threequartersprint, 
     main="Three Quarter Sprint by Player Position", 
     xlab="Basketball Player Position", ylab="Time in Seconds", pch=16)
dev.off()

pdf('ShuttleSpeedAnaerobicPowerbyPlayerPosition.pdf', 
    height = 7, width = 11)
plot(nbadraft$position, nbadraft$threequartersprint, 
     main="Shuttle Run by Player Position", 
     xlab="Basketball Player Position", ylab="Time in Seconds", pch=16)
dev.off()


# These are examples of various analyses of NBA draft data.
# As we can see from the box plots, there are substantial
# differences across positions on most physical performance
# tests. As with the NFL Combine data, there are also times
# when only one draft player at a particular position is tested.
# This explains the existence of horizontal lines in box plots.
# These are median lines with no surrounding boxes.
