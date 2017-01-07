# Analyzing Tennis Player Earnings (R)

#######################################
## Game, Set, Match Analytics R code ##
#######################################

library(MASS)
library(scales)

# Read in the data 
setwd()
ATP <- read.csv('/Users/Desktop/ATPearnings.csv')
WTA <- read.csv('/Users/Desktop/WTAearnings.csv')

# Get descriptives
summary(ATP)
summary(WTA)

#########################
## Earnings by Country ##
#########################

# Merge the rows from the ATP and WTA
MergedATPWTA <- rbind(ATP, WTA)

# The variable country within the Mergedfile, make it a character
MergedATPWTA$Country = as.character(WTA$Country)
group = NULL
for(i in 1:nrow(MergedATPWTA)){
  if (MergedATPWTA$Country[i] %in% 
        names(table(MergedATPWTA$Country))
      [table(MergedATPWTA$Country) >= 9]){
    group[i] = MergedATPWTA$Country[i]
  } else{
    group[i] = "Others"
  }
}
MergedATPWTA$group = as.character(group)

EarningsbyCountryLinearModel1 <- lm(Earnings ~ group, data = MergedATPWTA)

# Diagnostics for this model are poor
# outliers having an effect on the fit
summary(EarningsbyCountryLinearModel1)

# Call:
# lm(formula = Earnings ~ group, data = MergedATPWTA)

# Residuals:
# Min      1Q  Median      3Q     Max 
# -883123 -339451 -141100  130104 9326844 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)
# (Intercept)      435176     319946   1.360    0.175
# Belgium         -131998     452473  -0.292    0.771
# Croatia         -158764     452473  -0.351    0.726
# Czech Republic   199282     364795   0.546    0.585
# France           109632     452473   0.242    0.809
# Germany            9787     375170   0.026    0.979
# Italy            -10583     391853  -0.027    0.978
# Kazakhstan       -64606     452473  -0.143    0.887
# Others           -59105     341064  -0.173    0.863
# Romania          302848     404704   0.748    0.455
# Russia           473017     382409   1.237    0.217
# Serbia            30346     423249   0.072    0.943
# Slovakia        -104775     423249  -0.248    0.805
# Spain            482146     423249   1.139    0.256
# Switzerland      413441     452473   0.914    0.362
# USA              542344     357711   1.516    0.131

# Residual standard error: 959800 on 284 degrees of freedom
# Multiple R-squared:  0.06122,  Adjusted R-squared:  0.01163 
# F-statistic: 1.235 on 15 and 284 DF,  p-value: 0.2448

# Taking a look at the summary statistics we can see that
# The countries leading in earnings are USA, Spain, Russia, and Switzerland
# Three ways to deal with it:

# Perform a log transformation
EarningsbyCountryLinearModel1log <- lm(log(Earnings) ~ group, 
                                       data = MergedATPWTA)

# Use a non-parametric test
kruskal.test(Earnings ~ factor(group), data = MergedATPWTA)

# Utilize a Negative Binomial model
EarningsbyCountryLinearModel1nb <- glm.nb(Earnings ~ group, 
                                          data = MergedATPWTA) 
# uses a log link. Use exp() to get covariates back on the original scale

# To extrapolate and interpret findings use the following code
# For example, USA vs. Switzerland 
exp(coef(EarningsbyCountryLinearModel1nb)[16] - 
    coef(EarningsbyCountryLinearModel1nb)[15]) 
# Means that USA makes about 15% more than Switzerland

# Figure
# Creates a space to save the figure
pdf('EarningsbyCountry.pdf', height = 8, width = 6) 
par(mar = c(5.1, 6.8, 4.1, 2.1))
# Which variables to use
with(MergedATPWTA, boxplot(log(Earnings) ~ group,
                   # Changes x and y labels
                   xlab = '', ylab = '', 
                   # Color - see color chart
                   col = 'gray92', 
                   # Main title
                   main = 'Earnings by Country', 
                   # Size of x and y labels
                   cex.lab = 1.2,
                   # Makes the plot horizontal
                   horizontal = T, 
                   # Removes axis labels / tick marks
                   yaxt = 'n', xaxt = 'n'))  
# adds in vertical lines
abline(v = log(c(125000, 250000, 500000, 1000000, 2500000, 
                 5000000, 10000000)),
       col = 'lightgray', lty = 'dotted') 
# Which variables to use
with(MergedATPWTA, boxplot(log(Earnings) ~ group, 
                   # Changes x and y labels
                   xlab = '', ylab = '', 
                   # Color - see color chart
                   col = 'gray92', 
                   # Main title
                   main = 'Earnings by Country', 
                   # Size of x and y labels
                   cex.lab = 1.2,
                   # Makes the plot horizontal
                   horizontal = T, 
                   # Removes axis labels / tick marks
                   yaxt = 'n', xaxt = 'n',
                   # Re-draws the boxplots over the lines
                   add = T)) 
# Adds tick marks
axis(side = 1, at = log(c(125000, 250000, 500000, 1000000, 2500000, 
                          5000000, 10000000)),
     lab = FALSE) 
text(x = log(c(125000, 250000, 500000, 1000000, 2500000, 
               5000000, 10000000)), 
    labels = paste('$', c("125K", "250K", "500K", "1M", 
                           "2.5M", "5M", "10M"), sep = ''), 
    srt = 90, y = par("usr")[3] - 1.3, xpd = TRUE)
axis(side = 2, at = 1:16, label = rep('', 16))
text(y = 1:16, x = 10.2,
     labels = sort(unique(group)),
     xpd = TRUE) 
# All of this is to make those plots
# Closes the figure space
dev.off()

