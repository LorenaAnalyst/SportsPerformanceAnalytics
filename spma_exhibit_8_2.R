# Analyzing UEFA Passes Attempted and Completed (R)

####################################
## Golden Goal Analytics R code   ##
####################################
# Reading in the data 
SoccerAssists <- read.csv('/Users/Desktop/goalassist.csv')
SoccerGoalsScored <- read.csv('/Users/Desktop/goalscored.csv')

# Checking the structure of the data
str(SoccerAssists)
str(SoccerGoalsScored)

# Changing the positions variable to character
SoccerAssists$Position <- as.character(SoccerAssists$Position)
SoccerGoalsScored$Position <- as.character(SoccerGoalsScored$Position)

# Reading in the data 
UEFAdefenders <- read.csv('UEFAdefense.csv')
UEFAgoalies <- read.csv('UEFAgoalies.csv')
UEFAmidfielders <- read.csv('UEFAmidfielder.csv')

# Combine the rows for the three files read in and named the new file uefa
uefa <- rbind(UEFAdefenders, UEFAgoalies, UEFAmidfielders) 
uefa$Position <- c(rep('Defender', nrow(UEFAdefenders)), 
                   rep('Goalie', nrow(UEFAgoalies)), 
                   rep('Midfielder', nrow(UEFAmidfielders)))

# Get rid of rows with NA (missing values) across 
uefa <- uefa[!is.na(uefa$PA), ] 

# Presents a summary of your data
summary(uefa)

###################################################################
##  UEFA Champions League Passes Attempted by Player Position    ##
###################################################################

# Use a linear model to regress player position on pass attempts
PassAttemptsbyPositionLinearModel1 <- lm(PA ~ Position, data = uefa)

# If you do not want to include outliers then 
# check the Median using tapply function because the 
# mean will be skewed by outliers
tapply(uefa$PA, uefa$Position, median) 
tapply(uefa$PA, uefa$Position, mean) 

# Kruskal Wallis Test (a non-parametric equivalent of anova)
kruskal.test(PA ~ factor(Position), data = uefa)

# The following output is displayed
# Kruskal-Wallis rank sum test
# data:  PA by factor(Position)
# Kruskal-Wallis chi-squared = 2.7198, df = 2, p-value = 0.2567
# There was no significant difference by player position on 
# passes attempted with the sample from this dataset. 
# Examining the boxplot below we can tell that certain positions 
# do attempt to pass more although the results 
# were not statistically significant.

# Figure
# Creates a space to save the figure
pdf('UEFAPA.pdf', height = 4, width = 4 * (1 + sqrt(5)) / 2) 
# Which variables to use
with(uefa, boxplot(PA ~ Position, 
                   # Changes x and y labels
                   xlab = 'Position', ylab = 'Soccer Player Position', 
                   # Color - see color chart
                   col = 'gray92', 
                   # Main title
                   main = 'Number of Passes Attempted by Player Position (UEFA)', 
                   # Size of x and y labels 
                   cex.lab = 1.2)) 
# Closes the figure space
dev.off() 

#################################################################
##  UEFA Champions League Passes Completed by Player Position  ##
#################################################################

# Use a linear model to regress player position on pass attempts
PassesCompletedbyPositionLinearModel1 <- lm(PC ~ Position, data = uefa)

# Get rid of rows with NA (missing values) across 
uefa <- uefa[!is.na(uefa$PC), ] 

# Use a linear model to regress player position on passes completed
PassAttemptsbyPositionLinearModel1 <- lm(PC ~ Position, data = uefa)

# If you do not want to include outliers then check the Median 
# using tapply function because the mean will be skewed by outliers.
tapply(uefa$PC, uefa$Position, median) 
tapply(uefa$PC, uefa$Position, mean) 

# Kruskal Test
kruskal.test(PC ~ factor(Position), data = uefa)

# The following output is displayed
# Kruskal-Wallis rank sum test
# data:  PC by factor(Position)
# Kruskal-Wallis chi-squared = 6.5932, df = 2, p-value = 0.03701

# There is a significant difference by player position 
# on passes completed p=0.03

# Figure
# Creates a space to save the figure
pdf('UEFApassescompleted.pdf', 
    height = 4, width = 4 * (1 + sqrt(5)) / 2) 
# Which variables to use
with(uefa, boxplot(PC ~ Position, 
                   # Changes x and y labels
                   xlab = 'Soccer Player Position',
                   # Color - see color chart
                   col = 'gray92', 
                   # Main title
                   main = 'Number of Passes Completed by Player Position (UEFA)', 
                   # Size of x and y labels
                   cex.lab = 1.2))  
# Closes the figure space
dev.off() 

