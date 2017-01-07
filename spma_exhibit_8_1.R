# Analyzing UEFA  Assists and Goals Scored (R)

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

# Testing for a relationship between Time and Assists, 
# since there is no relationship using this dataset, 
# we can now ignore it in the regression model
AssistsTimeModel1 <- lm(Time ~ Position, data = SoccerAssists)

# Check summary for significance
summary(AssistsTimeModel1)

##################################################
##   UEFA Champions League Leaders in Assists   ##
##################################################

# Assists by position - simple linear regression model 
AssistsbyPositionLinearModel2 <- lm(Assists ~ Position, data = SoccerAssists)

# Check summary for significance
summary(AssistsbyPositionLinearModel2)

# Assists by position -  ANOVA
AssistsbyPositionANOVAModel3 <- aov(Assists ~ Position, data = SoccerAssists)

# Check summary for significance
summary(AssistsbyPositionANOVAModel3)

# Robust alternative - Kruskal Wallis use if data is not normally distributed
kruskal.test(Assists ~ factor(Position), data = SoccerAssists)

# Count data -> use a Poisson regression. Uses a log link to model data.
AssistsbyPositionsPoissonModel4 <- glm(Assists ~ Position, 
                                       data = SoccerAssists, family = poisson)

# Check summary for significance
summary(AssistsbyPositionsPoissonModel4)

# Examining the boxplot below we see that 
# there seem to be differences by player position.
# However they are not statistically significant. 
# The boxplot does show a trend of 
# greater number of assists by the midfielder position.

# Figure
# Creates a space to save the figure
pdf('SoccerAssists.pdf', height = 4, width = 4 * (1 + sqrt(5)) / 2) 
# Which variables to use
with(SoccerAssists, boxplot(Assists ~ Position, 
                       # Changes x and y labels
                       xlab = 'Soccer Player Position', ylab = 'Assists', 
                       # Names under the boxplots
                       names = c('Defender', 'Midfielder', 'Forward', 'Hybrid'), 
                       # Color - see color chart
                       col = 'gray92', 
                       # Main title
                       main = 'Number of Assists by Player Position (UEFA)', 
                       # Size of x and y labels 
                       cex.lab = 1.2)) 
# Closes the figure space
dev.off() 

########################################################
##   UEFA Champions League Leaders in Goals Scored    ##
########################################################

# Total by position - regression linear model (same as ANOVA)
SoccerGoalsScoredLinearModel1 <- lm(Total ~ Position, data = SoccerGoalsScored)
SoccerGoalsScoredANOVAModel2 <- aov(Total ~ Position, data = SoccerGoalsScored)

# Robust alternative - Kruskal Wallis since data is not normally distributed
kruskal.test(Total ~ factor(Position), data = SoccerGoalsScored)

# Count data -> use a Poisson regression. Uses a log link to model data.
SoccerGoalsScoredPoissonModel3 <- glm(Total ~ Position, 
                                      data = SoccerGoalsScored, family = poisson)
                                      
# Examining the boxplot below we see 
# that there seem to be differences by player position.
# However they are not statistically significant. 
# The boxplot does show a trend of greater number 
# of goals scored by the forward position.
# The lack of statistical significance could be due to the small sample size.
# These data are based on only the world leading scorers
# At that level midfielders also have a high scoring record.

# Figure
# Creates a space to save the figure
pdf('SoccerGoalsScored.pdf', height = 4, width = 4 * (1 + sqrt(5)) / 2) 
# Which variables to use
with(SoccerGoalsScored, boxplot(Total ~ Position, 
                       # Changes x and y labels         
                       xlab = 'Soccer Player Position', ylab = 'Total Goals', 
                       # Names under the boxplots
                       names = c('Midfielder', 'Forward', 'Hybrid'), 
                       # Color - see color chart
                       col = 'gray92', 
                       # Main title
                       main = 'Number of Goals Scored by Player (UEFA)', 
                       # Size of x and y labels 
                       cex.lab = 1.2)) 
# Closes the figure space
dev.off() 

#############################################################
##  UEFA Champions League Performance by Player Position   ##
#############################################################

# Reading in the data 
UEFAdefenders <- read.csv('/Users/Desktop/UEFAdefense.csv')
UEFAgoalies <- read.csv('/Users/Desktop/UEFAgoalies.csv')
UEFAmidfielders <- read.csv('/Users/Desktop/UEFAmidfielder.csv')

# Combine the rows for the three files read in and named the new file uefa
uefa <- rbind(UEFAdefenders, UEFAgoalies, UEFAmidfielders) 
uefa$Position <- c(rep('Defender', nrow(UEFAdefenders)), 
                   rep('Goalie', nrow(UEFAgoalies)), 
                   rep('Midfielder', nrow(UEFAmidfielders)))

# Get rid of rows with NA (missing values) across 
uefa <- uefa[!is.na(uefa$PA), ] 

# This presents a summary of your data.
summary(uefa)

