# Professional Tennis Player Earnings by Rank and Sex (R)

#######################################
## Game, Set, Match Analytics R code ##
#######################################

library(ggplot2)

# Reading in the data 

ATP <- read.csv('/Users/Desktop/ATPtop100earnings.csv')
WTA <- read.csv('/Users/Desktop/WTAtop100earnings.csv')

# Add Sex to data frames
ATP$Sex <- rep("Men", length = nrow(ATP))
WTA$Sex <- rep("Women", length = nrow(WTA))

players <- rbind(ATP, WTA)
players$Sex <- factor(players$Sex)
players$EarningsMM <- players$Earnings/1000000

# Examine the structure of the combined data frame
print(str(players))

pdf("fig_ysports_08_earnings_by_sex.pdf", width = 7, height = 7)
# Scatter plot of Earnings against Rank by Sex
ggplot_object <- ggplot(data = players, 
    aes(x = Rank, y = EarningsMM, shape = Sex,
        colour = Sex)) +
    geom_point(size = 3) +
    scale_shape_manual(values = c(5, 1)) +
    scale_colour_manual(values = c("darkred", "darkblue")) +
    labs(x = "Professional Tennis Rankings", 
         y = "Annual Earnings ($ millions)")
print(ggplot_object)    
dev.off()

# This graph shows shows that men and women are similar in 
# annual earnings overall. In comparison to other sports, however,
# there is greater inequality between the top ten ranked
# players and lower ranked players.

