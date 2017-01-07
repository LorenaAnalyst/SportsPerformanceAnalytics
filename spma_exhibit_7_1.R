# Analyzing Baseball Player Performance (R)

##################################
##   Home Run Analytics R code  ##
##################################

# Reading in the data 
MLBdata<-read.csv('/Users/Desktop/MLBdata.csv')

# Get the summary of descriptives
summary(MLBdata)
# The output is shown here for demonstration purposes

# RK              Team          Pos           G        
# Min.   :  1.00          : 83          :83   Min.   :116.0  
# 1st Qu.: 36.25   CHC    :  7   RF     :21   1st Qu.:140.2  
# Median : 71.50   KC     :  7   1B     :20   Median :150.5  
# Mean   : 71.48   TOR    :  7   3B     :20   Mean   :147.3  
# 3rd Qu.:106.75   NYY    :  6   SS     :19   3rd Qu.:155.8  
# Max.   :142.00   OAK    :  6   2B     :18   Max.   :162.0  
# NA's   :83       (Other):109   (Other):44   NA's   :83     

# AB              R                H              X2B       
# Min.   :429.0   Min.   : 34.00   Min.   :101.0   Min.   :14.00  
# 1st Qu.:505.0   1st Qu.: 63.00   1st Qu.:133.0   1st Qu.:24.00  
# Median :549.0   Median : 73.00   Median :146.5   Median :29.00  
# Mean   :544.6   Mean   : 74.13   Mean   :148.5   Mean   :29.06  
# 3rd Qu.:586.0   3rd Qu.: 85.00   3rd Qu.:164.8   3rd Qu.:33.75  
# Max.   :638.0   Max.   :122.00   Max.   :205.0   Max.   :45.00  
# NA's   :83      NA's   :83       NA's   :83      NA's   :83     

# X3B              HR             RBI               BB        
# Min.   : 0.00   Min.   : 2.00   Min.   : 26.00   Min.   : 13.00  
# 1st Qu.: 1.00   1st Qu.:11.00   1st Qu.: 56.00   1st Qu.: 32.25  
# Median : 2.50   Median :17.00   Median : 70.50   Median : 45.00  
# Mean   : 3.12   Mean   :18.39   Mean   : 71.28   Mean   : 49.27  
# 3rd Qu.: 5.00   3rd Qu.:23.00   3rd Qu.: 84.00   3rd Qu.: 58.00  
# Max.   :12.00   Max.   :47.00   Max.   :130.00   Max.   :143.00  
# NA's   :83      NA's   :83      NA's   :83       NA's   :83      

# SO               SB               CS              AVG_       
# Min.   : 38.00   Min.   : 0.000   Min.   : 0.000   Min.   :0.2100  
# 1st Qu.: 85.25   1st Qu.: 2.000   1st Qu.: 1.000   1st Qu.:0.2560  
# Median :106.50   Median : 6.500   Median : 3.000   Median :0.2705  
# Mean   :109.34   Mean   : 9.859   Mean   : 3.711   Mean   :0.2719  
# 3rd Qu.:134.50   3rd Qu.:15.750   3rd Qu.: 5.750   3rd Qu.:0.2900  
# Max.   :208.00   Max.   :58.000   Max.   :20.000   Max.   :0.3380  
# NA's   :83       NA's   :83       NA's   :83       NA's   :83      

# OBP              SLG              OPS        
# Min.   :0.2580   Min.   :0.3200   Min.   :0.5870  
# 1st Qu.:0.3093   1st Qu.:0.3925   1st Qu.:0.7115  
# Median :0.3340   Median :0.4350   Median :0.7635  
# Mean   :0.3350   Mean   :0.4378   Mean   :0.7728  
# 3rd Qu.:0.3590   3rd Qu.:0.4733   3rd Qu.:0.8217  
# Max.   :0.4600   Max.   :0.6490   Max.   :1.1090  
# NA's   :83       NA's   :83       NA's   :83      

# Make simple plots to get a picture of your data
plot(MLBdata$Pos, MLBdata$R, main="Runs by Baseball Player Position", 
     xlab="Baseball Player Position", ylab="Number of Runs", pch=16)

plot(MLBdata$Pos, MLBdata$H, main="Hits by Baseball Player Position", 
     xlab="Baseball Player Position", ylab="Number of Hits", pch=16)

plot(MLBdata$Pos, MLBdata$HR, main="Home Runs by Baseball Player Position", 
     xlab="Baseball Player Position", ylab="Number of Home Runs", pch=16)

plot(MLBdata$Pos, MLBdata$RBI, main="RBI by Baseball Player Position", 
     xlab="Baseball Player Position", ylab="Number of RBIs", pch=16)

plot(MLBdata$Pos, MLBdata$SO, main="Strikeouts by Baseball Player Position", 
     xlab="Baseball Player Position", ylab="Number of Strikeouts", pch=16)

# There are evident differences in performance outcomes by player position.

