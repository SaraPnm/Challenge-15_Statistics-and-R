####-----MPG Regression-----
# Read the csv file
data <- read.csv('MechaCar_mpg.csv', stringsAsFactors = F)
head(data)

# Run correlation with mpg as dependent variable and others as independent variable
data_matrix = data.matrix(data[,c('vehicle.length','vehicle.weight','spoiler.angle','ground.clearance','AWD','mpg')])
data_matrix
cor(data_matrix)

# Run multiple linear regression analysis with mpg as dependent variable and others as independent variable
linear_reg_data <- lm(mpg~vehicle.length+vehicle.weight+spoiler.angle+ground.clearance+AWD, data)
summary(linear_reg_data)

# Run multiple linear regression analysis with mpg as dependent variable and ground clearance and vehicle length as independent variable
linear_reg_data2 <- lm(mpg~vehicle.length+ground.clearance, data)
summary(linear_reg_data2)



####-----Suspension Coil Summary-----
# Read the csv file
coil_data <- read.csv('Suspension_Coil.csv', stringsAsFactors = F)
head(coil_data)

# Create the function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the statistical parameters
summarize_coil_data <- coil_data %>% group_by(Manufacturing_Lot) %>% summarize(PSI_mean=mean(PSI), PSI_median=median(PSI), PSI_variance=var(PSI), PSI_sd=sd(PSI), PSI_mode=getmode(PSI))
summarize_coil_data



####-----Suspension Coil t-Test-----
t.test(coil_data$PSI, mu=1500, alternative = 'two.sided')
