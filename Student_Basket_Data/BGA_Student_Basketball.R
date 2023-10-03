##################################################
#
# Black and Gold Analytics
#
# Sample Script to Demonstrate
# Logistic Regression 
# and the Linear Probability Model
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# October 3, 2023
#
##################################################
#
# BGA_Student_Basketball demonstrates examples of
#   a linear probability model and 
#   a logistic regression model
#   and then provides examples to consider 
#   the business value beyond the model prediction.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# You need to set the working directory to the location
# of your files.
# setwd("/path/to/your/folder")
# Find this path as follows:
# 1. Click on the "File" tab in the bottom right pane.
# 2. Browse to the folder on your computer that contains your R files.
# 3. Click the gear icon and choose the option "Set as Working Directory."
# 4. Copy the command from the Console in the bottom left pane.
# 5. Paste the command below:

setwd("C:/Users/le279259/OneDrive - University of Central Florida/Documents/Service/BGA_Club/Black_and_Gold_Analytics/Student_Basket_Data")

# Now, RStudio should know where your files are.


# The csv file used below must be in the working directory.
# If you an error message, make sure that the file is
# located in your working directory.
# Also make sure that the name has not changed.

# Load library for computing the AUROC.
# You will have to install this package the first time.
# Press the "Install" button or run the following (without the #):
# install.packages('pROC')
library(pROC)


##################################################
# Loading the Data
##################################################

# A sample dataset to predict patients' choice of hospital.
basket_data <- read.csv('BGA_basketball_data.csv')

# Inspect the contents.
summary(basket_data)

# Variables include:
# Made to indicate whether a player successfully made a basket,
#   0 if they missed
# Name is the name of the player taking the shot
# Height is the height of the player in inches
# Major is the the player's major at UCF
# Distance is the distance from the center of the hoop
#   from which the shot was taken

# Inspect the correlations between numeric explanatory variables.
cor(basket_data[, c('Made', 'Distance', 'Height')])
# Be aware of any explanatory variables that are highly correlated
# (both positively and negatively) with each other.

# Set Name and Major as categorical variables.
basket_data[, 'Name'] <- as.factor(basket_data[, 'Name'])
basket_data[, 'Major'] <- as.factor(basket_data[, 'Major'])


# Aside from any problems, inspect the data for correlation
# and take note to guide your modeling choices.

# Compare the distributions of variables
# for classes of the dependent variable.
summary(basket_data[basket_data[, 'Made'] == 0, ])
summary(basket_data[basket_data[, 'Made'] == 1, ])

# Differences in the two distributions should indicate
# the potential for prediction. 

# Restate in terms of Sink or Miss.
basket_data[, 'Result'] <- factor(NA, levels = c('Sink', 'Miss'))
sel_rows <- basket_data[, 'Made'] == 1
basket_data[sel_rows, 'Result'] <- 'Sink'
sel_rows <- basket_data[, 'Made'] == 0
basket_data[sel_rows, 'Result'] <- 'Miss'


##################################################
# Tabulate Shot Counts to Calculate Percentages
##################################################

# Tabulate by distance.
table(basket_data[, 'Distance'], 
      basket_data[, 'Result'], useNA = 'ifany')


# Aggregate the data to calculte averages by player.
agg_basket_data <- aggregate(x = Made ~ Name + Distance, 
                             data = basket_data, 
                             FUN = mean)


# Plot a boxplot to illustrate range of outcomes.
boxplot(Made ~ Distance, data = agg_basket_data,
        main = "Baskets sunk by Distance from Net",
        xlab = "Distance",
        ylab = "Perentage of Baskets")




# Tabulate by major.
table(basket_data[, 'Major'], 
      basket_data[, 'Result'], useNA = 'ifany')


# Plot a spinogram (requires library vcd).
library(vcd)
counts <- table(basket_data[, 'Major'], 
                basket_data[, 'Result'], useNA = 'ifany')
spine(counts, main = "Spinogram of Baskets sunk by Major")
# The width illustrates the number of players of each major.



##################################################
# Generating New Variables
##################################################

# You can create new variables in two ways:
# 1. Add commands within this program
# 2. Create new columns in a spreadsheet
#   (but you would need to re-load the dataset
#   after adding variables this way)


# You might consider transformations of height or distance, 
# such as taking square roots or logs or raising to a power.

# Examples:
basket_data[, 'Log_Distance'] <- log(basket_data[, 'Distance'])
basket_data[, 'Sq_Height'] <- basket_data[, 'Height']^2

# You could also create interaction terms.
basket_data[, 'Height_Distance'] <- basket_data[, 'Height']*basket_data[, 'Distance']



##################################################
# Calculating the expected point value
##################################################

# Note that the 20-foot, 25-foot and half-court shots
# will be worth 30 points. 

# Initialize points with baskets.
basket_data[, 'Points'] <- basket_data[, 'Made']*2

# Update points for three-point shots.
sel_rows <- basket_data[, 'Distance'] >= 20
basket_data[sel_rows, 'Points'] <- basket_data[sel_rows, 'Made']*3





##################################################
# Estimating a Linear Regression Model
# Model 1: Linear probability model for sinking a basket
##################################################

# Estimate a regression model.
lm_model_1 <- lm(data = basket_data,
                 formula = Made ~ Distance + Height) # Modify this line to change model.

# Output the results to screen.
summary(lm_model_1)

# Calculate the predictions of this model.
basket_data[, 'y_hat_lm'] <- predict(lm_model_1)

summary(basket_data[, 'y_hat_lm'])
# Does anything look unusual?

# If so, think about this when choosing your model.


##################################################
# Estimating a Logistic Regression Model
# Model 2: Logistic model for sinking a basket
##################################################

# Estimate a logistic regression model.
logit_model_1 <- glm(data = basket_data,
                     formula = Made ~ Distance + Height, # Modify this line to change model.
                     family = 'binomial')

# Output the results to screen.
summary(logit_model_1)


# Calculate the predictions of this model.
basket_data[, 'y_hat_logit'] <- predict(logit_model_1, type = 'response')

summary(basket_data[, 'y_hat_logit'])
# Does this look better?


##################################################
# Compare quality of prediction with AUROC
# The Area Under the ROC Curve
##################################################

# Calculate the AUROC for the logistic model.
roc(response = basket_data[, 'Made'],
    predictor = basket_data[, 'y_hat_logit'])


# Compare this to the estimate for the linear model.
roc(response = basket_data[, 'Made'],
    predictor = basket_data[, 'y_hat_lm'])

# A close race but maybe qualitative factors will settle the score.


##################################################
# Estimating a Logistic Regression Model
# Model 3: Logistic model for hospital choice
# Interaction between height and distance
##################################################

# Estimate a logistic regression model.
logit_model_2 <- glm(data = basket_data,
                     formula = Made ~ Distance + Height + Height_Distance, # Modify this line to change model.
                     family = 'binomial')

# Output the results to screen.
summary(logit_model_2)

# Calculate the predictions of this model.
basket_data[, 'y_hat_logit_2'] <- predict(logit_model_2, type = 'response')


# Calculate the AUROC for this logistic model.
roc(response = basket_data[, 'Made'],
    predictor = basket_data[, 'y_hat_logit_2'])

# A small improvement that might not be worth it.

# Try other models with additional variables
# or transformations of variables.

# Note that a nonlinear model will allow for some curvature
# and might suggest an interior solution
# for the choice of where to shoot.



##################################################
# Comparing the predictions to choose a distance
##################################################

# Your height is given and you are already enrolled in your major
# but you can choose where to stop dribbling the ball
# and take your shot at the basket. 
# From where should you shoot?

# Choose a range of distances to shoot from.
distance_list <- seq(1, 50, by = 1)

# Specify your height and the other relevant variables, 
# along with the full menu of distances.

# You must include a column for every variable in your model.
colnames(basket_data)

basket_predictions <- data.frame(Distance = distance_list, 
                                 Height = 67, 
                                 Height_Distance = 67*distance_list)

# Store the predictions from a model.
basket_predictions[, 'y_hat_logit_2'] <- predict(logit_model_2, 
                                                 newdata = basket_predictions, 
                                                 type = 'response')


plot(basket_predictions[, 'Distance'], 
     basket_predictions[, 'y_hat_logit_2'], 
     main = 'Probability of Sinking a Basket', 
     xlab = 'Distance', 
     ylab = 'Probability')

# Note that a more flexible model might allow
# for a different shape of predictions.

# Show the optimal shot for the given height.
# List in descending order of predicted probability of making the shot.
head(basket_predictions[order( - basket_predictions$y_hat_logit_2), ], 10)


# Consider the expected point value when making the decision
# of where to shoot.
basket_predictions[, 'Exp_Points'] <- basket_predictions[, 'y_hat_logit_2']*2
sel_rows <- basket_predictions[, 'Distance'] >= 20
basket_predictions[sel_rows, 'Exp_Points'] <- basket_predictions[sel_rows, 'y_hat_logit_2']*3

# Show the optimal shot for the given height, 
# in terms of number of points.
head(basket_predictions[order( - basket_predictions$Exp_Points), ], 10)
# Seems that, for someone of this height, 
# it is not worth going for the three-point shots.

# But, perhaps you will find a different answer with a different model
# or for a player of a different height.


##################################################
# Modeling the expected point value
# instead of the probability of sinking a basket
##################################################


# Estimate a logistic regression model.
lm_model_points_1 <- lm(data = basket_data,
                     formula = Points ~ Log_Distance + Sq_Height + Height_Distance)

# Output the results to screen.
summary(lm_model_points_1)



# You can improve on this model by adding other variables
# or transformations of variables.


# Store the predictions from this model.
# Requires that we add the extra variables.
basket_predictions[, 'Log_Distance'] <- log(basket_predictions[, 'Distance'])
basket_predictions[, 'Sq_Height'] <- basket_predictions[, 'Height']^2



basket_predictions[, 'y_hat_lm_points_1'] <- predict(lm_model_points_1, 
                                                 newdata = basket_predictions)

head(basket_predictions[order( - basket_predictions$y_hat_lm_points_1), ], 10)



##################################################
# End
##################################################
