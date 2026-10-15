#############################  Linear Regression Model #######################################################


#### A. DATA LOADING, EXPLORING AND PREPARATION----

# Goal: Clean the data, understand it, and inspect anything that could affect model validity.
# Step A1: Load data
df <- read.csv('Regression_Analysis.csv')
#*********************************************************************************************
# Step A2: Inspect the data   # identify the dependent and independent variables
str(df)          # variable types
summary(df)      # variable summaries
head(df)         # sample of the data
colSums(is.na(df))  # missing values check
colnames(df)
#*********************************************************************************************

# Step A3: Visualize relationships and linearity 
attach(df)

plot(TV.Ads.Spend..in..1000s., Sales..in..1000s.)
abline(lm(Sales..in..1000s. ~ TV.Ads.Spend..in..1000s.))

plot(Social.Media.Ads.Spend..in..1000s., Sales..in..1000s. )
abline(lm(Sales..in..1000s. ~ Social.Media.Ads.Spend..in..1000s.))

plot(Email.Campaign.Spend..in..1000s. , Sales..in..1000s. )
plot(Influencer.Marketing.Spend..in..1000s., Sales..in..1000s. )
#*********************************************************************************************
# Step A4: Check for outliers
# we check for outliers in dependent variable because extreme values in the outcome can pull 
# the regression line and skew model fit.

boxplot(Sales..in..1000s.)

boxplot(Social.Media.Ads.Spend..in..1000s.)
boxplot(TV.Ads.Spend..in..1000s.)
boxplot(Email.Campaign.Spend..in..1000s.)
boxplot(Influencer.Marketing.Spend..in..1000s.)

# How to identify the outliers rows and remove them from Sales
Q1 <- quantile(Sales..in..1000s., 0.25)
Q3 <- quantile(Sales..in..1000s., 0.75)
IQR_value <- Q3 - Q1

# Define outlier thresholds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Find row numbers of outliers

outliers <- which(Sales..in..1000s. < lower_bound | Sales..in..1000s. > upper_bound)
outliers

# remove Outliers
df_no_outliers <- df[Sales..in..1000s. >= lower_bound & Sales..in..1000s. <= upper_bound, ]

# replace outliers, if we are for sure that there is a data entry error.
df_new <- df
df_new$Sales..in..1000s.[7] <- 332.1219

head(df_new,7)
df_new$Sales..in..1000s.[7]
#*********************************************************************************************

# Step A5: Check correlation between predictors
cor_table <- cor(df_no_outliers[, c("TV.Ads.Spend..in..1000s.", "Email.Campaign.Spend..in..1000s.",
           "Social.Media.Ads.Spend..in..1000s.","Influencer.Marketing.Spend..in..1000s.")])
# If two predictors are highly correlated (say > 0.8), you might need to drop one
#*********************************************************************************************
#### B. FITTING THE MODEL----
# Now, based on your EDA, you choose the predictors and fit a linear model.

# Step B1: Fit a linear regression model where Sales is predicted by amounts of four Ads spend
attach(df_no_outliers)
model <- lm(Sales..in..1000s. ~ TV.Ads.Spend..in..1000s. + Social.Media.Ads.Spend..in..1000s.
            + Email.Campaign.Spend..in..1000s.+ Influencer.Marketing.Spend..in..1000s., data = df_no_outliers)
summary(model)

#*********************************************************************************************
# Interpret the results

# 1- Residuals: are the differences between the actual and predicted values.
#    a- The model is generally accurate, with most prediction errors (residuals) within about ±7 units.
#    b- There are a few larger outliers: one where the model was off by –64 (underprediction) 
#       and another by +40.5 (overprediction).
#    c- The median residual is close to zero, which means the model doesn’t systematically over or underpredict.

# 2- Coefficient Table:
#     a- Every $1,000 you spend contributes positively to sales, especially on TV and Social media marketing.
#     b- TV Ads and Social Media give the biggest sales increase. Email marketing is still useful, just less impact.
#     c- the standard error is 0.1683, meaning if you repeated this study many times, 
#        the coefficient might vary by ±0.17 (on average).
#     d- all p-values are less than 0.05 => statistically significant
#     e- All predictors have small standard errors, which means estimates are precise.

# 3- Model Fit:
#     a- The model explains nearly 85% of Sales variation, which is excellent in business contexts.
#     b- F- statistic is high and p-value is low so all ad types significantly affect Sales
#     c- A lower Residual standard error means more accurate predictions.
#     e- Multiple R-squared: 0.8504, the combination of the four ad spend variables very effectively predicts Sales.
#        R² always increases (or stays the same) when you add more predictors — even if those predictors are irrelevant.
#     f- Adjusted R-squared: 0.8439, Since your adjusted R² is still very high, it means:
#        predictors are all useful, and the model is not overfitting.
#        It increases only if a new predictor genuinely improves the model more than expected by chance.
#        It can decrease if the new variable adds no meaningful value.

#      NOTE : F=(1−R2)/(n−k−1)(R2/k)  where df= n-(k+1) where k+1 is four predictors + intercept => (98-4-1)

#    |  F-Statistic |	   Interpretation
#     **************     ***************
#    |      ≈ 1     |	   Model explains almost nothing (not significant)
#    |        5     |    Usually suggests a meaningful model
#    |        10    |    Strong evidence the model is significant 
#    |        50    |    Very strong model 
#    |        100   |    Extremely strong model 

#*************************************** Conclusion ******************************************
# The regression model is statistically significant and explains about 85% of the variation in the outcome, 
# with a very strong F-statistic and extremely small p-value, showing that the predictors collectively have a 
# powerful effect.
#*********************************************************************************************
# Insights:
# 1- Prioritize TV advertising to maximize your sales gains per dollar spent.
# 2- Invest strategically in social media and influencer marketing as important secondary channels.
# 3- Continue using email campaigns to support sales growth, but consider them more as a supplement rather than the primary driver.
# 4- Since the model fits very well, you can rely on these estimates to forecast sales based on advertising budgets.

#### C. CHECK MODEL ASSUMPTIONS----

# Step 1: Linearity 
# Fitted values = What your model predicts for each data point.
# Residuals = How far off those predictions are from the real observed data.
plot(model, which=1)

# 1- the mean of the residuals is nearly zero for all of residuals, this confirm linearity
# 2- Should show no clear pattern. If it curves, linearity may be violated
#*********************************************************************************************
# Step 2: Normality of residuals

hist(model$residuals, main = "Histogram of Residuals",freq = FALSE)
# Visualizes the distribution of residuals. it should look roughly bell-shaped (normal), centered around zero.
# Add density curve
lines(density(model$residuals), col = "blue", lwd = 2)

plot(model, which=2)
# Plots residuals against a theoretical normal distribution.
# If points lie close to the red line, residuals are approximately normal.

shapiro.test(model$residuals)
# The null hypothesis of the Shapiro-Wilk test is that the residuals are normally distributed.
# Since the p-value is much less than 0.05, you reject the null hypothesis.
# This means your residuals deviate from normality.
# for high number of observations(100 and more) shapiro test is very sensitive even if it 
# deviates slightly from normality, in this case we can go by qqplots
#*********************************************************************************************
# Step 3: Homoscedasticity
plot(model, which=1)
# The spread of the residuals should not get larger or smaller as the predicted values (fitted values) change.
# The error for low predictions and high predictions should be roughly equal.
#*********************************************************************************************
# Step 4: correlation
cor_table <- cor(df_no_outliers[, c("TV.Ads.Spend..in..1000s.", "Email.Campaign.Spend..in..1000s.",
                                    "Social.Media.Ads.Spend..in..1000s.","Influencer.Marketing.Spend..in..1000s.")])
cor_table
install.packages("car")
library(car)
vif(model)    #variance inflation factor
# measures how much the variance of a regression coefficient is increased due to collinearity
# (correlation) with other predictors. since all nearly equal to 1, means that there are no correlations
# usually anything less than 5 is generally OK.
#*********************************************************************************************
# Step 5: Independence - Durbin Watson Test
# One residual should not be influenced by or correlated with another.
install.packages("lmtest")
library(lmtest)
dwtest(model)
# If p-value is lower than 0.05 it means that residuals are correlated
#*********************************************************************************************
#### D. PREDICTIVE ANALYSIS----

# use our model for prediction
predict(model, newdata = data.frame(TV.Ads.Spend..in..1000s. = 70, Social.Media.Ads.Spend..in..1000s.
                                    = 40, Email.Campaign.Spend..in..1000s.=10, 
                                    Influencer.Marketing.Spend..in..1000s. =10 ))
#*********************************************************************************************
#### E. VISUALIZING THE REGRESSION MODEL----
library(ggplot2)
attach(df_no_outliers)

ggplot(df_no_outliers, aes(x= Sales..in..1000s. , y= TV.Ads.Spend..in..1000s. )) +
  geom_point(color = "darkred", size = 1, alpha = 0.7, shape = 16) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  #adds a straight best-fit line (Y = a + bX).
  labs(title = "Regression Line between X and Y",          # shaded area represents the area where the regression line would be in 95% confidence
       x = "Sales..in..1000s.", y = "TV.Ads.Spend..in..1000s.") #For a given TV ad spend, we’re 95% confident that the true mean sales value lies within the shaded area around the blue regression line.

# Near the center of your data (where there are many observations), the band is narrow — more data → less uncertainty.
# Toward the edges (extrapolation), the band widens because there’s less data → more uncertainty in the model’s prediction.
#*********************************************************************************************
# 3D visualization
# Step 1: Create the scatterplot
library(scatterplot3d)
attach(df_no_outliers)

s3d <- scatterplot3d(
  TV.Ads.Spend..in..1000s.,
  Social.Media.Ads.Spend..in..1000s.,
  Sales..in..1000s.,
  pch = 16,
  color = "blue",
  main = "3D Scatter: Sales vs TV & Social Media Spend",
  xlab = "TV Ads Spend (in 1000s)",
  ylab = "Social Media Ads Spend (in 1000s)",
  zlab = "Sales (in 1000s)"
)

# Step 2: Fit the regression model
model <- lm(Sales..in..1000s. ~ TV.Ads.Spend..in..1000s. + Social.Media.Ads.Spend..in..1000s., data = df_no_outliers)

# Step 3: Add regression plane
s3d$plane3d(model, col = "red", lty = 2, draw_polygon = TRUE) # The fitted linear model object (from lm()), which provides the intercept and slope values
                                                              # Specifies the line type used to draw the plane’s borders — 2 means dashed. (Other options: 1 = solid, 3 = dotted)
                                                              # Fills the plane as a semi-transparent polygon (instead of drawing only its outline)
#*********************************************************************************************
#### F. ADDITIONAL STEPS TO IMPROVE OUR MODEL OUTPUT----

# 1- identify Influential Points that are influencing the model, so we can make our model more precise
plot(model, which = 4) # Cook's distance #Points with high Cook’s distance may influence the model.
plot(model, which = 1) # residuals vs fitted

# 2- or Model Improvement
# If an assumption is violated: Try transforming the dependent variable:
model2 <- lm(log(Sales..in..1000s.) ~ TV.Ads.Spend..in..1000s. + Social.Media.Ads.Spend..in..1000s.
             + Email.Campaign.Spend..in..1000s. + Influencer.Marketing.Spend..in..1000s., data = df)
model2
# the model interpret the changes in percentage instead of actual value. this can stabilize the variance
# (reduce heteroscedasticity) and improve the normality of residual distribution.
# The log model and the raw model do not give the same coefficient meaning. 
# The raw model predicts changes in sales units, while the log model predicts percentage changes. 
# Because they answer different questions, they will produce different predictions — and that’s normal.

# 3- Or fit a robust model:
# less sensitive to outliers and influencial points, and does not rely heavily on distributions
install.packages('MASS')
library(MASS)
rlm_model <- rlm(Sales..in..1000s. ~ TV.Ads.Spend..in..1000s. + Social.Media.Ads.Spend..in..1000s.
                 + Email.Campaign.Spend..in..1000s. + Influencer.Marketing.Spend..in..1000s., data = df)
rlm_model
#*********************************************************************************************




#############################  Linear Regression Model #######################################################





