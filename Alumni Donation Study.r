library(ggplot2)
library(dplyr)
library(broom)


library("GGally")       
library("ggplot2")       
library("gridExtra")     
library("leaps")         
library("pdp")           
library("plotly")        
library("RBitmoji")      
library("scales")        
library("SMPracticals")  
library("tibble")

#read data
url <- "https://bgreenwell.github.io/uc-bana7052/data/alumni.csv"
alumni <- read.csv(url)

#view
head(alumni)

#fix first column name
colnames(alumni)[1] <- 'school'
head(alumni)

#add text column for private/public - unused
alumni <- mutate(alumni, type = ifelse(private == 1, "private", "public"))

########################
#descriptive statistics#
#and  describing   data#
########################

#mean
apply(alumni[,2:5], 2, mean)

#median
apply(alumni[,2:5], 2, median)

#skewness
apply(alumni[,2:5], 2, moments::skewness)

#sd/var
apply(alumni[,2:5], 2, sd)
apply(alumni[,2:5], 2, var)

#range
ran <- apply(alumni[,2:5], 2, range)
ran

#check for outliers
outliers <- matrix(nrow = 3, ncol = 2)
for(i in 2:4) {
  iqr.x <- IQR(alumni[,i])
  upper <- quantile(alumni[,i], 0.75)
  lower <- quantile(alumni[,i], 0.25)
  outliers[i-1,] <- c(lower - 1.5 * iqr.x, upper + 1.5 * iqr.x)
}

outliers
ran

#histograms
ggplot(alumni, aes(x = percent_of_classes_under_20)) +
  geom_histogram(binwidth = 4, color = 'black', fill = '#E69F00', alpha = 0.8) +
  labs(x = 'Percent of classes under 20 students', y = 'Count') +
  ggtitle(label = 'Histogram of Percent of Classes Under 20 Students', subtitle = 'Bin width = 4') +
  scale_y_continuous(breaks = c(1, 3, 5, 7, 9), labels = c(1, 3, 5, 7, 9)) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

ggplot(alumni, aes(x = student_faculty_ratio)) +
  geom_histogram(binwidth = 2, color = 'black', fill = '#56B4E9', alpha = 0.8) +
  labs(x = 'Student-Faculty Ratio', y = 'Count') +
  ggtitle(label = 'Histogram of Student-Facutly Ratio', subtitle = 'Bin width = 2') +
  scale_y_continuous(breaks = seq(0, 10, 2), labels = seq(0, 10, 2)) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

ggplot(alumni, aes(x = alumni_giving_rate)) +
  geom_histogram(binwidth = 5, color = 'black', fill = '#999999', alpha = 0.8) +
  labs(x = 'Percentage of alumni that donated', y = 'Count') +
  ggtitle(label = 'Histogram of alumni giving rate', subtitle = 'Bin width = 5') +
  scale_y_continuous() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#scatterplots
ggplot(alumni, aes(x = percent_of_classes_under_20, y = alumni_giving_rate, group = type)) +
  geom_point(size = 2, aes(color = type)) +
  labs(x = 'Percent of classes under 20 students',
       y = 'Percent of alumni that donated') +
  ggtitle(label = "XY Scatter Plot", subtitle = 'Response vs. percent classes under 20 students') +
  theme_light() +  
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


ggplot(alumni, aes(x = student_faculty_ratio, y = alumni_giving_rate, group = type)) +
  geom_point(size = 2, aes(color = type)) +
  labs(x = 'Student-faculty ratio', y = 'Percent of alumni that donated') +
  ggtitle(label = 'XY Scatter Plot', subtitle = 'Response vs. student-faculty ratio') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#unused plot
#ggplot(alumni, aes(x = private, y = alumni_giving_rate)) +
#  geom_point(size = 2, color = "#999999") +
#  theme_light() +
#  labs(title = "XY Scatter Plot 3", x = 'Is school private?', 
#       y = 'Percent of alumni that donated') +
#  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


#boxplots
par(mfrow = c(1,3), oma = c(0, 0, 2, 0))
boxplot(x = alumni$percent_of_classes_under_20, ylab = 'Percent of classes under 20 students')
boxplot(x = alumni$student_faculty_ratio, ylab = 'Student-faculty ratio')
boxplot(x = alumni$alumni_giving_rate, ylab = 'Percent of alumni that donated')
mtext('Boxplot of all variables', outer = TRUE, cex = 1.5)


#correlation matrix
cor(alumni[, 2:4])

#pairs graph
GGally::ggpairs(alumni[, 2:4])

###############
##REGRESSION!##
###############

reg_data <- alumni[,2:5]

library(leaps)

a1 <- regsubsets(alumni_giving_rate ~ ., data = reg_data, nbest = 3, nvmax = 5)

#plot shows lowest bic only with student_facutly_ratio
par(mfrow=c(1,1))
plot(a1, scale = 'bic')

#plot 2
res1 <- data.frame(
  "nvar" = apply(summary(a1)$which, 1, FUN = function(x) sum(x) - 1),
  "bic" = summary(a1)$bic,
  "adjr2" = summary(a1)$adjr2
)

# Plot results
p1 <- ggplot(res1, aes(x = nvar, y = bic)) +
  geom_point(alpha = 0.5, size = 2, color = "darkred") +
  stat_summary(fun.y = min, geom = "line", alpha = 0.5, linetype = "dashed") +
  theme_light() +
  labs(x = "Number of predictors", y = "BIC")
p2 <- ggplot(res1, aes(x = nvar, y = adjr2)) +
  geom_point(alpha = 0.5, size = 2, color = "darkgreen") +
  stat_summary(fun.y = max, geom = "line", alpha = 0.5, linetype = "dashed") +
  theme_light() +
  labs(x = "Number of predictors", y = "Adjusted R-squared")
gridExtra::grid.arrange(p1, p2, nrow = 2)

summary(best1 <- lm(alumni_giving_rate ~ student_faculty_ratio, data = reg_data))

reg_data_fit_values <- augment(best1) %>% 
                          mutate(row_num = 1:n())



# Model diagnostics -------------------------------------------------------


#plots from Vaidi


#The below plot shows that the residuals are scattered with constant variance
ggplot(reg_data_fit_values, aes(x = .fitted, y = .std.resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  geom_smooth(color = "forestgreen", alpha = 0.1, se = FALSE) +
  xlab("Fitted value") +
  ylab("Standardized residual") +
  theme_light()

#The below plot shows that the data is right skewed - errors are not normally distributed
ggplot(reg_data_fit_values, aes(sample = .std.resid)) +
  geom_qq(alpha = 0.5) +
  geom_qq_line(linetype = "dashed", color = "red2") +
  xlab("Theoretical quantile") +
  ylab("Sample quantile") +
  theme_light()

#The below plot shows that the observations are independent
ggplot(reg_data_fit_values, aes(x = row_num, y = .std.resid)) +
  geom_point(alpha = 0.3) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  xlab("Index") +
  ylab("Standardized residual") +
  theme_light()


#plot(alumni_giving_rate ~ student_faculty_ratio, data = reg_data, pch = 19, las = 1,
#     col = adjustcolor("darkblue", alpha.f = 0.5))
#abline(fit <- lm(alumni_giving_rate ~ student_faculty_ratio, data = reg_data), 
#       lwd = 2,
#       col = adjustcolor("darkred", alpha.f = 0.5))

####################
#add transformation#
####################

##Transforming alumni_giving_rate to make the errors Normally distributed

# 1. finding optimal lambda value for tranformation

bc <- MASS::boxcox(alumni_giving_rate ~ student_faculty_ratio, data = reg_data)
lambda <- bc$x[which.max(bc$y)]
lambda
reg_data$alumni_giving_rate_transformed <- (alumni$alumni_giving_rate ^ lambda - 1) / lambda

# 2. scatter plot and fitted model

par(mfrow = c(1, 2))  # side-by-side plots
plot(fit, which = 1:2)

# generating transformed reg_data dataset
reg_data
reg_data_transformed <- reg_data[,-3]
reg_data_transformed

# summary statistics after transformation
# we can see that the r squared value increased
summary(best_transformed <- lm(alumni_giving_rate_transformed ~ student_faculty_ratio, data = reg_data_transformed))

#reg_data_transformed_final <- augment(best_transformed)

#adjusted r squared increased from 0.

# transformed data plot for fitted value vs residual
#ggplot(reg_data_transformed_final, aes(x = .fitted, y = .std.resid)) +
 # geom_point(alpha = 0.6) +
#  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
#  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
#  geom_smooth(color = "forestgreen", alpha = 0.1, se = FALSE) +
#  xlab("Fitted value") +
#  ylab("Standardized residual") +
#  theme_light()

#ggplot(reg_data_transformed_final, aes(sample = .std.resid)) +
#  geom_qq(alpha = 0.5) +
#  geom_qq_line(linetype = "dashed", color = "red2") +
#  xlab("Theoretical quantile") +
#  ylab("Sample quantile") +
#  theme_light()

##################
#add interactions#
##################


#Trying to fit the best model using 2 way interaction
a3 <- regsubsets(alumni_giving_rate_transformed ~ .^2, data = reg_data_transformed, nbest = 40, nvmax = 1000)


#Here, we see that the best fit model with 2 way interation having at least 
#one independent variable and least BIC consists of the predictors
# Student faculty ratio and Private. 
par(mfrow=c(1,1))
plot(a3, scale = 'bic')


#plot 3
res1 <- data.frame(
  "nvar" = apply(summary(a3)$which, 1, FUN = function(x) sum(x) - 1),
  "bic" = summary(a3)$bic,
  "adjr2" = summary(a3)$adjr2
)

# Plot results - Model with 2 predictors will be the best model
p3 <- ggplot(res1, aes(x = nvar, y = bic)) +
  geom_point(alpha = 0.5, size = 2, color = "darkred") +
  stat_summary(fun.y = min, geom = "line", alpha = 0.5, linetype = "dashed") +
  theme_light() +
  labs(x = "Number of predictors", y = "BIC")
p4 <- ggplot(res1, aes(x = nvar, y = adjr2)) +
  geom_point(alpha = 0.5, size = 2, color = "darkgreen") +
  stat_summary(fun.y = max, geom = "line", alpha = 0.5, linetype = "dashed") +
  theme_light() +
  labs(x = "Number of predictors", y = "Adjusted R-squared")
gridExtra::grid.arrange(p3, p4, nrow = 2)

#best model
#id <- which.min(summary(a3)$bic)
#trms <- names(which(summary(a3)$which[id, ])[-1L])
#form <- as.formula(paste("alumni_giving_rate_transformed ~", paste(trms, collapse = "+")))
#summary(best2 <- lm(form, data = reg_data_transformed))

best2 <- lm(data = reg_data_transformed, 
            alumni_giving_rate_transformed ~ student_faculty_ratio + private)
reg_data_final <- augment(best2)  %>% 
                    mutate(row_num = 1:n())
summary(best2)
#vif
car::vif(best2)

# transformed data plot for fitted value vs residual
ggplot(reg_data_final, aes(x = .fitted, y = .std.resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  geom_smooth(color = "forestgreen", alpha = 0.1, se = FALSE) +
  xlab("Fitted value") +
  ylab("Standardized residual") +
  theme_light()

ggplot(reg_data_final, aes(sample = .std.resid)) +
  geom_qq(alpha = 0.5) +
  geom_qq_line(linetype = "dashed", color = "red2") +
  xlab("Theoretical quantile") +
  ylab("Sample quantile") +
  theme_light()


ggplot(reg_data_final, aes(x = row_num, y = .std.resid)) +
  geom_point(alpha = 0.3) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  xlab("Index") +
  ylab("Standardized residual") +
  theme_light()

reg_data_final <- mutate(reg_data_final, type = ifelse(private == 1, "private", "public"))

# alumni donation based on sf ratio and private
ggplot(reg_data_final, aes(x = student_faculty_ratio , y = .fitted)) +
  geom_point(aes(y = alumni_giving_rate_transformed , colour = type), size = 3) +
  geom_line(aes(group = type), color = "black") +
  labs(x = "s/f ratio", y = "Percent of alumni that donated") +
  theme_light()

#ggplot(alumni_all_1, aes(x = student_faculty_ratio, y = .fitted)) +
 # geom_point(aes(y = alumni_giving_rate , colour = type), size = 3) +
  #geom_line(aes(group = type), color = "black") +
  #labs(x = "s/f ratio", y = "Percent of alumni that donated") +
  #theme_light()

## h hat code
h <- hatvalues(best2)
plot(h, type = "h", ylim = extendrange(h, f = 0.15))
abline(h = 2 * 3 / nrow(reg_data), lty = "dotted")  #<<
text(h, labels = seq_len(nrow(reg_data)), pos = 3, col = "red2")

### randomly checking residual behaviour
hist(reg_data_final$.resid)
sum(reg_data_final$.resid)


# Backward elimination --------------------------
# Note that setting `k = 2` in the call to step(), which 
# is the default, corresponds to using AIC; below we set 
# it to `k = ln(n)`, which corresponds to using BIC!
# Main effects only (i.e., no interactions)
fit_max_1 <- lm(alumni_giving_rate_transformed ~ ., data = reg_data_transformed)
be_1 <- step(fit_max_1, direction = "backward", 
             trace = 0, k = log(nrow(reg_data_transformed)))
# Main effects and two-way interactions
fit_max_2 <- lm(alumni_giving_rate_transformed ~ .^2, data = reg_data_transformed)
be_2 <- step(fit_max_2, direction = "backward", 
             trace = 0, k = log(nrow(reg_data_transformed)))

# Forward selection -----------------------------
# Main effects only (i.e., no interactions)
fit_min <- lm(alumni_giving_rate_transformed ~ 1, data = reg_data_transformed)
fs_1 <- step(fit_min, direction = "forward", 
             scope = list(lower = fit_min,
                          upper = fit_max_1),
             trace = 0, k = log(nrow(reg_data_transformed)))
# Main effects and two-way interactions
fs_2 <- step(fit_min, direction = "forward", 
             scope = list(lower = fit_min,
                          upper = fit_max_2),
             trace = 0, k = log(nrow(reg_data_transformed)))

# Stepwise selection ----------------------------
# Main effects only (i.e., no interactions)
ss_1 <- step(be_1, direction = "both", 
             scope = list(lower = fit_min,
                          upper = fit_max_1),
             trace = 0, k = log(nrow(reg_data_transformed)))
# Main effects and two-way interactions
ss_2 <- step(be_2, direction = "both", 
             scope = list(lower = fit_min,
                          upper = fit_max_2),
             trace = 0, k = log(nrow(reg_data_transformed)))

#press
# Function to compute the PRESS statistic (a form of 
# cross-validation). Note: smaller is better!
PRESS <- function(object, ...) {
  if(!missing(...)) {
    res <- sapply(list(object, ...), FUN = function(x) {
      sum(rstandard(x, type = "predictive") ^ 2)
    })
    names(res) <- as.character(match.call()[-1L])
    res
  } else {
    sum(rstandard(object, type = "predictive") ^ 2)
  }
}
# Function to compute various model metrics
modelMetrics <- function(object, ...) {
  if(!missing(...)) {
    res <- sapply(list(object, ...), FUN = function(x) {
      c("AIC" = AIC(x), "BIC" = BIC(x), 
        "adjR2" = summary(x)$adj.r.squared,
        "RMSE"  = sigma(x), "PRESS" = PRESS(x), 
        "nterms" = length(coef(x)))
    })
    colnames(res) <- as.character(match.call()[-1L])
    res
  } else {
    c("AIC" = AIC(object), "BIC" = BIC(object), 
      "adjR2" = summary(object)$adj.r.squared, 
      "RMSE"  = sigma(object), "PRESS" = PRESS(object),
      "nterms" = length(coef(object)))
  }
}
# Compare models
res <- modelMetrics(be_1, be_2, fs_1, fs_2, ss_1, ss_2)
View(round(res, digits = 3))
