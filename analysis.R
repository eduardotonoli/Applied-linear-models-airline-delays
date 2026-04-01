rm(list = ls())
library(dplyr)
Airline <- read.csv("Airline_Delay_Cause.csv")
head(Airline)

Airline_sub <- Airline |>
  filter(year == 2019,
         airport %in% c("JFK", "LAX", "ORD", "ATL"),
         carrier %in% c("AA", "DL", "UA")) |>
  mutate(
    # Response (aggregated total delay minutes) rescaled for readability
    arr_delay_scaled = arr_delay / 10000
  )

# Convert categorical variables to factors
Airline_sub$airport <- factor(Airline_sub$airport)
Airline_sub$carrier <- factor(Airline_sub$carrier)
Airline_sub$month   <- factor(Airline_sub$month)

str(Airline_sub)
#View(Airline_sub)


par(mfrow = c(1,2))

# Histogram of response variable
hist(Airline_sub$arr_delay_scaled,
     breaks = 30,
     probability = TRUE,
     col = "lightblue",
     border = "white",
     main = "",
     xlab = "Average arrival delay per flight")

lines(density(Airline_sub$arr_delay_scaled),
      col = "red",
      lwd = 2)

full_model <- lm ( arr_delay_scaled ~
                       weather_delay + carrier_delay + nas_delay +
                       late_aircraft_delay + arr_flights +
                       airport + carrier + month ,
                     data = Airline_sub )
# Histogram of residuals
hist(residuals(full_model),
     breaks = 30,
     probability = TRUE,
     col = "lightblue",
     border = "white",
     main = "",
     xlab = "Residuals")

lines(density(residuals(full_model)),
      col = "red",
      lwd = 2)



par(mfrow = c(1,1))



plot(Airline_sub$weather_delay,
     Airline_sub$arr_delay_scaled,
     pch = 19,
     col = "black",
     xlab = "Weather delay (minutes)",
     ylab = "Total arrival delay",
     main = "Arrival delay vs Weather delay")

# Add regression line
abline(lm(arr_delay_scaled ~ weather_delay, data = Airline_sub),
       col = "red",
       lwd = 2)



par(mfrow = c(1,2))

# Boxplot by carrier
boxplot(arr_delay_scaled ~ carrier,
        data = Airline_sub,
        col = "lightblue",
        border = "darkblue",
        main = "Arrival Delay by Carrier",
        xlab = "Carrier",
        ylab = "Total arrival delay")

# Boxplot by airport
boxplot(arr_delay_scaled ~ airport,
        data = Airline_sub,
        col = "lightgreen",
        border = "darkgreen",
        main = "Arrival Delay by Airport",
        xlab = "Airport",
        ylab = "Total arrival delay")

par(mfrow = c(1,1))







num_vars <- Airline_sub[, c("arr_delay_scaled",
                            "weather_delay",
                            "carrier_delay",
                            "nas_delay",
                            "late_aircraft_delay",
                            "arr_flights")]

# Compute correlation matrix
cor_matrix <- cor(num_vars)

# Show correlation matrix
cor_matrix

library(corrplot)

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "white",      # numeri bianchi
         number.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("white", "lightblue", "steelblue"))(200),
         title = "Correlation Matrix",
         mar = c(0,0,1,0))

# variable selection: start from the full model
full_form <- arr_delay_scaled ~ 
  weather_delay + carrier_delay + nas_delay + late_aircraft_delay +
  arr_flights +
  airport + carrier + month

summary(full_form)


library(leaps)

# full model matrix: count how many dummies are created
X_full <- model.matrix(full_form, data = Airline_sub)
p <- ncol(X_full) - 1   # -1 i do not consider the intercept

best_subset <- regsubsets(full_form,
                          data = Airline_sub,
                          nvmax = p)

sum_bs <- summary(best_subset)
sum_bs

adjr2 <- sum_bs$adjr2
best_adjr2<-which.max(adjr2)
best_adjr2

cp <- sum_bs$cp
best_cp<- which.min(cp)
best_cp

bic <- sum_bs$bic
best_bic<-which.min(bic)
best_bic

rss   <- sum_bs$rss
k <- 1:length(rss)     # numbers of predictors (no intercept)
p <- k + 1             # parameters = predictors + intercept

n<-nrow(Airline_sub)
aic <- n * log(rss/n) + 2*p
best_aic<-which.min(aic)
best_aic


par(mfrow = c(2,2))

plot(k, adjr2, type="b", xlab="Number of Predictors", ylab="Adjusted R^2",
     main="Adjusted R^2")
points(which.max(adjr2), max(adjr2), pch=19, col="red")

plot(k, cp, type="b", xlab="Number of Predictors", ylab="Cp",
     main="Mallows Cp")
points(which.min(cp), min(cp), pch=19, col="red")

plot(k, bic, type="b", xlab="Number of Predictors", ylab="BIC",
     main="BIC")
points(which.min(bic), min(bic), pch=19, col="red")

plot(k, aic, type="b", xlab="Number of Predictors", ylab="AIC",
     main="AIC")
points(which.min(aic), min(aic), pch=19, col="red")


# LOOCV AND BEST SUBSET INSIEME :
library(leaps)

#0) Full formula
full_form <- arr_delay_scaled ~
  weather_delay + carrier_delay + nas_delay + late_aircraft_delay +
  arr_flights +
  airport + carrier + month

# --- 1) Model matrix 
X_full <- model.matrix(full_form, data = Airline_sub)
pmax <- ncol(X_full) - 1   

# --- 2) LOOCV setup ---
k <- nrow(Airline_sub)
set.seed(1)

# LOOCV: 
folds <- sample(1:k, k, replace = FALSE)


cv.errors <- matrix(NA, nrow = k, ncol = pmax)

# --- 3) LOOCV loop ---
for (j in 1:k) {
  
  # fit best subset on training (exclude j)
  best.fit <- regsubsets(full_form,
                         data  = Airline_sub[folds != j, ],
                         nvmax = pmax)
  
  
  mat_test <- model.matrix(full_form,
                           data = Airline_sub[folds == j, , drop = FALSE])
  
  y_test <- Airline_sub$arr_delay_scaled[folds == j]
  
  for (i in 1:pmax) {
    
    coefi <- coef(best.fit, id = i)
    xvars <- names(coefi)
    
    # predcition 
    pred <- mat_test[, xvars, drop = FALSE] %*% coefi
    
    cv.errors[j, i] <- (y_test - pred)^2
  }
}

# --- 4) LOOCV mean MSE 
cv.mean <- colMeans(cv.errors, na.rm = TRUE)
best.size <- which.min(cv.mean)

best.size
cv.mean[best.size]

# --- 5) Plot  ---
plot(1:pmax, cv.mean, type = "b",
     xlab = "Number of Predictors",
     ylab = "LOOCV MSE",
     main = "LOOCV Error vs Model Size")

points(best.size, cv.mean[best.size], col = "red", pch = 19)

# --- 6) Fit final best subset on the dataset (to get the coeff) ---
best_subset <- regsubsets(full_form, data = Airline_sub, nvmax = pmax)

best_coefs <- coef(best_subset, id = best.size)
best_coefs

# refit the model:
final_model <- lm(arr_delay_scaled ~
                    weather_delay +
                    carrier_delay +
                    nas_delay +
                    late_aircraft_delay +
                    airportLAX +
                    carrierDL +
                    carrierUA +
                    month6,
                  data = Airline_sub)

# 1) Model matrix del full model
X <- model.matrix(full_form, data = Airline_sub)

# 2) i select the columns obtain by BIC (given bye the  regsubsets)
sel <- c("weather_delay", "carrier_delay", "nas_delay", "late_aircraft_delay",
         "airportLAX", "carrierDL", "carrierUA", "month6")

# 3)
df_final <- data.frame(arr_delay_scaled = Airline_sub$arr_delay_scaled,
                       X_sel = X[, sel, drop = FALSE])

#
names(df_final) <- sub("^X_sel\\.", "", names(df_final))

# 4) Refit the model
final_model <- lm(arr_delay_scaled ~ ., data = df_final)

# 5) Summary 
options(scipen = 999)
summary(final_model)

#Collinearity Issue

library(car)
vif(final_model)

#DIAGNOSTICS: 

res<-residuals(final_model)
fit<-fitted(final_model)

#Homoscedasticity assumption:

plot(fit, res,
     pch = 19, cex = 0.7,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, lwd = 2, col = "red")

abline(h = 2*sd(residuals(final_model)), col="blue", lty=2)
abline(h = -2*sd(residuals(final_model)), col="blue", lty=2)

#Linearity assumption
par(mfrow=c(2,2))

plot(Airline_sub$weather_delay, residuals(final_model),
     pch=19,
     xlab="Weather delay",
     ylab="Residuals")
abline(h=0,col="red")

plot(Airline_sub$carrier_delay, residuals(final_model),
     pch=19,
     xlab="Carrier delay",
     ylab="Residuals")
abline(h=0,col="red")

plot(Airline_sub$nas_delay, residuals(final_model),
     pch=19,
     xlab="NAS delay",
     ylab="Residuals")
abline(h=0,col="red")

plot(Airline_sub$late_aircraft_delay, residuals(final_model),
     pch=19,
     xlab="Late aircraft delay",
     ylab="Residuals")
abline(h=0,col="red")

par(mfrow=c(1,1))


# normality assumption
par(mfrow=c(1,2))
qqnorm(residuals(final_model))
qqline(residuals(final_model), col="red", lwd=2)

hist(residuals(final_model),
     breaks=30,
     probability=TRUE,
     col="lightblue",
     border="white",
     xlab="Residuals",
     main="Histogram of residuals")

lines(density(residuals(final_model)), col="red", lwd=2)

shapiro.test(residuals(final_model))

#Outlier

stud_res <- rstudent(final_model)

plot(stud_res,
     pch = 19,
     xlab = "Observation index",
     ylab = "Studentized residuals",
     main = "Outlier detection")

abline(h = 3, col = "red", lty = 2, lwd = 2)
abline(h = -3, col = "red", lty = 2, lwd = 2)

stud_res <- rstudent(final_model)

idx_out <- which(abs(stud_res) > 3)
idx_out
length(idx_out)
stud_res[idx_out]
Airline_sub[69, ]
Airline_sub[69, c("airport","carrier","month",
                  "weather_delay",
                  "carrier_delay",
                  "nas_delay",
                  "late_aircraft_delay")]


# Cook's distance
cook <- cooks.distance(final_model)

plot(cook,
     pch = 19,
     xlab = "Observation index",
     ylab = "Cook's distance",
     main = "Influential observations (Cook's distance)")

abline(h = 4/length(cook), col = "blue", lty = 2, lwd = 2)
abline(h = 1, col = "red", lty = 2, lwd = 2)

library(ggplot2)
library(ggrepel)

cook <- cooks.distance(final_model)
df_cook <- data.frame(
  idx = seq_along(cook),
  cook = cook,
  label = paste(Airline_sub$airport, Airline_sub$month, Airline_sub$carrier, sep = "_")
)

# i choose lables ( 4 or the ones > 4/n)
n <- nobs(final_model)
thr <- 4/n
df_cook$to_label <- df_cook$cook > thr

topk <- 4
idx_top <- order(df_cook$cook, decreasing = TRUE)[1:topk]
df_cook$to_label <- FALSE
df_cook$to_label[idx_top] <- TRUE

ggplot(df_cook, aes(x = idx, y = cook)) +
  geom_segment(aes(xend = idx, y = 0, yend = cook), linewidth = 0.4) +
  geom_hline(yintercept = thr, linetype = "dashed") +
  labs(title = "Cook's Distance",
       x = "Observation index",
       y = "Cook's distance") +
  theme_minimal(base_size = 14) +
  geom_text_repel(
    data = subset(df_cook, to_label),
    aes(label = label),
    size = 3,
    max.overlaps = Inf
  )


#Testing a group of predictors
reduced_delay <- lm(arr_delay_scaled ~ airportLAX + carrierDL + carrierUA + month6,
                    data = df_final)

anova(reduced_delay, final_model)
summary(final_model)


#Prediction
colMeans(Airline_sub[,c("weather_delay",
                        "carrier_delay",
                        "nas_delay",
                        "late_aircraft_delay")])
new_obs <- data.frame(
  weather_delay = 3049.485,
  carrier_delay = 15661.992,
  nas_delay = 15474.758,
  late_aircraft_delay = 18322.742,
  airportLAX = 1,
  carrierDL = 0,
  carrierUA = 0,
  month6 = 1
)
predict(final_model, newdata = new_obs)




#========================
# POINT 14: SIMULATION
#========================
# 1)  df_final
full_form <- arr_delay_scaled ~
  weather_delay + carrier_delay + nas_delay + late_aircraft_delay +
  arr_flights + airport + carrier + month

X <- model.matrix(full_form, data = Airline_sub)

sel <- c("weather_delay",
         "carrier_delay",
         "nas_delay",
         "late_aircraft_delay",
         "airportLAX",
         "carrierDL",
         "carrierUA",
         "month6")

df_final <- data.frame(
  arr_delay_scaled = Airline_sub$arr_delay_scaled,
  X[, sel, drop = FALSE]
)

 Rifit the final model
final_model <- lm(arr_delay_scaled ~ ., data = df_final)


summary(final_model)


# Number of observations
n <- nrow(df_final)

# Design matrix of the fitted model
X <- model.matrix(final_model)

# Estimated coefficients
beta_hat <- coef(final_model)

# Estimated residual standard deviation
sigma_hat <- summary(final_model)$sigma

# Linear predictor
mu_hat <- as.vector(X %*% beta_hat)

# Simulate new response values from the fitted model
set.seed(123)
y_sim <- rnorm(n, mean = mu_hat, sd = sigma_hat)

# Create simulated dataset
df_sim <- df_final
df_sim$arr_delay_scaled_sim <- y_sim

# Quick summaries
summary(df_final$arr_delay_scaled)
summary(df_sim$arr_delay_scaled_sim)

#========================
# PLOT 1: Observed vs simulated values
#========================
png("observed_vs_simulated.png", width = 1800, height = 1400, res = 300)

plot(df_final$arr_delay_scaled, df_sim$arr_delay_scaled_sim,
     pch = 19,
     xlab = "Observed arr_delay_scaled",
     ylab = "Simulated arr_delay_scaled",
     main = "Observed vs Simulated Response Values")

abline(0, 1, lwd = 2, col = "red")

dev.off()

###
set.seed(12)

beta = coefficients(final_model)
X = model.matrix(final_model)
n = nrow(X)

y_hat = X %*% beta + rnorm(n, 0, sigma(final_model))
y_obs = final_model$model[,1]

plot(y_obs, y_hat,
     xlab = "Observed response",
     ylab = "Simulated response",
     pch = 16,
     col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)





















