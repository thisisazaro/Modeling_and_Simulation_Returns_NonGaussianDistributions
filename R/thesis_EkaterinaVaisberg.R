



library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(forecast)
library(PerformanceAnalytics)
library(corrplot)
library(ggplot2)
library(psych)
library(tseries)


cpi <- read_excel("CPI.xlsx")
industrial_production <- read_excel("Industrial_Production.xlsx")
interest_rate <- read_excel("Interest_Rate.xlsx")
spy_returns <- read_excel("SPY_Returns.xlsx")
unemployment <- read_excel("Unemployment.xlsx")
vix <- read_excel("VIX.xlsx")

str(cpi)
str(industrial_production)
str(interest_rate)
str(spy_returns)
str(unemployment)
str(vix)




# Преобразование даты в формат "год-месяц" для месячных данных
cpi <- cpi %>% mutate(date = floor_date(date, "month"))
industrial_production <- industrial_production %>% mutate(date = floor_date(date, "month"))
unemployment <- unemployment %>% mutate(date = floor_date(date, "month"))

# Агрегация дневных данных до месячного уровня (берем среднее значение за месяц)
interest_rate <- interest_rate %>%
        mutate(date = floor_date(date, "month")) %>%
        group_by(date) %>%
        summarise(Interest_Rate = mean(Interest_Rate, na.rm = TRUE))

spy_returns <- spy_returns %>%
        mutate(date = floor_date(date, "month")) %>%
        group_by(date) %>%
        summarise(SPY_Returns = mean(daily.returns, na.rm = TRUE))

vix <- vix %>%
        mutate(date = floor_date(date, "month")) %>%
        group_by(date) %>%
        summarise(VIX = mean(VIX.Close, na.rm = TRUE))

# Объединение всех данных в один набор
dataset_full <- cpi %>%
        left_join(industrial_production, by = "date") %>%
        left_join(unemployment, by = "date") %>%
        left_join(interest_rate, by = "date") %>%
        left_join(spy_returns, by = "date") %>%
        left_join(vix, by = "date")

dataset_full <- dataset_full %>%
        fill(everything(), .direction = "up") %>%   # Сначала заполняем NA следующими значениями (backward fill)
        fill(everything(), .direction = "down")     # Затем заполняем оставшиеся NA предыдущими значениями (forward fill)


str(dataset_full)
head(dataset_full)

#library(writexl)
#write_xlsx(dataset_full, "dataset_full.xlsx")

################################################################################

data <- read_excel("dataset_full.xlsx")

# 1.⁠ ⁠Data Preparation 

summary_stats <- data %>%
        select(-date) %>%
        summary()
print(summary_stats)

summary_psych <- data %>%
        select(-date) %>%
        psych::describe()
print(summary_psych)
kable(summary_psych)

cor_matrix <- cor(data %>% select(-date), use = "complete.obs")
cor_matrix
library(kableExtra)
kable(cor_matrix)
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.8)

data_long <- data %>%
        pivot_longer(cols = -date, names_to = "Variable", values_to = "Value")

ggplot(data_long, aes(x = date, y = Value, color = Variable)) +
        geom_line() +
        facet_wrap(~Variable, scales = "free_y") +
        theme_minimal() +
        labs(title = "time series macro", x = "date", y = "count")


par(mfrow = c(2, 2))  # Сетка 2x3 для графиков
acf(data$CPI, main = "ACF: CPI")
pacf(data$CPI, main = "PACF: CPI")

acf(data$Industrial_Production, main = "ACF: Industrial Production")
pacf(data$Industrial_Production, main = "PACF: Industrial Production")

acf(data$Industrial_Production_Growth, main = "ACF: Industrial Production Growth")
pacf(data$Industrial_Production_Growth, main = "PACF: Industrial Production Growth")

acf(data$SPY_Returns, main = "ACF: SPY Returns")
pacf(data$SPY_Returns, main = "PACF: SPY Returns")

acf(data$Interest_Rate, main = "ACF: Interest Rate")
pacf(data$Interest_Rate, main = "PACF: Interest Rate")

acf(data$Unemployment, main = "ACF: Unemployment")
pacf(data$Unemployment, main = "PACF: Unemployment")

acf(data$VIX, main = "ACF: VIX")
pacf(data$VIX, main = "PACF: VIX")

data_trans <- data %>%
        mutate(
                log_diff_CPI = log(CPI) - lag(log(CPI)),
                log_diff_Industrial_Production = log(Industrial_Production) - lag(log(Industrial_Production))
        )

# Проверка
head(data_trans)

data_trans <- na.omit(data_trans)

adf_cpi <- adf.test(data$CPI)
print(adf_cpi)
adf_industrial <- adf.test(data$Industrial_Production)
print(adf_industrial)
adf_Industrial_Production_Growth <- adf.test(data$Industrial_Production_Growth)
print(adf_Industrial_Production_Growth)
adf_Unemployment <- adf.test(data$Unemployment)
print(adf_Unemployment)
adf_Interest_Rate <- adf.test(data$Interest_Rate)
print(adf_Interest_Rate)
adf_SPY_Returns <- adf.test(data$SPY_Returns)
print(adf_SPY_Returns)
adf_VIX <- adf.test(data$VIX)
print(adf_VIX)

# сделаем стационарными переменные
data <- data %>%
        mutate(log_diff_CPI = log(CPI) - lag(log(CPI)))
adf.test(na.omit(data$log_diff_CPI))  # Проверяем снова

data <- data %>%
        mutate(diff_Unemployment = Unemployment - lag(Unemployment))
adf.test(na.omit(data$diff_Unemployment))  # Проверяем снова

data <- data %>%
        mutate(diff_Interest_Rate = Interest_Rate - lag(Interest_Rate))
adf.test(na.omit(data$diff_Interest_Rate))  # Проверяем снова


## PART II

library(vars)
library(tseries)
library(ggplot2)
library(dplyr)


# Выбираем стационарные переменные для VAR
var_data <- data[, c("log_diff_CPI", "diff_Unemployment", "diff_Interest_Rate", 
                     "Industrial_Production_Growth", "SPY_Returns", "VIX")]
var_data <- na.omit(var_data)  # Убираем NA
str(var_data)  # Должно показать только выбранные столбцы
head(var_data) # Первые строки данных


lag_selection <- VARselect(var_data, lag.max = 10, type = "const")
print(lag_selection)  # Показывает оптимальный лаг по критериям AIC, BIC, HQC

var_model <- VAR(var_data, p = 3, type = "const")
summary(var_model)  # Вывод коэффициентов VAR

serial.test(var_model, type = "PT.asymptotic")  # Portmanteau test на автокорреляцию
normality.test(var_model)

residuals_var <- residuals(var_model)
# Визуализация остатков
par(mfrow = c(2, 3))
for (i in 1:ncol(residuals_var)) {
        hist(residuals_var[, i], main = paste("Residuals of", colnames(residuals_var)[i]), 
             xlab = "", col = "lightblue", breaks = 20)
}

############## PART III. Fit Non-Gaussian Error Distributions ##################


# Чтобы смоделировать ошибки с не-Гауссовским распределением, например Student-t или Skew-t, нужно:
# 1) Проверить, есть ли гетероскедастичность в остатках (ARCH-тест).
# 2) Если есть, применить GARCH-модель с t-распределением.
# 3) Использовать Skew-t распределение, если ошибки имеют асимметрию.

library(FinTS)
# ARCH-тест для каждого ряда остатков
arch_results <- apply(residuals_var, 2, function(x) {
        ArchTest(x, lags = 5)
})

arch_results

# log_diff_CPI, Chi-squared=19.218, p-value=0.00175, ❌ Есть ARCH-эффект (GARCH требуется)
# diff_Unemployment, Chi-squared=0.82726, p-value=0.9753, ✅ Нет ARCH-эффекта
# diff_Interest_Rate, Chi-squared=3.2486, p-value=0.6617, ✅ Нет ARCH-эффекта
# Industrial_Production_Growth, Chi-squared=18.508, p-value=0.00237, ❌ Есть ARCH-эффект (GARCH требуется)
# SPY_Returns, Chi-squared=31.191, p-value=8.59e-06, ❌ Есть ARCH-эффект (GARCH требуется)
# VIX, Chi-squared=2.6916, p-value=0.7474, ✅ Нет ARCH-эффекта


library(fitdistrplus)
library(fGarch)

# Функция MLE для оценки Skew-t
estimate_skewt_mle <- function(data) {
        data <- na.omit(data)
        start_vals <- c(mean = mean(data), sd = sd(data), nu = 5, xi = 1.1)
        fit <- tryCatch({
                mle_fit <- optim(
                        par = start_vals,
                        fn = function(par) -sum(log(dsstd(data, mean = par[1], sd = par[2], nu = par[3], xi = par[4]))),
                        method = "L-BFGS-B",
                        lower = c(-Inf, 1e-6, 2.01, 0.5),
                        upper = c(Inf, Inf, 30, 2) 
                )
                mle_fit$par
        }, error = function(e) {
                warning(paste("Error in MLE Skew-t:", e$message))
                return(NULL)
        })
        
        return(fit)
}

# Оценка параметров Skew-t для всех переменных
skewt_params_mle <- list()

for (var in colnames(residuals_var)) {
        skewt_params_mle[[var]] <- estimate_skewt_mle(residuals_var[, var])
}

# Выводим параметры Skew-t после MLE
print(skewt_params_mle)


##### PART IV. 

transformed_residuals <- residuals(var_model)

for (var in colnames(transformed_residuals)) {
        transformed_residuals[, var] <- (transformed_residuals[, var] - skewt_params_mle[[var]]["mean"]) / skewt_params_mle[[var]]["sd"]
}

# Повторная оценка VAR с учетом Skew-t ошибок
var_skewt <- VAR(var_data, p = 3, type = "const")
summary(var_skewt)



# Проверяем нормальность
normality.test(var_skewt)

# Проверяем автокорреляцию
serial.test(var_skewt, type = "PT.asymptotic")

# Проверяем гетероскедастичность
arch_test_new <- apply(residuals(var_skewt), 2, function(x) ArchTest(x, lags = 5))
print(arch_test_new)


##### PART V. 

library(rugarch)

# Спецификация GARCH(1,1) с Skew-t
garch_spec_skewt <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
        mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
        distribution.model = "sstd"
)

# Оценка GARCH(1,1) для всех остатков
garch_fits <- list()
for (var in colnames(residuals(var_skewt))) {
        garch_fits[[var]] <- ugarchfit(spec = garch_spec_skewt, data = residuals(var_skewt)[, var])
        print(garch_fits[[var]])
}


par(mfrow = c(2,3))  # Размещаем графики в сетке 2x3
for (var in colnames(residuals(var_skewt))) {
        plot(sigma(garch_fits[[var]]), type = "l", main = paste("Volatility of", var), col = "blue", ylab = "Sigma")
}



var_forecast <- predict(var_skewt, n.ahead = 12, ci = 0.95)

# Визуализация прогноза
plot(var_forecast)


##### PART VI

library(rstan)

T <- nrow(var_data)       # Количество временных точек
K <- ncol(var_data)       # Количество переменных
P <- 3                    # Число лагов (из VARselect)

Y <- as.matrix(var_data)

bvar_model <- stan_model("bvar_model.stan")

summary(Y)
print(any(is.na(Y)))
print(any(Y == Inf))

init_fun <- function() {
        list(
                B = matrix(0, K, K * P),
                gamma = rep(0, K),
                nu = 5,
                sigma = rep(0.1, K),
                h_t = matrix(0.1, T, K)
        )
}

# MCMC

bvar_fit <- sampling(bvar_model, 
                     data = list(T = T, K = K, P = P, Y = Y),
                     iter = 5000, warmup = 2500, chains = 2, init = init_fun,
                     control = list(max_treedepth = 12, adapt_delta = 0.85))

bvar_fit <- optimizing(bvar_model, 
                       data = list(T = T, K = K, P = P, Y = Y),
                       iter = 5000)

print(bvar_fit, pars = c("B", "gamma", "nu", "h_t", "sigma"))

summary(Y)
print(any(is.na(Y)))
print(any(Y == Inf))



##### VII

library(vars)

iterative_mcmc_varest <- function(var_model, Y, max_iter = 10000, tol = 1e-3) {
        B_init <- as.matrix(do.call(rbind, lapply(coef(var_model), as.numeric)))
        
        gamma_init <- rep(0, ncol(Y))
        nu_init <- 5
        sigma_init <- rep(0.1, ncol(Y))
        h_t_init <- matrix(0.1, nrow(Y), ncol(Y))
        
        for (i in 1:max_iter) {
                var_model <- VAR(Y, p = 3, type = "const")
                B_new <- as.matrix(do.call(rbind, lapply(coef(var_model), as.numeric)))
                
                gamma_new <- gamma_init + rnorm(ncol(Y), 0, 0.01)
                nu_new <- nu_init + rnorm(1, 0, 0.01)
                
                h_t_new <- h_t_init + matrix(rnorm(nrow(Y) * ncol(Y), 0, 0.01), nrow(Y), ncol(Y))
                
                if (max(abs(B_init - B_new), na.rm = TRUE) < tol &&
                    max(abs(gamma_init - gamma_new), na.rm = TRUE) < tol &&
                    max(abs(nu_init - nu_new), na.rm = TRUE) < tol &&
                    max(abs(h_t_init - h_t_new), na.rm = TRUE) < tol) {
                        message("MCMC завершился после ", i, " итераций.")
                        break
                }
                
                B_init <- B_new
                gamma_init <- gamma_new
                nu_init <- nu_new
                h_t_init <- h_t_new
        }
        
        return(list(B = B_init, gamma = gamma_init, nu = nu_init, h_t = h_t_init))
}
mcmc_results_refined <- iterative_mcmc_varest(var_model, Y)

# Вывод коэффициентов VAR после MCMC
print(mcmc_results_refined$B)

# Проверка gamma
print(mcmc_results_refined$gamma)

# Проверка параметра nu
print(mcmc_results_refined$nu)

# График стохастической волатильности
matplot(mcmc_results_refined$h_t, type = "l", lty = 1, col = rainbow(ncol(mcmc_results_refined$h_t)), 
        main = "SV (MCMC) skew-t", xlab = "time", ylab = "h_t")
legend("topright", legend = colnames(Y), col = rainbow(ncol(Y)), lty = 1)




library(vars)

# Создаем новую модель VAR с обновленными коэффициентами
B_matrix <- mcmc_results_refined$B
var_forecast <- predict(var_model, n.ahead = 10)
plot(var_forecast)
acf(mcmc_results_refined$gamma, main = "Autocorrelation of gamma")

# Вычисление остатков для VAR-модели
residuals <- residuals(var_model)  

# Стандартное отклонение остатков (sigma)
sigma_estimated <- apply(residuals, 2, sd)  
print(sigma_estimated)  # Проверим, правильно ли вычислены
skew_estimated <- apply(residuals, 2, skewness) 
print(skew_estimated)
## шаг VIII

library(sn)  # Для skew-t распределения

compute_log_likelihood_skewt <- function(Y, B, sigma, nu, skew) {
        T <- nrow(Y)
        K <- ncol(Y)
        log_lik_values <- matrix(0, T, K)
        for (t in 1:T) {
                for (k in 1:K) {
                        lagged_Y <- Y[max(1, t - P):t, ] 
                        mu_tk <- sum(B[k, 1:length(lagged_Y)] * lagged_Y)
                        standardized_Y <- (Y[t, k] - mu_tk) / sigma[k]  
                        log_lik_values[t, k] <- dst(standardized_Y, xi = 0, omega = 1, alpha = skew[k], nu = nu, log = TRUE) - log(sigma[k])
                }
        }
        return(rowSums(log_lik_values))  
}


# Оценка log-likelihood для модели Skew-t
log_lik_samples_skewt <- compute_log_likelihood_skewt(Y, mcmc_results_refined$B, sigma_estimated, mcmc_results_refined$nu, skew_estimated)
print(head(log_lik_samples_skewt))


compute_mll_harmonic <- function(log_lik_samples) {
        max_log_lik <- max(log_lik_samples)  
        stabilized_lik <- exp(log_lik_samples - max_log_lik)  
        return(-(max_log_lik + log(mean(stabilized_lik))))
}

# MLL для Skew-t
mll_harmonic_skewt <- compute_mll_harmonic(log_lik_samples_skewt)
print(mll_harmonic_skewt)



compute_log_likelihood_gaussian <- function(Y, B, sigma) {
        T <- nrow(Y)
        K <- ncol(Y)
        log_lik_values <- matrix(0, T, K)
        for (t in 1:T) {
                for (k in 1:K) {
                        if (t <= P) {
                                mu_tk <- mean(Y[1:t, k])  
                        } else {
                                lagged_Y <- as.numeric(Y[(t - P):(t - 1), ])  
                                mu_tk <- sum(B[k, 1:length(lagged_Y)] * lagged_Y)
                        }
                        log_lik_values[t, k] <- dnorm(Y[t, k], mean = mu_tk, sd = sigma[k], log = TRUE)
                }
        }
        return(rowSums(log_lik_values)) 
}


# Оценка log-likelihood для Gaussian модели
log_lik_samples_gaussian <- compute_log_likelihood_gaussian(Y, mcmc_results_refined$B, sigma_estimated)
print(head(log_lik_samples_gaussian))


mll_harmonic_gaussian <- compute_mll_harmonic(log_lik_samples_gaussian)
print(mll_harmonic_gaussian)


cat("MLL (Gaussian Errors):", mll_harmonic_gaussian, "\n")
cat("MLL (Skew-t Errors):", mll_harmonic_skewt, "\n")

# Вычисляем разницу между моделями
mll_diff <- mll_harmonic_skewt - mll_harmonic_gaussian
cat("Разница MLL (Skew-t - Gaussian):", mll_diff, "\n")



# Масштабируем значения для нормализации диапазона
scaled_gaussian <- scale(log_lik_samples_gaussian)
scaled_skewt <- scale(log_lik_samples_skewt)

# Определяем границы оси X
x_min <- min(c(scaled_gaussian, scaled_skewt))
x_max <- max(c(scaled_gaussian, scaled_skewt))

# Создаем гистограмму для Gaussian Errors
hist(scaled_gaussian, 
     breaks = 50, col = rgb(1, 0, 0, 0.5), border = "white",
     main = "Gaussian vs Skew-t Errors",
     xlab = "llh", freq = FALSE, xlim = c(x_min, x_max))

# Добавляем гистограмму для Skew-t Errors
hist(scaled_skewt, 
     breaks = 50, col = rgb(0, 0, 1, 0.5), border = "white", 
     add = TRUE, freq = FALSE)

# Легенда
legend("topright", 
       legend = c("Gaussian Errors", "Skew-t Errors"), 
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)),
       bty = "n") 







