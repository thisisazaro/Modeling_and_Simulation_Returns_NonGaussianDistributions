

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


library(sn) 
library(MASS) 

# Функция для оценки параметров Multi Skew-t (MST)
estimate_mst_params <- function(residuals) {
        residuals <- residuals[complete.cases(residuals), ]
        xi <- colMeans(residuals)
        omega <- cov(residuals)
        alpha <- apply(residuals, 2, function(x) mean((x - mean(x))^3) / sd(x)^3) 
        nu <- apply(residuals, 2, function(x) { 6 / (mean((x - mean(x))^4) / var(x)^2 - 3) })  
        return(list(xi = xi, omega = omega, alpha = alpha, nu = nu))
}

# Оценка параметров MST
mst_params <- estimate_mst_params(residuals(var_model))

# Проверка
print(mst_params)


library(pracma)  # Для ортогонализации через PCA

# Функция для оценки параметров Orthogonal Skew-t
estimate_ost_params <- function(residuals) {
        residuals <- residuals[complete.cases(residuals), ]
        pca_result <- prcomp(residuals, center = TRUE, scale. = TRUE)
        orthogonal_residuals <- pca_result$x
        xi <- colMeans(orthogonal_residuals)
        omega <- cov(orthogonal_residuals)
        alpha <- apply(orthogonal_residuals, 2, function(x) mean((x - mean(x))^3) / sd(x)^3)
        nu <- apply(orthogonal_residuals, 2, function(x) { 6 / (mean((x - mean(x))^4) / var(x)^2 - 3) })
        return(list(xi = xi, omega = omega, alpha = alpha, nu = nu))
}

# Оценка параметров OST
ost_params <- estimate_ost_params(residuals(var_model))

# Проверка
print(ost_params)





### PART IV. Обновление VAR модели с MST и OST

# Функция для стандартизации остатков с учетом MST
transform_residuals_mst <- function(residuals, mst_params) {
        standardized_residuals <- residuals
        for (var in colnames(residuals)) {
                standardized_residuals[, var] <- (residuals[, var] - mst_params$xi[var]) / sqrt(mst_params$omega[var, var])
        }
        return(standardized_residuals)
}

# Аналогичная функция для OST
transform_residuals_ost <- function(residuals, ost_params) {
        standardized_residuals <- residuals
        for (pc in colnames(ost_params$xi)) {
                standardized_residuals[, pc] <- (residuals[, pc] - ost_params$xi[pc]) / sqrt(ost_params$omega[pc, pc])
        }
        return(standardized_residuals)
}

# Обновленные остатки с учетом MST
residuals_mst <- transform_residuals_mst(residuals(var_model), mst_params)

# Обновленные остатки с учетом OST
residuals_ost <- transform_residuals_ost(residuals(var_model), ost_params)

# Обновляем модель VAR для MST и OST
var_mst <- VAR(residuals_mst, p = 3, type = "const")
var_ost <- VAR(residuals_ost, p = 3, type = "const")

# Выводим результаты
summary(var_mst)
summary(var_ost)

# Проверяем нормальность
normality.test(var_mst)
normality.test(var_ost)
# Проверяем автокорреляцию
serial.test(var_mst, type = "PT.asymptotic")
serial.test(var_ost, type = "PT.asymptotic")

## PART V
## Добавление стохастической волатильности (SV)

library(rugarch)


# Спецификация GARCH(1,1) 
garch_spec_mst <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
        mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
        distribution.model = "sstd"
)

garch_spec_ost <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
        mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
        distribution.model = "sstd"
)

# Оценка GARCH(1,1) для всех переменных
garch_fits_mst <- list()
garch_fits_ost <- list()

for (var in colnames(residuals_mst)) {
        garch_fits_mst[[var]] <- ugarchfit(spec = garch_spec_mst, data = residuals_mst[, var])
        print(garch_fits_mst[[var]])
}

for (var in colnames(residuals_ost)) {
        garch_fits_ost[[var]] <- ugarchfit(spec = garch_spec_ost, data = residuals_ost[, var])
        print(garch_fits_ost[[var]])
}

# Визуализируем волатильность
par(mfrow = c(2,3))
for (var in colnames(residuals_mst)) {
        plot(sigma(garch_fits_mst[[var]]), type = "l", main = paste("Volatility (MST) of", var), col = "blue", ylab = "Sigma")
}

par(mfrow = c(2,3))
for (var in colnames(residuals_ost)) {
        plot(sigma(garch_fits_ost[[var]]), type = "l", main = paste("Volatility (OST) of", var), col = "red", ylab = "Sigma")
}


## PART VI
## Байесовская оценка с MCMC

library(rstan)
Y_mst <- as.matrix(residuals_mst)
Y_ost <- as.matrix(residuals_ost)
# Данные для MCMC
T <- min(nrow(Y_mst), nrow(Y_ost))  # Подстраиваем T под реальные данные
K <- ncol(var_data)
P <- 3



bvar_model <- stan_model("bvar_model.stan")

# Функция инициализации
init_fun <- function() {
        list(
                B = matrix(0, K, K * P),
                gamma = rep(0, K),
                nu = 5,
                sigma = rep(0.1, K),
                h_t = matrix(0.1, T, K)
        )
}


print(dim(Y_mst))  # Должно быть (T, K)
print(dim(Y_ost))  # Должно быть (T, K)
print(T)  # Убедись, что T = nrow(Y_mst) и nrow(Y_ost)
print(K)  # Убедись, что K = ncol(Y_mst) и ncol(Y_ost)


# MCMC для MST
bvar_fit_mst <- sampling(bvar_model, 
                         data = list(T = T, K = K, P = P, Y = Y_mst),
                         iter = 5000, warmup = 2500, chains = 2, init = init_fun,
                         control = list(max_treedepth = 12, adapt_delta = 0.85))

bvar_fit_mst <- optimizing(bvar_model, 
                       data = list(T = T, K = K, P = P, Y = Y_mst),
                       iter = 5000)

# MCMC для OST
bvar_fit_ost <- sampling(bvar_model, 
                         data = list(T = T, K = K, P = P, Y = Y_ost),
                         iter = 5000, warmup = 2500, chains = 2, init = init_fun,
                         control = list(max_treedepth = 12, adapt_delta = 0.85))

bvar_fit_ost <- optimizing(bvar_model, 
                           data = list(T = T, K = K, P = P, Y = Y_ost),
                           iter = 5000)

# Вывод параметров
print(bvar_fit_mst, pars = c("B", "gamma", "nu", "h_t", "sigma"))
print(bvar_fit_ost, pars = c("B", "gamma", "nu", "h_t", "sigma"))


# Запуск MCMC
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
mcmc_results_refined_mst <- iterative_mcmc_varest(var_model, Y_mst)
mcmc_results_refined_ost <- iterative_mcmc_varest(var_model, Y_ost)

# Вывод коэффициентов VAR после MCMC
print(mcmc_results_refined_mst$B)
print(mcmc_results_refined_ost$B)

# Проверка gamma
print(mcmc_results_refined_mst$gamma)
print(mcmc_results_refined_ost$gamma)

# Проверка параметра nu
print(mcmc_results_refined_mst$nu)
print(mcmc_results_refined_ost$nu)

# График стохастической волатильности mst
matplot(mcmc_results_refined_mst$h_t, type = "l", lty = 1, col = rainbow(ncol(mcmc_results_refined_mst$h_t)), 
        main = "SV (MCMC) mst", xlab = "time", ylab = "h_t")
legend("topright", legend = colnames(Y_mst), col = rainbow(ncol(Y_mst)), lty = 1)

# График стохастической волатильности ost
matplot(mcmc_results_refined_ost$h_t, type = "l", lty = 1, col = rainbow(ncol(mcmc_results_refined_ost$h_t)), 
        main = "SV (MCMC) ost", xlab = "time", ylab = "h_t")
legend("topright", legend = colnames(Y_ost), col = rainbow(ncol(Y_ost)), lty = 1)


## Marginal Log-Likelihood (MLL) для сравнения моделей

# Создаем новую модель VAR с обновленными коэффициентами MST
B_matrix_mst <- mcmc_results_refined_mst$B
var_forecast <- predict(var_model, n.ahead = 10)
plot(var_forecast)
acf(mcmc_results_refined_mst$gamma, main = "Autocorrelation of gamma (MST)")
residuals <- residuals(var_model)  
sigma_estimated <- apply(residuals, 2, sd)  
print(sigma_estimated)  # Проверим, правильно ли вычислены
skew_estimated <- apply(residuals, 2, skewness) 
print(skew_estimated)

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


# Функция для MLL через гармоническое среднее
compute_mll_harmonic <- function(log_lik_samples) {
        max_log_lik <- max(log_lik_samples)  
        stabilized_lik <- exp(log_lik_samples - max_log_lik)  
        return(-(max_log_lik + log(mean(stabilized_lik))))
}

print(bvar_fit_mst$nu)
print(length(bvar_fit_mst$nu))

# Если nu отсутствует, установим разумное значение по умолчанию
nu_value <- ifelse(is.null(bvar_fit_mst$nu) | length(bvar_fit_mst$nu) == 0, 5, bvar_fit_mst$nu)

# Проверяем правильность передаваемых параметров
print(nu_value)  # Должно вывести 5 или другое числовое значение
print(mst_params$alpha)  # Должен быть вектор с коэффициентами

# Вычисляем log-likelihood для MST
log_lik_samples_mst <- compute_log_likelihood_skewt(
        Y_mst,
        unlist(bvar_fit_mst$B), 
        sigma_estimated, 
        nu_value, 
        unlist(mst_params$alpha)
)

nu_value_ost <- ifelse(is.null(bvar_fit_ost$nu) | length(bvar_fit_ost$nu) == 0, 5, bvar_fit_ost$nu)

# Вычисление log-likelihood для MST и OST
log_lik_samples_ost <- compute_log_likelihood_skewt(
        Y_ost,
        unlist(bvar_fit_ost$B), 
        sigma_estimated, 
        nu_value_ost, 
        unlist(ost_params$alpha)
)


# MLL для MST и OST
mll_mst <- compute_mll_harmonic(log_lik_samples_mst)
mll_ost <- compute_mll_harmonic(log_lik_samples_ost)

cat("MLL (MST Errors):", mll_mst, "\n")
cat("MLL (OST Errors):", mll_ost, "\n")


# Вычисляем разницу между моделями
mll_diff <- mll_mst - mll_ost
cat("Разница MLL (MST Errors - OST Errors):", mll_diff, "\n")



# Масштабируем значения для нормализации диапазона
scaled_mst <- scale(log_lik_samples_mst)
scaled_ost <- scale(log_lik_samples_ost)

# Определяем границы оси X
x_min <- min(c(scaled_mst, scaled_ost))
x_max <- max(c(scaled_mst, scaled_ost))

# Создаем гистограмму для MST
hist(scaled_mst, 
     breaks = 50, col = rgb(1, 0, 0, 0.5), border = "white",
     main = "MST vs OST Errors",
     xlab = "llh", freq = FALSE, xlim = c(x_min, x_max))

# Добавляем гистограмму для OST
hist(scaled_ost, 
     breaks = 50, col = rgb(0, 0, 1, 0.5), border = "white", 
     add = TRUE, freq = FALSE)

# Легенда
legend("topright", 
       legend = c("MST Errors", "OST Errors"), 
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)),
       bty = "n") 



