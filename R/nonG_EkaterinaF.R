
library(vars)
library(tseries)
library(bayesm)
library(keras)
library(tensorflow)
library(bsts)
data <- read.csv("USA_macro_dataset.csv")

head(data)
str(data)
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# 1. Описание основных статистик
# Построение гистограммы доходностей
library(ggplot2)
ggplot(data, aes(x = Returns)) +
        geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.7) +
        labs(title = "Гистограмма доходностей", x = "Доходности", y = "Частота")



summary(data)
hist(data$Returns, breaks = 50, main = "Histogram of Returns", xlab = "Returns")

# данные и предварительный анализ показывают, что распределение доходностей 
# является лептокуртическим (характеризуется тяжёлыми хвостами). Это согласуется 
# с задачей изучения негауссовских распределений. 

library(moments)
library(MASS)
library(vars)

# 2. Проверка статистических свойств
# Вычисление эксцесса и асимметрии
kurtosis_value <- kurtosis(data$Returns)
skewness_value <- skewness(data$Returns)
cat("Kurtosis:", kurtosis_value, "\n")
cat("Skewness:", skewness_value, "\n")

# Значение эксцесса равно 12.15655, что значительно больше 3 (характерного для 
# нормального распределения). Это подтверждает наличие "тяжёлых хвостов" в 
# распределении доходностей. Финансовые данные часто демонстрируют такие свойства.

# Значение асимметрии -0.490172 указывает на небольшую левую асимметрию, что 
# означает, что распределение имеет слегка удлинённый хвост в отрицательную сторону. 
# Это согласуется с тем, что в финансовых рынках потери могут быть больше, чем прибыли.


# Наша задача — изучить альтернативные распределения:

# Проверка соответствия t-распределению
library(MASS)
library(ggplot2)

# Оценка параметров распределения Стьюдента
fit_t <- fitdistr(data$Returns, "t")
print(fit_t)
params_t <- fit_t$estimate
params_t

# Генерация выборки с параметрами распределения Стьюдента
n <- 10000
sample_t <- rt(n, df = fit_t$estimate["df"]) * fit_t$estimate["s"] + fit_t$estimate["m"]

# Построение гистограммы и плотности
# Построение гистограммы и плотности с легендой
ggplot(data.frame(sample_t), aes(x = sample_t)) +
        geom_histogram(aes(y = after_stat(density), fill = "Histogram"), bins = 50, color = "black", alpha = 0.7) +
        stat_function(
                aes(color = "t-Distribution"),
                fun = function(x) {
                        dt((x - fit_t$estimate["m"]) / fit_t$estimate["s"], df = fit_t$estimate["df"]) / fit_t$estimate["s"]
                },
                linewidth = 1
        ) +
        scale_fill_manual(name = "Legend", values = c("Histogram" = "blue")) +
        scale_color_manual(name = "Legend", values = c("t-Distribution" = "red")) +
        labs(title = "Student's t-Distribution", x = "Returns", y = "Density") +
        theme_minimal() +
        theme(
                legend.position = "top",                # Легенда сверху
                legend.title = element_blank(),         # Убираем слово "Legend"
                legend.text = element_text(size = 12)   # Увеличиваем шрифт
        )

# Результаты подгонки t-распределения
# Параметры t-распределения
# m (среднее): 0.00068 (близко к нулю, ожидаемо для финансовых временных рядов).
# s (масштаб): 0.00733 (характеризует разброс данных).
# df (степени свободы): 2.79 (тяжёлые хвосты, чем меньше df, тем тяжелее хвосты).

# Оценки стандартных ошибок
# Параметры имеют небольшие стандартные ошибки (это говорит о хорошей подгонке).
# Подгонка t-распределения является разумным выбором для описания данных.

# t-распределение хорошо описывает основную часть данных, но может недостаточно 
# точно моделировать крайние хвосты.
# Для более точного анализа хвостовых рисков можно использовать модели с ещё 
# более тяжёлыми хвостами, такие как обобщённое распределение 
# Парето (Generalized Pareto Distribution, GPD) или alpha-стабильные распределения.

# q-Gaussian distribution
# q-Гауссовское распределение используется для описания данных с тяжёлыми хвостами 
# и плато в центральной части. Оно гибче, чем t-распределение, из-за параметра q, 
# который позволяет моделировать дополнительные особенности данных.
# Мы можем сравнить, насколько q-Гауссовское распределение лучше описывает данные 
# по сравнению с t-распределением.
# Генерируем выборку для q-Гауссовского распределения.

q_gaussian <- function(x, q, beta) {
        if (q != 1) {
                return((1 + (1 - q) * beta * x^2)^(-1 / (1 - q)))
        } else {
                return(exp(-beta * x^2))
        }
}

# Генерация выборки q-Гауссовского распределения
x_vals <- seq(-5, 5, length.out = 1000)
y_vals_qg <- q_gaussian(x_vals, q = 1.5, beta = 0.2)

# Построение графика
ggplot(data.frame(x = x_vals, y = y_vals_qg), aes(x = x, y = y)) +
        geom_line(color = "blue") +
        labs(title = "q-Gaussian Distribution", x = "x", y = "Density")


# Этот график помогает визуализировать влияние параметров q и β на форму распределения. 
# Например: q>1: распределение имеет более тяжёлые хвосты. q=1: 
# распределение становится экспоненциальным (нормальным).

# Truncated Lévy distribution (TLD)
# Для генерации данных TLD потребуется использовать его характеристическую функцию.
# Усечённое Леви-распределение хорошо подходит для описания данных, где хвосты 
# ограничены, чтобы избежать бесконечных значений дисперсии.



#  генератор для усечённого Леви-распределения
truncated_levy <- function(n, alpha, gamma, lambda) {
        k <- rnorm(n)
        phi <- exp(-gamma * (k^2 + lambda^2)^(alpha/2))
        
        # Применение FFT и возвращение реальной части
        sample_tld <- Re(fft(phi))
        
        # Ограничение значений, чтобы избежать выбросов
        sample_tld <- sample_tld[sample_tld < quantile(sample_tld, 0.99)]
        
        return(sample_tld)
}

# Генерация данных TLD
sample_tld <- truncated_levy(10000, alpha = 1.5, gamma = 0.4, lambda = 0.18)

# Построение гистограммы
ggplot(data.frame(sample_tld), aes(x = sample_tld)) +
        geom_histogram(bins = 50, fill = "green", color = "black", alpha = 0.7) +
        labs(title = "Truncated Lévy Distribution", x = "Returns", y = "Frequency")


# Modified Weibull distribution (MWD)
# Генерация данных для модифицированного Вейбулловского распределения.

mwd <- function(x, c, chi) {
        return((c / (sqrt(pi) * chi)) * (abs(x) / chi)^(c/2 - 1) * exp(-abs(x) / chi))
}

# Генерация данных MWD
x_vals_mwd <- seq(-5, 5, length.out = 1000)
y_vals_mwd <- mwd(x_vals_mwd, c = 0.75, chi = 1)

# Построение графика
ggplot(data.frame(x = x_vals_mwd, y = y_vals_mwd), aes(x = x, y = y)) +
        geom_line(color = "purple") +
        labs(title = "Modified Weibull Distribution", x = "x", y = "Density")





library(ggplot2)
library(MASS)

# 2. Определение плотностей для каждого распределения
x_vals <- seq(min(data$Returns), max(data$Returns), length.out = 1000)

# Плотность t-распределения
density_t <- dt((x_vals - params_t["m"]) / params_t["s"], df = params_t["df"]) / params_t["s"]

# Плотность q-Гауссовского распределения
q_gaussian <- function(x, q, beta) {
        if (q != 1) {
                (1 + (1 - q) * beta * x^2)^(-1 / (1 - q))
        } else {
                exp(-beta * x^2)
        }
}
density_qg <- q_gaussian(x_vals, q = 1.5, beta = 0.2)

# Плотность TLD
truncated_levy_density <- function(x, alpha, gamma, lambda) {
        exp(-gamma * (x^2 + lambda^2)^(alpha / 2))
}
density_tld <- truncated_levy_density(x_vals, alpha = 1.5, gamma = 0.4, lambda = 0.18)

# Плотность MWD
density_mwd <- mwd(x_vals, c = 0.75, chi = 1)

# 3. Визуализация: Сравнение всех плотностей
ggplot() +
        geom_histogram(data = data.frame(x = data$Returns), aes(x = x, y = after_stat(density)),
                       bins = 50, fill = "gray", alpha = 0.5) +
        geom_line(data = data.frame(x = x_vals, y = density_t), aes(x = x, y = y, color = "t-Distribution")) +
        geom_line(data = data.frame(x = x_vals, y = density_qg), aes(x = x, y = y, color = "q-Gaussian")) +
        geom_line(data = data.frame(x = x_vals, y = density_tld), aes(x = x, y = y, color = "Truncated Lévy")) +
        geom_line(data = data.frame(x = x_vals, y = density_mwd), aes(x = x, y = y, color = "Modified Weibull")) +
        scale_color_manual(name = "Distributions", values = c("t-Distribution" = "red",
                                                              "q-Gaussian" = "blue",
                                                              "Truncated Lévy" = "green",
                                                              "Modified Weibull" = "purple")) +
        labs(title = "Comparison of Distributions", x = "Returns", y = "Density") +
        theme_minimal()

# 4. Оценка метрик соответствия
# AIC для t-распределения
log_likelihood_t <- sum(log(dt((data$Returns - params_t["m"]) / params_t["s"], df = params_t["df"]) / params_t["s"]))
aic_t <- -2 * log_likelihood_t + 2 * length(params_t)  # t имеет 3 параметра: m, s, df
cat("AIC for t-Distribution:", aic_t, "\n")

# AIC = -37223.74 — Очень низкое значение AIC, что говорит о высоком качестве 
# соответствия. Это распределение, скорее всего, хорошо подходит для описания данных.

# AIC для q-Гауссовского распределения
log_likelihood_qg <- sum(log(q_gaussian(data$Returns, q = 1.5, beta = 0.2)))
aic_qg <- -2 * log_likelihood_qg + 2 * 2  # q-Гауссовское имеет 2 параметра: q и beta
cat("AIC for q-Gaussian:", aic_qg, "\n")

# AIC для Modified Weibull
log_likelihood_mwd <- sum(log(pmax(mwd(data$Returns, c = 0.75, chi = 1), 1e-10)))
aic_mwd <- -2 * log_likelihood_mwd + 2 * 2  # 2 параметра: c и chi
cat("AIC for Modified Weibull (with correction):", aic_mwd, "\n")

# AIC для Truncated Lévy
truncated_levy_density <- function(x, alpha, gamma, lambda) {
        exp(-gamma * (x^2 + lambda^2)^(alpha / 2))
}
log_likelihood_tld <- sum(log(truncated_levy_density(data$Returns, alpha = 1.5, gamma = 0.4, lambda = 0.18)))
aic_tld <- -2 * log_likelihood_tld + 2 * 3  # Truncated Lévy имеет 3 параметра: alpha, gamma, lambda
cat("AIC for Truncated Lévy:", aic_tld, "\n")


# Используйте выбранное распределение для анализа хвостовых рисков (Value at Risk, 
# Conditional Value at Risk).

# Пример для t-распределения
VaR_95 <- qt(0.05, df = params_t["df"]) * params_t["s"] + params_t["m"]
cat("VaR (95%):", VaR_95, "\n")

# результат для VaR95% = -0.0171 показывает, что при доверительном уровне 95% 
# ожидаемое максимальное снижение доходности за период наблюдения составит -1.71%.

# Чтобы оценить более экстремальные хвосты, можно рассчитать VaR99%
VaR_99 <- qt(0.01, df = params_t["df"]) * params_t["s"] + params_t["m"]
cat("VaR (99%):", VaR_99, "\n")
# при доверительном уровне 99% ожидаемое максимальное снижение доходности 
# за период наблюдения составит -3.47%.

# Рассчитаем Conditional VaR (CVaR). CVaR (или Expected Shortfall) оценивает 
# средний убыток, если убыток превышает VaR
CVaR_95 <- params_t["m"] + params_t["s"] * (dt(qt(0.05, df = params_t["df"]), df = params_t["df"]) / 0.05)
cat("CVaR (95%):", CVaR_95, "\n")

# Сравним VaR для других распределений. Используем q-Гауссовское, чтобы сравнить VaR. 
VaR_qg_95 <- q_gaussian(0.05, q = 1.5, beta = 0.2)
cat("VaR (95%) for q-Gaussian:", VaR_qg_95, "\n")


# CVaR для 95% и 99%
CVaR_95 <- params_t["m"] + params_t["s"] * (dt(qt(0.05, df = params_t["df"]), df = params_t["df"]) / 0.05)
CVaR_99 <- params_t["m"] + params_t["s"] * (dt(qt(0.01, df = params_t["df"]), df = params_t["df"]) / 0.01)

cat("CVaR (95%):", CVaR_95, "\n")
cat("CVaR (99%):", CVaR_99, "\n")


ggplot(data.frame(x = x_vals, y = density_t), aes(x = x, y = y)) +
        geom_line(color = "red") +
        geom_vline(xintercept = VaR_95, linetype = "dashed", color = "blue") +
        geom_vline(xintercept = VaR_99, linetype = "dashed", color = "green") +
        geom_vline(xintercept = CVaR_95, linetype = "dotted", color = "blue") +
        geom_vline(xintercept = CVaR_99, linetype = "dotted", color = "green") +
        annotate("text", x = VaR_95, y = max(density_t) * 0.8, label = "VaR (95%)", color = "blue", angle = 90, vjust = -0.5) +
        annotate("text", x = VaR_99, y = max(density_t) * 0.6, label = "VaR (99%)", color = "green", angle = 90, vjust = -0.5) +
        annotate("text", x = CVaR_95, y = max(density_t) * 0.4, label = "CVaR (95%)", color = "blue", angle = 90, vjust = -0.5) +
        annotate("text", x = CVaR_99, y = max(density_t) * 0.2, label = "CVaR (99%)", color = "green", angle = 90, vjust = -0.5) +
        labs(title = "t-Distribution with VaR and CVaR", x = "Returns", y = "Density") +
        theme_minimal()


# Регрессионный анализ параметров распределения
# Рассчитаем параметры t-распределения для скользящих окон:

# Установим размер окна
window_size <- 100
n <- nrow(data)
rolling_params <- data.frame(start = integer(), df = numeric(), s = numeric(), m = numeric())

# Рассчёт параметров для каждого окна
for (i in seq(1, n - window_size + 1)) {
        window_data <- data$Returns[i:(i + window_size - 1)]
        fit <- fitdistr(window_data, "t")
        rolling_params <- rbind(rolling_params, c(i, fit$estimate["df"], fit$estimate["s"], fit$estimate["m"]))
}
colnames(rolling_params) <- c("start", "df", "s", "m")

# Добавление в основной набор данных
rolling_params$end <- rolling_params$start + window_size - 1
rolling_params$date <- data$date[rolling_params$end]


# Объединяем параметры и макроэкономические переменные
params_data <- merge(rolling_params, data, by.x = "date", by.y = "date")

# Регрессия для параметра df
model_df <- lm(df ~ InterestRate + Inflation + UnemploymentRate + ProductionGrowth, data = params_data)
summary(model_df)

# Регрессия для параметра s
model_s <- lm(s ~ InterestRate + Inflation + UnemploymentRate + ProductionGrowth, data = params_data)
summary(model_s)

library(ggplot2)

# График изменения параметра df
ggplot(params_data, aes(x = date, y = df)) +
        geom_line(color = "blue") +
        labs(title = "Dynamic Changes in Degrees of Freedom (df)", x = "Date", y = "df")

# Влияние InterestRate на df
ggplot(params_data, aes(x = InterestRate, y = df)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", color = "red") +
        labs(title = "Impact of Interest Rate on Degrees of Freedom", x = "Interest Rate", y = "df")



## Динамическое моделирование (GARCH/VAR)
library(rugarch)

# Определение модели GARCH
spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
        mean.model = list(armaOrder = c(1, 0)),
        distribution.model = "std" # t-распределение
)

# Подгонка модели
garch_fit <- ugarchfit(spec = spec, data = data$Returns)
summary(garch_fit)










# Тест на стационарность (ADF тест) для временных рядов
adf_test_results <- sapply(data[, c("Returns", "Inflation", "InterestRate", "UnemploymentRate", 
                                    "ProductionIndex", "ProductionGrowth")], adf.test)
print(adf_test_results)
# Применяем дифференцирование для нестационарных переменных
# Применяем дифференцирование для нестационарных переменных и добавляем NA для согласования размеров
data$InterestRate_diff <- c(NA, diff(data$InterestRate, differences = 1))
data$UnemploymentRate_diff <- c(NA, diff(data$UnemploymentRate, differences = 1))
data$ProductionIndex_diff <- c(NA, diff(data$ProductionIndex, differences = 1))

# Удаляем строки с NA (появляются из-за дифференцирования)
data <- na.omit(data)
# Построение модели VAR
var_model <- VAR(data[, c("Returns", "Inflation", "InterestRate_diff", "UnemploymentRate_diff", 
                          "ProductionIndex_diff", "ProductionGrowth")], p = 1)
summary(var_model)



# Прогнозирование с использованием VAR
forecast_result <- predict(var_model, n.ahead = 10)
plot(forecast_result)
library(bayesm)

# Построение модели BVAR (Байесовская VAR модель)
# Определение гиперпараметров для Байесовской модели


library(BVAR)

# Преобразуем данные для использования в модели BVAR
bvar_data <- data[, c("Returns", "Inflation", "InterestRate_diff", "UnemploymentRate_diff", 
                      "ProductionIndex_diff", "ProductionGrowth")]

# Инициализация векторов для AIC и BIC
aic_values <- numeric(5)
bic_values <- numeric(5)

# Пробуем разные значения лагов и выводим AIC/BIC
for (p in 1:5) {
        model <- bvar(bvar_data, lags = p)
        aic_values[p] <- AIC(model)
        bic_values[p] <- BIC(model)
}

# Просмотр значений AIC и BIC для разных лагов
data.frame(Lags = 1:5, AIC = aic_values, BIC = bic_values)

# AIC и BIC обычно уменьшаются по мере увеличения числа лагов, что может 
# указывать на более сложные модели с лучшей подгонкой к данным. Однако важно 
# найти баланс между подгонкой и сложностью модели.
# Минимальное значение AIC и BIC достигается при p = 1: AIC: -189201.3, BIC: -189154.5

# Построение модели BVAR
bvar_model <- bvar(bvar_data, lags = 1)

# Просмотр результатов модели
summary(bvar_model)


# Включение стохастической волатильности (Пример: модель GARCH)
install.packages("rugarch")
library(rugarch)
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0)))
garch_fit <- ugarchfit(garch_spec, data$Returns)
plot(garch_fit)





# Определение и обучение Distributional Neural Network (DNN)
model <- keras_model_sequential() %>%
        layer_dense(units = 64, activation = 'relu', input_shape = c(ncol(data) - 1), kernel_initializer = 'he_normal') %>%
        layer_dense(units = 32, activation = 'relu', kernel_initializer = 'he_normal') %>%
        layer_dense(units = 1)


# Компиляция модели
model %>% compile(loss = 'mean_squared_error', optimizer = 'adam')

# Стандартизация данных (z-оценка)
data_scaled <- scale(data[, -1])

# Обучение модели DNN
# Обучение модели на стандартизированных данных
model %>% fit(data_scaled, data$Returns, epochs = 50, batch_size = 32)
# Прогнозирование будущих доходностей с помощью модели DNN
predictions <- model %>% predict(as.matrix(data[, -1]))




# Визуализация прогнозов
plot(predictions, type = "l", col = "blue", main = "Прогнозы DNN vs Фактические доходности")
lines(data$Returns, col = "red")
legend("topright", legend = c("Прогнозы", "Фактические доходности"), col = c("blue", "red"), lty = 1)

# Диагностика модели: анализ остатков
residuals <- residuals(var_model)
acf(residuals)

# Резюме по модели Bayesian VAR
summary(bvar_model)

# Сохранение модели и прогнозов для дальнейшего анализа
saveRDS(bvar_model, "bvar_model.rds")
saveRDS(model, "dnn_model.rds")
write.csv(predictions, "predictions.csv")



