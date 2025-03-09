
library(readxl)
library(dplyr)

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