
data {
        int<lower=1> T;  // Количество временных точек
        int<lower=1> K;  // Количество переменных
        int<lower=1> P;  // Количество лагов
        matrix[T, K] Y;  // Входные данные (матрица)
}

parameters {
        matrix[K, K * P] B;  // Коэффициенты VAR
        vector[K] c;  // Константы
        cov_matrix[K] Sigma;  // Ковариационная матрица ошибок
}

model {
        for (t in (P+1):T) {
                row_vector[K] y_t = c';
    for (p in 1:P) {
      y_t += Y[t-p] * B[, (p-1)*K+1:p*K];
    }
    Y[t] ~ multi_normal(y_t, Sigma); // Байесовское моделирование
  }
}
