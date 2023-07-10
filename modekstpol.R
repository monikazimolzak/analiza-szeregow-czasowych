library(foreign)
library(xts)
library(urca)
library(lmtest)
library(fBasics)
library(forecast)
library(tseries)
library(fUnitRoots)
library(readxl)


###########################
# 3. Metoda Holta-Wintersa
###########################

#  1. 
# Importujemy dane z pliku "visits.csv" 


# import pliku CSV
tour2 <- 	read_excel("tourist.xlsx")  # separator dziesiatny


# weryfikujemy strukture obiektu
str(tour2)

# utworzymy obiekt typu ts (time series) dla danych 
tour2.ts <- 
  ts(tour2$liczba, start = c(2015, 1), freq = 12)

tour2.ts
str(tour2.ts)


#  2. 
#  Wizualizacja szeregu 

plot(tour2.ts,
     main = "tourist",
     xlab = "data",
     ylab = "tourist")


# 3.

# do procesu wygładzenia danych wykorzystamy okres bez ostatnich 12 obserwacji
# a następnie oszacujemy prognozy dla tego okresu (ostatnie 12 obserwacje)

# do zawężenia zbioru obserwacji możemy wykorzystać funkcję window()
tour2.ts.train <- 
  window(tour2.ts,
         end = c(2021, 12))

tour2.ts.test <- 
  window(tour2.ts,
         start = c(2022, 1))


# multiplikatywny model Holta-Wintersa 
tour2.HWmult <- HoltWinters(tour2.ts.train,
                             seasonal="multiplicative")
tour2.HWmult # info o modelu

plot(tour2.HWmult)

# oszacujmy prognozę na 12 okresu naprzód
tour2.HWmult.forecast <- predict(tour2.HWmult,
                                  n.ahead = 12,
                                  prediction.interval = TRUE)

# następnie porównajmy prognozę z oryginalnym szeregiem 
plot(tour2.ts)
lines(tour2.HWmult.forecast[, 1], col = "blue") # prognoza
lines(tour2.HWmult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(tour2.HWmult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2022, lty = 2) # dodajemy pionową linię referencyjną

# obejrzyjmy prognozę w zbliżeniu
length(tour2.ts)

# zmienimy w tym celu jedynie pierwszą linię (pierwsze polecenie)
plot(window(tour2.ts, start = c(2001, 12)))
lines(tour2.HWmult.forecast[, 1], col = "blue")  
lines(tour2.HWmult.forecast[, 2], col = "red", lty = 2) 
lines(tour2.HWmult.forecast[, 3], col = "red", lty = 2) 
abline(v = 2022, lty = 2) 

# Obliczanie błędów prognozowania

# MAE (Mean Absolute Error)
mae <- mean(abs(tour2.ts - tour2.HWmult.forecast[, 1]))

# MSE (Mean Squared Error)
mse <- mean((tour2.ts - tour2.HWmult.forecast[, 1])^2)

# MAPE (Mean Absolute Percentage Error)
mape <- mean(abs((tour2.ts - tour2.HWmult.forecast[, 1]) / tour2.ts)) * 100

# AMAPE (Adjusted Mean Absolute Percentage Error)
n <- length(tour2.ts)
amape <- sum(abs((tour2.ts - tour2.HWmult.forecast[, 1]) / tour2.ts)) * 100 / n

# Wyświetlanie wyników
results <- data.frame(mae = mae, mse = mse, mape = mape, amape = amape)
results


