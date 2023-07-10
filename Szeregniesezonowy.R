library(devtools)
library(dplyr)
library(lmtest) 
library(dynlm)
library(xts)
library(lmtest)
library(tseries)
library(urca)
library(fUnitRoots)
library(ggplot2)
library(AER)
library(zoo)
library(fBasics)
library(R6)
library(timeSeries)
library(readxl)
library(foreign)
library(forecast)
library(tseries)

###########################################################################
# I. Przygotowanie danych                                                 #
###########################################################################

# 1. Import danych
btc <- read_excel("btcusd.xlsx")

# obejrzymy dane zaimportowane
str(btc)

colnames(btc)[colnames(btc) == "Close"] <- "btc"

# formatujemy datę na format zrozumiały dla R
btc$Date <- as.Date(btc$Date, format="%Y-%m-%d")

# dodamy logarytm btc
btc$lbtc<-log(btc$btc)

# przekonwertujmy dane do obiektu typu xts
# (podobny do obiektu ts)
btc.xts <- xts(btc[, 1:3], # kolumny z danymi
               btc$Date)  # kolumny z datą/czasem

# Ustalamy okres IN-SAMPLE
btc_sample.xts <- window(btc.xts, end = as.Date("2021-06-01"))
tail(btc_sample.xts)
head(btc_sample.xts)

btc <- ts(data=btc$btc, frequency = 12,           
          start=c(2015,6), end=c(2023,6))
tail(btc)

###########################################################################
# PRACUJEMY NA PROBIE IN-SAMPLE                                           #
###########################################################################

# 3. Policzmy pierwsze roznice
dbtc <- diff(btc)

# 4. wykresy szeregu wejsciowego i pierwszych roznic
plot(btc,  type = "l", main = "btc")
plot(dbtc, type = "l", main = "pierwsze roznice btc")

# 5. Stosujemy transformacje logarytmiczna
lbtc <- log(btc)
dlbtc <- diff(lbtc)

# 6. wykresy logatymu szeregu wejsciogo i logarytmu pierwszych roznic
plot(lbtc,  type = "l", main = "ln(btc)")
plot(dlbtc, type = "l", main = "pierwsze roznice ln(btc)")

# 7.Analiza korelogramow ACF i PACF dla szeregu lbtc oraz dlbtc
tsdisplay(lbtc, lag.max=24)
# ich wartości wygasają bardzo powoli - wskazują zatem na niestacjonarność oryginalnego szeregu

# sprawdzamy dla pierwszych róźnic
tsdisplay(dlbtc, lag.max=24)


# 8. Okreslamy stopien integracji

# wczytanie funkcji do wykorzystania w analizie

source("/Users/monika/Desktop/Projekt zaliczeniowy/TESTDF.r")

testdf(variable = lbtc, ADF_type="c", ADF_max_order = 7,BG_max_order = 6)

#Test KPSS
kpss.test <- ur.kpss(lbtc,use.lag = 6)
summary(kpss.test)

# H0: lwpi jest zm. niestacjonarnorna 
# nie ma podstaw do odrzucenia H0

# W zwiazku z powyzszym powtarzamy test dla pierwszych roznic ln(btc)

testdf(variable = dlbtc,ADF_type="c", ADF_max_order = 7,BG_max_order = 6)

# H0: d.lwpi jest zm. niestacjonarnorna 
# odrzucamy H0.

#Test KPSS
kpss.test <- ur.kpss(dlbtc, use.lag = 6)
summary(kpss.test)

# 9. Czy zmienna jest bialym szumem?

# H0: zmienna dlbtc jest bialym szumem

# Test Ljung-Boxa
Box.test(dlbtc, type = "Ljung-Box", lag = 12)

# Test Boxa-Pearsa
Box.test(dlbtc, type = "Box-Pierce", lag = 24)

# ACF i PACF
tsdisplay(dlbtc, lag.max=24)

###########################################################################
# II. Identyfikacja                                                       #
###########################################################################

# IDENTYFIKACJA rzędów p i q

# Analiza korelogramow ACF i PACF dla szeregu dlbtc 

tsdisplay(dlbtc, lag.max=24)

# ACF i PACF sugerują proces ARIMA (0,0,0) dla lbtc


#ewentualnie
arima.best.AIC <- auto.arima(dlbtc,
                             d = 0,             # parameter d w modelu ARIMA
                             max.p = 4,         # p = maksymalna wartosc
                             max.q = 4,         # q = maksymalna wartosc
                             max.order = 8,     # suma p+q
                             start.p = 0,       # Wartosc startowa dla p
                             start.q = 0,       # Wartosc startowa dla q
                             ic = "bic",        # Wybor modelu na podstawie kryterium informcyjne
                             stepwise = FALSE,  # jezeli FALSE rozwaza wszystkie modeli
                             allowdrift = F, # model zawiera stalą
                             trace = TRUE)      # wyswietlenie rozwazonych modeli

#Best model: ARIMA(0,0,0)

###########################################################################
# II. Model EKSTRAPOLACYJNY                                               #
###########################################################################

# weryfikujemy strukture obiektu
str(btc)

#  2. 
#  Wizualizacja szeregu 

plot(btc,
     main = "kurs btc",
     xlab = "data",
     ylab = "btc")

abline(h =mean(btc), col="red", lty = 2)          # dodajemy pozioma linie referencyjna

# 3. 
#Podzial na in-sample i out-of-sample

# do procesu wygładzenia danych wykorzystamy okres bez ostatnich 4 obserwacji
# t < 93
# a następnie oszacujemy prognozy dla tego okresu (ostatnie 4 obserwacje)

# do zawężenia zbioru obserwacji możemy wykorzystać funkcję window()

# Ustalamy okres IN-SAMPLE
str(btc)


btc.train <- window(btc, start = c(2015, 6), end = c(2022, 12))
str(btc.train)

btc.test <- window(btc, start = c(2023, 1), end = c(2023, 6))
str(btc.test)


#  4. 
# Proste wygladzanie wykladnicze (bez trendu, bez efektów sezonowych)

# a. Oszacowanie modelu na probie in sample

# Proste wygladzanie wykladnicze (bez trendu, bez efektów sezonowych)

btc.SES <- HoltWinters(btc.train) 

btc.SES # info o modelu


# wykres na probie in-sample
plot(btc.SES)

# wartości wygładzone przechowywane są w $fitted 
# (razem z poszczególnymi komponentami po zdekomponowaniu)

plot(btc.SES$fitted)

# pierwsza kolumna w $fitted zawiera wartości wygładzone 

plot(btc.SES$fitted[, 1])

# b. Policzenie prognozy

# policzmy prognozę na 4 obserwacje do przodu
btc.SES.forecast <- predict(btc.SES,
                               n.ahead = 6,
                               prediction.interval = TRUE)

# c. Porównanie 

# porównamy prognozę z oryginalnym szeregiem
plot(btc)
lines(btc.SES.forecast[, 1], col = "blue") # prognozy 
lines(btc.SES.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności dla prognozy
lines(btc.SES.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności dla prognozy
abline(v = (2023), lty = 2) # dodajemy pionową linię referencyjną


# 5. 
# utwórzmy teraz szereg z wartościami wygładzonymi dla okresu in-sample
# oraz prognozami dla okresu out-of-sample

btc.SES$fitted[, 1]

# extend	
# If true, the start and end values are allowed to extend the series. 
# If false, attempts to extend the series give a warning and are ignored

btc.SES.summary <- 
  window(btc.SES$fitted[, 1], start = c(2015, 6), end = c(2022, 12), extend = TRUE)

# w miejsce braków danych na końcu wstawiamy prognozy 
window(btc.SES.summary, start = c(2023, 1), end = c(2023, 6)) <-
  btc.SES.forecast[, 1]


# 5.
# porownanie przecietnych bledow prognoz w okresie out-of-sample 
# z tymi z okresu in-sample

btc.summary$mae_SES     <- abs(btc.summary$SES-btc.summary$BTC)
print(btc.summary$mae_SES)

# Przygotowanie danych
rzeczywiste <- btc[92:97] # Rzeczywiste wartości (ostatnie 4 obserwacje)
prognozy <- btc.SES.forecast[, 1] # Prognozy

# Obliczanie błędów bezwzględnych
błędy <- abs(prognozy - rzeczywiste)

# Obliczanie MAE
mae <- mean(błędy)

# Wyświetlanie wyniku
print(mae)



