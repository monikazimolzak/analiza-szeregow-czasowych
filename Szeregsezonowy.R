# wczytanie pakietow do pamieci

library(foreign)
library(xts)
library(urca)
library(lmtest)
library(fBasics)
library(forecast)
library(tseries)
library(fUnitRoots)
library(readxl)

# wczytanie funkcji do wykorzystania w analizie

source("/Users/monika/Desktop/Projekt zaliczeniowy/TESTDF.r")

###########################################################################
# I. Przygotowanie danych                                                 #
###########################################################################

# 1. Import danych
tour <- read_excel("tourist.xlsx")

# obejrzymy dane zaimportowane
str(tour)

colnames(tour)[colnames(tour) == "liczba"] <- "tour"

class(tour) #obiekt typu "data frame"

# Po wczytaniu danych otrzymalismy obiekt typu data frame, przypominający arkusz
# kalkulacyjny, zawierający poszczególne zmienne w kolejnych kolumnach.

# obejrzymy dane zaimportowane
str(tour)

# formatujemy datę na format zrozumiały dla R
tour$data <- as.Date(tour$data, format="%Y-%m-%d")

# przekonwertujmy dane do obiektu typu xts
# (podobny do obiektu ts)
tour.xts <- xts(tour$tour,   # kolumny z danymi
                 tour$data)  # kolumny z datą/czasem
names(tour.xts)[1] <- "tour"
tail(tour.xts)

# 2. Ustalamy okres IN-SAMPLE
tour_sample.xts <- window(tour.xts, end = as.Date("2021-12-01"))
tail(tour_sample.xts)

options(scipen=999)
plot(tour.xts)


# 3. wykresy szeregu wejsciowego i pierwszych roznic

# wykresy szeregu wejsciowego
plot(tour_sample.xts)
# widoczna sezonowść i trend nieliniowy

# Obejrzyjmy korelogramy dla tour
par(mar = rep(2, 4))

par(mfrow = c(2, 1))
acf(tour_sample.xts$tour,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(tour_sample.xts$tour, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

# wprowadzenie roznic regularnych
tour_sample.xts$dtour <- diff.xts(tour_sample.xts$tour, lag = 1)
plot(tour_sample.xts$dtour)


# Obliczenie optymalnego parametru lambda dla transformacji Boxa-Coxa na szeregu czasowym 'ts_data'
library(MASS)
library(forecast)
boxcox_test <- BoxCox.lambda(tour.xts)

# Wyświetlenie optymalnego parametru lambda
print(boxcox_test)

#Dla wartości optymalnego parametru lambda wynoszącej 0.717, można przypuszczać, że wprowadzenie 
#logarytmowania danych może przynieść korzyści. 
#Wartość lambda różna od 1 wskazuje na potrzebę transformacji danych w celu uzyskania bardziej liniowego 
#lub symetrycznego rozkładu.

# 4. logarytmujemy szereg ma to na celu zminiejszenie wariancji
tour_sample.xts$ltour <- log(tour_sample.xts$tour)
plot(tour_sample.xts$ltour)

# Obejrzyjmy korelogramy dla lair
par(mar = rep(2, 4))

par(mfrow = c(2, 1))
acf(tour_sample.xts$ltour,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "black", na.action = na.pass)
pacf(tour_sample.xts$ltour, lag.max=36,
     lwd = 4,
     col = "black", na.action = na.pass)
par(mfrow = c(1, 1))


# 5. wprowadzenie roznic regularnych lair
tour_sample.xts$dltour <- diff.xts(tour_sample.xts$ltour, lag = 1)
plot(tour_sample.xts$dltour)

# Obejrzyjmy korelogramy dla pierwszych różnic
par(mfrow = c(2, 1))
acf(tour_sample.xts$dltour,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "black", na.action = na.pass)
pacf(tour_sample.xts$dltour, lag.max=36,
     lwd = 4,
     col = "black", na.action = na.pass)
par(mfrow = c(1, 1))

# Pierwsze różnice wykazują silną sezonowość
# (wolno wygasająca ACF wielokrotności 12-tego opóźnienia)
# A zatem dodatkowo różnicujemy szereg sezonowo, tj. obliczamy dwunaste różnice
# (ponieważ mamy dane miesięczne)

# 6. wprowadzenie roznic sezonowych dla dlair
tour_sample.xts$d12dltour <- diff.xts(tour_sample.xts$dltour, lag = 12)
plot(tour_sample.xts$d12dltour)

# Obejrzyjmy korelogramy dla roznic sezonowych dlair
par(mfrow = c(2, 1))
acf(tour_sample.xts$d12dltour,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "black", na.action = na.pass)
pacf(tour_sample.xts$d12dltour, lag.max=36,
     lwd = 4,
     col = "black", na.action = na.pass)
par(mfrow = c(1, 1))


# 7. Formalne testowanie
# Krok 1. Test Dickeya, Haszy, Fullera (DHF) dla lair

tour_sample.xts$d12ltour <- diff.xts(tour_sample.xts$ltour, lag = 12)
tour_sample.xts$lag12ltour <- lag.xts(tour_sample.xts$ltour, k = 12)

plot(tour_sample.xts$d12dltour)

model1=lm(d12ltour~0+lag12ltour, data=tour_sample.xts)
summary(model1)
bg1 <- bgtest(model1, order = 1)
bg1


# Test ADHF dla lair
tour_sample.xts$lagd12ltour <- lag.xts(tour_sample.xts$d12ltour, k = 1)

model2=lm(d12ltour~0+lag12ltour+lagd12ltour, data=tour_sample.xts)
summary(model2)
bg1 <- bgtest(model2, order = 1)
bg1

tour_sample.xts$lag2d12ltour <- lag.xts(tour_sample.xts$d12ltour, k = 2)

model3=lm(d12ltour~0+lag12ltour+lagd12ltour+lag2d12ltour, data=tour_sample.xts)
summary(model3)
bg1 <- bgtest(model3, order = 1)
bg1

#dalej mamy autokorelacje 

tour_sample.xts$lag3d12ltour <- lag.xts(tour_sample.xts$d12ltour, k = 3)

model4 <- lm(d12ltour ~ 0 + lag12ltour + lagd12ltour + lag2d12ltour + lag3d12ltour, data = tour_sample.xts)
summary(model4)
bg1 <- bgtest(model4, order = 1)
bg1
bg2 <- bgtest(model4, order = 2)
bg2
bg3 <- bgtest(model4, order = 3)
bg3
bg4 <- bgtest(model4, order = 4)
bg4
bg5 <- bgtest(model4, order = 5)
bg5
bg6 <- bgtest(model4, order = 6)
bg6

# Podsumowując test ADHF:
# Statystyka testowa: 2.650
# Statystyka krytyczna: -5.86
# Decyzja: nie ma podstaw do odrzucenia H0, czyli róznice sezonowe sa potrzebne

#test DF
# H0: zmienna d12ltour jest zmienna niestacjonarna

testdf(variable = tour_sample.xts$d12ltour ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 6)

# brak podstaw do odrzucenia H0.

#test DF
# H0: zmienna d12dlair jest zmienna niestacjonarna

testdf(variable = tour_sample.xts$d12dltour ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 6)

# odrzucamy H0.

# podsumowanie: otrzymaliśmy zatem szereg stacjonarny.
# d=1, D=1

# nalezy dodatkowo przeprowadzic test KPSS

# 8. Czy zmienna d12dltour jest bialym szumem?

# H0: zmienna d12dltour jest bialym szumem

# Test Ljung-Boxa
Box.test(tour_sample.xts$d12dltour, type = "Ljung-Box", lag = 36)

# Test Boxa-Pierce
Box.test(tour_sample.xts$d12dltour, type = "Box-Pierce", lag = 36)


###########################################################################
# II. Identyfikacja                                                       #
###########################################################################

# IDENTYFIKACJA rzędów P i Q

# Analiza korelogramow ACF i PACF dla szeregu d12dltour

par(mfrow = c(2, 1))
acf(tour_sample.xts$d12dltour,  lag.max = 36, 
    xlim=c(2,36),
    lwd = 4,
    col = "black", na.action = na.pass)
pacf(tour_sample.xts$d12dltour, lag.max=36,
     lwd = 4,
     col = "black", na.action = na.pass)
par(mfrow = c(1, 1))

# Korelogramy sugerują, że możemy mieć do czynienia z sezonowym procesem MA
# (malejąca PACF dla wielokrotnośći 12 opóźnienia)

#Q= 1. P= 1 bo wygasa i q=3 a p=2
#wiec mamy ARIMA (2,1,3) SARIMA(0,1,1,12)???



###########################################################################
# III. Estymacja                                                          #
###########################################################################
nobs <- length(tour_sample.xts$ltour)

# SARIMA(0,1,0)(1,1,1)
arima010111 <- arima(tour_sample.xts$ltour,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 1),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)

arima010111
coeftest(arima010111)
# parametr przy sezonowym efekcie SMA nie jest istotny - co wtedy? 

par(mfrow = c(2, 1))
acf(resid(arima010111), lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36), lwd = 4, col = "red")
pacf(resid(arima010111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))


# Estymacji modelu SARIMA(0,1,0)(0,1,1)

arima010011 <- arima(tour_sample.xts$ltour,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)

arima010011
coeftest(arima010011)
# parametr przy sezonowym efekcie SMA jest istotny

par(mfrow = c(2, 1))
acf(resid(arima010011), lag.max = 36,
    ylim = c(-0.4, 0.4),xlim=c(2,36), lwd = 4, col = "red")
pacf(resid(arima010011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# test LR
teststat<- 2*(as.numeric(logLik(arima010111))-as.numeric(logLik(arima010011)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )

#nterpretacja wyników jest taka, że p-wartość (0.1515131) jest większa niż typowy poziom istotności 
#(np. 0.05). Oznacza to, że nie ma wystarczających dowodów na odrzucenie hipotezy zerowej, 
#czyli braku istotnej różnicy między dwoma porównywanymi modelami. 
#Wnioskujemy, że sezonowy efekt SMA (sma1) nie jest istotny w modelu ARIMA(0,1,0)(1,1,1).


# Estymacji modelu SARIMA(0,1,0)(0,1,0)

arima010010 <- arima(tour_sample.xts$ltour,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 0),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)

arima010010
coeftest(arima010010)
# parametr przy sezonowym efekcie SMA jest istotny

# test LR
teststat<- 2*(as.numeric(logLik(arima010111))-as.numeric(logLik(arima010010)))
teststat

pchisq(teststat, df=2, lower.tail = FALSE )

#Interpretacja wyników jest taka, że p-wartość jest znacznie mniejsza od typowego poziomu istotności 
#(np. 0.05). Oznacza to, że istnieją istotne statystycznie różnice między dwoma porównywanymi modelami. 
#Wnioskujemy, że model ARIMA(0,1,0)(1,1,1) jest statystycznie lepszy od modelu ARIMA(0,1,0)(1,0,0), 
#a różnica w wartości log-wiarygodności między tymi modelami jest istotna.

# PODSUMOWUJAC: czesc sezonowa SARIMA(0,1,0)(1,1,1) - procedura od ogolu do szczegolu

# wartości AIC
AIC(arima010111, arima010011, arima010010)
#czesc sezonowa SARIMA(0,1,0)(0,1,1) - AIC
# wartosci BIC
BIC(arima010111, arima010011, arima010010) 
#czesc sezonowa SARIMA(0,1,0)(0,1,1) - BIC

#NAJLEPSZA ARIMA010111:

# Estymacji modelu SARIMA(0,1,0)(1,1,1)

arima010111 <- arima(tour_sample.xts$ltour,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 1),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)

arima010111
coeftest(arima010111)
# parametr przy sezonowym efekcie SMA jest istotny

par(mfrow = c(2, 1))
acf(resid(arima010111), lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36), lwd = 4, col = "red")
pacf(resid(arima010111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))


# efekty sezonowe zostały wyjaśnione
# przystępujemy do identyfikacji efektóW regularnych


# SARIMA(2,1,3)(1,1,1)
arima213111 <- arima(tour_sample.xts$ltour,
                     order = c(2, 1, 3),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima213111
coeftest(arima213111)
#nie da sie tego zrobic wiec zaczynam od SARIMA(2,1,2)(1,1,1)


# SARIMA(2,1,2)(1,1,1)
arima212111 <- arima(tour_sample.xts$ltour,
                     order = c(2, 1, 2),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima212111
coeftest(arima212111)

# SARIMA(1,1,2)(1,1,1)
arima112111 <- arima(tour_sample.xts$ltour,
                     order = c(1, 1, 2),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima112111
coeftest(arima112111)


# SARIMA(1,1,1)(1,1,1)
arima111111 <- arima(tour_sample.xts$ltour,
                     order = c(1, 1, 1),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima111111
coeftest(arima111111)

par(mfrow = c(2, 1))
acf(resid(arima111111),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
pacf(resid(arima111111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))


# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima111111), type = "Ljung-Box", lag = 36)
Box.test(resid(arima111111), type = "Box-Pierce", lag = 36)


# reszty wydają się być białym szumem

# SARIMA(0,1,1)(1,1,1)
arima011111 <- arima(tour_sample.xts$ltour,
                     order = c(0, 1, 1),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima011111
coeftest(arima011111)

par(mfrow = c(2, 1))
acf(resid(arima011111),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
pacf(resid(arima011111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima011111), type = "Ljung-Box", lag = 36)
Box.test(resid(arima011111), type = "Box-Pierce", lag = 36)

# reszty wydają się być białym szumem

# test LR
teststat<- 2*(as.numeric(logLik(arima111111))-as.numeric(logLik(arima011111)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )

# SARIMA(0,1,0)(1,1,1)
arima010111 <- arima(tour_sample.xts$ltour,
                     order = c(0, 1, 0),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima010111
coeftest(arima010111)

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima010111), type = "Ljung-Box", lag = 36)
Box.test(resid(arima010111), type = "Box-Pierce", lag = 36)

# reszty wydają się być białym szumem


# test LR
teststat<- 2*(as.numeric(logLik(arima111111))-as.numeric(logLik(arima010111)))
teststat

pchisq(teststat, df=2, lower.tail = FALSE )


# PODSUMOWUJAC: ostateczny model SARIMA(1,1,2)(1,1,1) - procedura od ogolu do szczegolu

# wartości AIC
AIC(arima212111,arima112111,arima111111, arima011111, arima010111)
#czesc sezonowa SARIMA(0,1,1)(1,1,1) - AIC
# wartosci BIC
BIC(arima212111,arima112111,arima111111, arima011111, arima010111) 
#czesc sezonowa SARIMA(0,1,1)(1,1,1) - BIC


# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima112111), type = "Ljung-Box", lag = 36)
Box.test(resid(arima112111), type = "Box-Pierce", lag = 36)


###########################################################################
# IV. Diagnostyka                                                         #
###########################################################################

# SARIMA(1,1,2)(1,1,1) 
arima112111 <- arima(tour_sample.xts$ltour,
                     order = c(1, 1, 2),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima112111
coeftest(arima112111)

par(mfrow = c(2, 1))
acf(resid(arima112111),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
pacf(resid(arima112111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))


###########################################################################
# V. Prognoza                                                             #
###########################################################################

# SARIMA(1,1,2)(1,1,1) 
arima112111 <- arima(tour_sample.xts$ltour,
                     order = c(1, 1, 2),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)


forecast <- predict(arima112111, n.ahead = 12)

# obejrzyjmy wyniki
forecast
str(forecast)

# zbior zawiera 2 elementy:
# pred - prognozy
# se - błąd standardowy prognozy

tour.xts$ltour<-log(tour.xts$tour)

# wykres prognoz
ts.plot(tour.xts[, 2],
        main = "12 months forecast of tourist")

# pocztek okresu prognozy
abline(v = 85, lty = 2, col = "gray")
lines(forecast$pred, col = "blue", lwd = 2)
lines(forecast$pred + 2 * forecast$se, col = "red", lty = 3)
lines(forecast$pred - 2 * forecast$se, col = "red", lty = 3)


# łączymy prognozy z oryginalnym szeregiem
tour_forecast <- data.frame(forecast = forecast$pred,
                           window(tour.xts$ltour,
                                  start = as.Date("2022-03-01")))
tour_forecast

# 7.
# sprawdzamy jakość prognozy
tour_forecast$mae <- abs(tour_forecast$ltour -
                           tour_forecast$forecast)
tour_forecast$mse <- (tour_forecast$ltour -
                        tour_forecast$forecast)^2
tour_forecast$mape <- abs((tour_forecast$ltour -
                             tour_forecast$forecast) /
                            tour_forecast$ltour)
tour_forecast$amape <- abs((tour_forecast$ltour -
                              tour_forecast$forecast) /
                            (tour_forecast$ltour +
                               tour_forecast$forecast))

colMeans(tour_forecast[, 3:6])

# aby zmienić 'naukowy' format liczb
# możemy skorzystać z opcji scipen
options(scipen = 5)
round(colMeans(tour_forecast[, 3:6]), 3)





