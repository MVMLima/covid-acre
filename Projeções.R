library(TSstudio)
library(forecast)


set.seed(1234)

Acre_ts = ts(Acre, start = c(2020,3,15), frequency = 7)

methods <- list(ets = list(method = "ets",
                           method_arg = list(opt.crit = "lik"),
                           notes = "ETS model"),
                auto.arima = list(method = "auto.arima",
                                  method_arg = NULL,
                                  notes = "Auto.arima Model"),
                nnetar = list(method = "nnetar",
                              method_arg = NULL,
                              notes = "NNETAR Model"),
                hw = list(method = "HoltWinters",
                          method_arg = NULL,
                          notes = "HoltWinters Model"),
                tslm = list(method = "tslm",
                            method_arg = list(formula = input ~ trend + season),
                            notes = "tslm model with trend and seasonal components"))

train_method = list(partitions = 10,
                    sample.out = 30,
                    space = 7)

md <- train_model(input = Acre_ts2,
                  methods = methods,
                  train_method = train_method,
                  horizon = 15,
                  error = "RMSE")

plot_error(md)

plot_model(md)


library(TSstudio)
library(forecast)

Acre <- read_excel("C:/Users/marcos.malveira/Desktop/COVID-19/Análise de Dados R/Gráficos R/Acre.xlsx")

set.seed(1234)

y <- msts(Acre, start = c(2020,31,3), seasonal.periods=c(7,365.25))
fit <- tbats(y)
fc <- forecast(fit, h = 7)
plot(fc)
accuracy(fc)
components = tbats.components(fit)
plot(components)
fc

split_Acre <- ts_split(ts.obj = y, sample.out = 7)
training <- split_Acre$train
testing <- split_Acre$test

length(y)

length(training)
length(testing)

fit2 <- tbats(training)
fc2 <- forecast(fit2, h = 7)
accuracy(fc2, testing)

fit3 = stl(training)
fc3 = forecast(fit3, h = 7)
accuracy(fc3, testing)

fit4 = nnetar(training)
fc4 = forecast(fit4, h = 7)
accuracy(fc4, testing)

fit5 = auto.arima(training, seasonal = T, seasonal.test = c("seas", "ocsb", "hegy", "ch"))
fc5 = forecast(fit5, h= 7)
accuracy(fc5, testing)

fcsim = forecast_sim(fit4, h=7, n=1000)
