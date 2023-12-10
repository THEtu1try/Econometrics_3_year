#построение модели
model = lm(log(df_flats$price)~ df_flats$floor + log(df_flats$totalArea) + df_flats$roomsCount
           + df_flats$hasInternet + df_flats$hasTv + df_flats$timeMetro + df_flats$floorsHouse)

summary(model)



#Тест Чоу на индентичность генеральных совокупностей по дамми-переменной наличие интернета
rss_p = deviance(model)

s1 = subset(df_flats, hasInternet >= 1)
s2 = subset(df_flats, hasInternet <= 0)

model_s1 = lm(log(price) ~ floor + log(totalArea) + roomsCount + hasTv + timeMetro + floorsHouse , data=s1)
rss_1 = deviance(model_s1)

model_s2 = lm(log(price) ~ floor + log(totalArea) + roomsCount + hasTv + timeMetro + floorsHouse ,data=s2)
rss_2 = deviance(model_s2)

f_st = ((rss_p-(rss_1+rss_2))/(8))/((rss_1+rss_2)/(1000-2*8))

2*(1-pf(f_st, 8, 984))


#Проверка гипотезы о том, что наличие телевизора имеет большее влияние, чем наличие интреннета
library('car')
help(linearHypothesis)
test_res = linearHypothesis(model, c("df_flats$hasTv - df_flats$hasInternet = 0"), alt='left', test = "F")
print(test_res)

1-pf(14.22, 1, 992)


#Выгрузка данных 
install.packages('stargazer')
library(stargazer)
stargazer(model, type = "html", out = "estimations_class2.doc")
stargazer(test_res, type = "html", out = "estimations_class2.doc")