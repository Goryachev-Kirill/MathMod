##Горячев Кирилл-для региона 68 рассчитайте урожайность пшеницы в 2006 году, взяв для рассчета средние суммы активных температур за текущий год ,
##с 10 ближайших метеостанций но убирая из рассчёта активных температур дни с температурой выше 25 градусов
#Устанавливаем нужную директорию 
setwd("D:/Goryachev-Kirill/MachMod/333/zadanie1_Goryachev")
getwd()

#Скачиваем необходимые пакеты
library(tidyverse)
library(rnoaa)

#Скачиваем и сохраняем на диск метеостанции
station_data=ghcnd_stations()
write.csv(station_data,"station_data2021.csv")
#Считывание списка метеостанций с диска
station_data = read.csv("station_data2021.csv") 

#После получения списка всех станций, выбираем из него список станций ближайших к столице нашего региона,Тамбов,создав таблицу с именем региона и координатами его столицы
tambov = data.frame(id = "TAMBOV", latitude = 52.73169,longitude = 41.44326)

# #Необходимо выбрать конечное число метеостанций ,которые имеют необходимые данные
# meteo_nearby_stations
tambov_around=meteo_nearby_stations(lat_lon_df = tambov,station_data = station_data,limit=10,var=c("PRCP","TAVG"),year_min=2006,year_max=2006)
tambov_around

#вспомним, как работать со списками
#1)очевидно что первым элементом таблицы будет
#идентификатор метеостанции Тамбова, его то мы и попытаемся получить
tambov_id = tambov_around[["TAMBOV"]][["id"]] [1]
tambov_id

all_tambov_data = meteo_tidy_ghcnd(stationid = tambov_id)
all_tambov_data

#Подумаем, какие из этих данных нам нужны
summary(all_tambov_data)

#Цикл для всех метеостанций
tambov_around = tambov_around[[1]]
tambov_around
station_names = tambov_around[["id"]]
station_names
for(station_name in station_names)
{
  station_data=meteo_tidy_ghcnd(stationid = station_name,var="TAVG", date_min = "2006-01-01", date_max = "2006-12-31")
  all_tambov_data=rbind(all_tambov_data)
}
#Запись полученных данных в файл
write.csv (all_tambov_data,"all_tambov_data.csv")
all_tambov_data

#2 часть
# считываем данные из файла all_tambov_data.csv
all_tambov_data = read.csv("all_tambov_data.csv")
str(all_tambov_data)
#ищем библиотеку из tidyverse, которая может помочь с датой
library(lubridate)

# вытащить год
#проверим, что работает
y = year(all_tambov_data$date); y
all_tambov_data[, "year"] =  year(all_tambov_data$ date)
#добавим месяц
all_tambov_data [,"month"]= month(all_tambov_data$date) 
#вытащить день от начала года
all_tambov_data [,"day_of_the_year"]= yday(all_tambov_data$date) 
#проверим результат
str(all_tambov_data)  

##Приведение средней суммы температур в подходящую форму, при помощи деления на 10
all_tambov_data[,"tavg"] = all_tambov_data$tavg/10
all_tambov_data

#Превращение всех NA и tavg <5 в нули 
all_tambov_data[is.na(all_tambov_data$tavg),"tavg"] = 0
all_tambov_data[all_tambov_data$tavg<5, "tavg"] = 0
all_tambov_data[all_tambov_data$tavg>25,"tavg"]=0
summary (all_tambov_data)

##Cуммарная температура за месяц за год для всех станций
#Группировка по метеостанциям, годам и месяцам при помощи функции group_by
alldays = group_by(all_tambov_data, id, year, month)
alldays

#функция summarize применяет некоторые действия к отдельным группам, полученным
#с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum
sumT_alldays_tambov = summarize(alldays, tsum = sum(tavg))

sumT_alldays_tambov


summary(sumT_alldays_tambov)


# Сгруппируем данные по месяцам  
groups_tambov_months = group_by(sumT_alldays_tambov,month)
# найдем для всех метеостанций среднее по месяцам
sumT_months = summarize(groups_tambov_months, St = mean(tsum))
sumT_months


## Подготовка к расчету по формуле Урожая ##
### Ввод констант
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000) 
# константа по табл.1. Создаем вектор
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000) 
# константа по табл. 1. Создаем вектор
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000) 

# отношение числа дней i-го месяца, 
#входящих в период вегетации культуры, к общему 
#числу дней в месяце,константа по табл. 1.
y = 1.0 
# Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
Kf = 300
# Коэффициент использования ФАР посевом 
Qj = 1600
# калорийность урожая культуры 
Lj = 2.2 
# сумма частей основной и побочной продукции 
Ej = 25 

# стандартная влажность культуры 
# Рассчитаем Fi по месяца
sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
##  Расчитываем урожай как сумму по месяцам ##
Yield = sum(sumT_months$Yi)

Yield
