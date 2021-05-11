#Горячев Кирилл Сергеевич - создайте модель множественной линейной регрессии дневных
#потоков углексилого газа за летний период 2013 года по данным измерений
#методом турбулентной пульсации

getwd()	

#Устанавливаем необходимые пакеты

library("tidyr")	
library("tibble")	
library("tidyverse") 	
library("stringr")    	
library("dplyr")      	
library("ggplot2")  	


#считаем данные и удалим ненужные строчки	

eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))	
eddypro = eddypro[-1, ]	
eddypro = select(eddypro, -(roll))	
eddypro = eddypro %>% mutate_if(is.character, factor)	


# назовем столбцы допустимыми символами	

names(eddypro) = names(eddypro) %>% 	
str_replace_all("[!]", "_emph_") %>% 	
str_replace_all("[?]", "_quest_") %>% 	
str_replace_all("[*]", "_star_") %>% 	
str_replace_all("[+]", "_plus_") %>%	
str_replace_all("[-]", "_minus_") %>%	
str_replace_all("[@]", "_at_") %>%	
str_replace_all("[$]", "_dollar_") %>%	
str_replace_all("[#]", "_hash_") %>%	
str_replace_all("[/]", "_div_") %>%	
str_replace_all("[%]", "_perc_") %>%	
str_replace_all("[&]", "_amp_") %>%	
str_replace_all("[\\^]", "_power_") %>%	
str_replace_all("[()]", "L_")	

glimpse(eddypro)	


# отберем данные с 1 июня по 31 августа и по дневному времени	
eddypro=drop_na(eddypro)	
eddypro=filter(eddypro, DOY >= 152 & DOY < 244)	
eddypro=filter(eddypro, daytime==TRUE)	
eddypro_numeric=eddypro[,sapply(eddypro,is.numeric)]	
eddypro_non_numeric=eddypro[,!sapply(eddypro,is.numeric)]	

cor_td=cor(eddypro_numeric)	
cor_td	
cor_td=cor(drop_na(eddypro_numeric)) %>% as.data.frame %>% select(co2_flux)	
vars=row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude	


# создадим обучающую и тестирующую выборки

row_numbers=1:length(eddypro_numeric$co2_flux)	
teach=sample(row_numbers,floor(length(eddypro_numeric$co2_flux)*.7))	
test=row_numbers[-teach]	
teaching_tbl=eddypro_numeric[teach,]	
testing_tbl=eddypro_numeric[test,]	


#модель 1	

mod1=lm(co2_flux~(.),data=teaching_tbl)	

summary(mod1)	
coef(mod1)	
resid(mod1)	
confint(mod1)	
anova(mod1)	
plot(mod1) 	

#Для первой модели R2 равен 0.99. Исключаем незначимые переменные
#и создаем вторую модель

mod2=lm(co2_flux~DOY+file_records+Tau+qc_Tau+rand_err_Tau+H+qc_H+rand_err_H+LE+qc_LE
        +rand_err_LE+qc_co2_flux+rand_err_co2_flux+h2o_flux+H_strg
        +co2_molar_density+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density
        +h2o_mole_fraction+h2o_mixing_ratio+h2o_time_lag+sonic_temperature+air_temperature
        +air_pressure+air_density+air_heat_capacity+air_molar_volume+water_vapor_density
        +e+es+specific_humidity+RH+VPD+Tdew+u_unrot+v_unrot+w_unrot+u_rot+w_rot+max_speed
        +yaw+u_star_+TKE+L_z_minus_dL__div_L+T_star_ +x_peak 
        +x_offset+x_10_perc_+x_30_perc_+x_50_perc_+x_70_perc_+un_Tau+Tau_scf
        +un_H+H_scf+un_LE+LE_scf+un_co2_flux+w_spikes+mean_value+co2_var+h2o_var,data = teaching_tbl)	

summary(mod2)	
coef(mod2)	
resid(mod2)	
confint(mod2)	
anova(mod2)	
plot(mod2)		


#Для модели 2 R2 равен 0.99. Продолжаем исключать переменные.

mod3=lm(co2_flux~DOY+file_records+Tau+qc_Tau+rand_err_Tau+H+qc_H+rand_err_H+LE+qc_LE
        +rand_err_LE+qc_co2_flux+rand_err_co2_flux+h2o_flux+H_strg
        +co2_molar_density+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density
        +h2o_mole_fraction+h2o_mixing_ratio+h2o_time_lag+sonic_temperature+air_temperature
        +air_pressure+air_density+air_heat_capacity+air_molar_volume+water_vapor_density
        +e+es+specific_humidity+RH+VPD+Tdew+u_unrot+v_unrot+w_unrot+u_rot+w_rot+max_speed
        +u_star_+TKE+L_z_minus_dL__div_L+T_star_ +x_peak 
        +x_offset+x_10_perc_+x_30_perc_+x_50_perc_+x_70_perc_+un_Tau+Tau_scf
        +un_H+H_scf+un_LE+LE_scf+un_co2_flux+mean_value+co2_var+h2o_var,data = teaching_tbl)	

summary(mod3)	
coef(mod3)	
resid(mod3)	
confint(mod3)	
anova(mod3)	
plot(mod3)	

#Для модели 3 R2 равен 0.99, продолжаем исключать переменные

mod4=lm(co2_flux~DOY+file_records+Tau+qc_Tau+rand_err_Tau+H+qc_H+rand_err_H+LE+qc_LE
        +rand_err_LE+qc_co2_flux+rand_err_co2_flux+h2o_flux+H_strg
        +co2_molar_density+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density
        +h2o_mole_fraction+h2o_mixing_ratio+h2o_time_lag+sonic_temperature+air_temperature
        +air_pressure+air_density+air_heat_capacity+air_molar_volume+water_vapor_density
        +e+es+specific_humidity+RH+VPD+Tdew+u_unrot+v_unrot+w_unrot+u_rot+w_rot+max_speed
        +u_star_+TKE+L_z_minus_dL__div_L+T_star_ +x_peak 
        +x_offset+x_10_perc_+x_30_perc_+x_50_perc_+x_70_perc_+un_Tau+Tau_scf
        +un_H+H_scf+un_LE+LE_scf+un_co2_flux+mean_value+co2_var+h2o_var,data = teaching_tbl)

summary(mod4)	
coef(mod4)	
resid(mod4)	
confint(mod4)	
anova(mod4)	
plot(mod4)	

#Корреляционный анализ

cor_teaching_tbl=select(teaching_tbl,co2_flux,DOY,file_records,Tau,qc_Tau,rand_err_Tau,H,qc_H,rand_err_H,LE,qc_LE,
                        rand_err_LE,qc_co2_flux,rand_err_co2_flux,h2o_flux,H_strg,
                        co2_molar_density,co2_mole_fraction,co2_mixing_ratio,h2o_molar_density,
                        h2o_mole_fraction,h2o_mixing_ratio,h2o_time_lag,sonic_temperature,air_temperature,
                        air_pressure,air_density,air_heat_capacity,air_molar_volume,water_vapor_density,
                        e,es,specific_humidity,RH,VPD,Tdew,u_unrot,v_unrot,w_unrot,u_rot,w_rot,max_speed,
                        u_star_,TKE,L_z_minus_dL__div_L,T_star_ ,x_peak, 
                        x_offset,x_10_perc_,x_30_perc_,x_50_perc_,x_70_perc_,un_Tau,Tau_scf,
                        un_H,H_scf,un_LE,LE_scf,un_co2_flux,mean_value,co2_var,h2o_var)

cor_td=cor(cor_teaching_tbl)%>%as.data.frame

qplot(co2_flux,co2_flux,data=teaching_tbl)+geom_line(aes(y=predict(mod4,teaching_tbl)))	
qplot(co2_flux,co2_flux,data=testing_tbl)+geom_line(aes(y=predict(mod4,testing_tbl)))	

#Примеры графиков	
qplot(DOY,co2_flux,data=testing_tbl)+geom_line(aes(y=predict(mod4,testing_tbl)))	
qplot(Tau,co2_flux,data=testing_tbl)+geom_line(aes(y=predict(mod4,testing_tbl)))	
