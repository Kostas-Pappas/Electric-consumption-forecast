-------------------------------------------------------------------
## 0.  Libraries ####
--------------------------------------------------------------------
library(caret)            #R modeling workhorse , ggplot2
library(tidyverse)        #Package for tidying data
library(lubridate)        #For working with dates/times of a time series
library(VIM)              #Visualizing and imputing missing values
library(Hmisc)            #for descriptive statistics
library(forecast)         #forcasting package
library(broom)            #Tidy statistical summary output
library(knitr)            #report generation
library(plotly)           #visualization
library(imputeTS)         #imputing
library(ggplot2)          #visualization
library(ggfortify)        
library(naniar)           # handling missing values
library(dplyr)            # data manipulation
library(fpp2)             # time series analysis package
library(simputation)      #imputing
------------------------------------------------------------------------------
## 1. Data preparation ####
-----------------------------------------------------------------------
#-Load data set
house_pwr<- read_delim('C:/Users/Constantinos/Desktop/Ubiqum/IOT/household_power_consumption.txt',
                       col_names = TRUE,
                       col_types = cols(Global_active_power='d',
                                        Global_reactive_power='d',
                                        Voltage='d', 
                                        Global_intensity='d', 
                                        Sub_metering_1='d', 
                                        Sub_metering_2='d', 
                                        Sub_metering_3='d'), 
                       delim=';',
                       na='?')

#-Create new DateTime feature by combining Date and Time 
house_pwr <- unite(house_pwr, Date, Time, col='DateTime', sep=' ')

#-Convert data type of new DateTime feature

house_pwr$DateTime <- dmy_hms(house_pwr$DateTime)

attr(house_pwr$DateTime, "tzone") <- "Europe/Paris"

## Create attributes with lubridate
house_pwr$year <- year(house_pwr$DateTime)
house_pwr$month <- month(house_pwr$DateTime)
house_pwr$weekday <-wday(house_pwr$DateTime)
house_pwr$day <-day(house_pwr$DateTime)
house_pwr$hour <-hour(house_pwr$DateTime)
house_pwr$quarter <-quarter(house_pwr$DateTime)
house_pwr$am <-am(house_pwr$DateTime)
house_pwr$semester <-semester(house_pwr$DateTime)
house_pwr$minute <-minute(house_pwr$DateTime)
house_pwr$week <-week(house_pwr$DateTime)

#-Check class of new DateTime feature
class(house_pwr$DateTime)

## Inspect the data types
str(house_pwr)

## Gather summary statistics
summary(house_pwr)

##- Histograms
hist(house_pwr$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
hist(house_pwr$Voltage, main = "Voltage", xlab = "minute-averaged voltage (in volt)", col = "green")
hist(house_pwr$Global_reactive_power, main = "Global_reactive_power", xlab = "Global Active Power (kilowatts)", col = "blue")
hist(house_pwr$Global_intensity, main = "Global_intensity", xlab = "Global Active Power (kilowatts)", col = "yellow")
hist(house_pwr$Sub_metering_1, main = "Sub_metering_1", xlab = "kitchen", col = "pink")
hist(house_pwr$Sub_metering_2, main = "Sub_metering_2", xlab = "laundry room", col = "brown")
hist(house_pwr$Sub_metering_3, main = "Sub_metering_3", xlab = "electric water-heater and an air-conditioner", col = "purple")
#hist(house_pwr$Sub_metering_4, main = "Sub_metering_4", xlab = "rest of consumprion", col = "black")
------------------------------------------------------------------------
## > Missing Values ####
---------------------------------------------------------------------
any_na(house_pwr) #  TRUE

n_miss(house_pwr) # 181853 total number of missing values in the data

n_miss(house_pwr$Global_active_power)# 25979 number of missing values in each attribute

prop_miss(house_pwr) #  0.005 NA %

prop_complete(house_pwr) #  0.995 Values%

miss_scan_count(data = house_pwr, search = list("N/A", "missing","na", " "))# No strange missing values

# Visualising missingness patterns
gg_miss_upset(house_pwr) # give an overall pattern of missingness.
gg_miss_fct(x = house_pwr, fct = year) #factor of interest: year
gg_miss_fct(x = house_pwr, fct = month)
gg_miss_fct(x = house_pwr, fct = day)
gg_miss_fct(x = house_pwr, fct = quarter)
gg_miss_fct(x = house_pwr, fct = am)
gg_miss_fct(x = house_pwr, fct = weekday)

# Build a Dataset with hot deck imputation

DI<-impute_knn(house_pwr, Global_active_power ~ Sub_metering_3 )

---------------------------------------------------------------------------
## 2. Visualization ####
----------------------------------------------------------------------
# Initial visualization

# ----Total consumption per quarter and per submitter ####
# dataset for quarters
quarters <- house_pwr %>% filter(year<2011,year>2006) %>% 
          group_by(quarter) %>% 
          select(quarter,
                 Global_active_power,
                 Sub_metering_1,
                 Sub_metering_2,
                 Sub_metering_3) %>% 
          summarise(Active_Power=sum(Global_active_power,na.rm=TRUE),
                    Kitchen=sum(Sub_metering_1,na.rm=TRUE),
                    Laundry=sum(Sub_metering_2,na.rm = TRUE),
                    Heating=sum(Sub_metering_3,na.rm = TRUE))

quarters <- as.data.frame(quarters)
# use tidyr::gather to make a tidy dataset key= the meters 
# use : to indicate like -> Kitchen:Heating
Tidy_quarters<-quarters%>% 
  gather(key="meter" ,value="total",Kitchen:Heating, factor_key=TRUE)

options(scipen = 999)# to have numbers big  without power notation
  
ggplot(Tidy_quarters,aes(x=factor(quarter), y=total)) +
  labs(x='Quarter of the Year', y='Wh') +
  ggtitle('Total Quarterly Energy Consumption') +
  geom_bar(stat='identity', aes(fill = meter), color='black') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 14))

------------------------------------------------------------------------
# ----Total consumption per month and per submitter ####
----------------------------------------------------------------------
months <- house_pwr %>% filter(year<2011,year>2006) %>% 
  group_by(month) %>% 
  select(month,
         Global_active_power,
         Sub_metering_1,
         Sub_metering_2,
         Sub_metering_3) %>% 
  summarise(Active_Power=sum(Global_active_power,na.rm=TRUE),
            Kitchen=sum(Sub_metering_1,na.rm=TRUE),
            Laundry=sum(Sub_metering_2,na.rm = TRUE),
            Heating=sum(Sub_metering_3,na.rm = TRUE))

months <- as.data.frame(months)

Tidy_months<-months%>% 
  gather(key="meter" ,value="total",Kitchen:Heating, factor_key=TRUE)

options(scipen = 999)# to have numbers big  without power notation

ggplot(Tidy_months,aes(x=factor(month), y=total)) +
  labs(x='Month of the Year', y='Wh') +
  ggtitle('Total Monthly Energy Consumption') +
  geom_bar(stat='identity', aes(fill = meter), color='black') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 14))

cycle(TS_m_Active)
boxplot(TS_m_Active~cycle(TS_m_Active),main='Monthly Watt/hour Consumption',
        xlab='Month', ylab = 'Watt/hour')
----------------------------------------------------------------------
# ----Total consumption per day and per submitter ####
---------------------------------------------------------------------
day <- house_pwr %>% filter(year<2011,year>2006) %>% 
  group_by(weekday) %>% 
  select(weekday,
         Global_active_power,
         Sub_metering_1,
         Sub_metering_2,
         Sub_metering_3) %>% 
  summarise(Active_Power=sum(Global_active_power,na.rm=TRUE),
            Kitchen=sum(Sub_metering_1,na.rm=TRUE),
            Laundry=sum(Sub_metering_2,na.rm = TRUE),
            Heating=sum(Sub_metering_3,na.rm = TRUE))

day <- as.data.frame(day)

Tidy_day<-day%>% 
  gather(key="meter" ,value="total",Kitchen:Heating, factor_key=TRUE)

options(scipen = 999)# to have numbers big  without power notation

ggplot(Tidy_day,aes(x=factor(weekday), y=total)) +
  labs(x='Day of the week', y='Wh') +
  ggtitle('Total daily Energy Consumption') +
  geom_bar(stat='identity', aes(fill = meter), color='black') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 14))

------------------------------------------------------------------------
# ----Total consumption hourly and per submitter ####
------------------------------------------------------------------------
hour <- house_pwr %>% filter(hour>5,year>2006,year<2011) %>% 
  group_by(hour) %>% 
  select(hour,
         Global_active_power,
         Sub_metering_1,
         Sub_metering_2,
         Sub_metering_3) %>% 
  summarise(Active_Power=sum(Global_active_power,na.rm=TRUE),
            Kitchen=sum(Sub_metering_1,na.rm=TRUE),
            Laundry=sum(Sub_metering_2,na.rm = TRUE),
            Heating=sum(Sub_metering_3,na.rm = TRUE))

hour <- as.data.frame(hour)

Tidy_hour<-hour%>% 
  gather(key="meter" ,value="total",Kitchen:Heating, factor_key=TRUE)

options(scipen = 999)# to have numbers big  without power notation

ggplot(Tidy_hour,aes(x=factor(hour), y=total)) +
  labs(x='Day of the week', y='Wh') +
  ggtitle('Total daily Energy Consumption') +
  geom_bar(stat='identity', aes(fill = meter), color='black') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 14))

----------------------------------------------------------------------
## 3. Creating Granular Datasets ####
-------------------------------------------------------------------------

  # adjusting kilowatts per min to watts per hour
house_pwr$Global_active_power<-house_pwr$Global_active_power*1000/60
house_pwr$Global_reactive_power<-house_pwr$Global_reactive_power*1000/60



# Monthly aggragate dataset nm = no mising 
Data_month_nm <- as.data.frame (house_pwr %>% group_by(year,month) %>% 
                               select(year,month,Global_active_power,Global_reactive_power,Sub_metering_1,Sub_metering_2,Sub_metering_3) %>% 
                               summarise(mtot_Active=sum(Global_active_power,na.rm = TRUE),
                                         mtot_Reactive=sum(Global_reactive_power,na.rm = TRUE),
                                         mtot_Sub_metering_1=sum(Sub_metering_1,na.rm = TRUE),
                                         mtot_Sub_metering_2=sum(Sub_metering_2,na.rm = TRUE),
                                         mtot_Sub_metering_3=sum(Sub_metering_3,na.rm = TRUE)))

Data_month <- as.data.frame (house_pwr %>% group_by(year,month) %>% 
                                  select(year,month,Global_active_power,Global_reactive_power,Sub_metering_1,Sub_metering_2,Sub_metering_3) %>% 
                                  summarise(mtot_Active=sum(Global_active_power),
                                            mtot_Reactive=sum(Global_reactive_power),
                                            mtot_Sub_metering_1=sum(Sub_metering_1),
                                            mtot_Sub_metering_2=sum(Sub_metering_2),
                                            mtot_Sub_metering_3=sum(Sub_metering_3)))

# Weekly aggragate dataset
Data_week_nm <- as.data.frame (house_pwr %>% group_by(year,month,week) %>% 
                              select(year,month,week,Global_active_power,Global_reactive_power,Sub_metering_1,Sub_metering_2,Sub_metering_3) %>% 
                              summarise(wtot_Active=sum(Global_active_power,na.rm = TRUE),
                                        wtot_Reactive=sum(Global_active_power,na.rm = TRUE),
                                        wtot_Sub_metering_1=sum(Sub_metering_1,na.rm = TRUE),
                                        wtot_Sub_metering_2=sum(Sub_metering_2,na.rm = TRUE),
                                        wtot_Sub_metering_3=sum(Sub_metering_3,na.rm = TRUE)))

Data_week <- as.data.frame (house_pwr %>% group_by(year,month,week) %>% 
                                 select(year,month,week,Global_active_power,Global_reactive_power,Sub_metering_1,Sub_metering_2,Sub_metering_3) %>% 
                                 summarise(wtot_Active=sum(Global_active_power),
                                           wtot_Reactive=sum(Global_active_power),
                                           wtot_Sub_metering_1=sum(Sub_metering_1),
                                           wtot_Sub_metering_2=sum(Sub_metering_2),
                                           wtot_Sub_metering_3=sum(Sub_metering_3)))
#Daily aggragate dataset( If there is time)
Data_day_nm <- as.data.frame (house_pwr %>% group_by(year,month,week,day) %>%
                              select(year,month,week,day,Global_active_power,Global_reactive_power,Sub_metering_1,Sub_metering_2,Sub_metering_3) %>%
                              summarise(dtot_Active=sum(Global_active_power,na.rm = TRUE)),
                                        dtot_Reactive=sum(Global_active_power,na.rm = TRUE),
                                        dtot_Sub_metering_1=sum(Sub_metering_1,na.rm = TRUE),
                                        dtot_Sub_metering_2=sum(Sub_metering_2,na.rm = TRUE),
                                        dtot_Sub_metering_3=sum(Sub_metering_3,na.rm = TRUE))

Data_day <- as.data.frame (house_pwr %>% group_by(year,month,week,day) %>%
                                select(year,month,week,day,Global_active_power,Global_reactive_power,Sub_metering_1,Sub_metering_2,Sub_metering_3) %>%
                                summarise(dtot_Active=sum(Global_active_power),
                                          dtot_Reactive=sum(Global_active_power),
                                          dtot_Sub_metering_1=sum(Sub_metering_1),
                                          dtot_Sub_metering_2=sum(Sub_metering_2),
                                          dtot_Sub_metering_3=sum(Sub_metering_3)))


# Imputing using kalman filter

Data_month[ ,4] <- na.kalman(Data_month[ ,4])
Data_month[ ,5] <- na.kalman(Data_month[ ,5])
Data_month[ ,6] <- na.kalman(Data_month[ ,6])
Data_month[ ,7] <- na.kalman(Data_month[ ,7])
Data_week[ ,4] <- na.kalman(Data_week[ ,4])
Data_week[ ,5] <- na.kalman(Data_week[ ,5])
Data_week[ ,6] <- na.kalman(Data_week[ ,6])
Data_week[ ,7] <- na.kalman(Data_week[ ,7])
Data_week[ ,8] <- na.kalman(Data_week[ ,8])
Data_day[ ,4] <- na.kalman(Data_day[ ,4])
Data_day[ ,5] <- na.kalman(Data_day[ ,5])
Data_day[ ,6] <- na.kalman(Data_day[ ,6])
Data_day[ ,7] <- na.kalman(Data_day[ ,7])
Data_day[ ,8] <- na.kalman(Data_day[ ,8])
Data_day[ ,9] <- na.kalman(Data_day[ ,9])

-------------------------------------------------------------------
## 4. Create time series and visualize ####
---------------------------------------------------------------------

#-----Time series for monthly aggregated consumption ####

# For months the no missing dataset is used  
TS_m_SM1<-ts(Data_month_nm$mtot_Sub_metering_1,frequency=12,start=c(2007,1))
autoplot(TS_m_SM1,ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter consumption paterns")
ggseasonplot(TS_m_SM1,xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1consumption paterns")
ggseasonplot(TS_m_SM1,main = "Sub-meter 1",polar = TRUE)

TS_m_SM2<-ts(Data_month_nm$mtot_Sub_metering_2,frequency=12,start=c(2007,1))
autoplot(TS_m_SM2,ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")
ggseasonplot(TS_m_SM2,main = "Sub-meter 2")
ggseasonplot(TS_m_SM2,main = "Sub-meter 2",polar = TRUE)

TS_m_SM3<-ts(Data_month_nm$mtot_Sub_metering_3,frequency=12,start=c(2007,1))
autoplot(TS_m_SM3,ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
ggseasonplot(TS_m_SM3, main = "Sub-meter 3")

TS_m_Active<-ts(Data_month_nm$mtot_Active,frequency=12,start=c(2007,1))
autoplot(TS_m_Active,ts.colour = 'blue', xlab = "Time", ylab = "Watt Hours", main = "Active Power consumption monthly")
ggseasonplot(TS_m_Active,main = "Active Power")

TS_m_Reactive<-ts(Data_month_nm$mtot_Reactive,frequency=12,start=c(2007,1))
autoplot(TS_m_Reactive,ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Reactive Power")
ggseasonplot(TS_m_Reactive,main = "Reactive Power")


#-----Time series for weekly aggregated consumption ####

TS_w_SM1<-ts(Data_week$wtot_Sub_metering_1,frequency=52,start=c(2007,1),end=c(2011,9))
autoplot(TS_w_SM1,ts.colour = 'brown', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")
ggseasonplot(TS_w_SM1,main = "Sub-meter 1")
ggseasonplot(TS_w_SM1,main = "Sub-meter 1",polar = TRUE)

TS_w_SM2<-ts(Data_week$wtot_Sub_metering_2,frequency=52,start=c(2007,1),end=c(2011,9))
autoplot(TS_w_SM2,ts.colour = 'brown', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")
ggseasonplot(TS_w_SM2,main = "Sub-meter 2")
ggseasonplot(TS_w_SM2,main = "Sub-meter 2",polar = TRUE)

TS_w_SM3<-ts(Data_week$wtot_Sub_metering_3,frequency=52,start=c(2007,1),end=c(2011,9))
autoplot(TS_w_SM3,ts.colour = 'brown', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
ggseasonplot(TS_w_SM3, main = "Sub-meter 3")

TS_w_Active<-ts(Data_week$wtot_Active,frequency=52,start=c(2007,1),end=c(2011,9))
autoplot(TS_w_Active,ts.colour = 'brown', xlab = "Time", ylab = "Watt Hours", main = "Active Power")
ggseasonplot(TS_w_Active,main = "Active Power")

TS_w_Reactive<-ts(Data_week$wtot_Reactive,frequency=52,start=c(2007,1),end=c(2011,9))
autoplot(TS_w_Reactive,ts.colour = 'brown', xlab = "Time", ylab = "Watt Hours", main = "Reactive Power")
ggseasonplot(TS_w_Reactive,main = "Reactive Power")

#all time series together
autoplot(TS_w_SM1,ts.colour = 'blue', xlab = "Time", ylab = "Watt Hours", main = "Power consumption weekly")+
autolayer(TS_w_SM3,ts.colour = 'green')+
autolayer(TS_w_SM2,ts.colour = 'red') 


#-----Time series for daily aggregated consumption ####

TS_d_SM1<-ts(Data_day$dtot_Sub_metering_1,frequency=365,start=c(2007,1),end=(2011))
autoplot(TS_d_SM1,ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")
ggseasonplot(TS_d_SM1,main = "Sub-meter 1")
ggseasonplot(TS_d_SM1,main = "Sub-meter 1",polar = TRUE)

TS_d_SM2<-ts(Data_day$dtot_Sub_metering_2,frequency=365,start=c(2007,1),end=(2011))
autoplot(TS_d_SM2,ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")
ggseasonplot(TS_d_SM2,main = "Sub-meter 2")
ggseasonplot(TS_d_SM2,main = "Sub-meter 2",polar = TRUE)

TS_d_SM3<-ts(Data_day$dtot_Sub_metering_3,frequency=365,start=c(2007,1),end=(2011))
autoplot(TS_d_SM3,ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
ggseasonplot(TS_d_SM3, main = "Sub-meter 3")

TS_d_Active<-ts(Data_day$dtot_Active,frequency=365,start=c(2007,1),end=(2011))
autoplot(TS_d_Active,ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Active Power")
ggseasonplot(TS_d_Active,main = "Active Power")

TS_d_Reactive<-ts(Data_day$dtot_Reactive,frequency=365,start=c(2007,1),end=(2011))
autoplot(TS_d_Reactive,ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Reactive Power")
ggseasonplot(TS_d_Reactive,main = "Reactive Power")

# Plot the time series

plot(TS_m_Active, main = "Active power [watts/min]")
lines(TS_m_SM1, col = "red")# Add lines
lines(TS_m_SM2, col = "blue")
lines(TS_m_SM3, col = "green")
axis(side = 4, at = pretty(TS_m_SM3))# Add a Y axis on the right side of the chart
# Add a legend in the bottom right corner
legend(x = "bottomright", 
       legend = c("Active","SubM1","SubM2", "SubM3"), 
       col = c("black", "red","blue","green"),
       lty = c(1, 1))

------------------------------------------------------------------
## 5. Forecasting ####
------------------------------------------------------------------------

# > 1 time series linear regression ####

--------------------------------------------------------------------
# Creating Test-train set (1)====
----------------------------------------------------------------------
# Monthly TS training sets
train_m_SM1 <- window(TS_m_SM1,start=c(2007,1),end=c(2010,12))
test_m_SM1  <- window(TS_m_SM1,start=c(2011,1))
train_m_SM2 <- window(TS_m_SM2,start=2007,end=c(2010,12))
test_m_SM2  <- window(TS_m_SM2,start=2011)
train_m_SM3 <- window(TS_m_SM3,start=2007,end=c(2010,12))
test_m_SM3  <- window(TS_m_SM3,start=2011)
train_m_Act <- window(TS_m_Active,start=2007,end=c(2010,12))
test_m_Act  <- window(TS_m_Active,start=2011)
train_m_React <- window(TS_m_Reactive,start=c(2007,1),end=c(2010,12))
test_m_React  <- window(TS_m_Reactive,start=c(2011,1))

# Weekly TS training sets
train_w_SM1 <- window(TS_w_SM1,start=2007,end=c(2011,1))
test_w_SM1  <- window(TS_w_SM1,start=c(2011,2),end=c(2011,9))
train_w_SM2 <- window(TS_w_SM2,start=2007,end=c(2011,1))
test_w_SM2  <- window(TS_w_SM2,start=c(2011,2),end=c(2011,9))
train_w_SM3 <- window(TS_w_SM3,start=2007,end=c(2011,1))
test_w_SM3  <- window(TS_w_SM3,start=c(2011,2),end=c(2011,9))
train_w_React <- window(TS_w_Reactive,start=2007,end=c(2011,1))
test_w_React  <- window(TS_w_Reactive,start=c(2011,2),end=c(2011,9))
train_w_Act <- window(TS_w_Active,start=2007,end=c(2011,1))
test_w_Act  <- window(TS_w_Active,start=c(2011,2),end=c(2011,9))
----------------------------------------------------------------------
# linear regression ####
---------------------------------------------------------------------
Fit_mon_SM1<- tslm(TS_m_SM1 ~ trend + season)
summary(Fit_mon_SM1)
Fit_mon_SM2<- tslm(TS_m_SM2 ~ trend + season)
summary(Fit_mon_SM2)
Fit_mon_SM3<- tslm(TS_m_SM3 ~ trend + season)
summary(Fit_mon_SM3)
Fit_mon_ACT<- tslm(TS_m_Active ~ trend + season)
summary(Fit_mon_SM3)
Fit_mon_ReACT<- tslm(TS_m_Reactive ~ trend + season)
summary(Fit_mon_ReACT)

# Weekly TS

Fit_week_SM1<- tslm(TS_w_SM1 ~ trend + season)
summary(Fit_week_SM1)
Fit_week_SM2<- tslm(TS_w_SM2 ~ trend + season)
summary(Fit_week_SM2)
Fit_week_SM3<- tslm(TS_w_SM3 ~ trend + season)
summary(Fit_week_SM3)
Fit_week_ACT<- tslm(TS_w_Active ~ trend + season)
summary(Fit_week_SM3)
Fit_week_ReACT<- tslm(TS_w_Reactive ~ trend + season)
summary(Fit_week_ReACT)

# Did not proceed to forecasts as linear regresion is unreliable for time series forecasting

--------------------------------------------------------------------
# > 2 Decomposing a Seasonal Time Series ####
---------------------------------------------------------------------

# monthly TS
comp_TS_m_SM1<- decompose(train_m_SM1)
comp_TS_m_SM3<- decompose(train_m_SM3)
comp_TS_m_SM2<- decompose(train_m_SM2)
comp_TS_m_Active<-decompose(train_m_Act)
comp_TS_m_Reactive<-decompose(train_m_React)

comp_TS_m_SM1 %>% plot() %>% summary()
comp_TS_m_SM2 %>% plot() %>% summary()
comp_TS_m_SM3 %>% plot() %>% summary()
comp_TS_m_Reactive %>% plot() %>% summary()
comp_TS_m_Active %>% plot() %>% summary()

# Weekly TS

comp_TS_w_SM1<- decompose(train_w_SM1)
comp_TS_w_SM3<- decompose(train_w_SM2)
comp_TS_w_SM2<- decompose(train_w_SM3)
comp_TS_w_Active<-decompose(train_w_Act)
comp_TS_w_Reactive<-decompose(train_w_React)

comp_TS_w_SM1 %>% plot() %>% summary()
comp_TS_w_SM2 %>% plot() %>% summary()
comp_TS_w_SM3 %>% plot() %>% summary()
comp_TS_w_Reactive %>% plot() %>% summary()
comp_TS_w_Active %>% plot() %>% summary()

--------------------------------------------------------------------
# > 3 Holt Winters forecasting ####
-------------------------------------------------------------------------
# Seasonal adjusting
Adj_m_SM1<-train_m_SM1 - comp_TS_m_SM1$seasonal
Adj_m_SM3<-train_m_SM3-comp_TS_m_SM3$seasonal
Adj_m_SM2<-train_m_SM2-comp_TS_m_SM2$seasonal
Adj_m_Active<-train_m_Act-comp_TS_m_Active$seasonal
Adj_m_Reactive<-train_m_React-comp_TS_m_Reactive$seasonal

Adj_w_SM1<-train_w_SM1-comp_TS_w_SM1$seasonal
Adj_w_SM3<-train_w_SM3-comp_TS_w_SM3$seasonal
Adj_w_SM2<-train_w_SM2-TS_w_SM2-comp_TS_w_SM2$seasonal
Adj_w_Active<-train_w_Act-comp_TS_w_Active$seasonal
Adj_w_Reactive<-train_w_React-comp_TS_w_Reactive$seasonal


# 1) HoltWinters Simple Exponential Smoothing

# Monthly forecast Submeter 1
HW_w_SM1<- HoltWinters(Adj_w_SM1, beta=FALSE, gamma=FALSE)#model
plot(HW_w_SM1, ylim = c(0, 25))#plot
checkresiduals(HW_w_SM1)#residuals
fc_HW_w_SM1<- forecast(HW_w_SM1, h=12, level=c(10,25))# forecast
plot(fc_HW_w_SM1, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")#plot forecast

autoplot(train_w_SM1,xlab="Year",ylab= "Wats/Hour") +
  autolayer(fc_HW_w_SM1) +
  autolayer(test_w_SM1)+
  ggtitle("HoltWinters Monthly forecast Sub-1")

#checking forecast with different level
x <- forecast(HW_m_SM1, h=12, level=c(80,95))# forecast with different level
#-Plot 4-quarter forecast of energy usage
plot(x, showgap=FALSE, include=3,
     shadecols=c('slategray3','slategray'),
     xlab='Year', ylab='kWh',
     main='4-Quarter Forecast of Quartlerly Energy Consumption \nfor Submeter-1')
minor.tick(nx=2)

# Monthly forecast Submeter 3
HW_m_SM3<- HoltWinters(Adj_m_SM3, beta=FALSE, gamma=FALSE)
plot(Adj_m_SM3, ylim = c(0, 25))
checkresiduals(HW_m_SM3)
fc_HW_m_SM3<- forecast(HW_m_SM3, h=25, level=c(10,25))# forecast
plot(fc_HW_m_SM3, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

# Monthly forecast Submeter 2
HW_m_SM2<- HoltWinters(Adj_m_SM2, beta=FALSE, gamma=FALSE)
plot(HW_m_SM2, ylim = c(0, 25))
fc_HW_m_SM2<- forecast(HW_m_SM2, h=25, level=c(10,25))# forecast
plot(fc_HW_m_SM2, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

# Monthly forecast Active Power
HW_m_Active<- HoltWinters(Adj_m_Active, beta=FALSE, gamma=FALSE)
plot(HW_m_Active, ylim = c(0, 25))
fc_HW_m_Active<- forecast(HW_m_Active, h=25, level=c(10,25))# forecast
plot(fc_HW_m_Active, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

# Monthly forecast Reactive Power
HW_m_Reactive<- HoltWinters(Adj_m_Reactive, beta=FALSE, gamma=FALSE)
plot(HW_m_Reactive, ylim = c(0, 25))
fc_HW_HW_m_Reactive<- forecast(HW_m_Reactive, h=25, level=c(10,25))# forecast
plot(fc_HW_m_Reactive, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")


############### Other methods##########################################

----------------------------------------------------------------------------------------------------
# >1. Automatic forecasting with exponential smoothing ####
-----------------------------------------------------------------------------------------------------
#Weekly
CTS_w_SM1<- tsclean(TS_w_SM1,lambda = TRUE)
CTS_w_SM2<- tsclean(TS_w_SM2,lambda =TRUE)
CTS_w_SM3<- tsclean(TS_w_SM3,lambda =TRUE)
CTS_w_Act<- tsclean(TS_w_Active,lambda = TRUE)
CTS_w_React<- tsclean(TS_w_Reactive,lambda = TRUE)

#monthly
CTS_m_SM1<- tsclean(TS_m_SM1,lambda = TRUE)
CTS_m_SM2<- tsclean(TS_m_SM2,lambda =TRUE)
CTS_m_SM3<- tsclean(TS_m_SM3,lambda =TRUE)
CTS_m_Act<- tsclean(TS_m_Active,lambda = TRUE)
CTS_m_React<- tsclean(TS_m_Reactive,lambda = TRUE)

# Create training sets
train_SM1 <- subset(CTS_w_SM1, end = length(TS_w_SM1) - 26)
train_SM2 <- subset(CTS_w_SM2, end = length(TS_w_SM2) - 26)
train_SM3 <- subset(CTS_w_SM3, end = length(TS_w_SM3) - 26)
train_ACT <- subset(CTS_w_Act, end = length(TS_w_Active) - 26)
train_React <- subset(CTS_w_React, end = length(TS_w_Reactive) - 26)

mtrain_SM1 <- subset(CTS_m_SM1, end = length(TS_m_SM1) - 12)
mtrain_SM2 <- subset(CTS_m_SM2, end = length(TS_m_SM2) - 12)
mtrain_SM3 <- subset(CTS_m_SM3, end = length(TS_m_SM3) - 12)
mtrain_ACT <- subset(CTS_m_Act, end = length(TS_m_Active) - 12)
mtrain_React <- subset(CTS_m_React, end = length(TS_m_Reactive) - 12)

mtest_SM1<-window(CTS_m_SM1,start=c(2011,1),end=c(2011,10))

fstlf <- function(y, h) {  # Function to return ETS forecasts
  forecast(stlf(y), h = h)
}
# Weekly forecast
fstlf(train_SM1,26) %>% summary()
autoplot(fstlf(train_SM1,26))+
  autolayer(test_w_SM1)
  
fstlf(train_SM2,26) %>% summary()
autoplot(fstlf(train_SM2,26))+
  autolayer(test_w_SM2)
fstlf(train_SM3,26)%>% summary()
autoplot(fstlf(TS_w_SM3,26))+
  autolayer(test_w_SM3)
fstlf(train_ACT,26)%>% summary()
autoplot(fstlf(train_ACT,26))+
 autolayer(test_w_Act)

EXsm <- fstlf(train_ACT,26)
fstlf(train_React,h=26)%>% summary()
autoplot(fstlf(train_React,26))+
 autolayer(test_w_React)

accuracy(fstlf(train_SM1,h=26),TS_w_SM1)

# monthly forecast
fstlf(mtrain_SM1,12) %>% summary()
autoplot(fstlf(mtrain_SM1,12))+
  autolayer(test_m_SM1)
fstlf(mtrain_SM2,12) %>% summary()
autoplot(fstlf(mtrain_SM2,12))+
  autolayer(test_m_SM1)
fstlf(mtrain_SM3,12)%>% summary()
autoplot(fstlf(mtrain_SM3,12))+
  autolayer(test_m_SM3)
fstlf(mtrain_ACT,12)%>% summary()
autoplot(fstlf(mtrain_ACT,12))+
  autolayer(test_m_Act)
fstlf(mtrain_React,12)%>% summary()
autoplot(fstlf(mtrain_React,12))+
  autolayer(test_m_React)

------------------------------------------------------------------
#>2. Auto-Arima ####
-------------------------------------------------------------------
#weekly

fit_AR_SM1 <- auto.arima(train_w_SM1,stepwise = FALSE)
checkresiduals(fit_AR_SM1)
summary(fit_AR_SM1)
for_AR_SM1<-fit_AR_SM1 %>% forecast(h = 6)
autoplot(for_AR_SM1)+
  autolayer(test_w_SM1)

fit_AR_SM2 <- auto.arima(train_w_SM1,stepwise = FALSE)
checkresiduals(fit_AR_SM2)
summary(fit_AR_SM2)
for_AR_SM2<-fit_AR_SM2 %>% forecast(h = 6)
autoplot(for_AR_SM2)+
           autolayer(test_w_SM2)
         
fit_AR_SM3 <- auto.arima(train_w_SM3,stepwise = FALSE)
checkresiduals(fit_AR_SM3)
summary(fit_AR_SM3)
for_AR_SM3<-fit_AR_SM3 %>% forecast(h = 6)
autoplot(for_AR_SM3)+
autolayer(test_w_SM3)

fit_AR_ACT <- auto.arima(train_w_Act,stepwise = FALSE)
checkresiduals(fit_AR_ACT)
summary(fit_AR_ACT)
for_AR_ACT<-fit_AR_ACT %>% forecast(h = 26)
autoplot(for_AR_ACT)

fit_AR_React <- auto.arima(train_w_React,stepwise = FALSE)
checkresiduals(fit_AR_React)
summary(fit_AR_React)
for_AR_React<- fit_AR_React %>% forecast(h = 6)
autoplot(for_AR_React)+
  autolayer(for_AR_React)

--------------------------------------------------------------
#monthly Arima ####
--------------------------------------------------------------
  
mfit_AR_SM1 <- auto.arima(TS_m_SM1,stepwise = FALSE)
checkresiduals(fit_AR_SM1)
summary(mfit_AR_SM1)
mfit_AR_SM1 %>% forecast(h = 12) 
  autoplot(mfit_AR_SM1)+
  autolayer(test_m_SM1)
  
mfit_AR_SM2 <- auto.arima(TS_m_SM2,stepwise = FALSE)
checkresiduals(mfit_AR_SM2)
summary(mfit_AR_SM2)
mfit_AR_SM2 %>% forecast(h = 12) 
  autoplot(mfit_AR_SM2)+
  autolayer(TS_m_SM2)

mfit_AR_SM3 <- auto.arima(TS_m_SM3,stepwise = FALSE)
checkresiduals(fit_AR_SM3)
summary(mfit_AR_SM3)
mfit_AR_SM3 %>% forecast(h = 12) 
autoplot(mfit_AR_SM3)+
  autolayer(TS_m_SM3)

mfit_AR_ACT <- auto.arima(TS_m_Active,stepwise = FALSE)
checkresiduals(mfit_AR_ACT)
summary(mfit_AR_ACT)
mfit_AR_ACT %>% forecast(h = 12)
autoplot(mfit_AR_ACT)+
  autolayer(TS_m_SM3)

mfit_AR_React <- auto.arima(TS_m_Reactive,stepwise = FALSE)
checkresiduals(mfit_AR_React)
summary(mfit_AR_React)
mfit_AR_ACT %>% forecast(h = 12)
autoplot(mfit_AR_ACT)+
  autolayer(TS_m_SM3)

------------------------------------------------------------------
#>3. Forecast with harmonic regressor ####
---------------------------------------------------------------

# Set up harmonic regressors of order 12
harmonics <- fourier(train_w_SM1, K = 13)
harmonicsAct <- fourier(train_w_Act, K = 13)
# Fit regression model with ARIMA errors
fit2 <- auto.arima(train_w_SM1, xreg = harmonics, seasonal = FALSE)
fitAct <- auto.arima(train_w_Act, xreg = harmonics, seasonal = FALSE)
summary(fitAct)
checkresiduals(fitAct)

# Forecasts next 26 weeks

newharmonics <- fourier(train_w_SM1, K = 13, h = 26)
fcSM1 <- forecast(fit2, xreg = newharmonics)

newharmonicsAct <- fourier(train_w_Act, K = 13, h = 26)
fcAct <- forecast(fitAct, xreg = newharmonicsAct)
# Plot forecasts 
autoplot(fcSM1)
autoplot(fcAct)
---------------------------------------------------------------------
#>4.Fourier 2nd attempt ####
-----------------------------------------------------------------
# fourier(x, K, h = NULL)
fit <- auto.arima(TS_w_SM1, xreg = fourier(TS_w_SM1, K = 13),
                  seasonal = FALSE, lambda = 0.8)
BFIT<-fit %>%
  forecast(xreg = fourier(TS_w_SM1, K = 10, h = 24)) %>%
  autoplot()

Afit <- auto.arima(TS_w_SM1, xreg = fourier(TS_w_SM1, K = 15),
                     seasonal = FALSE, lambda = 0.8783565)
fc<-forecast(Afit,h=25)
autoplot(fc)#-0.9999242

Bfit <- auto.arima(train_w_Act, xreg = fourier(train_w_Act, K = 13),
                   seasonal = FALSE, lambda = -0.9999242)
fc2<-forecast(Bfit,xreg = fourier(train_w_Act, K = 13,h=26))
autoplot(fc2)

summary(fc2)


#  checking out TBATS model ####

fit3 <- tbats(TS_w_SM1)

# Forecast the series for the next 5 years
fc3 <- forecast(fit3, h = 26)

# Plot the forecasts
autoplot(fc3)


--------------------------------------------------------------------
# Forecast models graphs and comparison ####
-----------------------------------------------------------------
#sm1 models forecast weekly
autoplot(TS_w_SM1,xlab="Year",ylab= "Wats/Hour") +
  autolayer(for_AR_SM1,series="Auto-ARIMA",PI =FALSE) +
  autolayer(fcSM1,series="ARIMA with Harm.Reg1",PI =FALSE) +
  autolayer(fc,series="ARIMA with Harm.Reg2",PI =FALSE) +
  ggtitle("Forecasting models  comparison")

#Active models forecast weekly
autoplot(test_w_Act,xlab="Year",ylab= "Wats/Hour") +
  autolayer(fcAct,series="ARIMA with Harm.Reg1",PI =FALSE) +
  autolayer(fc2,series="ARIMA with Harm.Reg2",PI =FALSE) +
  autolayer(EXsm,series="Exp. smooth.",PI =FALSE) +
  ggtitle("Forecasting models  comparison")

--------------------------------------------------------------------------------------------------------------
#5 Cost calculation ####
-------------------------------------------------------------------------------------------------------------
cost_SM1<-ts(Data_month_nm$mtot_Sub_metering_1*0.0000283,frequency=12,start=c(2007,1),end=(2011))
ggseasonplot(cost_SM1,xlab = "Time", ylab = "monthly cost", main = "Kitchen cost paterns (with 2019 av.pricing)")

cost_SM2<-ts(Data_month_nm$mtot_Sub_metering_2*0.0000283,frequency=12,start=c(2007,1),end=(2011))
ggseasonplot(cost_SM2,xlab = "Time", ylab = "monthly cost", main = "Laundry cost paterns (with 2019 av.pricing)")

cost_SM3<-ts(Data_month_nm$mtot_Sub_metering_3*0.0000283,frequency=12,start=c(2007,1),end=(2011))
ggseasonplot(cost_SM3,xlab = "Time", ylab = "monthly cost", main = "Heating cost paterns (with 2019 av.pricing)")

cost_Act<-ts(Data_month_nm$mtot_Active*0.00000283,frequency=12,start=c(2007,1),end=(2011))
ggseasonplot(cost_Act,xlab = "Time", ylab = "monthly cost", main = "Active Power cost paterns (with 2019 av.pricing)")


