## Task 1
getwd()
setwd('F:\\Clark_Universiy\\Clark_Teaching\\2025Spring\\geog-247_Statistics\\docs\\Labs\\Practice01')

## Task 2
economics = read.csv('economic_indicators.csv')
wifi = readxl::read_excel('free_wifi_locations.xls')

## Task 3 
economics$diff_unemp_labor = economics$labor_force_part_rate - economics$unemp_rate 
economics[order(economics$diff_unemp_labor, decreasing = TRUE),c('Year','Month')]

?order

summary(wifi)

summary(wifi$OID_)
summary(wifi$neighborhood_id)

economics$Time = c(1:nrow(economics))
colnames(economics)
## Task 4
boxplot(economics$logan_intl_flights)

boxplot(logan_intl_flights ~ Month, data = economics)
        
plot(logan_intl_flights~Time, data = economics, type = 'l')
abline(lm(logan_intl_flights~Time, data=economics))



