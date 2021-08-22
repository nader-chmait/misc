rm(list=ls())
library(dplyr)
library(janitor)

h <- read.csv("/Users/nchmait/Documents/Rdir/CryptoAnalytics/misc/misc/archive/coin_Bitcoin.csv", header = T)
#str(h)

h<- h %>% clean_names() 
h$dt <- as.Date(h$date)#as.Date(h$date, "%B-%d-%Y")
h$d <- weekdays(h$dt)
h$w <- lubridate::week(h$dt)
h$y <- lubridate::year(h$dt)
h$quarter <- lubridate::quarter(h$dt)

h<- h %>% arrange(dt) %>% mutate(Row = 1:n()) %>% #group_by(item) %>%
  mutate(Percentage_Change = (high-lag(high))/lag(high) * 100) %>% ungroup() 
h$Percentage_Change[is.na(h$Percentage_Change)] <- 0

h<- h %>%group_by(y,quarter) %>% 
  mutate(up.q  =  quantile(h$Percentage_Change, 0.998), 
         low.q = quantile(h$Percentage_Change, 0.002)
         ) %>% ungroup()

h$outlier<-as.logical(h$Percentage_Change<h$low.q |  h$Percentage_Change>h$up.q)

#h<- h %>%group_by(y,w) %>% mutate(dv =sd(high))
#h<- as.data.frame(h %>% filter(outlier == FALSE))
#table(h$outlier)
h<- h %>% group_by(y,w) %>% mutate(numdays = n_distinct(d)) %>% ungroup() %>% filter(numdays==7)

daily <- h %>% select(-date, -dt,-numdays, -Row, -Percentage_Change, -quarter,  -up.q, -low.q, -outlier, -numdays) 

d.open <-  daily %>% select(-s_no,-name,-symbol,-close,-high,-low ,-marketcap,-volume) %>% tidyr::pivot_wider(names_from = d, values_from = open)  %>%na.omit() #values_fill = list(open > 0)
d.close <- daily %>% select(-s_no,-name,-symbol,-open ,-high,-low ,-marketcap,-volume) %>% tidyr::pivot_wider(names_from = d, values_from = close)  %>%na.omit() #values_fill = list(open > 0)
d.high <-  daily %>% select(-s_no,-name,-symbol,-close,-open,-low ,-marketcap,-volume) %>% tidyr::pivot_wider(names_from = d, values_from = high)  %>%na.omit() #values_fill = list(open > 0)
d.low <-   daily %>% select(-s_no,-name,-symbol,-close,-high,-open,-marketcap,-volume) %>% tidyr::pivot_wider(names_from = d, values_from = low)  %>%na.omit() #values_fill = list(open > 0)


d.open <- d.open %>% select(y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%
  mutate(OpenOverMon = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Monday),
         OpenOverTue = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Tuesday),
         OpenOverWed = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Wednesday),
         OpenOverThu = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Thursday),
         OpenOverFri = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Friday),
         OpenOverSat = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Saturday),
         OpenOverSun = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Sunday)
         )

d.close <- d.close %>% select(y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%         
  mutate(CloseOverMon = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Monday),
         CloseOverTue = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Tuesday),
         CloseOverWed = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Wednesday),
         CloseOverThu = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Thursday),
         CloseOverFri = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Friday),
         CloseOverSat = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Saturday),
         CloseOverSun = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Sunday)
         )

d.high <- d.high %>% select(y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%         
  mutate(HighOverMon = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Monday),
         HighOverTue = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Tuesday),
         HighOverWed = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Wednesday),
         HighOverThu = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Thursday),
         HighOverFri = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Friday),
         HighOverSat = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Saturday),
         HighOverSun = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/ Sunday)
         )
         
d.low <- d.low %>% select(y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%
  mutate(LowOverMon = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/Monday),
         LowOverTue = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/Tuesday),
         LowOverWed = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/Wednesday),
         LowOverThu = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/Thursday),
         LowOverFri = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/Friday),
         LowOverSat = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/Saturday),
         LowOverSun = rowSums(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"))/Sunday)
         )
  
s <- as.data.frame(d.open[,c(1,2,10:16)]) %>% 
  left_join(as.data.frame(d.close[,c(1,2,10:16)]))  %>% 
  left_join(as.data.frame(d.high[,c(1,2,10:16)]))  %>% 
  left_join(as.data.frame(d.low[,c(1,2,10:16)])) 


s.tot <- as.data.frame(colSums(s)) %>% slice(-1,-2) %>% 
  rename(pc = `colSums(s)`) %>% 
  add_rownames(var = "metric") 

s.tot$grp <- gsub("Over|Open|Close|High|Low","",s.tot$metric,ignore.case = T)
s.tot$metric.type <- gsub("Over.*","",s.tot$metric,ignore.case = T)
s.tot <- s.tot %>% group_by(as.factor(metric.type)) %>% mutate(lowest = min(pc)) %>% ungroup
s.tot$tot <-  as.numeric(s.tot$pc) - as.numeric(s.tot$lowest)


s.summary <- daily %>% select(-s_no,-name,-symbol, -volume, -marketcap, -y,-w) %>%  na.omit() %>%
  group_by(d) %>%  summarise_all(sum) %>%
  tidyr::pivot_longer(!d, names_to = "metric", values_to = "total")

s.summary <- s.summary %>% group_by(metric) %>% mutate(lowest = min(total)) %>% ungroup
s.summary$tot <-  as.numeric(s.summary$total) - as.numeric(s.summary$lowest)

#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
library(ggplot2)
#Shows total number of days where coin value was higher than given day
s.tot %>% ggplot(aes(x=metric, y=tot, fill=grp)) + 
  geom_bar(stat = "identity")  +  ggtitle(h$name[1]) + #coord_flip() +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + facet_wrap(~ metric.type, scales = "free")

#Shows total value of coin for each day of week
s.summary %>% ggplot(aes(x=d, y=tot, fill=d)) +
  geom_bar(stat = "identity")  +  ggtitle(h$name[1]) + #coord_flip() +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + facet_wrap(~ metric, scales = "free")

#Price history
h %>% ggplot(aes(x=dt, y=high)) +
  geom_line(stat = "identity")  +  ggtitle(h$name[1]) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))  

#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------

