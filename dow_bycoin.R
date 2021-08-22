rm(list=ls())
library(dplyr)
library(janitor)


h <- read.csv("/Users/nchmait/Documents/Rdir/CryptoAnalytics/misc/misc/archive/coin_Cardano.csv", header = T)
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
h<- as.data.frame(h %>% filter(outlier == FALSE))
#table(h$outlier)
h<- h %>% group_by(y,w) %>% mutate(numdays = n_distinct(d)) %>% ungroup() %>% filter(numdays==7)

daily <- h %>% select(-date, -dt,-numdays, -Row, -Percentage_Change, -quarter,  -up.q, -low.q, -outlier, -numdays) 

d.open <-  daily %>% select(-s_no,-name,-symbol,-close,-high,-low ,-marketcap,-volume) %>% tidyr::pivot_wider(names_from = d, values_from = open)  %>%na.omit() #values_fill = list(open > 0)
d.close <- daily %>% select(-s_no,-name,-symbol,-open ,-high,-low ,-marketcap,-volume) %>% tidyr::pivot_wider(names_from = d, values_from = close)  %>%na.omit() #values_fill = list(open > 0)
d.high <-  daily %>% select(-s_no,-name,-symbol,-close,-open,-low ,-marketcap,-volume) %>% tidyr::pivot_wider(names_from = d, values_from = high)  %>%na.omit() #values_fill = list(open > 0)
d.low <-   daily %>% select(-s_no,-name,-symbol,-close,-high,-open,-marketcap,-volume) %>% tidyr::pivot_wider(names_from = d, values_from = low)  %>%na.omit() #values_fill = list(open > 0)


d.open <- d.open %>% select(y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%
  mutate(
         OpenMon = Monday/ Monday,
         OpenTue = Tuesday/ Monday,
         OpenWed = Wednesday/ Monday,
         OpenThu = Thursday/ Monday,
         OpenFri = Friday/ Monday,
         OpenSat = Saturday/ Monday,
         OpenSun = Sunday/ Monday
  )
         

d.close <- d.close %>% select(y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%         
  mutate(CloseMon = Monday/ Monday,
         CloseTue = Tuesday/ Monday,
         CloseWed = Wednesday/ Monday,
         CloseThu = Thursday/ Monday,
         CloseFri = Friday/ Monday,
         CloseSat = Saturday/ Monday,
         CloseSun = Sunday/ Monday
  )

d.high <- d.high %>% select(y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%         
  mutate(HighMon = Monday/ Monday,
         HighTue = Tuesday/ Monday,
         HighWed = Wednesday/ Monday,
         HighThu = Thursday/ Monday,
         HighFri = Friday/ Monday,
         HighSat = Saturday/ Monday,
         HighSun = Sunday/ Monday
         )
         
d.low <- d.low %>% select(y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%
  mutate(LowMon = Monday/ Monday,
         LowTue = Tuesday/ Monday,
         LowWed = Wednesday/ Monday,
         LowThu = Thursday/ Monday,
         LowFri = Friday/ Monday,
         LowSat = Saturday/ Monday,
         LowSun = Sunday/ Monday
         )


s <- as.data.frame(d.open[,c(1,2,10:16)]) %>% 
  left_join(as.data.frame(d.close[,c(1,2,10:16)]))  %>% 
  left_join(as.data.frame(d.high[,c(1,2,10:16)]))  %>% 
  left_join(as.data.frame(d.low[,c(1,2,10:16)])) 


#--------------------------------------------------------------------------------------------------------
# sum coin %diff by dow
#--------------------------------------------------------------------------------------------------------
s.tot <- as.data.frame(colSums(s)) %>% slice(-1,-2) %>% 
  rename(pc = `colSums(s)`) %>% 
  add_rownames(var = "metric") 

s.tot$grp <- gsub("Over|Open|Close|High|Low","",s.tot$metric,ignore.case = T)
s.tot$metric.type <- substr(s.tot$metric,1,nchar(s.tot$metric)-3)
s.tot <- s.tot %>% group_by(metric.type) %>% mutate(lowest = min(pc)) %>% ungroup
s.tot$tot <-  as.numeric(s.tot$pc) - as.numeric(s.tot$lowest)

s.tot$grp <- factor(s.tot$grp, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

#--------------------------------------------------------------------------------------------------------
# sum coin value by dow
#--------------------------------------------------------------------------------------------------------

s.summary <- daily %>% select(-s_no,-name,-symbol, -volume, -marketcap, -y,-w) %>%  na.omit() %>%
  group_by(d) %>%  summarise_all(sum) %>%
  tidyr::pivot_longer(!d, names_to = "metric", values_to = "total")

s.summary <- s.summary %>% group_by(metric) %>% mutate(lowest = min(total)) %>% ungroup
s.summary$tot <-  as.numeric(s.summary$total) - as.numeric(s.summary$lowest)

#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
library(ggplot2)
#Shows total number of days where coin value was higher than given day
s.tot %>% ggplot(aes(x=grp, y=tot, fill=grp, label =round(pc,1))) + 
  geom_bar(stat = "identity")  + geom_label() + ggtitle(h$name[1]) + #coord_flip() +
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

