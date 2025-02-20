rm(list=ls())
library(dplyr)
library(janitor)



setwd("/Users/nchmait/Documents/Rdir/CryptoAnalytics/misc/misc/archive/")
dd <- plyr::ldply(list.files(), read.csv, header=TRUE)
h<- dd %>% clean_names() 

h$dt <- as.Date(h$date)#as.Date(h$date, "%B-%d-%Y")
h$d <- weekdays(h$dt)
h$w <- lubridate::week(h$dt)
h$y <- lubridate::year(h$dt)
h$quarter <- lubridate::quarter(h$dt)

h<- h %>% arrange(dt) %>% mutate(Row = 1:n()) %>% group_by(name) %>%
  mutate(Percentage_Change = (high-lag(high))/lag(high) * 100) %>% ungroup() 
h$Percentage_Change[is.na(h$Percentage_Change)] <- 0

h<- h %>%group_by(y,quarter,name) %>% 
  mutate(up.q  =  quantile(h$Percentage_Change, 0.998), 
         low.q = quantile(h$Percentage_Change, 0.002)
  ) %>% ungroup()

h$outlier<-as.logical(h$Percentage_Change<h$low.q |  h$Percentage_Change>h$up.q)

#h<- h %>%group_by(y,w) %>% mutate(dv =sd(high))
h<- as.data.frame(h %>% filter(outlier == FALSE))
#table(h$outlier)
h<- h %>% group_by(y,w,name) %>% mutate(numdays = n_distinct(d)) %>% ungroup() %>% filter(numdays==7)

daily <- h %>% select(-date, -dt,-numdays, -Row, -Percentage_Change, -quarter,  -up.q, -low.q, -outlier, -numdays) 

d.open <-  daily %>% select(-s_no,-symbol,-close,-high,-low ,-marketcap,-volume) %>% group_by(name)  %>% tidyr::pivot_wider(names_from = d, values_from = open)  %>%na.omit() #values_fill = list(open > 0)
d.close <- daily %>% select(-s_no,-symbol,-open ,-high,-low ,-marketcap,-volume) %>% group_by(name) %>% tidyr::pivot_wider(names_from = d, values_from = close)  %>%na.omit() #values_fill = list(open > 0)
d.high <-  daily %>% select(-s_no,-symbol,-close,-open,-low ,-marketcap,-volume) %>% group_by(name) %>% tidyr::pivot_wider(names_from = d, values_from = high)  %>%na.omit() #values_fill = list(open > 0)
d.low <-   daily %>% select(-s_no,-symbol,-close,-high,-open,-marketcap,-volume) %>% group_by(name) %>% tidyr::pivot_wider(names_from = d, values_from = low)  %>%na.omit() #values_fill = list(open > 0)




d.open <- d.open %>% select(name,y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%
  mutate(
    OpenMon = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Monday),
    OpenTue = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Tuesday),
    OpenWed = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Wednesday),
    OpenThu = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Thursday),
    OpenFri = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Friday),
    OpenSat = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Saturday),
    OpenSun = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Sunday)
  )


d.close <- d.close %>% select(name,y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%         
  mutate(
    CloseMon = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Monday),
    CloseTue = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Tuesday),
    CloseWed = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Wednesday),
    CloseThu = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Thursday),
    CloseFri = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Friday),
    CloseSat = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Saturday),
    CloseSun = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Sunday)
  )

d.high <- d.high %>% select(name,y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%         
  mutate(
    HighMon = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Monday),
    HighTue = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Tuesday),
    HighWed = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Wednesday),
    HighThu = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Thursday),
    HighFri = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Friday),
    HighSat = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Saturday),
    HighSun = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Sunday)
  )

d.low <- d.low %>% select(name,y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%
  mutate(
    LowMon = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Monday),
    LowTue = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Tuesday),
    LowWed = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Wednesday),
    LowThu = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Thursday),
    LowFri = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Friday),
    LowSat = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Saturday),
    LowSun = rowMeans(across(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))/ Sunday)
  )


s <- as.data.frame(d.open[,c(1,2,3,11:17)]) %>% 
  left_join(as.data.frame(d.close[,c(1,2,3,11:17)]))  %>% 
  left_join(as.data.frame(d.high[,c(1,2,3,11:17)]))  %>% 
  left_join(as.data.frame(d.low[,c(1,2,3,11:17)])) 


#--------------------------------------------------------------------------------------------------------
# sum coin %diff by dow
#--------------------------------------------------------------------------------------------------------
s.tot <- as.data.frame(colSums((s%>% select(-name)))) 
names(s.tot)[1]<- "pc"
s.tot <- s.tot %>% slice(-1,-2) %>%  
  add_rownames(var = "metric") 

s.tot$grp <- gsub("Over|Open|Close|High|Low","",s.tot$metric,ignore.case = T)
s.tot$metric.type <- substr(s.tot$metric,1,nchar(s.tot$metric)-3)
s.tot <- s.tot %>% group_by(metric.type) %>% mutate(lowest = min(pc)) %>% ungroup
s.tot$tot <-  as.numeric(s.tot$pc) - as.numeric(s.tot$lowest)

s.tot$grp <- factor(s.tot$grp, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

#--------------------------------------------------------------------------------------------------------
# sum coin value by dow
#--------------------------------------------------------------------------------------------------------
daily.scaled <- daily %>% group_by(name) %>% mutate_at(c(4,5,6,7), funs(c(scale(.)))) %>% ungroup()

s.summary <- daily.scaled %>% select(-s_no,-symbol, -volume, -marketcap, -y,-w) %>%  na.omit() %>%
  group_by(name, d) %>%  summarise_all(sum) %>%
  tidyr::pivot_longer(!c(name,d), names_to = "metric", values_to = "total")

s.summary <- s.summary %>% group_by(name, metric) %>% mutate(rank =rank(total)) %>% ungroup
s.summary <- s.summary %>% group_by(d, metric) %>% mutate(tot =sum(total)) %>% ungroup
#s.summary$tot <- log(s.summary$tot)
s.summary <- s.summary %>% group_by(d, metric) %>% summarise(rank=sum(rank), tot= sum(tot))

s.summary$d <- factor(s.summary$d, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
library(ggplot2)
#Shows total number of days where coin value was higher than given day
s.tot %>% ggplot(aes(x=grp, y=tot, fill=grp, label =round(pc,1))) + 
  geom_bar(stat = "identity")  + geom_label() + ggtitle("all coins") + #coord_flip() +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + facet_wrap(~ metric.type, scales = "free")

#Shows total value of coin for each day of week
s.summary %>% ggplot(aes(x=d, y=tot, fill=d, label=round(rank))) +
  geom_bar(stat = "identity")   + geom_label() +  ggtitle("all coins") + #coord_flip()
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + facet_wrap(~ metric, scales = "free")

#Price history
h %>% ggplot(aes(x=dt, y=high)) +
  geom_line(stat = "identity")  +  ggtitle("all coins") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))  

#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------

