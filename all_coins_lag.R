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
#h<- h %>% group_by(y,w,name) %>% mutate(numdays = n_distinct(d)) %>% ungroup() %>% filter(numdays==7)

daily <- h %>% select(-date, -numdays,-volume, -Row, -Percentage_Change, -quarter,  -up.q, -low.q, -outlier, -numdays) 
daily <- daily %>% group_by(name)%>%mutate(o =  (open/lag(open, n =3)+open/lag(open, n =2)+open/lag(open, n =1)+open/lead(open, n =1)+open/lead(open, n =2)+open/lead(open, n =3)),
                                           c =  (close/lag(close, n =3)+close/lag(close, n =2)+close/lag(close, n =1)+close/lead(close, n =1)+close/lead(close, n =2)+close/lead(close, n =3)),
                                           h =  (high/lag(high, n =3)+high/lag(high, n =2)+high/lag(high, n =1)+high/lead(high, n =1)+high/lead(high, n =2)+high/lead(high, n =3)),
                                           l =  (close/lag(close, n =3)+close/lag(close, n =2)+close/lag(close, n =1)+close/lead(close, n =1)+close/lead(close, n =2)+close/lead(close, n =3))
                                           ) %>% ungroup 



#--------------------------------------------------------------------------------------------------------
# sum coin %diff by dow
#--------------------------------------------------------------------------------------------------------
s.tot <- daily %>% select(name,d,o,c,h,l) %>% mutate_if(is.numeric,funs(. /6))%>% na.omit()
s.tot$d <- factor(s.tot$d, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
s.tot <- s.tot%>%
  tidyr::pivot_longer(!c(name,d), names_to = "metric", values_to = "total")

s.tot <- s.tot%>%group_by(metric,d)%>%summarise(val.sum=sum(total), 
                                                val.mean=mean(total))

s.tot <- s.tot%>%group_by(metric)%>%mutate(val.min=min(val.sum))%>% ungroup() %>% mutate(com=val.sum-val.min)

library(ggplot2)
#Shows total number of days where coin value was higher than given day
s.tot %>% ggplot(aes(x=d, y=com, fill=d, label =round(val.sum))) + 
  geom_bar(stat = "identity")  + geom_label() + ggtitle("all coins") + #coord_flip() +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + facet_wrap(~ metric, scales = "free")


#Price history
h %>% ggplot(aes(x=dt, y=high)) +
  geom_line(stat = "identity")  +  ggtitle("all coins") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))  

#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------

