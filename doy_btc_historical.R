
h <- readxl::read_excel ("//TennisAustralia.local/files/Users/NChmait/Documents/RDir/HIT/bitcoin_2018-1-1_2021-8-1.xlsx")
str(h)

h<- h %>% clean_names() 
h$dt <- as.Date(h$date, "%B-%d-%Y")
h$d <- weekdays(h$dt)
h$w <- lubridate::week(h$dt)
h$y <- lubridate::year(h$dt)

daily <- h %>% select(-date, -dt) 

#AO.presale.matrix <- tidyr::spread(data = AO.presale %>% select(-fname, -priority, -state) , key = source, value = isMail)
d.open <- daily %>% select(-close,-high,-low,-market_cap,-volume) %>% tidyr::pivot_wider(names_from = d, values_from = open) #values_fill = list(open > 0)
d.open <- d.open %>% select(y,w,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%na.omit() %>%  mutate(across(c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"  ), .fns =  ~.*100/Wednesday))

d.open <- d.open %>% mutate_if(is.numeric,
                 round,
                 digits = 1)

d.open$dayshigherWed <- rowSums(d.open[,c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"  )]>100) - rowSums(d.open[,c("Monday", "Tuesday",  "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday"  )]<100)
open <- d.open %>% group_by(dayshigherWed) %>% summarise(n=n())

library(ggplot2)
open %>% ggplot(aes(x=dayshigherWed, y=n, fill=dayshigherWed<0, label = n)) + 
  geom_bar(stat = "identity") + geom_label(aes(x=dayshigherWed, y=n))
  
            
            