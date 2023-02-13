if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, 
               gdata, MatchIt, cobalt, Matching)

#pulling the data in
final.hcris.data=read_rds('data/output/HCRIS_Data.rds')

#1. How many hospitals filed more than one report in the same year

grouped_data <- final.hcris %>% 
  group_by(provider_number, fyear) %>% 
  summarize(count= n()) 

filtered_data <- grouped_data %>%
  filter(count>1)

table1<-table(filtered_data$fyear, filtered_data$count)
table1_df<-as.data.frame.matrix(table1)
table1_df<-mutate(table1_df, countSum= table1_df$`2` + table1_df$`3`)
table1_df<-rownames_to_column(table1_df, 'year')


ggplot(table1_df, aes(x=year, y=countSum, group=1)) +
  geom_line() +
  ggtitle("Number of Hospitals with more than one report ")+
  xlab("Year") +
  ylab("Number of hospitals with more than one report from 1996 to 2017") 



#2 After removing/combining multiple reports, how many unique hospital IDs (Medicare provider numbers) exist in the data?
unique_hospitalIDS<-length(unique(final.hcris$provider_number))
unique_hospitalIDS


#3

ggplot(final.hcris.data, aes(x = year, y = tot_charges, group=year)) +
  geom_jitter(alpha= 0.05) +
  geom_violin(alpha=.9)



#4

#HRRP is always a penalty 

final.hcris.data1 <-final.hcris.data %>%
  mutate(discount_factor = 1-tot_discounts/tot_charges)%>%
  mutate(price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment) %>%
  mutate(price_denom = tot_discharges - mcare_discharges) %>%
  mutate(price = price_num/price_denom)

Justin <- final.hcris.data1 %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30) %>%
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)),
          penalty = (hvbp_payment-hrrp_payment<0))  
Justin1<-Justin %>%
  filter(price<25000)
ggplot(Justin1, aes(x = year, y = price)) +
  geom_jitter(alpha= 0.05) +
  geom_violin(alpha=.9)



#5

avg_price_penalty <- Justin %>%
  filter(year==2012) %>%
  filter(penalty==TRUE) %>%
  summarise(penalty_price=mean(price))
avg_price_penalty  
avg_price_nopenalty <- Justin %>%
  filter(year==2012) %>%
  filter(penalty==FALSE) %>%
  summarise(no_penalty_price=mean(price))
avg_price_nopenalty  

#6

# Calculate the quartiles of the data set
data_2012<-Justin%>% filter(year==2012)
quartiles <- quantile(data_2012$beds, probs = c(0.25, 0.5, 0.75,1), na.rm=TRUE)

q1 <- quartiles[1]
q2 <- quartiles[2]
q3 <- quartiles[3]
q4<- quartiles[4]


# Indicate which observations belong to which group
data_with_groups <- data_2012 %>%
  mutate(Q1 = ifelse(beds <= q1, 1, 0)) %>%
  mutate(Q2 = ifelse(beds >q1 & beds <=q2, 1, 0)) %>%
  mutate(Q3 = ifelse(beds >q2 & beds <=q3, 1, 0)) %>%
  mutate(Q4 = ifelse(beds >q3 & beds <=q4, 1, 0)) 


data_with_CT<- data_with_groups %>%
  filter(!is.na(price)) %>%
  mutate(group = ifelse(penalty==FALSE, "Control", "Treatment")) 


results<- data_with_CT %>%
  group_by(group, Q1, Q2, Q3, Q4) %>%
  
  summarize(mean_price=mean(price, na.rm = TRUE))
results 

view(data_with_CT)

#7


nn.est1 <- Matching::Match(Y=Justin$price,
                           Tr=Justin$penalty,
                           X=Justin$beds,
                           M=1,
                           Weight=1,
                           ties=FALSE,
                           estimand="ATE")

summary(nn.est1)

nn.est2 <- Matching::Match(Y=data_with_CT$price,
                           Tr=data_with_CT$group,
                           X=data_with_CT$provider_number,
                           M=1,
                           Weight=2,
                           estimand="ATE")

logit.reg <- glm(d_alt ~ x+z,
                 data = data_with_CT, family = binomial(link = 'logit'))
select.dat <- select.dat %>%
  mutate(ps = predict(logit.reg, type = 'response')) %>%
  filter(ps>0 & ps<1)
#Honestly I spent about 8 hours trying to figure this part out and I was really struggling. I used every resource from in class code to online resources but still fell short. Hopefully time in class and group time will help rectify this problem. 


#8? 
#VERY DIFFERENT 
#9
#I did not estimate a causual inference because i was unable to correctly complete the ATE estimates 
#but to actually answer this question yes because we are able to see the effect of the control and treatment groups on price by also having weights to better estimate the causal inference
#10 
#honestly this homework was incredibly hard and frustrating. I think i spent at least 20 hours of straight coding or problem solving 
#Also i feel very lost in terms of the new material we are covering in class which does not help when i need to apply this knowledge with real data in the homework 
#one thing i did learn was i am getting better at cleanining up the data and I believe I did good work until question 5 ish 
#however one thing that still frustrates me is my inability to understanding the matching function and concepts we have learned in this module.  





