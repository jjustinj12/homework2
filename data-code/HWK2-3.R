if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, 
               gdata, MatchIt, cobalt, Matching)
library(knitr)

#pulling the data in
final.hcris.data=read_rds('data/output/HCRIS_Data.rds')
final.hcris=read_rds('data/output/HCRIS_Data_11.rds')

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


figure1<-ggplot(table1_df, aes(x=year, y=countSum, group=1)) +
  geom_line() +
  ggtitle("Number of Hospitals with more than one report between 1997 and 2017 ")+
  xlab("Year") +
  ylab("Number of hospitals with more than one report ") 



#2 After removing/combining multiple reports, how many unique hospital IDs (Medicare provider numbers) exist in the data?
unique_hospitalIDS<-length(unique(final.hcris$provider_number))
unique_hospitalIDS


#3

figure3<-ggplot(final.hcris.data, aes(x = year, y = tot_charges, group=year)) +
  geom_jitter(alpha= 0.05) +
  labs(title = "Distribution of Total Charges in Each Year between 1997 and 2017", x = "Year", y = "Total Charges")+
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
# Justin1<-Justin %>%
#   filter(price<25000)
figure4<-ggplot(Justin, aes(x =as.factor(year), y = price)) +
  geom_jitter(alpha= 0.05) +
  labs(title = "Distribution of Estimated Prices Each Year", x = "Year", y = "Price")+
  geom_violin(alpha=.9)


#5
# 
# avg_price_penalty <- Justin %>%
#   filter(year==2012) %>%
#   filter(penalty==TRUE) %>%
#   summarise(penalty_price=mean(price))
# avg_price_penalty  
# avg_price_nopenalty <- Justin %>%
#   filter(year==2012) %>%
#   filter(penalty==FALSE) %>%
#   summarise(no_penalty_price=mean(price))
# avg_price_nopenalty  

avg_price <- Justin %>%
  filter(year==2012) %>%
  group_by(penalty) %>%
  summarise(Average_Price=mean(price))
avg_price<-data.frame(avg_price)
avg_price<-kable(avg_price, caption = "The average price among penalized versus non-penalized hospitals")


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

results<-kable(results, caption = "The average price among treated/control groups dependent among bed size stratafied by quartiles")
results


#7


nn.est1 <- Matching::Match(Y=data_with_CT$price,
                           Tr=data_with_CT$penalty,
                           X=(data_with_CT %>% dplyr::select(Q1, Q2, Q3, Q4)),
                           M=1,
                           Weight=1,
                           estimand="ATE")

summary(nn.est1)

nn.est2 <- Matching::Match(Y=data_with_CT$price,
                           Tr=data_with_CT$penalty,
                           X=(data_with_CT %>% dplyr::select(Q1, Q2, Q3, Q4)),
                           M=1,
                           Weight=2,
                           estimand="ATE")
summary(nn.est2)


##propensity score
logit.reg <- glm(penalty ~ Q1+ Q2+ Q3 + Q4,
                 data = data_with_CT, family = binomial)
data_with_CT <- data_with_CT %>%
  mutate(ps = predict(logit.reg, type = 'response')) %>%
  filter(ps>0 & ps<1)


# Create IPW weights
data_with_CT <- data_with_CT %>%
  mutate(ipw = case_when(
    penalty == 1 ~ 1/ps,
    penalty == 0 ~ 1/(1-ps),
    TRUE~NA_real_
  ))


mean.t1 <- data_with_CT %>% 
  filter(penalty==1) %>% 
  dplyr::select(price, ipw) %>%
  summarize(mean_y=weighted.mean(price, w=ipw))
mean.t0 <- data_with_CT %>% 
  filter(penalty==0) %>% 
  dplyr::select(price, ipw) %>%
  summarize(mean_y=weighted.mean(price, w=ipw))
mean.t1$mean_y - mean.t0$mean_y
reg.ipw <- lm(price ~ penalty, data=data_with_CT, weights=ipw)




## regression
reg1.dat <- data_with_CT %>% filter(penalty==1)
reg1 <- lm(price ~ Q1+ Q2+ Q3 + Q4, data=reg1.dat)

reg0.dat <- data_with_CT %>% filter(penalty==0)
reg0 <- lm(price ~ Q1+ Q2+ Q3 + Q4, data=reg0.dat)

pred1_alt <- predict(reg1,new=data_with_CT)
pred0_alt <- predict(reg0,new=data_with_CT)
mean(pred1_alt-pred0_alt)


## regression in a single step

regression<-lm(price~penalty+Q1+Q2+Q3+Q4, data=data_with_CT)
regression$coefficients["penaltyTRUE"]



# create a table of the output variables

# Estimate1 <- nn.est1$est
# Estimate2<-nn.est2$est
# Estimate3<-invisible(coef(reg.ipw)[2])
# Estimate4<-regression$coefficients["penaltyTRUE"]

output_table <- data.frame("Inverse_variance" = nn.est1$est,
                           "Mahalanobis"=nn.est2$est, 
                           "Inverse_propensity_weighting"=reg.ipw$coefficients['penaltyTRUE'],
                           "Simple_Regression"= mean(pred1_alt-pred0_alt))
rownames(output_table)[1]<-"Average Treatment Effect"

kable(output_table)

rm(list=c("data_2012", "data_with_CT",
          "data_with_groups", "final.hcris", "final.hcris.data","final.hcris.data1","grouped_data",
          "Justin", "logit.reg","mean.t0","mean.t1","nn.est1","filtered_data", "nn.est2",
          "reg.ipw", "reg0", "reg0.dat", "reg1", "reg1.dat", "regression", "table1_df"))

save.image("Hwk2_workspace_3.Rdata")

#8
#The first three models of the estimate of the average treatment effect usuing Inverse variance, mahalanobis and inverse prospensity weighting are equivalent. However, simple regression provides different estimate because of the difference in compairson to matching vs a regression in modeling the effect of penalty on price 
#9
#Yes I believe I was able to estimate a causal inference 

#I did not estimate a causal inference because i was unable to correctly complete the ATE estimates 
#but to actually answer this question yes because we are able to see the effect of the control and treatment groups on price by also having weights to better estimate the causal inference
#10 
#honestly this homework was incredibly hard and frustrating. I think i spent at least 20 hours of straight coding or problem solving 
#Also i feel very lost in terms of the new material we are covering in class which does not help when i need to apply this knowledge with real data in the homework 
#one thing i did learn was i am getting better at cleanining up the data and I believe I did good work until question 5 ish 
#however one thing that still frustrates me is my inability to understanding the matching function and concepts we have learned in this module.  







