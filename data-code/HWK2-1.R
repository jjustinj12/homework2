library(ggplot2)
#pulling the data in
final.hcris.data=read_rds('data/output/HCRIS_Data.rds')
#1. How many hospitals filed more than one report in the same year

# Group the data by hospital and year, and count the number of reports filed by each hospital in each year
grouped_data <- final.hcris %>% 
  group_by(provider_number, fyear) %>% 
  summarize(count= n()) 
  
# Filter the grouped data to only include hospitals that filed more than one report in a given year

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

final.hcris.data1 <-final.hcris.data %>%
  mutate(discount_factor = 1-tot_discounts/tot_charges)%>%
  mutate(price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment) %>%
  mutate(price_denom = tot_discharges - mcare_discharges) %>%
  mutate(price = price_num/price_denom)

  
  
  ggplot(final.hcris.data1, aes(x = year, y = price, group=year)) +
    geom_jitter(alpha= 0.05) +
    geom_violin(alpha=.9)
  
  
#5


  
data_2012 <-final.hcris.data1 %>%
  filter(year==2012) %>%
  mutate(penalty=hvbp_payment +hrrp_payment)
  
z<- data_2012 %>%
  filter(penalty > 0)
non_penalty_avg_price <- mean(z$price, na.rm=TRUE)
non_penalty_avg_price


y <- data_2012 %>%
  filter(penalty < 0 )
  
penalty_avg_price <- mean(y$price, na.rm=TRUE)
penalty_avg_price


  




