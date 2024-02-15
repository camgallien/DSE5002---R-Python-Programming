project_data_csv <- read.csv("R Project/data.csv"
                      ,stringsAsFactors=FALSE
)

library(stringr)
library(dplyr)
library(ggplot2)

ds_size_csv<-project_data_csv %>%
  #filter((grepl('Data Sci', job_title))) %>%
  filter(employment_type!="PT") %>%
  filter(experience_level!="EN") %>%
  filter(company_size!="L") %>%
  filter(experience_level!="EX") %>%
  mutate(newsalary = salary_in_usd/1000) %>%
  mutate(experience_level=ifelse(experience_level=="MI", "Mid-Level", experience_level))%>%
  mutate(experience_level=ifelse(experience_level=="SE", "Senior-Level", experience_level))%>%
  #mutate(experience_level=ifelse(experience_level=="EX", "Executive-Level", experience_level))%>%
  mutate(company_size=ifelse(company_size=="S", "1-49", company_size))%>%
  mutate(company_size=ifelse(company_size=="M", "50-250", company_size))%>%
  mutate(remote_ratio=ifelse(remote_ratio==0, "On-site", remote_ratio))%>%
  mutate(remote_ratio=ifelse(remote_ratio==50, "Hybrid", remote_ratio))%>%
  mutate(remote_ratio=ifelse(remote_ratio==100, "Remote", remote_ratio))%>%
  #changed all out-side US workers to Off-shore as that would be their status with this company
  mutate(employee_residence=ifelse(employee_residence!="US", "Off-shore", employee_residence))
 

# exp_salary_df <-data_science_csv %>%
#   group_by(experience_level)%>%
#   summarize(avg_salary = mean(salary_in_usd))
# 
# size_salary_df <-data_science_csv %>%
#   group_by(company_size) %>%
#   summarize(avg_salary=mean(salary_in_usd))
# 
# salary_df <- data_science_csv %>%
#   group_by(company_size,experience_level) %>%
#   summarize(avg_salary=mean(salary_in_usd))
# 
# summary(data_science_csv)

SE_S <- ds_size_csv %>%
  group_by(experience_level, company_size) %>%
  filter(experience_level=="Senior-Level")%>%
  filter(company_size=="1-49")

summary(SE_S$salary_in_usd)

SE_M <- ds_size_csv %>%
  group_by(experience_level, company_size) %>%
  filter(experience_level=="Senior-Level")%>%
  filter(company_size=="50-250")

summary(SE_M$salary_in_usd)

MI_S <- ds_size_csv %>%
  group_by(experience_level, company_size) %>%
  filter(experience_level=="Mid-Level")%>%
  filter(company_size=="1-49")

summary(MI_S$salary_in_usd)

MI_M <- ds_size_csv %>%
  group_by(experience_level, company_size) %>%
  filter(experience_level=="Mid-Level")%>%
  filter(company_size=="50-250")

summary(MI_M$salary_in_usd)

MI_off <- ds_size_csv %>%
  group_by(experience_level, employee_residence) %>%
  filter(experience_level=="Mid-Level")%>%
  filter(employee_residence=="Off-shore")

SE_off <- ds_size_csv %>%
  group_by(experience_level, employee_residence) %>%
  filter(experience_level=="Senior-Level")%>%
  filter(employee_residence=="Off-shore")

MI_US <- ds_size_csv %>%
  group_by(experience_level, employee_residence) %>%
  filter(experience_level=="Mid-Level")%>%
  filter(employee_residence=="US")

SE_US <- ds_size_csv %>%
  group_by(experience_level, employee_residence) %>%
  filter(experience_level=="Senior-Level")%>%
  filter(employee_residence=="US")

summary(MI_off$salary_in_usd)
summary(SE_off$salary_in_usd)
summary(MI_US$salary_in_usd)
summary(SE_US$salary_in_usd)

# ds_US_csv<-project_data_csv %>%
#   filter((grepl('Data Scientist', job_title))) %>%
#   filter(employment_type!="PT") %>%
#   filter(experience_level!="EN") %>%
#   filter(experience_level!="EX") %>%
#   filter(employee_residence=="US")
# 
# ds_OS_csv<-project_data_csv %>%
#   filter((grepl('Data Scientist', job_title))) %>%
#   filter(employment_type!="PT") %>%
#   filter(experience_level!="EN") %>%
#   filter(experience_level!="EX") %>%
#   filter(employee_residence!="US")

ggplot(ds_size_csv, aes(x=company_size, y=newsalary, fill=experience_level))+
  geom_boxplot()+
  labs(x='Number of Employees',
       y='Salary in Thousands (USD)',
       fill='Experience Level',
       title='Data Science Salaries by Company Size')+
  scale_y_continuous(limits=c(0, 275),breaks=c(0,25,50,75,100,125,150,175,200,225,250,275))

ggplot(ds_size_csv, aes(x=employee_residence, y=newsalary, fill=experience_level))+
  geom_boxplot()+
  labs(x='Employee Location',
       y='Salary in Thousands (USD)',
       fill='Experience Level',
       title='Data Science Salaries by Employee Location')+
  scale_y_continuous(limits=c(0, 275),breaks=c(0,25,50,75,100,125,150,175,200,225,250,275))

ggplot(ds_size_csv, aes(x=remote_ratio, y=newsalary, fill=employee_residence))+
  geom_boxplot(outlier.shape=NA)+
  labs(x='Remote Work Ratio',
       y='Salary in Thousands (USD)',
       fill='Employee Location',
       title='Salary by Remote Work Ratio')+
  scale_y_continuous(limits=c(0, 250), breaks=c(0,25,50,75,100,125,150,175,200,225,250))

ds_year_csv<-ds_size_csv %>%
  group_by(experience_level, work_year)%>%
  summarize(mean_per_year=mean(newsalary))

ggplot(ds_year_csv, aes(x=work_year, y=mean_per_year, color=experience_level))+
  geom_line(aes(color=experience_level))+
  geom_point()+
  labs(x='Work Year',
       y='Avg. Salary in Thousands',
       fill= 'Experience Level',
       title='Average Salary per Year')+
  guides(color=guide_legend(title="Experience Level"))+
  scale_x_continuous(breaks=c(2020, 2021, 2022))+
  scale_y_continuous(breaks=c(0,25,50,75,100,125,150))

SE_OS_csv <- ds_size_csv %>%
  filter(experience_level=="Senior-Level") %>%
  filter(company_size=="50-250") %>%
  filter(employee_residence=="Off-shore") 

SE_US_csv <- ds_size_csv %>%
  filter(experience_level=="Senior-Level") %>%
  filter(company_size=="50-250") %>%
  filter(employee_residence=="US") 

summary(SE_OS_csv$salary_in_usd)
summary(SE_US_csv$salary_in_usd)

ggplot(SE_OS_csv, aes(x=employee_residence, y=newsalary, fill=experience_level))+
  geom_boxplot( fill="cyan")+
  labs(x='Employee Location',
       y='Salary in Thousands (USD)',
       title='Senior-Level Salaries - 50-250 Emp.')+
  scale_y_continuous(limits=c(0, 250), breaks=c(0,25,50,75,100,125,150,175,200, 225, 250))





