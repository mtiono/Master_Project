# Prepare the exclusion data
exclusion = read.csv("../MS_Project_data/UPDATED.csv")
exclusion = subset(exclusion, NPI>0)
exclusion$year = exclusion$EXCLDATE %/% 10000


library(data.table)

# Data preparation for 2020
medicare2020 = fread("Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2020.csv")
excl2020 = unique(exclusion$NPI[exclusion$year==2020])
medicare2020$exclud_year = (medicare2020$Rndrng_NPI %in% excl2020)
table(medicare2020$exclud_year)

# Similarly, prepare data for all years 2013-2020

# Combine the data
medicare_allyear = rbind(medicare2020, medicare2019, medicare2018, medicare2017,
                         medicare2016, medicare2015, medicare2014, medicare2013)
medicare_allyear$exclud_ever = (medicare_allyear$Rndrng_NPI %in% unique(exclusion$NPI))
table(medicare_allyear$exclud_ever)
table(medicare_allyear$exclud_year)


## Create the NPI data frame
library(tidyverse)
ydat = read.csv("../MS_Project_data/data.csv")

exclud_NPI = ydat %>%  group_by(Rndrng_NPI) %>% summarise(
  # background information
  gender = Rndrng_Prvdr_Gndr[1],
  Ent_Cd = Rndrng_Prvdr_Ent_Cd[1],
  Ent_Cd_count = length(unique(Rndrng_Prvdr_Ent_Cd)),
  city = Rndrng_Prvdr_City[1],
  city_count = length(unique(Rndrng_Prvdr_City)),
  state = Rndrng_Prvdr_State_Abrvtn[1],
  state_count = length(unique(Rndrng_Prvdr_State_Abrvtn)),
  type = Rndrng_Prvdr_Type[1],
  type_count = length(unique(Rndrng_Prvdr_Type)),
  
  # time information
  service_firstyear = min(year),
  service_lastyear = max(year),
  service_period = service_lastyear - service_firstyear,
  exclud_firstyear = min(year[exclud_year],na.rm=TRUE),
  exclud_lastyear = max(year[exclud_year],na.rm=TRUE),
  exclud_period = exclud_lastyear - exclud_firstyear,
  exclud_count = sum(exclud_year),
  
  # summary for regular years
  RUCA_regular = mean(Rndrng_Prvdr_RUCA[!exclud_year]),
  HCPCS_regular_count = length(unique(HCPCS_Cd[!exclud_year])),
  #HCPCS_regular_top1 = names(sort(table(HCPCS_Cd[!exclud_year]),decreasing=TRUE)[1]),
  Drug_Ind_Y_regular = mean(HCPCS_Drug_Ind[!exclud_year] == "Y"),
  Place_Of_Srvc_O_regular = mean(Place_Of_Srvc[!exclud_year] == "O"),
  Total_Beneficiary_regular = mean(Tot_Benes[!exclud_year]),
  Total_Service_regular = mean(Tot_Srvcs[!exclud_year]),
  Total_Bene_Day_Srvcs_regular = mean(Tot_Bene_Day_Srvcs[!exclud_year]),
  Avg_Sbmtd_Chrg_regular = mean(Avg_Sbmtd_Chrg[!exclud_year]),
  Avg_Mdcr_Alowd_Amt_regular = mean(Avg_Mdcr_Alowd_Amt[!exclud_year]),
  Avg_Mdcr_Pymt_Amt_regular = mean(Avg_Mdcr_Pymt_Amt[!exclud_year]),
  Avg_Mdcr_Stdzd_Amt_regular = mean(Avg_Mdcr_Stdzd_Amt[!exclud_year]),
  
  # summary for excluded years
  RUCA_excluded = mean(Rndrng_Prvdr_RUCA[exclud_year]),
  HCPCS_excluded_count = length(unique(HCPCS_Cd[exclud_year])),
  #HCPCS_excluded_top1 = names(sort(table(HCPCS_Cd[exclud_year]),decreasing=TRUE)[1]),
  Drug_Ind_Y_excluded = mean(HCPCS_Drug_Ind[exclud_year] == "Y"),
  Place_Of_Srvc_O_excluded = mean(Place_Of_Srvc[exclud_year] == "O"),
  Total_Beneficiary_excluded = mean(Tot_Benes[exclud_year]),
  Total_Service_excluded = mean(Tot_Srvcs[exclud_year]),
  Total_Bene_Day_Srvcs_excluded = mean(Tot_Bene_Day_Srvcs[exclud_year]),
  Avg_Sbmtd_Chrg_excluded = mean(Avg_Sbmtd_Chrg[exclud_year]),
  Avg_Mdcr_Alowd_Amt_excluded = mean(Avg_Mdcr_Alowd_Amt[exclud_year]),
  Avg_Mdcr_Pymt_Amt_excluded = mean(Avg_Mdcr_Pymt_Amt[exclud_year]),
  Avg_Mdcr_Stdzd_Amt_excluded = mean(Avg_Mdcr_Stdzd_Amt[exclud_year])
)

table(exclud_NPI$exclud_count)

sub_NPI = subset(exclud_NPI, exclud_count>0)
summary(sub_NPI)

## Plot the output by NPI
excludedNPI = ydat %>% 
  filter(Rndrng_NPI %in% sub_NPI$Rndrng_NPI) %>% 
  group_by(Rndrng_NPI, year) %>% 
  summarise(
  state = Rndrng_Prvdr_State_Abrvtn[1],
  Total_Beneficiary = sum(Tot_Benes),
  Total_Service = sum(Tot_Srvcs),
  Total_Bene_Day_Srvcs = sum(Tot_Bene_Day_Srvcs),
  Avg_Sbmtd_Chrg = sum(Avg_Sbmtd_Chrg),
  Avg_Mdcr_Alowd_Amt = sum(Avg_Mdcr_Alowd_Amt),
  Avg_Mdcr_Pymt_Amt = sum(Avg_Mdcr_Pymt_Amt),
  Avg_Mdcr_Stdzd_Amt = sum(Avg_Mdcr_Stdzd_Amt),
  exclud_year = exclud_year[1]
) 

for (i in 4:10){
  varname = names(excludedNPI)[i]
  tmp = log(unname(unlist(excludedNPI[,i])))
  p = ggplot(data=excludedNPI, aes(x=year, y=tmp, 
                                 group = Rndrng_NPI, colour = state)) + 
  geom_line() + 
  geom_point(size=I(0.5)) + 
  geom_point(data=excludedNPI[excludedNPI$exclud_year,],
             aes(y=tmp[excludedNPI$exclud_year]), size=I(2)) +
  facet_wrap(~state, ncol=7) +
  labs(y= sprintf("Log(%s)",varname), x = "year")  + 
  theme_bw() +
  theme(legend.position = 'None')

  pdf(file=sprintf("%s.pdf",varname),width=10,height=8)
  print(p)
  dev.off()
}



## Summarizing the entire dataset
medicare_allyear = fread('C:/Users/xycheng/Documents/medicare.csv')
NPI_excluded = intersect(unique(medicare_allyear$Rndrng_NPI), unique(exclusion$NPI))

set.seed(1)
NPI_sample = sample(unique(medicare_allyear$Rndrng_NPI), 50000)
NPI_sample = sort(unique(c(NPI_sample, NPI_excluded)))
medicare_sample = medicare_allyear %>% filter(Rndrng_NPI %in% NPI_sample)

#write.csv(medicare_sample, file='../MS_Project_data/medicare_sample_50K.csv',
#          row.names=FALSE)

group_NPI = medicare_sample %>%  group_by(Rndrng_NPI) %>% summarise(
  # background information
  gender = Rndrng_Prvdr_Gndr[1],
  Ent_Cd = Rndrng_Prvdr_Ent_Cd[1],
  Ent_Cd_count = length(unique(Rndrng_Prvdr_Ent_Cd)),
  city = Rndrng_Prvdr_City[1],
  city_count = length(unique(Rndrng_Prvdr_City)),
  state = Rndrng_Prvdr_State_Abrvtn[1],
  state_count = length(unique(Rndrng_Prvdr_State_Abrvtn)),
  type = Rndrng_Prvdr_Type[1],
  type_count = length(unique(Rndrng_Prvdr_Type)),

  # time information
  service_firstyear = min(year),
  service_lastyear = max(year),
  service_period = service_lastyear - service_firstyear,
  exclud_count = sum(exclud_year),
  
  # summary information
  HCPCS_regular_count = length(unique(HCPCS_Cd)),
  Drug_Ind_Y_mean = mean(HCPCS_Drug_Ind == "Y"),
  Place_Of_Srvc_O_mean = mean(Place_Of_Srvc == "O"),
  
  RUCA_regular_mean = mean(Rndrng_Prvdr_RUCA),
  RUCA_regular_median = median(Rndrng_Prvdr_RUCA),
  RUCA_regular_min = min(Rndrng_Prvdr_RUCA),
  RUCA_regular_max = max(Rndrng_Prvdr_RUCA),
  RUCA_regular_std = sd(Rndrng_Prvdr_RUCA),
  RUCA_regular_sum = sum(Rndrng_Prvdr_RUCA),
  RUCA_regular_corr = cor(Rndrng_Prvdr_RUCA,year),
  RUCA_regular_slope = cor(Rndrng_Prvdr_RUCA,year)*(sd(Rndrng_Prvdr_RUCA)/sd(year)),
  
  Total_Beneficiary_mean = mean(Tot_Benes),
  Total_Beneficiary_median = median(Tot_Benes),
  Total_Beneficiary_min = min(Tot_Benes),
  Total_Beneficiary_max = max(Tot_Benes),
  Total_Beneficiary_std = sd(Tot_Benes),
  Total_Beneficiary_sum = sum(Tot_Benes),
  Total_Beneficiary_corr = cor(Tot_Benes,year),
  Total_Beneficiary_slope = cor(Tot_Benes,year)*(sd(Tot_Benes)/sd(year)),
  
  Total_Service_mean = mean(Tot_Srvcs),
  Total_Service_median = median(Tot_Srvcs),
  Total_Service_min = min(Tot_Srvcs),
  Total_Service_max = max(Tot_Srvcs),
  Total_Service_std = sd(Tot_Srvcs),
  Total_Service_sum = sum(Tot_Srvcs),
  Total_Service_corr = cor(Tot_Srvcs, year),
  Total_Service_slope =  cor(Tot_Srvcs,year)*(sd(Tot_Srvcs)/sd(year)),
  
  Total_Bene_Day_Srvcs_mean = mean(Tot_Bene_Day_Srvcs),
  Total_Bene_Day_Srvcs_median = median(Tot_Bene_Day_Srvcs),
  Total_Bene_Day_Srvcs_min = min(Tot_Bene_Day_Srvcs),
  Total_Bene_Day_Srvcs_max = max(Tot_Bene_Day_Srvcs),
  Total_Bene_Day_Srvcs_std = sd(Tot_Bene_Day_Srvcs),
  Total_Bene_Day_Srvcs_sum = sum(Tot_Bene_Day_Srvcs),
  Total_Bene_Day_Srvcs_corr = cor(Tot_Bene_Day_Srvcs, year),
  Total_Bene_Day_Srvcs_slope =  cor(Tot_Bene_Day_Srvcs,year)*(sd(Tot_Bene_Day_Srvcs)/sd(year)),
  
  Avg_Sbmtd_Chrg_mean = mean(Avg_Sbmtd_Chrg),
  Avg_Sbmtd_Chrg_median = median(Avg_Sbmtd_Chrg),
  Avg_Sbmtd_Chrg_min = min(Avg_Sbmtd_Chrg),
  Avg_Sbmtd_Chrg_max = max(Avg_Sbmtd_Chrg),
  Avg_Sbmtd_Chrg_std = sd(Avg_Sbmtd_Chrg),
  Avg_Sbmtd_Chrg_sum = sum(Avg_Sbmtd_Chrg),
  Avg_Sbmtd_Chrg_corr = cor(Avg_Sbmtd_Chrg, year),
  Avg_Sbmtd_Chrg_slope =  cor(Avg_Sbmtd_Chrg,year)*(sd(Avg_Sbmtd_Chrg)/sd(year)),
  
  Avg_Mdcr_Alowd_Amt_mean = mean(Avg_Mdcr_Alowd_Amt),
  Avg_Mdcr_Alowd_Amt_median = median(Avg_Mdcr_Alowd_Amt),
  Avg_Mdcr_Alowd_Amt_min = min(Avg_Mdcr_Alowd_Amt),
  Avg_Mdcr_Alowd_Amt_max = max(Avg_Mdcr_Alowd_Amt),
  Avg_Mdcr_Alowd_Amt_std = sd(Avg_Mdcr_Alowd_Amt),
  Avg_Mdcr_Alowd_Amt_sum = sum(Avg_Mdcr_Alowd_Amt),
  Avg_Mdcr_Alowd_Amt_corr = cor(Avg_Mdcr_Alowd_Amt, year),
  Avg_Mdcr_Alowd_Amt_slope =  cor(Avg_Mdcr_Alowd_Amt,year)*(sd(Avg_Mdcr_Alowd_Amt)/sd(year)),
  
  Avg_Mdcr_Pymt_Amt_mean = mean(Avg_Mdcr_Pymt_Amt),
  Avg_Mdcr_Pymt_Amt_median = median(Avg_Mdcr_Pymt_Amt),
  Avg_Mdcr_Pymt_Amt_min = min(Avg_Mdcr_Pymt_Amt),
  Avg_Mdcr_Pymt_Amt_max = max(Avg_Mdcr_Pymt_Amt),
  Avg_Mdcr_Pymt_Amt_std = sd(Avg_Mdcr_Pymt_Amt),
  Avg_Mdcr_Pymt_Amt_sum = sum(Avg_Mdcr_Pymt_Amt),
  Avg_Mdcr_Pymt_Amt_corr = cor(Avg_Mdcr_Pymt_Amt, year),
  Avg_Mdcr_Pymt_Amt_slope =  cor(Avg_Mdcr_Pymt_Amt,year)*(sd(Avg_Mdcr_Pymt_Amt)/sd(year)),
  
  Avg_Mdcr_Stdzd_Amt_mean = mean(Avg_Mdcr_Stdzd_Amt),
  Avg_Mdcr_Stdzd_Amt_median = median(Avg_Mdcr_Stdzd_Amt),
  Avg_Mdcr_Stdzd_Amt_min = min(Avg_Mdcr_Stdzd_Amt),
  Avg_Mdcr_Stdzd_Amt_max = max(Avg_Mdcr_Stdzd_Amt),
  Avg_Mdcr_Stdzd_Amt_std = sd(Avg_Mdcr_Stdzd_Amt),
  Avg_Mdcr_Stdzd_Amt_sum = sum(Avg_Mdcr_Stdzd_Amt),
  Avg_Mdcr_Stdzd_Amt_corr = cor(Avg_Mdcr_Stdzd_Amt,year),
  Avg_Mdcr_Stdzd_Amt_slope =  cor(Avg_Mdcr_Stdzd_Amt,year)*(sd(Avg_Mdcr_Stdzd_Amt)/sd(year))
) 
dim(group_NPI)
write.csv(group_NPI, file="../MS_Project_data/final_data_50K.csv", row.names=FALSE)
