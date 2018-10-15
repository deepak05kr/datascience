################### Investment Case Study   #################################
################### Done in Windows 8.1     #################################

#including libraries 
library(tidyr)
library(stringr)
library(dplyr)
library(magrittr)

# setting working directory
#setwd("F:\Education\IIIT\Docs\WorkSheets\0008 Group Case study")

# Reading files 
rounds2 <-read.csv("rounds2.csv",stringsAsFactors = F)
companies <-read.delim("companies.txt", sep =  "\t", stringsAsFactors = F)

# Cleaning the data - we are setting the permalink in both companies & rounds2 datasets to lowercase,
# as there is a case mismatch
companies$permalink <- toupper(companies$permalink)
rounds2$company_permalink <- toupper(rounds2$company_permalink)


#1.1.1 How many unique companies are present in rounds2?
# we need to use toupper() as there are both lower & upper cases in the column
no_of_unique_companies_Rounds <- length(unique(toupper(rounds2$company_permalink)))

#1.1.2 How many unique companies are present in the companies file?
no_of_unique_companies_Companies <- length(unique(toupper(companies$permalink)))

#1.1.3 In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.	
print("permalink") 

#1.1.4 Are there any companies in the rounds2 file which are not Â present inÂ companies ? Answer Y/N.
#Ans  we can do left outer join with rounds2$company_permalink and companies$permalink
#     if the length of the the merged df is not same as # of unique companies, 
#     then there are companies present in Rounds2 that are not present in companies
x <- as.data.frame( unique(toupper(rounds2$company_permalink))) 
colnames(x)<-c("permalink1")
y <- as.data.frame(unique(toupper(companies$permalink)))
colnames(y)<-c("permalink2")
companies_list <-merge(x,y,by.x = c("permalink1") , by.y = c("permalink2") , all.x = TRUE)
if(nrow(companies_list) == no_of_unique_companies_Companies){
  print("Y")
}else {
  print("N")
}
#1.1.5 Merge the two data frames so that all .variables (columns) .in the companies frame are added to the rounds2 data frame.Â Name the merged frame master_frame.Â How many observations are present in master_frame ?
# Ans - we can use nrow function to calculate the no of rows in master_frame
master_frame <-merge(companies,rounds2,by.x = c("permalink") , by.y = c("company_permalink") )
print(nrow(master_frame))


#2.1.1 Average funding amount of venture type
# from the master frame, we need to filter the data based on the funding round type as venture
# and then calculate its average
venture_data<-filter(master_frame,funding_round_type == "venture")
average_venture<-mean(venture_data$raised_amount_usd ,na.rm = T )

#2.1.2 Average funding amount of angel type
# from the master frame, we need to filter the data based on the funding round type as angel
# and then calculate its average
angel_data<-filter(master_frame,funding_round_type == "angel")
average_angel<-mean(angel_data$raised_amount_usd ,na.rm = T )

#2.1.3 Average funding amount for seed investment type
# from the master frame, we need to filter the data based on the funding round type as "seed"
# and then calculate its average
seed_data<-filter(master_frame,funding_round_type == "seed")
average_seed<-mean(seed_data$raised_amount_usd ,na.rm = T )

#2.1.4 Average funding amount for private equity investment type
# from the master frame, we need to filter the data based on the funding round type as "private equity"
# and then calculate its average
privequity_data<-filter(master_frame,funding_round_type == "private_equity")
average_privequity<-mean(privequity_data$raised_amount_usd ,na.rm = T )

# 2.1.5 Considering that Spark Funds wants to invest between 5 to 15 million USD per  investment round, which investment type is the most suitable for them?
# The following steps are performed to get the preferred investment type
# Step 1 : we need to consider the following conditions
#           1. funding round types can be any of - Venture, Angel, Seed, Private Equity
#           2. raised amount used between $5,000,000 and $15,000,000
# Step 2 : We can create groups based on the founding round type
# Step 3 : calculate the number of investements for each funding round type
# Step 4 : Select the investment type with maximum no number of investements

rounds2_filtered <- filter(rounds2,((funding_round_type == "venture" |funding_round_type == "angel"|funding_round_type == "seed" 
                                     |funding_round_type == "private_equity" )
                                    &(raised_amount_usd>5000000 & raised_amount_usd < 15000000)))

fund_round_Type_groups<- group_by(rounds2_filtered, funding_round_type)
count_investments <- count(fund_round_Type_groups,"funding_round_permalink")
filter(count_investments, n == max(count(fund_round_Type_groups,"funding_round_permalink")$n))$funding_round_type

#3 Analysing the Top 3 English-Speaking Countries		

# We need to consider only english speaking countries before proceeding with our next analysis
# identifying english speaking countries is a very important step. 

# We have taken the country codes of english speaking countries referring to the PDF & world database
english_spk_countries <-as.data.frame(c(  "ATG","AUS","BRB","BLZ","BWA","CMR","CAN","DMA","ERI","ETH","GHA","GRD","GUY","IND","IRL","JAM","KEN","KIR","LSO","LBR","MWI","MLT","MHL","MUS","NAM","NRU","NZL","NGA","PAK","PLW","PNG","PHL","RWA","KNA","LCA","VCT","WSM","SYC","SLE","SGP","SLB","ZAF","SDN","SWZ","TZA","TON","TTO","TUV","UGA","GBR","USA","VUT","ZMB","ZWE"))
colnames(english_spk_countries) <- c("country_code")


# We are now arriving at top 9 countries which have recieved highest total funding across all sectors
# The following steps are executed 
# Step 1 : Get the investment details for Venture investment type - make sure na records are removed 
# Step 2 : Get the groups based on the country code
# Step 3 : get the sum of total amount raised for each country code
# Step 4 : Sort the list based on the amount raised
# Step 5 : Get top 9 countries from the above list
venture_data_updated<-venture_data[!is.na(venture_data$raised_amount_usd),]
groupby_country<-group_by(venture_data_updated,country_code)
summ_country<-summarise(groupby_country,total_amount=sum(raised_amount_usd))
summ_country <- filter(summ_country, country_code != "")
top9 <-arrange(summ_country,desc(total_amount)) %>% head(9)

# We can use head to get top 3 countries
top3 <- arrange( merge(top9, english_spk_countries, by = c("country_code")), desc(total_amount)) %>% head(3)

print(top3[1,]$country_code)
print(top3[2,]$country_code)
print(top3[3,]$country_code)

#5. Sector-wise Investment Analysis				

#reading the data from mapping.csv
mapping <- read.csv("mapping.csv", stringsAsFactors = F)

#cleaning the data
mapping <- mapping[,-3]
mapping$category_list <- str_replace_all(mapping$category_list,"0","na")

mapping_modified <- gather(mapping, key=sector_name,value=sector_value, "Automotive...Sports":"Social..Finance..Analytics..Advertising" )
mapping_modified <- mapping_modified[-which(mapping_modified$sector_value == 0), ]
mapping_modified <- mapping_modified[,-3]
colnames(mapping_modified)[2] <- "main_sector"

#venture_data_sep <- venture_data_updated %>% 
#  mutate(category_list = strsplit(category_list, "|",TRUE)) %>% 
#  unnest(category_list)

venture_data_sep <- venture_data_updated
venture_data_sep$primary_sector <-  as.character(lapply(strsplit(as.character(venture_data_updated$category_list), split="|", TRUE),head, n=1))


venture_sector_mapping$primary_sector <- tolower(venture_sector_mapping$primary_sector)
mapping_modified$main_sector <- tolower(mapping_modified$main_sector)

venture_sector_mapping <- merge(venture_data_sep, mapping_modified, by.x = c("primary_sector"), by.y = c("category_list"))



#sector analysis



#colnames(venture_sector_mapping)[16] <- "main_sector"
# 5. Sector-wise Investment Analysis				

#1. Total number of Investments (count)

usa_total_no_Of_investments <- nrow(filter(venture_data_updated,  country_code == "USA", raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000))
gbr_total_no_Of_investments <- nrow(filter(venture_data_updated, country_code == "GBR", raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000))
ind_total_no_Of_investments <- nrow(filter(venture_data_updated, country_code == "IND", raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000))

#2 Total amount of investment (USD)

usa_total_sum_investments <- sum((filter(venture_data_updated,  country_code == "USA", raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000))$raised_amount_usd)
gbr_total_sum_investments <- sum((filter(venture_data_updated, country_code == "GBR", raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000))$raised_amount_usd)
ind_total_sum_investments <- sum((filter(venture_data_updated, country_code == "IND", raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000))$raised_amount_usd)


#3 top sector name


D1 <- filter(venture_sector_mapping,  country_code == "USA", raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)

usa_ventures_sector_filtered <- filter(venture_sector_mapping,((country_code == "USA")&(raised_amount_usd>=5000000 & raised_amount_usd <= 15000000)))
usa_venture_sector_groups <- group_by(usa_ventures_sector_filtered,main_sector )
usa_count_investments_sector_country <- count(usa_venture_sector_groups,"funding_round_permalink")
usa_count_investments_sector_country <- usa_count_investments_sector_country[,-2]
colnames(usa_count_investments_sector_country)[2] <- "count_investements"
usa_avg_investments_sector_country <- summarise(usa_venture_sector_groups,Avg_investment  = mean(raised_amount_usd))

D1 <- merge(D1,usa_count_investments_sector_country,by = "main_sector")
D1 <- merge(D1,usa_avg_investments_sector_country,by = "main_sector")


D2 <- filter(venture_sector_mapping,  country_code == "GBR", raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)

gbr_ventures_sector_filtered <- filter(venture_sector_mapping,((country_code == "GBR")&(raised_amount_usd>=5000000 & raised_amount_usd <= 15000000)))
gbr_venture_sector_groups <- group_by(gbr_ventures_sector_filtered,main_sector )
gbr_count_investments_sector_country <- count(gbr_venture_sector_groups,"funding_round_permalink")
gbr_count_investments_sector_country <- gbr_count_investments_sector_country[,-2]
colnames(gbr_count_investments_sector_country)[2] <- "count_investements"
gbr_avg_investments_sector_country <- summarise(gbr_venture_sector_groups,Avg_investment  = mean(raised_amount_usd))

D2 <- merge(D2,gbr_count_investments_sector_country,by = "main_sector")
D2 <- merge(D2,gbr_avg_investments_sector_country,by = "main_sector")


D3 <- filter(venture_sector_mapping,  country_code == "IND", raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)

ind_ventures_sector_filtered <- filter(venture_sector_mapping,((country_code == "IND")&(raised_amount_usd>=5000000 & raised_amount_usd <= 15000000)))
ind_venture_sector_groups <- group_by(ind_ventures_sector_filtered,main_sector )
ind_count_investments_sector_country <- count(ind_venture_sector_groups,"funding_round_permalink")
ind_count_investments_sector_country <- ind_count_investments_sector_country[,-2]
colnames(ind_count_investments_sector_country)[2] <- "count_investements"
ind_avg_investments_sector_country <- summarise(ind_venture_sector_groups,Avg_investment  = mean(raised_amount_usd))

D3 <- merge(D3,ind_count_investments_sector_country,by = "main_sector")
D3 <- merge(D3,ind_avg_investments_sector_country,by = "main_sector")


