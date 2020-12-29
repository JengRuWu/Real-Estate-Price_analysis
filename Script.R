#Please set the working directory
setwd("~/Desktop/Assignment")
library(haven)
library(ggplot2)
library(zoo)
library(plyr)
library(stringr)
library(stringi)
library(ggpubr)
library(gridExtra)


#data input
building <- read_dta("local_building_raw.dta")
land <- read_dta("local_land_raw.dta")

#transfer rocyear and rocquarter into date(yyyy qq)
land$date <- ifelse(land$rocquarter=="一",paste(land$rocyear+1911,"Q1"),
                    ifelse(land$rocquarter=="二",paste(land$rocyear+1911,"Q2"),
                           ifelse(land$rocquarter=="三",paste(land$rocyear+1911,"Q3"),paste(land$rocyear+1911,"Q4"))))

building$date <- ifelse(building$rocquarter=="一",paste(building$rocyear+1911,"Q1"),
                    ifelse(building$rocquarter=="二",paste(building$rocyear+1911,"Q2"),
                           ifelse(building$rocquarter=="三",paste(building$rocyear+1911,"Q3"),paste(building$rocyear+1911,"Q4"))))

#create a file to store the results of the seven questions
dir.create(file.path(getwd(),"results"))

############Q1
#get the median unit price for each quarter-year and city
na_stripped_land <- land[!is.na(land$unit_price),] #strip the NAs first
median_city_date <- aggregate(na_stripped_land["unit_price"],by=na_stripped_land[c("date","city")],FUN=median)
#save the results
dir.create(file.path(getwd(),"results","Q1"))
write_dta(median_city_date, paste(getwd(),"results","Q1","median_by_city_date.dta",sep="/"))

############Q2
#create a new dataframe for plotting
data_for_plot <- data.frame("city" = na_stripped_land$city, "unit_price" = na_stripped_land$unit_price, "date"=na_stripped_land$date)
#change the "date" into date format 
data_for_plot$date <- as.yearqtr(data_for_plot$date, format = "%Y Q%q")

#Plot median of Taiwan
Taiwan_median <- aggregate(data_for_plot["unit_price"],by=data_for_plot["date"],FUN=median) #aggregate to get the median of each date
base_number_Taiwan <- Taiwan_median$unit_price[Taiwan_median$date=="2000 Q1"] #normalization-calculate base number
Taiwan_median$unit_price <- Taiwan_median$unit_price / base_number_Taiwan #normalization-divide by base number
plot_Taiwan <- ggplot(Taiwan_median,aes(date,unit_price,group=1)) + geom_line() + geom_point()+ theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Taiwan)",x ="Date(yyyy-q)", y = "Median Price Index")#plot the graph

#Plot median of Taipei
Taipei_median <- aggregate(unit_price~date, median, data = subset(data_for_plot, city == "台北市"))#aggregate by median within subset of Taipei
base_number_Taipei <- Taipei_median$unit_price[Taipei_median$date=="2000 Q2"] 
Taipei_median$unit_price <- Taipei_median$unit_price / base_number_Taipei 
plot_Taipei <- ggplot(Taipei_median,aes(date,unit_price,group=1)) + geom_line() + geom_point()+ theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Taipei)",x ="Date(yyyy-q)", y = "Median Price Index")

#Plot median of Taoyuan
Taoyuan_median <- aggregate(unit_price~date, median, data = subset(data_for_plot, city == "桃園市"))
base_number_Taoyuan <- Taoyuan_median$unit_price[Taoyuan_median$date=="2000 Q1"] 
Taoyuan_median$unit_price <- Taoyuan_median$unit_price / base_number_Taoyuan 
plot_Taoyuan <- ggplot(Taoyuan_median,aes(date,unit_price,group=1)) + geom_line() + geom_point()+ theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Taoyuan)",x ="Date(yyyy-q)", y = "Median Price Index")

#Plot median of Taichung
Taichung_median <- aggregate(unit_price~date, median, data = subset(data_for_plot, city == "台中市"))
base_number_Taichung <- Taichung_median$unit_price[Taichung_median$date=="2000 Q1"] 
Taichung_median$unit_price <- Taichung_median$unit_price / base_number_Taichung 
plot_Taichung <- ggplot(Taichung_median,aes(date,unit_price,group=1)) + geom_line() + geom_point()+ theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Taichung)",x ="Date(yyyy-q)", y = "Median Price Index")

#Plot median of Tainan
Tainan_median <- aggregate(unit_price~date, median, data = subset(data_for_plot, city == "台南市"))
base_number_Tainan <- Tainan_median$unit_price[Tainan_median$date=="2000 Q1"] 
Tainan_median$unit_price <- Tainan_median$unit_price / base_number_Tainan 
plot_Tainan <- ggplot(Tainan_median,aes(date,unit_price,group=1)) + geom_line() + geom_point()+ theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Tainan)",x ="Date(yyyy-q)", y = "Median Price Index")

#Plot median of Kaohsiung
Kaohsiung_median <- aggregate(unit_price~date, median, data = subset(data_for_plot, city == "高雄市"))
base_number_Kaohsiung <- Kaohsiung_median$unit_price[Kaohsiung_median$date=="2000 Q1"] 
Kaohsiung_median$unit_price <- Kaohsiung_median$unit_price / base_number_Kaohsiung 
plot_Kaohsiung <- ggplot(Kaohsiung_median,aes(date,unit_price,group=1)) + geom_line() + geom_point()+ theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Kaohsiung)",x ="Date(yyyy-q)", y = "Median Price Index")

#Plot median of Xinbei
Xinbei_median <- aggregate(unit_price~date, median, data = subset(data_for_plot, city == "新北市"))
base_number_Xinbei <- Xinbei_median$unit_price[Xinbei_median$date=="2000 Q1"] 
Xinbei_median$unit_price <- Xinbei_median$unit_price / base_number_Xinbei 
plot_Xinbei <- ggplot(Xinbei_median,aes(date,unit_price,group=1)) + geom_line() + geom_point() +theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Xinbei)",x ="Date(yyyy-q)", y = "Median Price Index")

#save the plots
dir.create(file.path(getwd(),"results","Q2"))
ggsave("Taiwan.jpeg",plot_Taiwan,path=file.path(getwd(),"results","Q2"))
ggsave("Taipei.jpeg",plot_Taipei,path=file.path(getwd(),"results","Q2"))
ggsave("Taoyuan.jpeg",plot_Taoyuan,path=file.path(getwd(),"results","Q2"))
ggsave("Taichung.jpeg",plot_Taichung,path=file.path(getwd(),"results","Q2"))
ggsave("Tainan.jpeg",plot_Tainan,path=file.path(getwd(),"results","Q2"))
ggsave("Kaohsiung.jpeg",plot_Kaohsiung,path=file.path(getwd(),"results","Q2"))
ggsave("Xinbei.jpeg",plot_Xinbei,path=file.path(getwd(),"results","Q2"))

############Q3
#strip the NAs in "transact_value" and "unit_price"
na_stripped_land_Top20 <- land[!is.na(land$transact_value),]
na_stripped_land_Top20 <- na_stripped_land_Top20[!is.na(na_stripped_land_Top20$unit_price),]

#retrieve the top 20% of transact value
top20 = subset(na_stripped_land_Top20, transact_value > quantile(transact_value, prob = 1 - 20/100))
#get the median of each year-quarter and city
median_city_date_top20 <- aggregate(top20["unit_price"],by=top20[c("date","city")],FUN=median)

#save median by city and date of Top 20%
dir.create(file.path(getwd(),"results","Q3"))
write_dta(median_city_date_top20, paste(getwd(),"results","Q3","median_by_city_date_top20%.dta",sep="/"))

#create a new dataframe for plotting
data_for_plot_top20 <- data.frame("city" = top20$city, "unit_price" = top20$unit_price,"date" = top20$date)
#change the "date" to date format
data_for_plot_top20$date <- as.yearqtr(data_for_plot_top20$date, format = "%Y Q%q")

#Plot median of Taiwan
Taiwan_median_Top20 <- aggregate(data_for_plot_top20["unit_price"],by=data_for_plot_top20["date"],FUN=median)#aggregate to get the median of each date
base_number_Taiwan_Top20 <- Taiwan_median_Top20$unit_price[Taiwan_median_Top20$date=="2000 Q1"] #normalization-get base number
Taiwan_median_Top20$unit_price <- Taiwan_median_Top20$unit_price / base_number_Taiwan_Top20 #normalization-divide by base number
plot_Taiwan_Top20 <- ggplot(Taiwan_median_Top20,aes(date,unit_price,group=1)) + geom_line() +geom_point() + theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Taiwan)(Top 20%)",x ="Date(yyyy-q)", y = "Median Price Index")#plot the graph

#Plot median of Taipei
Taipei_median_Top20 <- aggregate(unit_price~date, median, data = subset(data_for_plot_top20, city == "台北市"))
base_number_Taipei_Top20 <- Taipei_median_Top20$unit_price[Taipei_median_Top20$date=="2000 Q2"] 
Taipei_median_Top20$unit_price <- Taipei_median_Top20$unit_price / base_number_Taipei_Top20 
plot_Taipei_Top20 <- ggplot(Taipei_median_Top20,aes(date,unit_price,group=1)) + geom_line() +geom_point()+ theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Taipei)(Top 20%)",x ="Date(yyyy-q)", y = "Median Price Index")

#Plot median of Taoyuan
Taoyuan_median_Top20 <- aggregate(unit_price~date, median, data = subset(data_for_plot_top20, city == "桃園市"))
base_number_Taoyuan_Top20 <- Taoyuan_median_Top20$unit_price[Taoyuan_median_Top20$date=="2000 Q1"] 
Taoyuan_median_Top20$unit_price <- Taoyuan_median_Top20$unit_price / base_number_Taoyuan_Top20 
plot_Taoyuan_Top20 <- ggplot(Taoyuan_median_Top20,aes(date,unit_price,group=1)) + geom_line() +geom_point()+ theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Taoyuan)(Top 20%)",x ="Date(yyyy-q)", y = "Median Price Index")

#Plot median of Taichung
Taichung_median_Top20 <- aggregate(unit_price~date, median, data = subset(data_for_plot_top20, city == "台中市"))
base_number_Taichung_Top20 <- Taichung_median_Top20$unit_price[Taichung_median_Top20$date=="2000 Q1"] 
Taichung_median_Top20$unit_price <- Taichung_median_Top20$unit_price / base_number_Taichung_Top20 
plot_Taichung_Top20 <- ggplot(Taichung_median_Top20,aes(date,unit_price,group=1)) + geom_line() +geom_point()+ theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Taichung)(Top 20%)",x ="Date(yyyy-q)", y = "Median Price Index")

#Plot median of Tainan
Tainan_median_Top20 <- aggregate(unit_price~date, median, data = subset(data_for_plot_top20, city == "台南市"))
base_number_Tainan_Top20 <- Tainan_median_Top20$unit_price[Tainan_median_Top20$date=="2000 Q1"] 
Tainan_median_Top20$unit_price <- Tainan_median_Top20$unit_price / base_number_Tainan_Top20 
plot_Tainan_Top20 <- ggplot(Tainan_median_Top20,aes(date,unit_price,group=1)) + geom_line() +geom_point()+ theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Tainan)(Top 20%)",x ="Date(yyyy-q)", y = "Median Price Index")

#Plot median of Kaohsiung
Kaohsiung_median_Top20 <- aggregate(unit_price~date, median, data = subset(data_for_plot_top20, city == "高雄市"))
base_number_Kaohsiung_Top20 <- Kaohsiung_median_Top20$unit_price[Kaohsiung_median_Top20$date=="2000 Q1"] 
Kaohsiung_median_Top20$unit_price <- Kaohsiung_median_Top20$unit_price / base_number_Kaohsiung_Top20 
plot_Kaohsiung_Top20 <- ggplot(Kaohsiung_median_Top20,aes(date,unit_price,group=1)) + geom_line() +geom_point()+ theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Kaohsiung)(Top 20%)",x ="Date(yyyy-q)", y = "Median Price Index")

#Plot median of Xinbei
Xinbei_median_Top20 <- aggregate(unit_price~date, median, data = subset(data_for_plot_top20, city == "新北市"))
base_number_Xinbei_Top20 <- Xinbei_median_Top20$unit_price[Xinbei_median_Top20$date=="2000 Q1"] 
Xinbei_median_Top20$unit_price <- Xinbei_median_Top20$unit_price / base_number_Xinbei_Top20 
plot_Xinbei_Top20 <- ggplot(Xinbei_median_Top20,aes(date,unit_price,group=1)) + geom_line() +geom_point()+ theme(axis.text.x = element_text(angle = 90))+labs(title="Median Price Index of Land Unit Price(Xinbei)(Top 20%)",x ="Date(yyyy-q)", y = "Median Price Index")

#save the plots
ggsave("Taiwan_Top20.jpeg",plot_Taiwan,path=file.path(getwd(),"results","Q3"))
ggsave("Taipei_Top20.jpeg",plot_Taipei,path=file.path(getwd(),"results","Q3"))
ggsave("Taoyuan_Top20.jpeg",plot_Taoyuan,path=file.path(getwd(),"results","Q3"))
ggsave("Taichung_Top20.jpeg",plot_Taichung,path=file.path(getwd(),"results","Q3"))
ggsave("Tainan_Top20.jpeg",plot_Tainan,path=file.path(getwd(),"results","Q3"))
ggsave("Kaohsiung_Top20.jpeg",plot_Kaohsiung,path=file.path(getwd(),"results","Q3"))
ggsave("Xinbei_Top20.jpeg",plot_Xinbei,path=file.path(getwd(),"results","Q3"))

############Q4
#Merge two data into one
#rename "transact value","unit_price" and "land_area", as both files have such variables
land_variable_renamed <- land
land_variable_renamed <- rename(land_variable_renamed, c("transact_value" = "land_transact_value", "unit_price" = "land_unit_price","land_area" = "land_land_area","land_use" = "land_land_use"))
building_variable_renamed <- building
building_variable_renamed <- rename(building_variable_renamed, c("transact_value" = "building_transact_value", "unit_price" = "building_unit_price", "land_area" = "building_land_area","land_use"="building_land_use" ))

#merge the two files with the keys:city,township,address,date,roadwidth,streetsection
merged <- merge(land_variable_renamed,building_variable_renamed,by = c("city","township","address","date","rocyear","rocquarter","roadwidth","street_section"),all=TRUE)

#using land-trade exclusive variable "shape" and building-trade exclusive variable "build_mat" to determine land/building/land and bulding
merged$land_or_building <- ifelse(is.na(merged$shape),"building only",
                                  ifelse(is.na(merged$build_mat),"land only","land and building"))
#save the results
#Transfer empty data into NA before saving
merged[merged==""]<-NA
dir.create(file.path(getwd(),"results","Q4"))
write_dta(merged, paste(getwd(),"results","Q4","land_building_merged.dta",sep="/"))


############Q5
#Use general expression to get the year of construction(several type of date format considered)
merged$year_of_building <- ifelse(is.na(merged$construct_date),NA,
                                  ifelse(stri_length(merged$construct_date)==0 |merged$construct_date=="0" ,NA,
                                         ifelse(stri_length(merged$construct_date)==1 ,str_extract(merged$construct_date,"[0-9]+"),
                                                ifelse(stri_length(merged$construct_date)==5, str_extract(merged$construct_date,"[0-9][0-9][0-9]"),
                                                                                                          str_extract(merged$construct_date,"[0-9][0-9]")))))
#Transfer the year of building into numeric data
merged$year_of_building <- as.numeric(merged$year_of_building)
#calculate the age of building, using "rocyear" and "year_of_building"
merged$age_of_building <- merged$rocyear - merged$year_of_building
#transfer property floor into numeric data(set 全 equal to num_of_floor)
merged$property_floor <- ifelse(is.na(merged$property_floor),NA,
                                      ifelse(merged$property_floor=="全",merged$num_floor,merged$property_floor))

#Sort the properties into five categories based on several criteria within the variable "main_use"                                
merged$broad_use_code <- ifelse(is.na(merged$main_use)|merged$main_use=="",NA,
                                      ifelse(grepl("店",merged$main_use)|grepl("辦公",merged$main_use),"commercial (offices/stores)",
                                             ifelse(grepl("廠",merged$main_use)|grepl("倉庫",merged$main_use),"industrial",
                                                    ifelse(grepl("透天",merged$main_use),"single-family housing",
                                                           ifelse(grepl("農舍",merged$main_use),"agricultural",
                                                                  ifelse(grepl("其他",merged$main_use),NA,"apartments"))))))

#combine city and township as region, since some cities have townships with the same name
merged$region <- paste(merged$city,merged$township,sep="")

#save the data
dir.create(file.path(getwd(),"results","Q5"))
write_dta(merged, paste(getwd(),"results","Q5","local_alltrans_clean.dta",sep="/"))

############Q6
#create a new data frame for regression, retrieving the subset of "building only" and "land and building"
merged_for_regression <- subset(merged, land_or_building == "building only" | land_or_building =="land and building")

#change number of floors into factor type(make it a dummy variable)
merged_for_regression$num_floor <- factor(merged_for_regression$num_floor) 
#run regression
attach(merged_for_regression)
fit <- lm(log(building_transact_value)~
            region + 
            date + 
            roadwidth + 
            broad_use_code + 
            num_floor + 
            age_of_building^2 + 
            build_area^2 + 
            building_land_area^2)

#print regression results
options(max.print = 9999)#allow it to print all results
summary(fit)

#save the summary
dir.create(file.path(getwd(),"results","Q6"))
sink(paste(getwd(),"results","Q6","regression result.txt",sep = "/"))
print(summary(fit))
sink()

detach(merged_for_regression)

#retrieve the coefficients of beta for plotting 
coefficient_saved <-data.frame(keyName=names(summary(fit)$coefficients[,1]), coefficeient=summary(fit)$coefficients[,1], SE = summary(fit)$coefficients[,2], row.names=NULL)

#get the coefficient of the year-quarters and extract them from irrelevant texts
hedonic_price_index <- subset(coefficient_saved,grepl("Q",coefficient_saved$keyName))
hedonic_price_index$keyName <- str_extract(as.character(hedonic_price_index$keyName),"[0-9]+ Q[0-9]")

#add the "2000 Q1" in, which was considered "0" under the dummy variable method
hedonic_price_index[nrow(hedonic_price_index) + 1,] = list("2000 Q1","0","0")
#change the "date" into date format
hedonic_price_index$keyName <- as.yearqtr(hedonic_price_index$keyName, format = "%Y Q%q")
#calculate 95% interval and use exp() to transform the data
hedonic_price_index$mean <- exp(as.numeric(hedonic_price_index$coefficeient))
hedonic_price_index$upper <- exp(as.numeric(hedonic_price_index$coefficeient)+2*as.numeric(hedonic_price_index$SE))
hedonic_price_index$lower <- exp(as.numeric(hedonic_price_index$coefficeient)-2*as.numeric(hedonic_price_index$SE))

#plot the data
plot_hedonic <- ggplot(hedonic_price_index,) + geom_line(aes(keyName,mean,color="Mean")) + geom_line(aes(keyName,lower,color="Lower Bound"))+ geom_line(aes(keyName,upper,color="Upper Bound")) + theme(axis.text.x = element_text(angle = 90))+ theme(axis.text.x = element_text(angle = 90))+labs(title="Hedonic Price Index",x ="Date(yyyy-q)", y = "Hedonic Price Index")

#save the plot
ggsave("Hedonic Price Index.jpeg",plot_hedonic,path=file.path(getwd(),"results","Q6"))

###########Q7
#create a new data frame for plotting
plot_by_transact_type <- merged

#define the "unit price" of ecery transaction type
plot_by_transact_type$unit_price_defined <- ifelse(plot_by_transact_type$land_or_building == "land only",plot_by_transact_type$land_unit_price,
                                                   ifelse(plot_by_transact_type$land_or_building == "building only",plot_by_transact_type$building_unit_price, (plot_by_transact_type$building_transact_value+plot_by_transact_type$land_transact_value)/(plot_by_transact_type$land_land_area+plot_by_transact_type$build_area)))
#get rid of the NAs
plot_by_transact_type <- plot_by_transact_type[!is.na(plot_by_transact_type$unit_price_defined),]
#get the median of unit price of every city,date, and transaction type
plot_by_transact_type_cities <- aggregate(plot_by_transact_type["unit_price_defined"],by=plot_by_transact_type[c("date","city","land_or_building")],FUN=median)
#transfer the "date" to date format
plot_by_transact_type_cities$date <- as.yearqtr(plot_by_transact_type_cities$date, format = "%Y Q%q")



#plot the data. However, there is no "land and building" type in the six cities, thus this category won't be shown

#plot Taipei
#retrieve Taipei's data
Taipei_median_transact_type <- subset(plot_by_transact_type_cities, city == "台北市")
#get the base number for normalization
Taipei_median_transact_type$base_number <- ifelse(Taipei_median_transact_type$land_or_building == "land only",Taipei_median_transact_type$unit_price[Taipei_median_transact_type$date=="2000 Q2" & Taipei_median_transact_type$land_or_building=="land only"],
                                                  ifelse(Taipei_median_transact_type$land_or_building == "building only",Taipei_median_transact_type$unit_price[Taipei_median_transact_type$date=="2000 Q1" & Taipei_median_transact_type$land_or_building=="building only"],Taipei_median_transact_type$unit_price[Taipei_median_transact_type$date=="2000 Q1" & Taipei_median_transact_type$land_or_building=="land and building"]))
#divide the unit price by the base number
Taipei_median_transact_type$median_index = Taipei_median_transact_type$unit_price_defined / Taipei_median_transact_type$base_number
#plot
transact_type_Taipei <- ggplot() + geom_line(data = subset(Taipei_median_transact_type, land_or_building == "building only"),aes(date,median_index,group=1,color="building only"))+ geom_line(data = subset(Taipei_median_transact_type, land_or_building == "land only"),aes(date,median_index,group=1,color="land only")) +theme(axis.text.x = element_text(angle = 90))+labs(title="Taipei",x ="Date(yyyy-q)", y = "Median Price Index")

#plot Taichung
#retrieve Taichung's data
Taichung_median_transact_type <- subset(plot_by_transact_type_cities, city == "台中市")
#get the base number for normalization
Taichung_median_transact_type$base_number <- ifelse(Taichung_median_transact_type$land_or_building == "land only",Taichung_median_transact_type$unit_price[Taichung_median_transact_type$date=="2000 Q1" & Taichung_median_transact_type$land_or_building=="land only"],
                                                  ifelse(Taichung_median_transact_type$land_or_building == "building only",Taichung_median_transact_type$unit_price[Taichung_median_transact_type$date=="2000 Q1" & Taichung_median_transact_type$land_or_building=="building only"],Taichung_median_transact_type$unit_price[Taichung_median_transact_type$date=="2000 Q1" & Taichung_median_transact_type$land_or_building=="land and building"]))
#divide the unit price by the base number
Taichung_median_transact_type$median_index = Taichung_median_transact_type$unit_price_defined / Taichung_median_transact_type$base_number
#plot
transact_type_Taichung <- ggplot() + geom_line(data = subset(Taichung_median_transact_type, land_or_building == "building only"),aes(date,median_index,group=1,color="building only"))+ geom_line(data = subset(Taichung_median_transact_type, land_or_building == "land only"),aes(date,median_index,group=1,color="land only")) +theme(axis.text.x = element_text(angle = 90))+labs(title="Taichung",x ="Date(yyyy-q)", y = "Median Price Index")

#plot Taoyuan
#retrieve Taoyuan's data
Taoyuan_median_transact_type <- subset(plot_by_transact_type_cities, city == "桃園市")
#get the base number for normalization
Taoyuan_median_transact_type$base_number <- ifelse(Taoyuan_median_transact_type$land_or_building == "land only",Taoyuan_median_transact_type$unit_price[Taoyuan_median_transact_type$date=="2000 Q1" & Taoyuan_median_transact_type$land_or_building=="land only"],
                                                    ifelse(Taoyuan_median_transact_type$land_or_building == "building only",Taoyuan_median_transact_type$unit_price[Taoyuan_median_transact_type$date=="2000 Q1" & Taoyuan_median_transact_type$land_or_building=="building only"],Taoyuan_median_transact_type$unit_price[Taoyuan_median_transact_type$date=="2000 Q1" & Taoyuan_median_transact_type$land_or_building=="land and building"]))
#divide the unit price by the base number
Taoyuan_median_transact_type$median_index = Taoyuan_median_transact_type$unit_price_defined / Taoyuan_median_transact_type$base_number
#plot
transact_type_Taoyuan <- ggplot() + geom_line(data = subset(Taoyuan_median_transact_type, land_or_building == "building only"),aes(date,median_index,group=1,color="building only"))+ geom_line(data = subset(Taoyuan_median_transact_type, land_or_building == "land only"),aes(date,median_index,group=1,color="land only")) +theme(axis.text.x = element_text(angle = 90))+labs(title="Taoyuan",x ="Date(yyyy-q)", y = "Median Price Index")

#plot Tainan
#retrieve Tainan's data
Tainan_median_transact_type <- subset(plot_by_transact_type_cities, city == "台南市")
#get the base number for normalization
Tainan_median_transact_type$base_number <- ifelse(Tainan_median_transact_type$land_or_building == "land only",Tainan_median_transact_type$unit_price[Tainan_median_transact_type$date=="2000 Q1" & Tainan_median_transact_type$land_or_building=="land only"],
                                                   ifelse(Tainan_median_transact_type$land_or_building == "building only",Tainan_median_transact_type$unit_price[Tainan_median_transact_type$date=="2000 Q1" & Tainan_median_transact_type$land_or_building=="building only"],Tainan_median_transact_type$unit_price[Tainan_median_transact_type$date=="2000 Q1" & Tainan_median_transact_type$land_or_building=="land and building"]))
#divide the unit price by the base number
Tainan_median_transact_type$median_index = Tainan_median_transact_type$unit_price_defined / Tainan_median_transact_type$base_number
#plot
transact_type_Tainan <- ggplot() + geom_line(data = subset(Tainan_median_transact_type, land_or_building == "building only"),aes(date,median_index,group=1,color="building only"))+ geom_line(data = subset(Tainan_median_transact_type, land_or_building == "land only"),aes(date,median_index,group=1,color="land only")) +theme(axis.text.x = element_text(angle = 90))+labs(title="Tainan",x ="Date(yyyy-q)", y = "Median Price Index")


#plot Kaohsiung
#retrieve Kaohsiung's data
Kaohsiung_median_transact_type <- subset(plot_by_transact_type_cities, city == "高雄市")
#get the base number for normalization
Kaohsiung_median_transact_type$base_number <- ifelse(Kaohsiung_median_transact_type$land_or_building == "land only",Kaohsiung_median_transact_type$unit_price[Kaohsiung_median_transact_type$date=="2000 Q1" & Kaohsiung_median_transact_type$land_or_building=="land only"],
                                                     ifelse(Kaohsiung_median_transact_type$land_or_building == "building only",Kaohsiung_median_transact_type$unit_price[Kaohsiung_median_transact_type$date=="2000 Q1" & Kaohsiung_median_transact_type$land_or_building=="building only"],Kaohsiung_median_transact_type$unit_price[Kaohsiung_median_transact_type$date=="2000 Q1" & Kaohsiung_median_transact_type$land_or_building=="land and building"]))
#divide the unit price by the base number
Kaohsiung_median_transact_type$median_index = Kaohsiung_median_transact_type$unit_price_defined / Kaohsiung_median_transact_type$base_number
#plot
transact_type_Kaohsiung <- ggplot() + geom_line(data = subset(Kaohsiung_median_transact_type, land_or_building == "building only"),aes(date,median_index,group=1,color="building only"))+ geom_line(data = subset(Kaohsiung_median_transact_type, land_or_building == "land only"),aes(date,median_index,group=1,color="land only")) +theme(axis.text.x = element_text(angle = 90))+labs(title="Kaohsiung",x ="Date(yyyy-q)", y = "Median Price Index")

#plot Xinbei
#retrieve Xinbei's data
Xinbei_median_transact_type <- subset(plot_by_transact_type_cities, city == "新北市")
#get the base number for normalization
Xinbei_median_transact_type$base_number <- ifelse(Xinbei_median_transact_type$land_or_building == "land only",Xinbei_median_transact_type$unit_price[Xinbei_median_transact_type$date=="2000 Q1" & Xinbei_median_transact_type$land_or_building=="land only"],
                                                     ifelse(Xinbei_median_transact_type$land_or_building == "building only",Xinbei_median_transact_type$unit_price[Xinbei_median_transact_type$date=="2000 Q1" & Xinbei_median_transact_type$land_or_building=="building only"],Xinbei_median_transact_type$unit_price[Xinbei_median_transact_type$date=="2000 Q1" & Xinbei_median_transact_type$land_or_building=="land and building"]))
#divide the unit price by the base number
Xinbei_median_transact_type$median_index = Xinbei_median_transact_type$unit_price_defined / Xinbei_median_transact_type$base_number
#plot
transact_type_Xinbei <- ggplot() + geom_line(data = subset(Xinbei_median_transact_type, land_or_building == "building only"),aes(date,median_index,group=1,color="building only"))+ geom_line(data = subset(Xinbei_median_transact_type, land_or_building == "land only"),aes(date,median_index,group=1,color="land only")) +theme(axis.text.x = element_text(angle = 90))+labs(title="Xinbei",x ="Date(yyyy-q)", y = "Median Price Index")

#combine the six plots into 3*2 graph
figure <- ggarrange(transact_type_Taipei, transact_type_Taoyuan, transact_type_Taichung, transact_type_Kaohsiung, transact_type_Tainan,transact_type_Xinbei,ncol = 2, nrow = 3)

#save the six plots and the combined graph

dir.create(file.path(getwd(),"results","Q7"))
ggsave("Taipei Median Price Index by Transaction Type.jpeg",transact_type_Taipei,path=file.path(getwd(),"results","Q7"))
ggsave("Taichung Median Price Index by Transaction Type.jpeg",transact_type_Taichung,path=file.path(getwd(),"results","Q7"))
ggsave("Taoyuan Median Price Index by Transaction Type.jpeg",transact_type_Taoyuan,path=file.path(getwd(),"results","Q7"))
ggsave("Tainan Median Price Index by Transaction Type.jpeg",transact_type_Tainan,path=file.path(getwd(),"results","Q7"))
ggsave("Kaohsiung Median Price Index by Transaction Type.jpeg",transact_type_Kaohsiung,path=file.path(getwd(),"results","Q7"))
ggsave("Xinbei Median Price Index by Transaction Type.jpeg",transact_type_Xinbei,path=file.path(getwd(),"results","Q7"))
ggsave("Median Price Index by Transaction Type_Cities Combined.jpeg",figure,path=file.path(getwd(),"results","Q7"))


#
growth_rate <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(growth_rate) <- c("city","transaction_type","2000 Q1","2007 Q4","2008 Q1","2010 Q4", "2011 Q1","2012 Q3","2000-2007(%)","2008-2010(%)","2011-2012(%)")

#Retrieve the growthrate data for building only(retrieve it city by city from different data frame)
growth_rate[nrow(growth_rate) + 1,] = list("Xinbei City","building only",
                                           Xinbei_median_transact_type$unit_price_defined[Xinbei_median_transact_type$date=="2000 Q1" & Xinbei_median_transact_type$land_or_building=="building only"],
                                           Xinbei_median_transact_type$unit_price_defined[Xinbei_median_transact_type$date=="2007 Q4" & Xinbei_median_transact_type$land_or_building=="building only"],
                                           Xinbei_median_transact_type$unit_price_defined[Xinbei_median_transact_type$date=="2008 Q1" & Xinbei_median_transact_type$land_or_building=="building only"],
                                           Xinbei_median_transact_type$unit_price_defined[Xinbei_median_transact_type$date=="2010 Q4" & Xinbei_median_transact_type$land_or_building=="building only"],
                                           Xinbei_median_transact_type$unit_price_defined[Xinbei_median_transact_type$date=="2011 Q1" & Xinbei_median_transact_type$land_or_building=="building only"],
                                           Xinbei_median_transact_type$unit_price_defined[Xinbei_median_transact_type$date=="2012 Q3" & Xinbei_median_transact_type$land_or_building=="building only"],
                                           NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Taipei City","building only",
                                           Taipei_median_transact_type$unit_price_defined[Taipei_median_transact_type$date=="2000 Q1" & Taipei_median_transact_type$land_or_building=="building only"],
                                           Taipei_median_transact_type$unit_price_defined[Taipei_median_transact_type$date=="2007 Q4" & Taipei_median_transact_type$land_or_building=="building only"],
                                           Taipei_median_transact_type$unit_price_defined[Taipei_median_transact_type$date=="2008 Q1" & Taipei_median_transact_type$land_or_building=="building only"],
                                           Taipei_median_transact_type$unit_price_defined[Taipei_median_transact_type$date=="2010 Q4" & Taipei_median_transact_type$land_or_building=="building only"],
                                           Taipei_median_transact_type$unit_price_defined[Taipei_median_transact_type$date=="2011 Q1" & Taipei_median_transact_type$land_or_building=="building only"],
                                           Taipei_median_transact_type$unit_price_defined[Taipei_median_transact_type$date=="2012 Q3" & Taipei_median_transact_type$land_or_building=="building only"],
                                           NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Taichung City","building only",
                                           Taichung_median_transact_type$unit_price_defined[Taichung_median_transact_type$date=="2000 Q1" & Taichung_median_transact_type$land_or_building=="building only"],
                                           Taichung_median_transact_type$unit_price_defined[Taichung_median_transact_type$date=="2007 Q4" & Taichung_median_transact_type$land_or_building=="building only"],
                                           Taichung_median_transact_type$unit_price_defined[Taichung_median_transact_type$date=="2008 Q1" & Taichung_median_transact_type$land_or_building=="building only"],
                                           Taichung_median_transact_type$unit_price_defined[Taichung_median_transact_type$date=="2010 Q4" & Taichung_median_transact_type$land_or_building=="building only"],
                                           Taichung_median_transact_type$unit_price_defined[Taichung_median_transact_type$date=="2011 Q1" & Taichung_median_transact_type$land_or_building=="building only"],
                                           Taichung_median_transact_type$unit_price_defined[Taichung_median_transact_type$date=="2012 Q3" & Taichung_median_transact_type$land_or_building=="building only"],
                                           NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Kaohsiung City","building only",
                                           Kaohsiung_median_transact_type$unit_price_defined[Kaohsiung_median_transact_type$date=="2000 Q1" & Kaohsiung_median_transact_type$land_or_building=="building only"],
                                           Kaohsiung_median_transact_type$unit_price_defined[Kaohsiung_median_transact_type$date=="2007 Q4" & Kaohsiung_median_transact_type$land_or_building=="building only"],
                                           Kaohsiung_median_transact_type$unit_price_defined[Kaohsiung_median_transact_type$date=="2008 Q1" & Kaohsiung_median_transact_type$land_or_building=="building only"],
                                           Kaohsiung_median_transact_type$unit_price_defined[Kaohsiung_median_transact_type$date=="2010 Q4" & Kaohsiung_median_transact_type$land_or_building=="building only"],
                                           Kaohsiung_median_transact_type$unit_price_defined[Kaohsiung_median_transact_type$date=="2011 Q1" & Kaohsiung_median_transact_type$land_or_building=="building only"],
                                           Kaohsiung_median_transact_type$unit_price_defined[Kaohsiung_median_transact_type$date=="2012 Q3" & Kaohsiung_median_transact_type$land_or_building=="building only"],
                                           NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Tainan","building only",
                                           Tainan_median_transact_type$unit_price_defined[Tainan_median_transact_type$date=="2000 Q1" & Tainan_median_transact_type$land_or_building=="building only"],
                                           Tainan_median_transact_type$unit_price_defined[Tainan_median_transact_type$date=="2007 Q4" & Tainan_median_transact_type$land_or_building=="building only"],
                                           Tainan_median_transact_type$unit_price_defined[Tainan_median_transact_type$date=="2008 Q1" & Tainan_median_transact_type$land_or_building=="building only"],
                                           Tainan_median_transact_type$unit_price_defined[Tainan_median_transact_type$date=="2010 Q4" & Tainan_median_transact_type$land_or_building=="building only"],
                                           Tainan_median_transact_type$unit_price_defined[Tainan_median_transact_type$date=="2011 Q1" & Tainan_median_transact_type$land_or_building=="building only"],
                                           Tainan_median_transact_type$unit_price_defined[Tainan_median_transact_type$date=="2012 Q3" & Tainan_median_transact_type$land_or_building=="building only"],
                                           NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Taoyuan","building only",
                                           Taoyuan_median_transact_type$unit_price_defined[Taoyuan_median_transact_type$date=="2000 Q1" & Taoyuan_median_transact_type$land_or_building=="building only"],
                                           Taoyuan_median_transact_type$unit_price_defined[Taoyuan_median_transact_type$date=="2007 Q4" & Taoyuan_median_transact_type$land_or_building=="building only"],
                                           Taoyuan_median_transact_type$unit_price_defined[Taoyuan_median_transact_type$date=="2008 Q1" & Taoyuan_median_transact_type$land_or_building=="building only"],
                                           Taoyuan_median_transact_type$unit_price_defined[Taoyuan_median_transact_type$date=="2010 Q4" & Taoyuan_median_transact_type$land_or_building=="building only"],
                                           Taoyuan_median_transact_type$unit_price_defined[Taoyuan_median_transact_type$date=="2011 Q1" & Taoyuan_median_transact_type$land_or_building=="building only"],
                                           Taoyuan_median_transact_type$unit_price_defined[Taoyuan_median_transact_type$date=="2012 Q3" & Taoyuan_median_transact_type$land_or_building=="building only"],
                                           NA,NA,NA)

#Retrieve the growthrate data for land only(retrieve it city by city from different data frame)
growth_rate[nrow(growth_rate) + 1,] = list("Xinbei City","land only",
                                           Xinbei_median_transact_type$unit_price_defined[Xinbei_median_transact_type$date=="2000 Q1" & Xinbei_median_transact_type$land_or_building=="land only"],
                                           Xinbei_median_transact_type$unit_price_defined[Xinbei_median_transact_type$date=="2007 Q4" & Xinbei_median_transact_type$land_or_building=="land only"],
                                           Xinbei_median_transact_type$unit_price_defined[Xinbei_median_transact_type$date=="2008 Q1" & Xinbei_median_transact_type$land_or_building=="land only"],
                                           Xinbei_median_transact_type$unit_price_defined[Xinbei_median_transact_type$date=="2010 Q4" & Xinbei_median_transact_type$land_or_building=="land only"],
                                           Xinbei_median_transact_type$unit_price_defined[Xinbei_median_transact_type$date=="2011 Q1" & Xinbei_median_transact_type$land_or_building=="land only"],
                                           Xinbei_median_transact_type$unit_price_defined[Xinbei_median_transact_type$date=="2012 Q3" & Xinbei_median_transact_type$land_or_building=="land only"],
                                           NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Taipei","land only",
                                           Taipei_median_transact_type$unit_price_defined[Taipei_median_transact_type$date=="2000 Q2" & Taipei_median_transact_type$land_or_building=="land only"],
                                           Taipei_median_transact_type$unit_price_defined[Taipei_median_transact_type$date=="2008 Q3" & Taipei_median_transact_type$land_or_building=="land only"],
                                           NA,NA,NA,NA,NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Taichung","land only",
                                           Taichung_median_transact_type$unit_price_defined[Taichung_median_transact_type$date=="2000 Q1" & Taichung_median_transact_type$land_or_building=="land only"],
                                           Taichung_median_transact_type$unit_price_defined[Taichung_median_transact_type$date=="2007 Q4" & Taichung_median_transact_type$land_or_building=="land only"],
                                           Taichung_median_transact_type$unit_price_defined[Taichung_median_transact_type$date=="2008 Q1" & Taichung_median_transact_type$land_or_building=="land only"],
                                           Taichung_median_transact_type$unit_price_defined[Taichung_median_transact_type$date=="2010 Q4" & Taichung_median_transact_type$land_or_building=="land only"],
                                           Taichung_median_transact_type$unit_price_defined[Taichung_median_transact_type$date=="2011 Q1" & Taichung_median_transact_type$land_or_building=="land only"],
                                           Taichung_median_transact_type$unit_price_defined[Taichung_median_transact_type$date=="2012 Q3" & Taichung_median_transact_type$land_or_building=="land only"],
                                           NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Kaohsiung","land only",
                                           Kaohsiung_median_transact_type$unit_price_defined[Kaohsiung_median_transact_type$date=="2000 Q1" & Kaohsiung_median_transact_type$land_or_building=="land only"],
                                           Kaohsiung_median_transact_type$unit_price_defined[Kaohsiung_median_transact_type$date=="2007 Q4" & Kaohsiung_median_transact_type$land_or_building=="land only"],
                                           Kaohsiung_median_transact_type$unit_price_defined[Kaohsiung_median_transact_type$date=="2008 Q1" & Kaohsiung_median_transact_type$land_or_building=="land only"],
                                           Kaohsiung_median_transact_type$unit_price_defined[Kaohsiung_median_transact_type$date=="2010 Q4" & Kaohsiung_median_transact_type$land_or_building=="land only"],
                                           Kaohsiung_median_transact_type$unit_price_defined[Kaohsiung_median_transact_type$date=="2011 Q1" & Kaohsiung_median_transact_type$land_or_building=="land only"],
                                           Kaohsiung_median_transact_type$unit_price_defined[Kaohsiung_median_transact_type$date=="2012 Q3" & Kaohsiung_median_transact_type$land_or_building=="land only"],
                                           NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Tainan","land only",
                                           Tainan_median_transact_type$unit_price_defined[Tainan_median_transact_type$date=="2000 Q1" & Tainan_median_transact_type$land_or_building=="land only"],
                                           Tainan_median_transact_type$unit_price_defined[Tainan_median_transact_type$date=="2007 Q4" & Tainan_median_transact_type$land_or_building=="land only"],
                                           Tainan_median_transact_type$unit_price_defined[Tainan_median_transact_type$date=="2008 Q1" & Tainan_median_transact_type$land_or_building=="land only"],
                                           Tainan_median_transact_type$unit_price_defined[Tainan_median_transact_type$date=="2010 Q4" & Tainan_median_transact_type$land_or_building=="land only"],
                                           Tainan_median_transact_type$unit_price_defined[Tainan_median_transact_type$date=="2011 Q1" & Tainan_median_transact_type$land_or_building=="land only"],
                                           Tainan_median_transact_type$unit_price_defined[Tainan_median_transact_type$date=="2012 Q3" & Tainan_median_transact_type$land_or_building=="land only"],
                                           NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Taoyuan","land only",
                                           Taoyuan_median_transact_type$unit_price_defined[Taoyuan_median_transact_type$date=="2000 Q1" & Taoyuan_median_transact_type$land_or_building=="land only"],
                                           Taoyuan_median_transact_type$unit_price_defined[Taoyuan_median_transact_type$date=="2007 Q4" & Taoyuan_median_transact_type$land_or_building=="land only"],
                                           Taoyuan_median_transact_type$unit_price_defined[Taoyuan_median_transact_type$date=="2008 Q1" & Taoyuan_median_transact_type$land_or_building=="land only"],
                                           Taoyuan_median_transact_type$unit_price_defined[Taoyuan_median_transact_type$date=="2010 Q4" & Taoyuan_median_transact_type$land_or_building=="land only"],
                                           Taoyuan_median_transact_type$unit_price_defined[Taoyuan_median_transact_type$date=="2011 Q1" & Taoyuan_median_transact_type$land_or_building=="land only"],
                                           Taoyuan_median_transact_type$unit_price_defined[Taoyuan_median_transact_type$date=="2012 Q3" & Taoyuan_median_transact_type$land_or_building=="land only"],
                                           NA,NA,NA)

#Retrieve the growth rate data for land and building(set NA for all)
growth_rate[nrow(growth_rate) + 1,] = list("Taoyuan","land and building",NA,NA,NA,NA,NA,NA,NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Taipei","land and building",NA,NA,NA,NA,NA,NA,NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Xinbei","land and building",NA,NA,NA,NA,NA,NA,NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Kaohsiung","land and building",NA,NA,NA,NA,NA,NA,NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Taichung","land and building",NA,NA,NA,NA,NA,NA,NA,NA,NA)
growth_rate[nrow(growth_rate) + 1,] = list("Tainan","land and building",NA,NA,NA,NA,NA,NA,NA,NA,NA)

#compute the growth rate with the data retrieved
growth_rate$`2000-2007(%)` <- (growth_rate$`2007 Q4`-growth_rate$`2000 Q1`)/growth_rate$`2000 Q1`
growth_rate$`2008-2010(%)` <- (growth_rate$`2008 Q1`-growth_rate$`2010 Q4`)/growth_rate$`2008 Q1`
growth_rate$`2011-2012(%)` <- (growth_rate$`2011 Q1`-growth_rate$`2012 Q3`)/growth_rate$`2011 Q1`

#delete the data columns, leaving just the growth rate
growth_rate$`2000 Q1` <- NULL
growth_rate$`2007 Q4`<- NULL
growth_rate$`2008 Q1` <- NULL
growth_rate$`2010 Q4`<- NULL
growth_rate$`2011 Q1` <- NULL
growth_rate$`2012 Q3`<- NULL

#sort the data frame by transaction type and city name
growth_rate <- growth_rate[order(growth_rate$transaction_type,growth_rate$city),]
#save the dataframe as .png
png(paste(getwd(),"results","Q7","growthrate.png",sep="/"), height = 25*nrow(growth_rate), width = 100*ncol(growth_rate))
grid.table(growth_rate)
dev.off()

