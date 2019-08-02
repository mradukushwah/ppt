#Importing data
setwd(dir = "C:/Users/mradu.kushwah/Documents/data")
sales_data<-read.csv("sales_1.csv")

#----------------EXPLORATORY DATA ANALYSIS------------------------------------------------------------

#First look of the data
head(sales_data)
sales_data[!complete.cases(sales_data),]
str(sales_data)

#install.packages("plyr")
library(plyr)
plyr::count(sales_data,vars="product_category")
plyr::count(sales_data,vars="sub_product_category")
plyr::count(sales_data,vars="sales_group_desc")
plyr::count(sales_data,vars="customer_group_desc")


#Checking for percent of revenue with missing values of each groups
round(sum(sales_data[sales_data$product_category=='N',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$sub_product_category=='N',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$sales_group_desc=='N',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$customer_group_desc=='N',]$revenue)/sum(sales_data$revenue)*100,3)

#Percentage Revenue from each product Category
round(sum(sales_data[sales_data$product_category=='1 Ink',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$product_category=='2 Core',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$product_category=='3 Toner',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$product_category=='4 Other',]$revenue)/sum(sales_data$revenue)*100,3)

#Percentage Revenue from each sales group
round(sum(sales_data[sales_data$sales_group_desc=='Direct Sales',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$sales_group_desc=='Inside Sales',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$sales_group_desc=='Web',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$sales_group_desc=='ISA',]$revenue)/sum(sales_data$revenue)*100,5)
round(sum(sales_data[sales_data$sales_group_desc=='Partners',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$sales_group_desc=='SSI',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$sales_group_desc=='Supply',]$revenue)/sum(sales_data$revenue)*100,3)


round(sum(sales_data[sales_data$customer_group_desc=='Commercial',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$customer_group_desc=='Dealers',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$customer_group_desc=='Federal Government',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$customer_group_desc=='Independent Dealers',]$revenue)/sum(sales_data$revenue)*100,5)
round(sum(sales_data[sales_data$customer_group_desc=='Local Government',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$customer_group_desc=='Non Profits',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$customer_group_desc=='State Government',]$revenue)/sum(sales_data$revenue)*100,3)
round(sum(sales_data[sales_data$customer_group_desc=='Strategic',]$revenue)/sum(sales_data$revenue)*100,3)


#removing the N values
sales_data<-sales_data[which(sales_data$product_category!='N'),]
sales_data<-sales_data[which(sales_data$sales_group_desc!='N'),]
sales_data<-sales_data[which(sales_data$customer_group_desc!='N'),]

sales_data<-droplevels(sales_data)
rownames(sales_data)<-NULL
#Number of unique customers

library(dplyr)
nrow(distinct(sales_data,sales_data$ship_to1))
#number of distinct bills
nrow(distinct(sales_data,sales_data$sales_document1))
sum(sales_data$revenue)
a#Creating a column for month and year
library(lubridate)
sales_data$month<-month(ymd(sales_data$order_creation_date))
sales_data$year<-year(ymd(sales_data$order_creation_date))
sales_data$quarter<-quarters(ymd(sales_data$order_creation_date))
sales_data<-sales_data[order(sales_data$year,sales_data$quarter,sales_data$month),]
sales_data$year<-as.factor(sales_data$year)
sales_data$month<-as.factor(sales_data$month)
sales_data$quarter<-as.factor(sales_data$quarter)

#Visualisation

library(ggplot2)

#Subsetting the data with equal months so that we can compare the revenues

g<-ggplot(sales_data,aes(y=revenue/10000,x=year,fill=year))
g+geom_col()+theme_light()
d<-subset(sales_data,month==4|month==5|month==6 |month==7)
#Product category

g<-ggplot(d,aes(y=revenue/10000,x=product_category,fill=product_category))
g+geom_col()+labs(title ="Product category vs Revenue",
                  x ="Product Category",
                  y ="Revenue('0000s)")+theme_light()

#Individual Graphs for product category
pgroup<-c("1 Ink","2 Core","3 Toner","4 Other")

yearly_graph<-function(df,a){
  d<-subset(df,product_category==a)
  d<-droplevels(d)
  rownames(d)<-NULL
  g<-ggplot(d,aes(y=revenue/10000,x=year,fill=year))
  g+geom_col()+labs(title=paste("Year vs Revenue for",a),x ="Year",y ="Revenue('0000s)")+theme_light()
}
yearly_graph(d,pgroup[1])
yearly_graph(d,pgroup[2])
yearly_graph(d,pgroup[3])
yearly_graph(d,pgroup[4])


#Sales_group_desc
library(ggplot2)
g<-ggplot(d,aes(y=revenue/10000,x=sales_group_desc,fill=sales_group_desc))
g+geom_col()+labs(title="Sales Group Vs Revenue",x="Sales Group",y="Revenue '0000s")+theme_light()

#Individual Graphs for sales group category
sgroup<-c("Inside Sales","Supply","Web")
yearly_graph<-function(df,a){
   d<-subset(df,sales_group_desc==a)
            d<-droplevels(d)
            rownames(d)<-NULL
            g<-ggplot(d,aes(y=revenue/10000,x=year,fill=year))
            g+geom_col()+labs(title=paste("Year vs Revenue for",a),x ="Year",y ="Revenue('0000s)")+theme_light()
          }
    yearly_graph(d,sgroup[1])
    yearly_graph(d,sgroup[2])
    yearly_graph(d,sgroup[3])
          
          
    #customer_group_desc
     library(ggplot2)
     g<-ggplot(d,aes(y=revenue/10000,x=customer_group_desc,fill=customer_group_desc))
     g+geom_col()+labs(title="Customer Group Vs Revenue",x="Customer Group",y="Revenue '0000s")+theme_light()
     
     #Individual Graphs for customer group category
          cgroup<-c("Commercial","Strategic")
          yearly_graph<-function(df,a){
            d<-subset(df,customer_group_desc==a)
            d<-droplevels(d)
            rownames(d)<-NULL
            g<-ggplot(d,aes(y=revenue/10000,x=year,fill=year))
            g+geom_col()+labs(title=paste("Year vs Revenue for",a),x ="Year",y ="Revenue('0000s)")+theme_light()
          }
      yearly_graph(d,cgroup[1])
      yearly_graph(d,cgroup[2])
          

#Total revenue comparison for each month of year
g<-ggplot(sales_data,aes(y=revenue/10000,x=month,fill=month))
g+geom_col()+facet_wrap(vars(year))+labs(title=paste("Monthly Revenue"))
+labs(x ="Year", y ="Revenue('0000s)")+theme_light()


#-------------------------TIME SERIES ANALYSIS-------------------------------------------------------------------------------------

#Splitting the data into test and train for validation that is there really a dip in the revenue or is it just a monthly effect
sales_train<-subset(sales_data,year!="2019")
sales_test<-subset(sales_data,year=="2019")

ts_train<-data.frame(month=c(),revenue=c())
for (i in 2016:2018){
  
  df<-sales_train[sales_train$year==i,]
  df<-aggregate(df$revenue,by=list(df$month),FUN=sum)
  colnames(df)<-c("month","revenue")
  
  ts_train<-rbind(ts_train,df)
  
}

myts<- ts(ts_train$revenue, start=c(2016, 4), end=c(2018, 12), frequency=12)
plot(myts)
fit1 <- HoltWinters(myts)
require(forecast)
print(accuracy(fit1$fitted, myts))
plot(forecast(fit1, 7))

#Applying time series to full data 

par(mfrow=c(1,2))

#Function to convert the data to a aggregated data based on sum(monthly)
time_series_month <- function(d) {
  sales_final<-data.frame(month=c(),revenue=c(),year=c())
  for (i in 2016:2019){
    
    df<-d[d$year==i,]
    df<-aggregate(df$revenue,by=list(df$month),FUN=sum)
    df$year<-paste(i)
    colnames(df)<-c("month","revenue","year")
    sales_final<-rbind(sales_final,df)
    
  }
  return(sales_final)
}
#Function to deploy all the time series evaluations
mytime_month <- function(d) {
  myts<- ts(d$revenue, start=c(2016, 4), end=c(2019, 7), frequency=12)
  plot(myts)
  fit1 <- HoltWinters(myts)
  plot(decompose(myts,type=c("multiplicative")))
  print(accuracy(fit1$fitted, myts))
  plot(forecast(fit1, 17))
  print(forecast(fit1,17))
  a<-forecast(fit1,17)
  x<-deparse(substitute(d))
  write.csv(a$mean,paste(x,".csv"))
  
 
}




ts_salesdata<-time_series_month(sales_data)
mytime_month(ts_salesdata)


#Making a time series model for each product category
#subsetting the data with each product category

sales_ink<-subset(sales_data,product_category=='1 Ink')
sales_core<-subset(sales_data,product_category=='2 Core')
sales_toner<-subset(sales_data,product_category=='3 Toner')
sales_other<-subset(sales_data,product_category=='4 Other')

#Function Calls
ts_ink<-time_series_month(sales_ink)
mytime_month(ts_ink)
ts_core<-time_series_month(sales_core)
mytime_month(ts_core)
ts_toner<-time_series_month(sales_toner)  
mytime_month(ts_toner)


#Sales group
sales_direct<-subset(sales_data,sales_group_desc=="Direct Sales")
sales_inside<-subset(sales_data,sales_group_desc=='Inside Sales')
sales_web<-subset(sales_data,sales_group_desc=='Web')
sales_partners<-subset(sales_data,sales_group_desc=='Partners')

#Function Calls
ts_inside<-time_series_month(sales_inside)
mytime_month(ts_inside)
ts_web<-time_series_month(sales_web)  
mytime_month(ts_web)


#------------------------------------FINAL RESULTS---------------------------------------
#Plotting the results
#Total SALES
sales<-read.csv("ts_salesdata .csv",header=TRUE)
colnames(sales)<-c("month","revenue")
sales$year<-"2019"
sales$year[6:17]<-"2020"
ts_salesdata$year<-as.factor(ts_salesdata$year)
sales$month<-as.factor(sales$month)
sales$year<-as.factor(sales$year)
ts_salesdata<-rbind(ts_salesdata,sales)
ggplot(ts_salesdata,aes(x=year,y=revenue/10000,fill=year))+geom_col()+
  labs(title="Forecasted Total Revenue ",x="Year",y="Revenue '000s")+theme_light()
ts_salesdata 
sales

#INK
ink<-read.csv("ts_ink .csv",header=TRUE)
colnames(ink)<-c("month","revenue")
ink$year<-"2019"
ink$year[6:17]<-"2020"
ts_ink$year<-as.factor(ts_ink$year)
ink$month<-as.factor(ink$month)
ink$year<-as.factor(ink$year)
ts_ink<-rbind(ts_ink,ink)
ts_ink$month[]
ggplot(ts_ink,aes(x=year,y=revenue/10000,fill=year))+geom_col()+labs(title="Forecasted revenue from Ink",
                                                                     x="Year",y="Revenue '0000s")+theme_light()
#CORE
core<-read.csv("ts_core .csv",header=TRUE)
colnames(core)<-c("month","revenue")
core$year<-"2019"
core$year[6:17]<-"2020"
ts_core$year<-as.factor(ts_core$year)
core$month<-as.factor(core$month)
core$year<-as.factor(core$year)
ts_core<-rbind(ts_core,core)
ggplot(ts_core,aes(x=year,y=revenue/10000,fill=year))+geom_col()+labs(title="Forecasted revenue from Core",
                                                                     x="Year",y="Revenue '0000s")+theme_light()
#Toner
toner<-read.csv("ts_toner .csv",header=TRUE)
colnames(toner)<-c("month","revenue")
toner$year<-"2019"
toner$year[6:17]<-"2020"
ts_toner$year<-as.factor(ts_toner$year)
toner$month<-as.factor(toner$month)
toner$year<-as.factor(toner$year)
ts_toner<-rbind(ts_toner,toner)
ggplot(ts_toner,aes(x=year,y=revenue/10000,fill=year))+geom_col()+labs(title="Forecasted revenue for Toner",
                                                                      x="Year",y="Revenue '0000s")+theme_light()
#Inside Sales
inside<-read.csv("ts_inside .csv",header=TRUE)
colnames(inside)<-c("month","revenue")
inside$year<-"2019"
inside$year[6:17]<-"2020"
ts_inside$year<-as.factor(ts_inside$year)
inside$month<-as.factor(inside$month)
inside$year<-as.factor(inside$year)
ts_inside<-rbind(ts_inside,inside)
ggplot(ts_inside,aes(x=year,y=revenue/10000,fill=year))+geom_col()+labs(title="Forecasted revenue for Inside Sales",x="Year",y="Revenue '0000s")+theme_light()
  
#WEB SALES
web<-read.csv("ts_web .csv",header=TRUE)
colnames(web)<-c("month","revenue")
web$year<-"2019"
web$year[6:17]<-"2020"
ts_web$year<-as.factor(ts_web$year)
web$month<-as.factor(web$month)
web$year<-as.factor(web$year)
ts_web<-rbind(ts_web,web)
ggplot(ts_web,aes(x=year,y=revenue/10000,fill=year))+geom_col()
+labs(title="Forecasted revenue for Web Sales",x="Year",y="Revenue '0000s")+theme_light()
                                                                     
