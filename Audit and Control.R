sales <- read.csv("sales.csv" , header = T)
inventory<-read.csv("inventory.csv" , header=T)
credit<-read.csv("credit.csv" , header=T)
collections<-read.csv("collections.csv" , header=T)
sales2016<-sales[format.Date(sales$date, "%Y")=="2016",]
collections2016 <- collections[format.Date(collections$date, "%Y")=="2016",]

# 1.a. Sales revenue
sum(sales2016$total)

#1.b. Cost of goods sold
CGS<-merge(sales2016,inventory, by="sku")
sum(CGS$qty * CGS$unitcost)

#1.c. Unpaid accounts receivable
AC<-merge(sales2016,collections2016,by="invoice",all.x=TRUE)
AC<-na.omit(AC)
accounts<-sum(AC$total)-sum(AC$collected)


#1.d. allowance for doubtful AC
ADA<- merge(sales2016,collections2016, by="invoice")
ADA$diff_in_days<- difftime(ADA$date.y ,ADA$date.x , units = c("days"))
ADA$over180days=0
for(i in 1:length(ADA$over180days)){
  if(ADA$diff_in_days[i] > 180.0000){
    ADA$over180days[i] = ADA$qty[i] * ADA$unitprice[i]
  }
}
ADA$over180days

duration > 180 

# 1.e. inventory
sum(inventory$unitcost*inventory$endstock)



#3. Markup-Purchases
PI<-merge(purchases,inventory,by="sku")
MP_Purchases<-(((PI$unitprice)/(PI$unitcost.x))-1)*100
summary(MP_Purchases)

#3. Markup-Sales
CG<-merge(sales,inventory, by="sku")
MP_Sales<-(((CG$unitprice.x)/(CG$unitcost))-1)*100
summary(MP_Sales)

#3. Markup-Collections
MPC<-merge(collections,sales,by="invoice")
MP<-merge(MPC,inventory,by="sku")
MP_Collection<-(((MP$collected)/(MP$unitcost))-1)*100
summary(MP_Collection)

#3.( ii ) daily avg
salesagg <- aggregate(total~date,sales,sum)
summary(salesagg$total)

purchases$purTot<-purchases$quantity*purchases$unitcost
salespurchasesagg <- aggregate(purTot~date,purchases,sum)
summary(salespurchasesagg$purTot)

collectionsagg <- aggregate(collected~date,collections,sum)
summary(collectionsagg$collected)

#3.(iii) Range
summary(as_date(sales$date))
summary(as_date(collections$date))
summary(as_date(purchases$date))

# 4.a duplicates
duplicated(sales2016$invoice)
sales2016$invoice[duplicated(sales2016$invoice)]

# 4.b. Omitted values
fulllist<-(sales$invoice)
datalist<-(seq(from = 1, to = 1300000, by=1))
setdiff(fulllist,datalist)

#5.b.Markup_Inventory
MP_Inventory<-(((inventory$unitprice)/(inventory$unitcost))-1)
MP_Inventory<-na.omit(MP_Inventory)
mean(MP_Inventory)
summary(MP_Inventory)

#6. 
allowance = AC
allowance$a<-(allowance$total-allowance$collected)
allowance$due<-as.Date(as.character("2016-12-31"),format="%Y-%m-%d")-as.Date(as.character(allowance$date.x,format="%Y-%m-%d"))
allowanceClean<-allowance[allowance$a>0,]
View(allowanceClean)

allowanceClean$amount = 0
for(i in 1:nrow(allowanceClean))
{
  if(allowanceClean$due[i]>180)
  {
    allowanceClean$amount[i]=0.4*allowanceClean$a[i]
  }
}


#7.
purchasesort<-purchases[with(purchases, order(sku, date)),]
purchasesort$month<-month(ymd(purchasesort$date))
inv_pur<-merge(purchasesort,inventory, by="sku")
inv_pur$adjinitinv = 0
for(i in 1:nrow(inv_pur))
{
  if(inv_pur$month[i]==1)
  {
    inv_pur$adjinitinv[i]=inv_pur$quantity[i]+inv_pur$beginstock[i]
  }
  else
  {inv_pur$adjinitinv[i]=inv_pur$quantity[i]}
}
inv_pur$cquantity <- ave(inv_pur$adjinitinv, inv_pur$sku, FUN=cumsum)
sales2016test<-sales2016
sales2016test$month<-month(ymd(sales2016$date))
sales2016sort <- aggregate(qty~sku+month,sales2016test,sum)
sales2016sort<-sales2016sort[with(sales2016sort, order(sku, month)),]
sales2016sort$cqty <- ave(sales2016sort$qty, sales2016sort$sku, FUN=cumsum)
stockout<-merge(sales2016sort,inv_pur,by=c("sku","month"))
stockout$stockoutflag[stockout$cqty>stockout$cquantity] <- 1
stockout$stockoutflag[stockout$cqty<stockout$cquantity] <- 0
stockoutlist<-stockout[stockout$stockoutflag=="1",]
stockoutunique<-stockoutlist[!duplicated(stockoutlist$sku),]


install.packages("psych")
library(psych)
install.packages("plyr")
library("plyr")
install.packages("lubridate")
library("lubridate")

quantile(Audit_Sales$total, probs = c(0,0.25,0.5,0.75,1), na.rm=T) 


sales<-read.csv(file.choose())
collections<-read.csv(file.choose())
Audit_Credit<-read.csv(file.choose())


audityear = interval(ymd(20160101), ymd(20161231))
sales$datep = as_date(sales$date)
sales$year = year(sales$datep)
Audit_Sales = split(sales,sales$year)$'2016' #Sales2016

audityear = interval(ymd(20160101), ymd(20161231))
collections$datep = as_date(collections$date)
collections$year = year(collections$datep)
Audit_Collections = split(collections,collections$year)$'2016' #collection2016


colnames(Audit_Sales)[6]<- "Sales_Date"
colnames(Audit_Collections)[4]<-"Collection_Date"
colnames(Audit_Credit)[2] <- "cust.no"




m1 = merge(Audit_Collections, Audit_Sales, by="invoice",all.y = TRUE)
AC_1<-arrange(AC,AC$cust.no, AC$date.y)

AS = merge(credit, sales2016, by="cust.no",all.y = TRUE)
AS_1<-arrange(AS,AS$cust.no ,AS$date )

transactions<- AC_1[0,]
CustIdUnique<-unique(AC_1$cust.no)
exceededBy<-data.frame(cust.no=0,invoice=0,currentCredit=0)
AC_1$collected[AC_1$collected<0]<-0

for(c in 1:nrow(tempCust1)) {
  tempCust1<-AC_1[(AC_1$cust.no==CustIdUnique[c]),]
  tempCust2<-AS_1[(AS_1$cust.no==CustIdUnique[c]),]
  
  tempCust1<-arrange(tempCust1,tempCust1$date)
  tempCust2<-arrange(tempCust2,tempCust2$date)
  
  creditLimitForTheParticularCustomer<-tempCust2[1,]$limit
  maxCredit<-creditLimitForTheParticularCustomer
  
  prevSalesDate<-tempCust2[1,]$date
  previousSalesDate<-as.POSIXct(prevSalesDate)
  
  for(i in 1:nrow(tempCust1)) {
    
    creditLimitForTheParticularCustomer<-creditLimitForTheParticularCustomer-tempCust2[i,]$total
    
    
    currentSalesDate<-as.POSIXct(tempCust2[i,]$date)
    tempCust1$date<-as.POSIXct(tempCust1$date)
    if(is.na(tempCust1[i,]$date)){
      collection<- tempCust1[0,]
    }else{
      collection<-tempCust1[(tempCust1[i,]$date>previousSalesDate&&tempCust1[i,]$date<=currentSalesDate), ]
    }
    
    if(nrow(collection)>0){
      colAmt<-sum(collection$collected)
    }else{
      colAmt<-0
    }
    
    creditLimitForTheParticularCustomer<-creditLimitForTheParticularCustomer+colAmt
    
    if(creditLimitForTheParticularCustomer>maxCredit){
      creditLimitForTheParticularCustomer<-maxCredit
    }
    
    if(creditLimitForTheParticularCustomer<0) {
      transactions <- rbind(transactions, tempCust2[i,])
      
      exceededBy<-rbind(exceededBy,c(tempCust2[i,]$cust.no,tempCust2[i,]$invoice,creditLimitForTheParticularCustomer))
    }
    previousSalesDate<-currentSalesDate
    
  }
}

View(transactions)
View(exceededBy)


exceededBy<-exceededBy[-1,]
transactions1 = merge(transactions, exceededBy, by=c("invoice","cust.no"),all.x = TRUE)
transactions1<-arrange(transactions1,transactions1$cust.no,transactions1$date)
View(transactions1)



MPC<-merge(collections,sales,by="invoice")
MP<-merge(MPC,inventory,by="sku")
MP_Collection<-(((MP$collected)/(MP$unitcost))-1)*100
summary(MP_Collection)
