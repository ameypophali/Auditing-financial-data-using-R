#Reading the datasets
sales = read.csv("D:/UIC Spring/IDS 523 Audit and Control of Information Systems/Mid-Term exam/sales.csv")
inventory = read.csv("D:/UIC Spring/IDS 523 Audit and Control of Information Systems/Mid-Term exam/inventory.csv")
purchases = read.csv("D:/UIC Spring/IDS 523 Audit and Control of Information Systems/Mid-Term exam/purchases.csv")
collections = read.csv("D:/UIC Spring/IDS 523 Audit and Control of Information Systems/Mid-Term exam/collections.csv")
credit = read.csv("D:/UIC Spring/IDS 523 Audit and Control of Information Systems/Mid-Term exam/credit.csv")

#Filtering for the correct dates
purchases$date = as.Date(purchases$date)
purchases_2016 <- subset(purchases, date <= "2016-12-31")
purchases_2016 <- subset(purchases_2016, date >= "2016-01-01")

collections$date = as.Date(collections$date)
collections_2016 <- subset(collections, date <= "2016-12-31")
collections_2016 <- subset(collections_2016, date >= "2016-01-01")

# 1 (a)
#Sales Revenue
sales$date = as.Date(sales$date)
Sales_2016 <- subset(sales, date <= "2016-12-31")
Sales_2016 <- subset(Sales_2016, date >= "2016-01-01")
Sales_revenue_2016 = sum(Sales_2016$total)
Sales_revenue_2016

# 1 (b) 
# Cost of Goods Sold 
CGS = merge(Sales_2016,inventory, by = "sku")
CostOfGoodSold = sum(CGS$qty*CGS$unitcost)
CostOfGoodSold
TotalSales = sum(CGS$total)
Markup = TotalSales/CostOfGoodSold
Markup

# 1(c)
Accounts_Receivable = merge(Sales_2016,collections_2016,all.x = TRUE, by = "invoice")
Accounts_Receivable$collected[is.na(Accounts_Receivable$collected)] =0
Accounts_Receivable$collected[Accounts_Receivable$collected<0]<-0
sum(Accounts_Receivable$total) - sum(Accounts_Receivable$collected)

# 1(d)
#Allowance for Doubtful Accounts
Accounts_Receivable$AR = Accounts_Receivable$total - Accounts_Receivable$collected
Accounts_Receivable$duePeriod <-as.Date(as.character("2016-12-31"),format="%Y-%m-%d")-as.Date(as.character(Accounts_Receivable$date.x),format="%Y-%m-%d")
Accounts_Receivable = Accounts_Receivable[Accounts_Receivable$AR>0,]
for (i in 1:nrow(Accounts_Receivable))
{
  if(Accounts_Receivable$duePeriod[i]>180){
    Accounts_Receivable$AllownaceAmt[i] = 0.4*Accounts_Receivable$AR[i]
  }else if(Accounts_Receivable$duePeriod[i]>=90 && Accounts_Receivable$duePeriod[i]<=180){
    Accounts_Receivable$AllownaceAmt[i] = 0.2*Accounts_Receivable$AR[i]
  }else{
    Accounts_Receivable$AllownaceAmt[i] = 0
  }
}
sum(Accounts_Receivable$AllownaceAmt)

# 1(e)
#Inventory on hand on 12/31/2016 
Total_endInv = sum(inventory$endstock*inventory$unitcost)
Total_endInv

# 2
sum(Sales_2016$total)
sum(purchases$total_Pur)

Sales_2016$X = as.factor(Sales_2016$X)
Sales_2016$cashtrue = as.factor(Sales_2016$cashtrue)
Sales_2016$cust.no = as.factor(Sales_2016$cust.no)
Sales_2016$sku = as.factor(Sales_2016$sku)
collections_2016$X = as.factor(collections_2016$X)
collections_2016$invoice = as.factor(collections_2016$invoice)
collections_2016$receipt.no = as.factor(collections_2016$receipt.no)
credit$cust.no = as.factor(credit$cust.no)
inventory$X = as.factor(inventory$X)
inventory$sku = as.factor(inventory$sku)
purchases$X = as.factor(purchases$X)
purchases$sku = as.factor(purchases$sku)
purchases$PO.no= as.factor(purchases$PO.no)

summary(Sales_2016)
summary(collections_2016)
summary(credit)
summary(inventory)
summary(purchases_2016)

# 3
summary(as.Date(sales$date))
summary(as.Date(collections$date))
summary(as.Date(purchases$date))

# 3(a)
#Sales markup
sales_mUp = merge(sales,inventory, by="sku")
sales_mUp$mUpValue = (sales_mUp$unitprice.x/sales_mUp$unitcost)-1
summary(sales_mUp$mUpValue)

#Collection Markup
collection_mUp = merge(collections,sales, by = "invoice")
collection_mUp_1 = merge(collection_mUp, inventory, by = "sku")
collection_mUp_1$mUpValue = (collection_mUp_1$unitprice.x/collection_mUp_1$unitcost) - 1 
summary (collection_mUp_1$mUpValue)

#purchase markup 
purchase_mUp = merge(purchases,inventory, by = "sku")
purchase_mUp$mUpValue = (purchase_mUp$unitprice/purchase_mUp$unitcost.x)-1
summary (purchase_mUp$mUpValue)


# 3(b)
#sales dataset
sales$total = as.numeric(sales$total)
sales_day_avg = aggregate(sales$total, list(sales$date), sum)
names(sales_day_avg)[1] = "Date"
names(sales_day_avg)[2] = "Average"
summary(sales_day_avg$Average)

#collections dataset
collections$collected = as.numeric(collections$collected)
collected_avg = aggregate(collections$collected, list(collections$date), sum)
names(collected_avg)[1] = "Date"
names(collected_avg)[2] = "Average"
summary(collected_avg$Average)

#purchases dataset
purchases$total_Pur = purchases$unitcost * purchases$quantity
purchase_avg = aggregate(purchases$total_Pur, list(purchases$date), sum)
names(purchase_avg)[1] = "Date"
names(purchase_avg)[2] = "Average"
summary(purchase_avg$Average)

#3(c) - No. It contains values from 2015 and 2017 as well.

#3(d) 
#data cleaning - sales
sales$date = as.Date(sales$date)
Sales_2016 <- subset(sales, date <= "2016-12-31")
Sales_2016 <- subset(Sales_2016, date >= "2016-01-01")
View(Sales_2016)

#data cleaning - collections
collections$date = as.Date(collections$date)
collections_2016 <- subset(collections, date <= "2016-12-31")
collections_2016 <- subset(collections_2016, date >= "2016-01-01")
View(collections_2016)


# 4
# 4 (a) - Duplicate transactions
duplicate_invoice = duplicated(Sales_2016$invoice)
duplicates = Sales_2016[duplicate_invoice,] 

# 4(b) - Omitted transactions
#for Sales_2016 dataset
Omitted_transactions <- seq(1, length(Sales_2016$invoice), by = 1) 
head(Omitted_transactions[!match(Omitted_transactions,Sales_2016$invoice,nomatch=FALSE)],30)

#for sales dataset
Omitted_transactions1 <- seq(1, length(sales$invoice), by = 1) 
head(Omitted_transactions1[!match(Omitted_transactions,sales$invoice,nomatch=FALSE)],30)


# 4(c)
#Sales cutt off test
sales$year = format(sales$date,"%Y")
collections$year = format(collections$date,"%Y")
sqldf("select * from sales inner join collections on sales.invoice=collections.invoice where sales.year='2016' and collections.year=2017 ")

# 5
#5(b)
CGS = merge(Sales_2016,inventory, by = "sku")
CostOfGoodSold = sum(CGS$qty*CGS$unitcost)
CostOfGoodSold
TotalSales = sum(CGS$total)
Markup = TotalSales/CostOfGoodSold
Markup

# 5(c)
inventory$markUp=(inventory$unitprice-inventory$unitcost)/(inventory$unitcost)*100
summary(inventory$markUp)


# 6
# 1(c)
Accounts_Receivable = merge(Sales_2016,collections_2016,all.x = TRUE, by = "invoice")
#Accounts_Receivable = Accounts_Receivable[Accounts_Receivable$cashtrue==0,]
Accounts_Receivable$collected[is.na(Accounts_Receivable$collected)] =0
Accounts_Receivable$collected[Accounts_Receivable$collected<0]<-0
sum(Accounts_Receivable$total) - sum(Accounts_Receivable$collected)

# 1(d)
#Allowance for Doubtful Accounts
Accounts_Receivable$AR = Accounts_Receivable$total - Accounts_Receivable$collected
Accounts_Receivable$duePeriod <-as.Date(as.character("2016-12-31"),format="%Y-%m-%d")-as.Date(as.character(Accounts_Receivable$date.x),format="%Y-%m-%d")
Accounts_Receivable = Accounts_Receivable[Accounts_Receivable$AR>0,]
for (i in 1:nrow(Accounts_Receivable))
{
  if(Accounts_Receivable$duePeriod[i]>180){
    Accounts_Receivable$AllownaceAmt[i] = 0.4*Accounts_Receivable$AR[i]
  }else if(Accounts_Receivable$duePeriod[i]>=90 && Accounts_Receivable$duePeriod[i]<=180){
    Accounts_Receivable$AllownaceAmt[i] = 0.2*Accounts_Receivable$AR[i]
  }else{
    Accounts_Receivable$AllownaceAmt[i] = 0
  }
}
sum(Accounts_Receivable$AllownaceAmt)

# 7 
#Group sales by customer 
names(credit)[2] = "cust.no"
sales_credit = merge(Sales_2016,credit, by = "cust.no")
sales_credit = sales_credit[,-2]
sales_credit = sales_credit[,-3]
sales_credit = sales_credit[,-3]
sales_credit = sales_credit[,-3]
sales_credit = sales_credit[,-4]
sales_credit = sales_credit[,-5]

sales_credit_coll = merge(sales_credit,collections, by = "invoice")
sales_credit_coll = sales_credit_coll[,-6]
sales_credit_coll = sales_credit_coll[,-7]
sales_credit_coll = sales_credit_coll[,-7]
sales_credit_coll = sales_credit_coll[,-7]
sales_credit_coll = sales_credit_coll[,-1]

result = aggregate(sales_credit_coll$total, list(sales_credit_coll$date,sales_credit_coll$cust.no), sum)
result2 = aggregate(sales_credit_coll$limit, list(sales_credit_coll$cust.no), mean)
result3 = aggregate(sales_credit_coll$collected, list(sales_credit_coll$date,sales_credit_coll$cust.no), sum)
names(result) = c("Date","Cust_no.","Total_sales")
names(result2) = c("Cust_no.","Credit_Limit")
names(result3) = c("Date","Cust_no.","Collected")


# 8
Accounts_Receivable = subset(Accounts_Receivable, sku == 272)
sum(Accounts_Receivable$qty)


Accounts_Receivable = subset(Accounts_Receivable, qty == 0)
write.csv(Accounts_Receivable, file =  "D:/ga/abc.csv")
