install.packages("readxl")
library(readxl)

install.packages("stringr")
library(stringr)

install.packages("lubridate")
library(stringr)

install.packages("benford.analysis")
library(benford.analysis)

install.packages("pwr")
library(pwr)

install.packages("pps")
library(pps)

importAccounts = function() {
  library(readxl, readr)
  path = "/Users/saai/Coding/RStudio/Audit" ## folder for files downloaded from UIC Blackboard
  files = c("arConfirmations.csv", "custCredit.csv", "empReimbursements.csv", "inventoryCounts.csv", "inventoryPerpetual.csv", "arCollections.csv", "purchases.csv", "sales.csv")
  dataFrameList = list()
  for(i in 1:length(files)){
    dataFrameName = strsplit(files[i], ".", fixed = TRUE)[[1]][1]
    fileType = strsplit(files[i], ".", fixed = TRUE)[[1]][2]
    if(fileType == "xlsx") {
      dataFrame = read_excel(paste(path, files[i], sep = "/"))
    } else {
      dataFrame = read.csv(paste(path, files[i], sep = "/"))
    }
    namedFrame = assign(dataFrameName, dataFrame)
    dataFrameList[[dataFrameName]] = namedFrame
  }
  return(dataFrameList)
}


convertAccounts = function(accounts) {
  library(stringr)
  library(lubridate)
  for(i in 1:length(accounts)) {
    for (n in 1:length(accounts[[i]])) {
      dataFrame = accounts[[i]]
      if(str_detect(names(dataFrame[n]), "date") | str_detect(names(dataFrame[n]), "dateColl")){
        if(is.factor(dataFrame[[n]])){
          accounts[[i]][[n]] = ymd(as.character(dataFrame[[n]]))
        }
      }
      else if(str_detect(names(dataFrame[n]), "sku") | str_detect(names(dataFrame[n]), "invoice")
              | str_detect(names(dataFrame[n]), ".no") | str_detect(names(dataFrame[n]), ".No")  | str_detect(names(dataFrame[n]), "customer")){
        accounts[[i]][[n]] = as.character(dataFrame[[n]])
      }
      else if (str_detect(names(dataFrame[n]), "cashtrue")) {
        accounts[[i]][[n]] = as.logical(dataFrame[[n]])
      }
      else if(str_detect(names(dataFrame[n]), "Amount")){
        accounts[[i]][[n]] = as.numeric(dataFrame[[n]])
      }
    }
  }
  return(accounts)
}


createCostofGoodsSold = function(accounts){
  costOfGoodsSold = merge(accounts$sales, accounts$inventoryPerpetual, by="sku", all.x=T)
  costOfGoodsSold$COGS = costOfGoodsSold$unitcost * costOfGoodsSold$qty
  accounts[["costOfGoodsSold"]] = costOfGoodsSold
  return(accounts)
}


createUnpaidAccountsReceivable = function(accounts) {
  splitSalesbyTransaction = split(accounts$sales, accounts$sales$cashtrue)
  credit = splitSalesbyTransaction[["FALSE"]]
  allCreditAccounts = merge(credit, accounts$arCollections, by="invoice", all.x = T)
  allCreditAccounts$notCollected = is.na(allCreditAccounts$amt.received)
  allCreditAccountsbyCollection = split(allCreditAccounts, allCreditAccounts$notCollect)
  unpaidAccountsReceivable = allCreditAccountsbyCollection[["TRUE"]]
  accounts[["unpaidAccountsReceivable"]] = unpaidAccountsReceivable
  return(accounts)
}



createAllowanceForDoubtfulAccounts = function(accounts) {
  x = accounts$unpaidAccountsReceivable
  endDateVector = rep(ymd("2016/12/31"), length(accounts$unpaidAccountsReceivable$invoice))
  x$endDate = endDateVector
  x$daysSincePurchase = x$endDate - x$date
  x$interval = findInterval(x$daysSincePurchase, c(90, 180))
  accounts[["doubtfulAccounts"]] = x
  return(accounts)
}



createOutofStock = function(accounts){
  salesBySKU = aggregate(qty~sku, accounts$sales,sum)
  purchasesBySKU = aggregate(quantity~sku,accounts$purchases,sum)
  purcahsesSalesBySKU = merge(salesBySKU, purchasesBySKU, by="sku")
  purchasesSalesInventoryBySKU = merge(purcahsesSalesBySKU, accounts$inventory, by="sku")
  purchasesSalesInventoryBySKU$turnover = (purchasesSalesInventoryBySKU$qtypurchasesSalesInventoryBySKU$quantity)/purchasesSalesInventoryBySKU$endstock
  turnover = data.frame(purchasesSalesInventoryBySKU$sku,purchasesSalesInventoryBySKU$turnover)
  colnames(turnover)=c("sku","times")
  accounts[["turnover"]] = turnover
  return(accounts)
}



createAccountsByYear = function(accounts, year) {
  for(i in 1:length(accounts)) {
    for (n in 1:length(accounts[[i]])) {
      dataFrame = accounts[[i]]
      dateColumnExists = FALSE
      if(str_detect(names(dataFrame[n]), "date") | str_detect(names(dataFrame[n]), "dateColl")){
        dateColumn = n
        dateColumnExists = TRUE
        break()
      }
    }
    if(dateColumnExists == TRUE) {
      accounts[[i]]$year = year(accounts[[i]][[dateColumn]])
      dataFramebyYear = split(accounts[[i]], accounts[[i]][["year"]])
      accounts[[i]] = dataFramebyYear[[year]]
    }
  }
  return(accounts)
}



#Filter	Audit	Year	(2016)	Transactions
accounts = importAccounts()
accounts = convertAccounts(accounts)
accounts2016 = createAccountsByYear(accounts, year = "2016")
accounts2016 = createCostofGoodsSold(accounts2016)
accounts2016 = createUnpaidAccountsReceivable(accounts2016)
accounts2016 = createAllowanceForDoubtfulAccounts(accounts2016)


#Question  2: TESTS OF INTERNAL CONTROLS

#Part (1): Customers who exceeded their Credit Limit


findCreditNegatives = function(accounts) {
  library(plyr, dplyr)
  #Prepare Sales table
  sales = split(accounts$sales, accounts$sales$cashtrue)[["FALSE"]]
  sales = subset(sales, select = c(date, cust.no, total))
  names(sales)[names(sales) == "total"] = "trans"
  sales$trans = sales$trans*-1
  #Prepare Collections table
  collections = merge(accounts$sales, accounts$arCollections, by = "invoice", all.x = T)
  collections = na.omit(collections)
  collections = subset(collections, select = c(dateColl, cust.no.x, amt.received))
  names(collections)[names(collections) == "dateColl"] = "date"
  names(collections)[names(collections) == "amt.received"] = "trans"
  names(collections)[names(collections) == "cust.no.x"] = "cust.no"
  #TransactionsTable
  transTable = rbind(sales, collections)
  transTable = arrange(transTable, date)
  #Create TransByCustomer
  transByCustomer = split(transTable, transTable$cust.no)
  
  #Loop through customers
  badCreditAccount = data.frame()
  for(i in 1:length(transByCustomer)) {
    customer = transByCustomer[[i]]
    customerNumber = transByCustomer[[i]][1,]$cust.no
    customer$subTotal = accounts$custCredit[as.numeric(customerNumber),]$limit
    #loop through customer
    for(n in 1:length(customer$subTotal)) {
      if(n != 1) {
        customer[n,]$subTotal = customer[n - 1,]$subTotal + customer[n,]$trans
        if(sign(customer[n,]$subTotal) == -1) {
          badCreditAccount = rbind(badCreditAccount, customer[n,])
          break
        }
      }
    }
  }
  accounts[["overlimitCreditApprovals"]] = badCreditAccount
  return(accounts)
}
accounts2016 = findCreditNegatives(accounts2016)
View(accounts2016$overlimitCreditApprovals)


# Part (2.a): DUPLICATE TRANSACTIONS

findDuplicates = function(dataframe, column) {
  dataframe$test = as.numeric(dataframe[[column]])
  dataframe$dup = duplicated(dataframe$test)
  x = split(dataframe, dataframe$dup)
  y = x[["TRUE"]]
  print(y)
  print ("Duplicates (head)")
  head(y)
}
findDuplicates(dataframe = accounts2016$sales, column = "invoice")

#ANSWER: 
#[1] "Duplicates (head)"
#NULL



# Part (2.b): OMITTED TRANSACTIONS

findMissingEntries =function(max,set) {
  good = 1:max
  test = as.numeric(set)
  missing = setdiff(good, set)
  print(missing)
  print ("Missing (head)")
  head(missing)
}
findMissingEntries(max = length(accounts2016$sales$invoice), set = accounts2016$sales$invoice)

#ANSWER:
#[1] "Missing (head)"
#[1]  9 12 13 15 26 38



# Part (2.c): TRANSACTION CUT OFF TEST 

findSalesNotIn2016 = function(accounts) {
  x = accounts$sales
  x$year = year(accounts$sales$date)
  y = split(x, x$year)
  z = rbind(y[["2015"]], y[["2017"]])
  print("Transactions not in 2016")
  print(z)
  print ("Transactions not in 2016 (head)")
  head(z)
}
findSalesNotIn2016(accounts)

#ANSWER: 
#[1] "Transactions not in 2016 (head)"
#    X invoice  sku qty cashtrue       date unitprice   total cust.no year
#9   9       9  445  42    FALSE 2015-12-17      4.56  191.52     307 2015
#12 12      12 1343  37    FALSE 2015-12-13     18.93  700.41     544 2015
#26 26      26   83 154    FALSE 2015-12-12     20.71 3189.34     146 2015
#38 38      38 1545  60    FALSE 2015-12-29     27.55 1653.00     800 2015
#46 46      46  699  46    FALSE 2015-12-30     14.36  660.56     474 2015
#75 75      75  896   5    FALSE 2015-12-10     20.59  102.95      37 2015


#Question 3: RECOMPUTE THE TRIAL BALANCE

# PART (0):

accountTotals = function(accounts) {
  
  #SALES REVENUE:
  print("Sales Revenue")
  totalSalesRevenue = sum(accounts$sales$total)
  print(totalSalesRevenue)
  
  #SALES RETURNS:
  print("Sales Returns")
  x = aggregate((returns)*unitprice ~ sku, accounts$inventoryPerpetual, sum)
  print(sum(x$`(returns) * unitprice`))
  
  #COGS:
  print("COGS")
  totalCOGS = sum(accounts$costOfGoodsSold$COGS)
  print(totalCOGS)
  
  #ACCOUNTS RECEIVABLE:
  print("Accounts Receivable")
  totalAR = sum(accounts$unpaidAccountsReceivable$total)
  print(sum(accounts$unpaidAccountsReceivable$total))
  
  #COLLECTIONS:
  print("Collections")
  totalCollections = sum(accounts$arCollections$amt.received)
  print(totalCollections)
  
  #INVENTORY:
  print("Inventory Perpetual on 1/1/2016")
  print(sum(accounts$inventoryPerpetual$beginstock))
  print("Inventory Perpetual on 12/31/2016")
  print(sum(accounts$inventoryPerpetual$endstock))
  print("Inventory Perpetual Cost on 1/1/2016")
  beginInventoryValue = sum(accounts$inventoryPerpetual$unitcost*accounts$inventoryPerpetual$beginstock)
  print(beginInventoryValue)
  print("Inventory Perpetual Cost on 12/31/2016")
  endInventoryValue = sum(accounts$inventoryPerpetual$unitcost*accounts$inventoryPerpetual$endstock)
  print(endInventoryValue)
  
  #PURCHASES:
  print("Purchases Cost")
  totalPurchasesCost = sum(accounts$purchases$unitcost*accounts$purchases$quantity)
  print(totalPurchasesCost)
  
  #EMPLOYEE REIMBURSEMENTS:
  print("Employee Reimbursements total")
  totalEmployeeReimbursements = sum(accounts$empReimbursements$Amount)
  print(totalEmployeeReimbursements)

}

accountTotals(accounts2016)

#ANSWER:

#[1] "Sales Revenue"
#[1] 960030574
#[1] "Sales Returns"
#[1] 2014072
#[1] "COGS"
#[1] 350802594
#[1] "Accounts Receivable"
#[1] 333286020
#[1] "Collections"
#[1] 650887909
#[1] "Inventory Perpetual on 1/1/2016"
#[1] 25086639
#[1] "Inventory Perpetual on 12/31/2016"
#[1] 25059323
#[1] "Inventory Perpetual Cost on 1/1/2016"
#[1] 151790200
#[1] "Inventory Perpetual Cost on 12/31/2016"
#[1] 152765109
#[1] "Purchases Cost"
#[1] 418576367
#[1] "Employee Reimbursements total"
#[1] 72750312


# PART (1.a): Foot(total)

#SALES Foot(total):
print("Foot(total) of Sales")
footTotalOfSales = sum(accounts2016$sales$total)
print(footTotalOfSales)

#ANSWER:

#[1] "Foot(total) of Sales"
#[1] 960030574 - Materiality error exists and hence this value needs to be updated in the trial balance


# PART (1.b): Statistical summary of the transactions in the datasets

summarizeAccount = function(accounts) {
  for(i in 1:length(accounts)){
    print(names(accounts[i]))
    print(summary(accounts[[i]]))
  }
}
summarizeAccount(accounts2016)

# PART (1.c): What does the above results indicate? 

#Answer in Summary.txt file



# PART (2): Range of dates of sales, purchases and collections


createDailySales = function(accounts) {
  totalSales = accounts$sales
  totalSales$amt = totalSales$qty * totalSales$unitprice
  dailySales = aggregate(amt~date,totalSales,sum)
  accounts[["dailySales"]] = dailySales
  return(accounts)
}


createDailyPurchases = function(accounts) {
  totalPurchases = accounts$purchases
  totalPurchases$amt = totalPurchases$quantity * totalPurchases$unitcost
  dailyPurchases = aggregate(amt~date,totalPurchases,sum)
  accounts[["dailyPurchases"]] = dailyPurchases
  return(accounts)
}


createDailyCollections= function(accounts) {
  totalCollections = accounts$arCollections
  dailyCollections = aggregate(amt.received~dateColl,totalCollections,sum)
  accounts[["dailyCollected"]] = dailyCollections
  return(accounts)
}


# PART (2.a): Compute the min max quartiles etc:
# PART (2.b): Compute daily averages

accounts2016 = createDailySales(accounts2016)
summary(accounts2016$dailySales)
accounts2016 = createDailyPurchases(accounts2016)
summary(accounts2016$dailyPurchases)
accounts2016 = createDailyCollections(accounts2016)
summary(accounts2016$dailyCollected)

# > summary(accounts2016$dailySales)
# date                 amt         
# Min.   :2016-01-01   Min.   :1758475  
# 1st Qu.:2016-04-01   1st Qu.:2022356  
# Median :2016-07-01   Median :2840832  
# Mean   :2016-07-01   Mean   :2623034  
# 3rd Qu.:2016-09-30   3rd Qu.:2912291  
# Max.   :2016-12-31   Max.   :3098749  
# > summary(accounts2016$dailyPurchases)
# date                 amt          
# Min.   :2016-01-05   Min.   :34881364  
# 1st Qu.:2016-03-25   1st Qu.:34881364  
# Median :2016-06-17   Median :34881364  
# Mean   :2016-06-17   Mean   :34881364  
# 3rd Qu.:2016-09-08   3rd Qu.:34881364  
# Max.   :2016-12-02   Max.   :34881364  
# > summary(accounts2016$dailyCollected)
# dateColl           amt.received    
# Min.   :2016-01-01   Min.   : 355863  
# 1st Qu.:2016-04-01   1st Qu.:1360810  
# Median :2016-07-01   Median :1937318  
# Mean   :2016-07-01   Mean   :1778382  
# 3rd Qu.:2016-09-30   3rd Qu.:2292952  
# Max.   :2016-12-31   Max.   :2555145 

# PART (2.c): Range falls within if filtered data is passed else NO

# PART (2.d): If range not in audit year, the apply year filter using lubridate feature

# PART (2.e): Computed accounts would not change unless the non filtered data set is used.


# Question 3: Employee Expenditure Audit

# Part (1): COMEBACK

# Part (2): Benford's Law

#Benford test
accounts2016$empReimbursements$Employee.No = as.integer(accounts2016$empReimbursements$Employee.No)
accounts2016$empReimbursements$Receipt.No = as.integer(accounts2016$empReimbursements$Receipt.No)

#Amount
benford_Emp_amount <- benford(accounts2016$empReimbursements$Amount,number.of.digits = 1, sign = "both", round = 3 )
benford_Emp_amount
plot(benford_Emp_amount)
suspects <- getSuspects(benford_Emp_amount, accounts2016$empReimbursement, how.many=2)
suspects

# Part (3): Predicted vs actual first digits in Receipt and Employee Number columns

#Employee Number
benford_Emp_EmpNo <- benford(accounts2016$empReimbursements$Employee.No,number.of.digits = 1, sign = "both", round = 3 )
benford_Emp_EmpNo
plot(benford_Emp_EmpNo)

#Receipts
benford_Emp_Receipts <- benford(accounts2016$empReimbursements$Receipt.No,number.of.digits = 1, sign = "both", round = 3 )
benford_Emp_Receipts
plot(benford_Emp_Receipts)


# Part (4): Report any Suspicious findings:
suspects <- getSuspects(benford_Emp_amount, accounts2016$empReimbursement, how.many=2)
suspects


# Question 4: Accounts Receivable Audit

# Part (1): UNPAID ACCOUNTS RECEIVABLE

print("Unpaid Accounts Receivable")
totalAR = sum(accounts2016$unpaidAccountsReceivable$total)
print(sum(accounts2016$unpaidAccountsReceivable$total))

#ANSWER: [1] 333286020


# Part (2): ALLOWANCE FOR DOUBTFUL ACCOUNTS

print("Uncollected Accounts Receivable")
accounts2016 = createdoubtfulTotals = aggregate(total~interval, accounts2016$doubtfulAccounts, sum)
UnpaidAccountsReceivable(accounts2016)
print(sum(accounts2016$unpaidAccountsReceivable$total))
print("Allowance for Doubtful Accounts")
accounts2016 = createAllowanceForDoubtfulAccounts(accounts2016)
print(0.3*doubtfulTotals$total[2] + 0.5*doubtfulTotals$total[3])

#ANSWER:
#[1] "Allowance for Doubtful Accounts"
#[1] 58398058


# Part (4): SALES CUT OFF TEST

findSalesNotIn2016 = function(accounts) {
  x = accounts$sales
  x$year = year(accounts$sales$date)
  y = split(x, x$year)
  z = rbind(y[["2015"]], y[["2017"]])
  print("Transactions not in 2016")
  print(z)
  print ("Transactions not in 2016 (head)")
  head(z)
}
findSalesNotIn2016(accounts)

#ANSWER: 
#[1] "Transactions not in 2016 (head)"
#    X invoice  sku qty cashtrue       date unitprice   total cust.no year
#9   9       9  445  42    FALSE 2015-12-17      4.56  191.52     307 2015
#12 12      12 1343  37    FALSE 2015-12-13     18.93  700.41     544 2015
#26 26      26   83 154    FALSE 2015-12-12     20.71 3189.34     146 2015
#38 38      38 1545  60    FALSE 2015-12-29     27.55 1653.00     800 2015
#46 46      46  699  46    FALSE 2015-12-30     14.36  660.56     474 2015
#75 75      75  896   5    FALSE 2015-12-10     20.59  102.95      37 2015




# Part (6 a)
d=1000000/333286020
library(pwr)
pwr.t.test (n = NULL, d = 0.003, sig.level = 0.05, power = 0.8, type = "one.sample")

mergeSalesAndARConfirmations = function(accounts) {
  allARAccounts = merge(accounts$arCollections, accounts$arConfirmations, by="invoice", all.x = T)
  allARAccounts = subset(allARAccounts, select = c(invoice, amt.received.x, amt.received.y))
  allARAccounts = na.omit(allARAccounts)
  accounts[["allARConfirmationsAndCollections"]] = allARAccounts
  return(accounts)
}
accounts2016 = mergeSalesAndARConfirmations(accounts2016)

# Part (6 b): 

sampleConfirmation = accounts2016$allARConfirmationsAndCollections[ppss(accounts2016$allARConfirmationsAndCollection$amt.received.y, 1483718),]
distinctSampleConfirmation = unique(sampleConfirmation)
difference = sum(distinctSampleConfirmation$amt.received.y - distinctSampleConfirmation$amt.received.x)
totalConfirmedAmounts = sum(distinctSampleConfirmation$amt.received.y)
percentageError = (difference/totalConfirmedAmounts)*100
percentageError
#0.029%

# Part (7): 
# The error percentage i.e. percentage change in audited value against the recorded values is observed to be around 0.029% percent.
# Since the error is too negligible we can consider it to be more or less accurate



#Question 5: Inventory Audit

#Part 1:

accounts2016 = createCostofGoodsSold(accounts2016)
sum(accounts2016$costOfGoodsSold$COGS)
#[1] 350802594

#Part 1 a:
#Matching Principle


#Part 2 a:
summary((((accounts2016$costOfGoodsSold$qty*accounts2016$costOfGoodsSold$unitprice.x))/accounts2016$costOfGoodsSold$COGS) - 1)
#MarkUp percentage:
# 173.9 %
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.503   1.078   1.745   1.739   2.374   3.000   11004 

#Part 3 a: Stocked out

findOutOfStockDemand = function(accounts) {
  library(plyr)
  #prepare tables
  sales = subset(accounts$sales, select = c(sku, date, qty))
  sales$qty = sales$qty*-1
  
  purchases = accounts$purchases
  purchases$qty = purchases$quantity
  purchases = subset(purchases, select = c(sku, date, qty))
  
  inventoryTrans = rbind(sales, purchases)
  inventoryTrans = arrange(inventoryTrans, date)
  
  #Create dataframe by sku
  inventoryTransBySku = split(inventoryTrans, inventoryTrans$sku)
  
  stockOutSkus = list()
  for(i in 1:length(inventoryTransBySku)) {
    sku = inventoryTransBySku[[i]]
    skuNumber = as.numeric(sku[1,]$sku)
    sku$onHand = accounts$inventoryPerpetual[skuNumber,]$beginstock
    
    for(n in 1:length(sku$qty)) {
      if(n == 1) {
        sku[n,]$onHand = sku[n,]$onHand + sku[n,]$qty
      }
      else {
        sku[n,]$onHand = sku[n-1,]$onHand + sku[n,]$qty
      }
    }
    if(sum(sku$onHand < 0) > 0) {
      stockOutSkus[[length(stockOutSkus) + 1]] = skuNumber
    }
    inventoryTransBySku[[i]] = sku
  }
  stockOutTrans = data.frame()
  for(i in 1:length(stockOutSkus)){
    skuNumber = stockOutSkus[[i]]
    sku = inventoryTransBySku[[as.character(skuNumber)]]
    times = which(diff(sign(sku$onHand)) > 0)
    for(n in 1:length(times)) {
      stockOutTrans = rbind(stockOutTrans, sku[times[n],])
    }
  }
  accounts[["stockOutTrans"]] = stockOutTrans
  return(accounts)
}
accounts2016 = findOutOfStockDemand(accounts2016)
accounts2016$stockOutTrans = na.omit(accounts2016$stockOutTrans)
accounts2016$stockOutTrans$sku


#Part 4(a)
d=1000000/152765109
d
library(pwr)
pwr.t.test (n = NULL, d = 0.0065, sig.level = 0.05, power = 0.8, type = "one.sample")

mergeInventoryPerpetualAndCounts = function(accounts) {
  allInventory = merge(accounts$inventoryPerpetual, accounts$inventoryCounts, by="sku", all.x = T)
  allInventory = subset(allInventory, select = c(sku, beginstock,endstock.x, endstock.y,unitcost))
  allInventory = na.omit(allInventory)
  accounts[["allInventoryMatched"]] = allInventory
  return(accounts)
}
accounts2016 = mergeInventoryPerpetualAndCounts(accounts2016)

# Part (4 b): 

sampleConfirmation = accounts2016$allInventoryMatched[ppss(accounts2016$allInventoryMatched$endstock.y, 185774),]
distinctSampleConfirmation = unique(sampleConfirmation)
sum(distinctSampleConfirmation$endstock.x)
difference = sum(distinctSampleConfirmation$endstock.y - distinctSampleConfirmation$endstock.x)
totalConfirmedAmounts = sum(distinctSampleConfirmation$endstock.y)
percentageError = (difference/totalConfirmedAmounts)*100
percentageError
#ANSWER: [1] 0.241898 %

# Part 4 c:

#The inventory is overstatied by 0.24 % and this would impact the balance sheet but to a minimal extent


# Part 5: Foot total(inventory accounts balance -> endstock*unitprice)
totalInventoryBalanceAfterAdjusting = sum(accounts2016$allInventoryMatched$endstock.y*accounts2016$allInventoryMatched$unitcost)
totalInventoryBalanceAfterAdjusting
#ANSWER: [1] 153129104
# Difference: $364,104 after computing the inventory counts, this indicates there is a deviation from the stated trial balance value


# Part 6 : Ageing of Inventory

createInventoryAgeingData = function(accounts){
  inventoryAgeing = merge(accounts$sales, accounts$allInventoryMatched, by="sku", all.x=T)
  inventoryAgeing = subset(inventoryAgeing, select = c(sku, date, qty,unitcost,beginstock,endstock.y,total))
  inventoryAgeing$COGS = inventoryAgeing$unitcost * inventoryAgeing$qty
  inventoryAgeing$AvgInvCost = ((inventoryAgeing$endstock.y + inventoryAgeing$beginstock)* inventoryAgeing$unitcost / 2)
  inventoryAgeing$turnover = inventoryAgeing$COGS/inventoryAgeing$AvgInvCost
  accounts[["inventoryAgeing"]] = inventoryAgeing
  return(accounts)
}
accounts2016 = createInventoryAgeingData(accounts2016)
names(accounts2016$inventoryAgeing)[names(accounts2016$inventoryAgeing) == "endstock.y"] = "endstock"

createInventoryAgeingFinal = function(accounts){
  accountsInventoryAgeingSorted=accounts$inventoryAgeing
  accountsInventoryAgeingSortedFiltered = sqldf("Select sku, sum(qty) as qty,unitcost,endstock,AvgInvCost from accountsInventoryAgeingSorted group by sku")
  accountsInventoryAgeingSortedFiltered$COGS = accountsInventoryAgeingSortedFiltered$qty*accountsInventoryAgeingSortedFiltered$unitcost
  accountsInventoryAgeingSortedFiltered$turnOverRatio = accountsInventoryAgeingSortedFiltered$COGS/accountsInventoryAgeingSortedFiltered$AvgInvCost
  #accountsInventoryAgeingSortedFiltered = accountsInventoryAgeingSortedFiltered[!(accountsInventoryAgeingSortedFiltered$turnOverRatio==0),]
  accountsInventoryAgeingSortedFiltered$age = 365 / accountsInventoryAgeingSortedFiltered$turnOverRatio
  accounts[["inventoryAgeingFinal"]] = accountsInventoryAgeingSortedFiltered
  return(accounts)
}
accounts2016 = createInventoryAgeingFinal(accounts2016)
accounts2016_backup = accounts2016

head(accounts2016$inventoryAgeingFinal)
effectiveCostUnderSixty=0
effectiveCostOverSixtyLessOneEighty=0
effectiveCostOver180Less365=0
effectiveCostOver365=0
i=as.integer()
accounts2016$inventoryAgeingFinal$age = as.numeric(accounts2016$inventoryAgeingFinal$age)
na.omit(accounts2016$inventoryAgeingFinal)

inventoryAgeingCheckData = accounts2016$inventoryAgeingFinal
inventoryAgeingCheckData[complete.cases(inventoryAgeingCheckData),]


for (i in 1:2000){
  print(i)
  print(accounts2016$inventoryAgeingFinal$age[i])
  if(is.na(accounts2016$inventoryAgeingFinal$age[i])){
    next
  }
  if(accounts2016$inventoryAgeingFinal$age[i] < 60){
    effectiveCostUnderSixty = effectiveCostUnderSixty + (accounts2016$inventoryAgeingFinal$unitcost[i]*accounts2016$inventoryAgeingFinal$endstock[i])
  }else
    if(accounts2016$inventoryAgeingFinal$age[i]>=60 && accounts2016$inventoryAgeingFinal$age[i]<180){
      effectiveCostOverSixtyLessOneEighty = effectiveCostOverSixtyLessOneEighty + (0.50)*(accounts2016$inventoryAgeingFinal$unitcost[i]*accounts2016$inventoryAgeingFinal$endstock[i])
    }else
      if(accounts2016$inventoryAgeingFinal$age[i]>=180 && accounts2016$inventoryAgeingFinal$age[i]<365){
        effectiveCostOver180Less365 = effectiveCostOver180Less365 + (accounts2016$inventoryAgeingFinal$unitcost[i]*accounts2016$inventoryAgeingFinal$endstock[i])
      }else{
        effectiveCostOver365 = effectiveCostOver365 + (accounts2016$inventoryAgeingFinal$unitcost[i]*accounts2016$inventoryAgeingFinal$endstock[i])
      }
}

agedInventoryTotal = effectiveCostUnderSixty + effectiveCostOverSixtyLessOneEighty + effectiveCostOver180Less365 + effectiveCostOver365
agedInventoryTotal
#[1] 106273976

effectiveCostUnderSixty
effectiveCostOverSixtyLessOneEighty
effectiveCostOver180Less365
effectiveCostOver365

# Part 6 a
percentageOfTotalLess60 = (effectiveCostUnderSixty/agedInventoryTotal)*100
#Answer: [1] 0

# Part 6 b
percentageOfTotalOver60Less180 = (effectiveCostOverSixtyLessOneEighty/agedInventoryTotal)*100
#Answer: [1] 44.089

# Part 6 c
percentageOfTotalOver180Less365 = (effectiveCostOver180Less365/agedInventoryTotal)*100
#Answer: [1] 55.911

# Part 6 d
percentageOfTotalOver365 = (effectiveCostOver365/agedInventoryTotal)*100
#Answer: 0



percentageOfTotalLess60
percentageOfTotalOver60Less180
percentageOfTotalOver180Less365
percentageOfTotalOver365


# # Part 7
# sales_list <- list()
# for (i in 1:2000){
#   print(i)
#   if(is.na(accounts2016$inventoryAgeingFinal$qty[i])){
#     print("first if")
#     next
#   }
# if(accounts2016$inventoryAgeingFinal$qty[i]*10 > accounts2016$inventoryAgeingFinal$endstock[i]){
#   print("second if")
#   sales_list <- c(sales_list,accounts2016$inventoryAgeingFinal$sku[i])
#   print(sales_list)
# }}
# 
# print(sales_list)
