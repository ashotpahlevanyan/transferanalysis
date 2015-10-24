#input : two files
#   customers file
#   requests file

#output : What can you tell us about the data?
#   What can you tell about fraudulent customers and their transactions?
#   Based on your results, what actions would you do or recommend doing for letting less fraudulent transactions go through (at the same time having the least impact on our good customers)?"
###############################################################################################
#source("http://www.bioconductor.org/biocLite.R")
#biocLite("xlsx")

### Load existing Data
customers <- read.csv("customer_data.csv", header = TRUE)
requests <- read.csv("request_data-2.csv", header = TRUE)

colnames(customers)
customers$fraudulent_cst
unique(requests$payment_type)
colnames(requests)


##############################################################################################
#################################### Customers Processing ####################################
##############################################################################################

## handle customer duplications
length(customers$customer_id)
## 2129 customers in the list
unique(customers$customer_id)
unique(paste(customers$customer_id, customers$cst_successful_transfers_cnt, customers$cst_profile_age_days, sep="_"))
## 2090 unique customers in the list

#detect duplicated and unique customers
duplicateCustomers<- customers[duplicated(customers$customer_id),]
uniqueCustomers <- customers[unique(customers$customer_id),]

#customers <- uniqueCustomers
## Later on we will refer to "uniqueCustomers" only

#################################### Successfull Transfers ############################################

### Calculate quantile percentage of customer successful transfer count
percentage <- quantile(customers$cst_successful_transfers_cnt, probs = seq(0, 1, 0.2), na.rm = TRUE)
percentage
### define the coefficient step, in order not to decrease dramatically customer chances to make the transfer,
# we start from 0.5
coeffStep <- 5/(length(percentage)-1)

# we want something like this, that is for each quantile we need a coefficient, e.g.
# for customers with 5 (3 <= 5 < 7) successful transfers, the coefficient will be 0.7 as in the following lists
# 1    2    3    7   13 1307
#  0.5  0.6  0.7  0.8  0.9

tmp <- cut(customers$cst_successful_transfers_cnt,percentage, include.lowest=TRUE, right=FALSE )
customers$successTransferCoeff <- 0.5 + (as.integer(tmp)-1)*coeffStep/10
# percentage
# View(data.frame(orig=customers$cst_successful_transfers_cnt, res=tmp, res2=as.integer(tmp), res3=0.5 + (as.integer(tmp)-1)*coeffStep/10 ))


head(customers$successTransferCoeff, 20)

#### now we have customers with one more column (historyTrust) with coefficient based on customer successful transfer history

#################################### Fraudulent Type ############################################

### customer fraud type coefficients
fairCustTrust <- 0.95
oneFraudCustTrust <- 0.5
multipleFraudCustTrust <- 0.3
superTrustyCustomer <- 1

setCustomerFraudType <- function(type) {
    if( type == 0 ) return(fairCustTrust)
    if( type == 1 ) return(oneFraudCustTrust)
    if( type > 1 ) return(multipleFraudCustTrust)
    return(superTrustyCustomer)
}

customers$fraudCoeff <- sapply(customers$fraudulent_cst, setCustomerFraudType)

#################################### Suspicious Type ############################################

### customer suspicious type coefficients
cleanCustTrust <- 0.95
oneSuspCustTrust <- 0.75
multipleSuspCustTrust <- 0.4
goldenCustomer <- 1

setCustomerSuspType <- function(type) {
    if( type == 0 ) return(cleanCustTrust)
    if( type == 1 ) return(oneSuspCustTrust)
    if( type > 1 ) return(multipleSuspCustTrust)
    return(goldenCustomer)
}

customers$suspCoeff <- sapply(customers$suspicious_cst, setCustomerSuspType)


#################################### History Days ############################################

### Calculate quantile percentage of customer history in days
percentageDays <- quantile(customers$cst_profile_age_days, probs = seq(0, 1, 0.1))
percentageDays
### define the coefficient step, in order not to decrease dramatically customer chances to make the transfer,
# we start from 0.5
coeffStepDays <- 5/(length(percentageDays)-1)

tmp <- cut(customers$cst_profile_age_days,percentageDays, include.lowest=TRUE, right=FALSE )
customers$daysCoeff <- 0.5 + (as.integer(tmp)-1)*coeffStepDays/10
# percentageDays
# View(data.frame(orig=customers$cst_profile_age_days, res=tmp, res2=as.integer(tmp), res3=0.5 + (as.integer(tmp)-1)*coeffStepDays/10 ))



##############################################################################################
#################################### Requests Processing #####################################
##############################################################################################


#################################### Transaction Type ############################################
#### transaction type coefficients
bankTrust <- 0.9
achTrust <- 0.7
cardTrust <- 0.5
handTrust <- 1 # if the transfer was hand to hand :D


setTransactionType <- function(type) {
    if( type == "bank" ) return(bankTrust)
    if( type == "ach" ) return(achTrust)
    if( type == "card" ) return(cardTrust)
    return(handTrust)
}

## add transferTrust column
requests$transferTrust <- sapply(requests$payment_type,setTransactionType)


#################################### Forgetting Coefficients of Transfers ############################################

requests$daysPassed <- difftime(as.Date(as.character(requests$request_date), "%d/%m/%Y"),
                                as.Date("1970-01-01"), units = "days")
requests$daysPassed <- requests$daysPassed - min(requests$daysPassed)

forgettingExpFunction <- function(x) {
    return (1.5 - (0.5)^(x/max(unclass(requests$daysPassed))))  #max(unclass(requests$daysPassed)
}
#### here we need to forget the mistakes/fraud/suspicious behaviour of customer, so we introduce en exponential function
# f(x) <- 1.5 - (0.5)^(x/max(days passed)), the shift with 1.5 is needed to avoid 0 values, since we will use those coefficient in product

requests$forgetCoeff <- forgettingExpFunction((as.numeric(requests$daysPassed)))
plot(requests$forgetCoeff)

#################################### Customer ID Check ############################################

checkCustomerID <- function(id) {
    return(any(as.character(customers$customer_id) == as.character(id)))
}

requests$senderExists <- sapply(requests$customer_id, checkCustomerID)
length(requests[requests$senderExists == TRUE,]$customer_id)

### All the receivers are not from our Customers List, only one is in the list
requests$recipientExists <- sapply(requests$target_recipient_id, checkCustomerID)
length(requests[requests$recipientExists == FALSE,]$customer_id)

#################################### Customer ID Check ############################################

#############  Get Customer Send Count
sendersTable <- table(requests$customer_id)
getSendCount <- function(id) {
    return(sendersTable[[id]])
}
requests$customerSendCount <- sapply(requests$customer_id, getSendCount)

#############  Get Recepient Receive Count
receiversTable <- table(requests$target_recipient_id)
getReceiveCount <- function(id) {
    return(receiversTable[[id]])
}
requests$customerReceiveCount <- sapply(requests$target_recipient_id, getReceiveCount)

###########################################################################################
### Calculate Customer Send Coefficients
sendExpFunction <- function(x) {
    return ((0.5)^(x/max(requests$customerSendCount)))
}

requests$sendCoeff <- sendExpFunction((as.numeric(requests$customerSendCount)))

###########################################################################################
### Calculate Target Recepient Receive Coefficients
receiveExpFunction <- function(x) {
    return ((0.5)^(x/max(requests$customerReceiveCount)))
}

requests$receiveCoeff <- receiveExpFunction((as.numeric(requests$customerReceiveCount)))

###########################################################################################
### Calculate Final Coefficients

### get the request ids in customers list
requests$idInCustomers <- match(requests$customer_id, customers$customer_id)

# requests$successTransferCoeff <- customers[requests$idInCustomers,]$successTransferCoeff
# requests$fraudCoeff <- customers[requests$idInCustomers,]$fraudCoeff
# requests$suspCoeff <- customers[requests$idInCustomers,]$suspCoeff
# requests$daysCoeff <- customers[requests$idInCustomers,]$daysCoeff

### fill calculated coefficient columns from customers
requests[c("successTransferCoeff", "fraudCoeff", "suspCoeff", "daysCoeff")] <-
    customers[requests$idInCustomers,][c("successTransferCoeff", "fraudCoeff", "suspCoeff", "daysCoeff")]

### Calculate final decision coefficients
requests$decisionCoeff <- with(requests,
            successTransferCoeff * fraudCoeff * suspCoeff *
            daysCoeff * transferTrust * forgetCoeff * sendCoeff * receiveCoeff )

### filter the coefficients columns to look
filteredRequests <- requests[c("customer_id", "payment_type", "transferTrust",
                               "daysPassed", "customerSendCount", "customerReceiveCount",
                               "forgetCoeff", "sendCoeff", "receiveCoeff",
                               "successTransferCoeff", "fraudCoeff", "suspCoeff",
                               "daysCoeff", "decisionCoeff")]

plot(filteredRequests$decisionCoeff)
#### rank requests by decision coefficient
ranks <- rank(requests$decisionCoeff)

### calculate quantiles from decision coefficients
decisionQuantiles <- quantile(requests$decisionCoeff, seq(0,1,0.05))
decisionQuantiles
### select suspicious requests, the value 0.03800667 is 5% less trustful transfers
susp <- customers[requests[requests$decisionCoeff < 0.03800667,]$idInCustomers,]

# ########################## The above part is OK :D

