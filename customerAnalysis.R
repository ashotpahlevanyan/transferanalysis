#input : two files
#   customers file
#   requests file

#output : What can you tell us about the data?
#   What can you tell about fraudulent customers and their transactions?
#   Based on your results, what actions would you do or recommend doing for letting less fraudulent transactions go through (at the same time having the least impact on our good customers)?"
###############################################################################################

require("xlsx")

customers <- read.csv("customer_data.csv", header = TRUE)
requests <- read.csv("request_data-2.csv", header = TRUE)

colnames(customers)
customers$fraudulent_cst
unique(requests$payment_type)

colnames(requests)

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
requests.df <- requests
requests.df$transferTrust <- sapply(requests$payment_type,setTransactionType)

### customer type coefficients
fairCustTrust <- 0.95
oneFraudCustTrust <- 0.5
multipleFraudCustTrust <- 0.3

filtered <- customers[customers$fraudulent_cst == 1 & customers$suspicious_cst == 0, ]
filtered$fraudulent_cst

setCustomerType <- function(type) {
    if( type == 0 ) return(fairCustTrust)
    if( type == "ach" ) return(achTrust)
    if( type == "card" ) return(cardTrust)
    return(handTrust)
}

sapply(requests$payment_type,setTransactionType)



setCustomerType <- function(fraud) {

}






# cardTrust <- 0.5
# achTrust <- 0.7
# bankTrust <- 0.9

##### fill transaction coefficient columns based on data
setTransType <- function(type) {

}


#make a criteria, on transactions, that would make decision on previous fraudulentness/not of customers sending money
#e.g.
#clean customers have 1 coefficient of pass
#fraudulent customers have 0.75 coefficient
#fraudulent customers with more than 1 fraud have 0.5 coefficients

#bank transaction trustCoefficient = 0.9
#ach transaction trustCoefficient = 0.7
#card transaction trustCoefficient = 0.5

# target recipient id should not appear very frequent in the given period.
# if it appears frequently, then the corresponding sender id coefficient should be decreased

# for each transaction add trust coefficient columns like for card, user id, recipient id, and also some coefficient for day date (may be taken into account the day == weekend day or workday),
#At the  end add another column with overall trust coefficient




