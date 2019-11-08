library(DSI)
library(dsBase)
library(dsBaseClient)
library(RCurl)
library(dsStatsClient)
library(dsGraphicsClient)
library(dsModellingClient)

source('~/Documents/GitHub/ds.client.connection.server/ds.client.connection.server/R/ds.login.R')

ip_address_1 <- paste("https://192.168.56.100:8443", sep="")
ip_address_2 <- paste("https://192.168.56.100:8443", sep="")
ip_address_3 <- paste("https://192.168.56.100:8443", sep="")

user_1 <- "administrator"
user_2 <- "administrator"
user_3 <- "administrator"

password_1 <- "datashield_test&"
password_2 <- "datashield_test&"
password_3 <- "datashield_test&"

server <- c("study1", "study2", "study3")
url <- c(ip_address_1,ip_address_2,ip_address_3)
user <- c(user_1,user_2,user_3)
password <- c(password_1,password_2,password_3)
table <- c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3")
login.data <- data.frame(server,url,user,password,table)


stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER',
                  'FACTOR_INTEGER')


connection.DSI <- ds.login(login.data,assign=TRUE,variables=stats.var)
#connection.DSI <- datashield.login(logins=login.data, assign=TRUE,variables=stats.var)
print(connection.DSI)

datashield.logout(connection.DSI)



