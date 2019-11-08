library(DSI)
library(DSOpal)
library(testthat)
library(httr)

source("definition_tests/def-ds.assign.value.R")
source("connection_to_datasets/init_all_datasets.R")

.test.all.parameters.correct()

if (FALSE)
{
context('ds.login():incorrect format')
test_that('incorrect format',
{
  .test.no.login.info()
  .test.incorrect.format(c('study2', 'study3'),
     c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3),
     c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
     c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
     c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
  
  .test.incorrect.format(c('study1', 'study2', 'study3'),
     c(ds.test_env$ip_address_2,ds.test_env$ip_address_3),
     c(ds.test_env$user_1,ds.test_env$user_3),
     c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
     c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
  
  .test.incorrect.format(c('study1', 'study2', 'study3'),
     c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3),
     c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
     c(ds.test_env$password_2,ds.test_env$password_3),
     c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
  
  
  .test.incorrect.format(c('study1', 'study2', 'study3'),
     c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3),
     c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
     c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
     c("TESTING.DATASET1","TESTING.DATASET3"))
  
  .test.empty()
  
})

context('ds.login():http_connection::multiple')
test_that('http connection multiple',
{
  .test.http.connection.multiple()
})

context('ds.login():http_connection::single')
test_that('http connection single',
{
    .test.http.connection.single()
})

context('ds.login():https_connection::multiple')
test_that('https connection multiple',
{
 
  .test.https.connection.multiple()
})

context('ds.login():https_connection::single')
test_that('https connection single',
{
  .test.https.connection.single()
})

context('ds.login()::incorrect_url::single')
test_that('https connection single',
{
  .test.https.incorrect.URL()
})
}
