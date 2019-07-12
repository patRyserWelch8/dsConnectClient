library(DSI)
library(DSOpal)
library(testthat)

source("definition_tests/def-ds.login.R")


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


