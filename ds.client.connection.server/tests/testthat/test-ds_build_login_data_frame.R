library(DSI)
library(DSOpal)
library(testthat)

source("definition_tests/def-ds_build_login_data_frame.r")



context('ds.build.login.data.frame()::correct format::multiple')
test_that ('The login information is correct ',
{
  .test.correct.data(c('study1', 'study2', 'study3'),
          c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3),
          c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
          c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
          c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
  
  .test.correct.data(c('study1', 'study2'),
          c(ds.test_env$ip_address_1,ds.test_env$ip_address_2),
          c(ds.test_env$user_1,ds.test_env$user_2),
          c(ds.test_env$password_1,ds.test_env$password_2),
          c("TESTING.DATASET1", "TESTING.DATASET2"))
  
  .test.correct.data(c('study2', 'study3'),
          c(ds.test_env$ip_address_2,ds.test_env$ip_address_3),
          c(ds.test_env$user_2,ds.test_env$user_3),
          c(ds.test_env$password_2,ds.test_env$password_3),
          c("TESTING.DATASET2", "TESTING.DATASET3"))
  
  .test.correct.data(c('study1', 'study3'),
          c(ds.test_env$ip_address_1,ds.test_env$ip_address_3),
          c(ds.test_env$user_1,ds.test_env$user_3),
          c(ds.test_env$password_1,ds.test_env$password_3),
          c("TESTING.DATASET1", "TESTING.DATASET3"))
})

context('ds.build.login.data.frame()::correct format::single')
test_that ('The login information is correct ',
{
  .test.correct.data(c('study1'),
          c(ds.test_env$ip_address_1),
          c(ds.test_env$user_1),
          c(ds.test_env$password_1),
          c("TESTING.DATASET1"))
  
  .test.correct.data(c('study2'),
          c(ds.test_env$ip_address_2),
          c(ds.test_env$user_2),
          c(ds.test_env$password_2),
          c("TESTING.DATASET2"))
  
  .test.correct.data(c('study3'),
          c(ds.test_env$ip_address_3),
          c(ds.test_env$user_3),
          c(ds.test_env$password_3),
          c("TESTING.DATASET3"))
})

context('ds.build.login.data.frame()::incorrect url::multiple')
test_that ('At least one url is incorrect ',
{
  .test.incorrrect.url (c('study1', 'study2', 'study3'),
  c('//192.168.56.100:8080',ds.test_env$ip_address_2,ds.test_env$ip_address_3),
  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
  c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
 
  .test.incorrrect.url (c('study1', 'study2', 'study3'),
  c('//192.168.56.100:8080','//192.168.56.100:8080',ds.test_env$ip_address_3),
  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
  c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
  
  .test.incorrrect.url (c('study1', 'study2', 'study3'),
  c('//192.168.56.100:8080','//192.168.56.100:8080',ds.test_env$ip_address_3),
  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
  c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
  
  .test.incorrrect.url (c('study1', 'study2', 'study3'),
  c('//192.168.56.100:8080','//192.168.56.100:8080','//192.168.56.100:8080'),
  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
  c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
  
  .test.incorrrect.url (c('study1', 'study2', 'study3'),
  c('//192.168.56.100:8080',ds.test_env$ip_address_2,'//192.168.56.100:8080'),
  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
  c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
  
  .test.incorrrect.url (c('study1', 'study2', 'study3'),
  c(ds.test_env$ip_address_1,'//192.168.56.100:8080','//192.168.56.100:8080'),
  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
  c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
  
  
})

context('ds.build.login.data.frame()::incorrect url::single')
test_that ('At least one url is incorrect ',
{
  .test.incorrrect.url (c('study1'),
  c('//192.168.56.100:8080'),
  c(ds.test_env$user_1),
  c(ds.test_env$password_1),
  c("TESTING.DATASET1"))
})

context('ds.build.login.data.frame()::incorrect correct format::multiple')
test_that ('The login information is in an incorrect format ',
{
  .test.incorrect.format(c('study2', 'study3'),
          c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3),
          c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
          c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
          c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
  
  .test.incorrect.format(c('study1', 'study2', 'study3'),
          c(ds.test_env$ip_address_1,ds.test_env$ip_address_3),
          c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
          c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
          c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
  
  .test.incorrect.format(c('study1', 'study2', 'study3'),
          c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3),
          c(ds.test_env$user_1,ds.test_env$user_2),
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
          c("TESTING.DATASET1", "TESTING.DATASET3"))
  
})


context('ds.build.login.data.frame():: incorrect format::single')
test_that ('The login information is an incorrect format',
{
  .test.incorrect.format(c(),
          c(ds.test_env$ip_address_1),
          c(ds.test_env$user_1),
          c(ds.test_env$password_1),
          c("TESTING.DATASET1"))
  
  .test.incorrect.format(c('study2'),
          c(),
          c(ds.test_env$user_2),
          c(ds.test_env$password_2),
          c("TESTING.DATASET2"))
  
  .test.incorrect.format(c('study3'),
          c(ds.test_env$ip_address_3),
          c(ds.test_env$user_3),
          c(),
          c("TESTING.DATASET3"))
  
  .test.incorrect.format(c('study3'),
          c(ds.test_env$ip_address_3),
          c(),
          c(ds.test_env$password_3),
          c("TESTING.DATASET3"))
  
  .test.incorrect.format(c('study3'),
          c(ds.test_env$ip_address_3),
          c(),
          c(ds.test_env$password_3),
          c())
})


