library(DSI)
library(DSOpal)
library(testthat)

source("definition_tests/def-ds_build_login_data_frame.r")
#source("connection_to_datasets/init_all_datasets.R")


datasets <- build.all.datasets()
studies  <- c('study1', 'study2', 'study3')
ssl      <- build.all.ssl()


context('ds.build.login.data.frame()::incorrect parameters::multiple')
test_that("incorrect server names",
{
 

 
  .test.incorrrect.url()
  .test.incorrrect.url(studies)
  .test.incorrrect.url(studies,login.details$get_ip_addresses(3))
  .test.incorrrect.url(studies,login.details$get_ip_addresses(3),datasets)
  .test.incorrrect.url(studies,login.details$get_ip_addresses(3),datasets,login.details$get_users(3))
  .test.incorrrect.url(studies,login.details$get_ip_addresses(3),datasets,login.details$get_users(3), 
                       login.details$get_passwords(3))
  .test.incorrrect.url(studies,login.details$get_ip_addresses(3),datasets,login.details$get_users(3), 
                       login.details$get_passwords(3),ssl[c(1,2,3)])
  
  
})



context('ds.build.login.data.frame()::incorrect url::multiple')
test_that ('At least one url is incorrect ',
{
  .test.incorrrect.url (c('study1', 'study2', 'study3'),
  c('//192.168.56.100:8080',ds.test_env$ip_address_2,ds.test_env$ip_address_3),
  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
  c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"),
  c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)"),
  c("OpalDriver","OpalDriver","OpalDriver"))
 
  .test.incorrrect.url (c('study1', 'study2', 'study3'),
  c('//192.168.56.100:8080','//192.168.56.100:8080',ds.test_env$ip_address_3),
  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
  c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"),
  c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)"),
  c("OpalDriver","OpalDriver","OpalDriver"))
  
  .test.incorrrect.url (c('study1', 'study2', 'study3'),
  c('//192.168.56.100:8080','//192.168.56.100:8080',ds.test_env$ip_address_3),
  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
  c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"),
  c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)"),
  c("OpalDriver","OpalDriver","OpalDriver"))
  
  .test.incorrrect.url (c('study1', 'study2', 'study3'),
  c('//192.168.56.100:8080','//192.168.56.100:8080','//192.168.56.100:8080'),
  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
  c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"),
  c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)"),
  c("OpalDriver","OpalDriver","OpalDriver"))
  
  .test.incorrrect.url (c('study1', 'study2', 'study3'),
  c('//192.168.56.100:8080',ds.test_env$ip_address_2,'//192.168.56.100:8080'),
  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
  c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"),
  c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)"),
  c("OpalDriver","OpalDriver","OpalDriver"))
  
  .test.incorrrect.url (c('study1', 'study2', 'study3'),
  c(ds.test_env$ip_address_1,'//192.168.56.100:8080','//192.168.56.100:8080'),
  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
  c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"),
  c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)"),
  c("OpalDriver","OpalDriver","OpalDriver"))
})



context('ds.build.login.data.frame()::incorrect url::single')
test_that ('At least one url is incorrect ',
{
  .test.incorrrect.url (c('study1'),
  c('//192.168.56.100:8080'),
  c(ds.test_env$user_1),
  c(ds.test_env$password_1),
  c("TESTING.DATASET1"),
  c("c(ssl.verifyhost=0,ssl.verifypeer=0)"),
  c("OpalDriver"))
})


context('ds.build.login.data.frame()::incorrect correct format::multiple')
test_that ('The login information is in an incorrect format ',
{
  .test.incorrect.format(c('study2', 'study3'),
          c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3),
          c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
          c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
          c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"),
          c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)"),
          c("OpalDriver","OpalDriver","OpalDriver"))
  
  .test.incorrect.format(c('study1', 'study2', 'study3'),
          c(ds.test_env$ip_address_1,ds.test_env$ip_address_3),
          c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
          c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
          c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"),
          c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)"),
          c("OpalDriver","OpalDriver","OpalDriver"))
  
  .test.incorrect.format(c('study1', 'study2', 'study3'),
          c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3),
          c(ds.test_env$user_1,ds.test_env$user_2),
          c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
          c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"),
          c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)"),
          c("OpalDriver","OpalDriver","OpalDriver"))
  
  .test.incorrect.format(c('study1', 'study2', 'study3'),
          c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3),
          c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
          c(ds.test_env$password_2,ds.test_env$password_3),
          c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"),
          c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)"),
          c("OpalDriver","OpalDriver","OpalDriver"))
  
  .test.incorrect.format(c('study1', 'study2', 'study3'),
          c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3),
          c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3),
          c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3),
          c("TESTING.DATASET1", "TESTING.DATASET3"),
          c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)"),
          c("OpalDriver","OpalDriver","OpalDriver"))
  
  .test.empty()
  
})


context('ds.build.login.data.frame()::incorrect format::single')
test_that ('The login information is an incorrect format',
{
  .test.incorrect.format(c(),
          c(ds.test_env$ip_address_1),
          c(ds.test_env$user_1),
          c(ds.test_env$password_1),
          c("TESTING.DATASET1"),
          c("c(ssl.verifyhost=0,ssl.verifypeer=0)"),
          c("OpalDriver"))
  
  .test.incorrect.format(c('study2'),
          c(),
          c(ds.test_env$user_2),
          c(ds.test_env$password_2),
          c("TESTING.DATASET2"),
          c("c(ssl.verifyhost=0,ssl.verifypeer=0)"),
          c("OpalDriver"))
  
  .test.incorrect.format(c('study3'),
          c(ds.test_env$ip_address_3),
          c(ds.test_env$user_3),
          c(),
          c("TESTING.DATASET3"),
          c("c(ssl.verifyhost=0,ssl.verifypeer=0)"),
          c("OpalDriver"))
  
  .test.incorrect.format(c('study3'),
          c(ds.test_env$ip_address_3),
          c(),
          c(ds.test_env$password_3),
          c("TESTING.DATASET3"),
          c("c(ssl.verifyhost=0,ssl.verifypeer=0)"),
          c("OpalDriver"))
  
  .test.incorrect.format(c('study3'),
          c(ds.test_env$ip_address_3),
          c(),
          c(ds.test_env$password_3),
          c(),
          c(),
          c())
})



context('ds.build.login.data.frame()::correct format::multiple_and_single')
test_that ('The login information is correct ',
{
  
  .test.correct.data(studies[c(1,2,3)],
          login.details$get_ip_addresses(3),
          login.details$get_users(3),
          login.details$get_passwords(3),
          datasets[c(1,2,3)],
          ssl[c(1,2,3)],
          login.details$get_drivers(3))
  
  .test.correct.data(studies[c(1,2)],
          login.details$get_ip_addresses(2),
          login.details$get_users(2),
          login.details$get_passwords(2),
          datasets[c(1,2)],
          ssl[c(1,2)],
          login.details$get_drivers(2))
  
  .test.correct.data(studies[c(1)],
          login.details$get_ip_addresses(1),
          login.details$get_users(1),
          login.details$get_passwords(1),
          datasets[c(1)],
          ssl[c(1)],
          login.details$get_drivers(1))
  
})


