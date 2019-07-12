library(DSI)
library(DSOpal)
library(testthat)

source("definition_tests/init_login_data.R")


context('ds.login:incorrect format')
test_that('incorrect format',
{
  error <-  ds.login(NULL)
  print(error)
  #§ expect_true(identical(error, "ERR:003"))
  
     
   l <- ds.login(init.correct.data())
   print(l)
  # expect_error(ds.login(init.incorrect.server()))
  # expect_error(ds.login(init.incorrect.url()))
  # expect_error(ds.login(init.incorrect.password()))
  # expect_error(ds.login(init.incorrect.user()))
   #¢expect_error(ds.login(init.incorrect.table()))
   
})

