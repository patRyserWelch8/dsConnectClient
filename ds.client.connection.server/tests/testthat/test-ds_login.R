library(DSI)
library(testthat)

source("connection_to_datasets/init_login_data.R")


context('ds.login:incorrect format')
test_that('incorrect format',
{
    print(ds.login(init.incorrect.server()))
  #  expect_error(ds.login(init.incorrect.server()))
  #  expect_error(ds.login(init.incorrect.url()))
  #  expect_error(ds.login(init.incorrect.password()))
  #  expect_error(ds.login(init.incorrect.user()))
  #  expect_error(ds.login(init.incorrect.table()))
})

context('ds.login:correct format')
test_that ('The number of columns is equal five -- correct data',
{
             login.info <- init.correct.data()
             expect_that(login.info,is_a('data.frame'))
             expect_that(length(login.info), equals(5))
             expect_that(colnames(login.info)[1], equals('server'))
             expect_that(colnames(login.info)[2], equals('url'))
             expect_that(colnames(login.info)[3], equals('user'))
             expect_that(colnames(login.info)[4], equals('password'))
             expect_that(colnames(login.info)[5], equals('table'))
})

context('ds.login:HTTPS connection')
test_that ('The url start with http(s)',
{
             
             expect_error(init.incorrect.url.http())
             login.info <- init.correct.data()
             url <- as.vector(login.info$url)
             expect_true(all(startsWith(url,'http')))
             
})


