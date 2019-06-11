library(DSI)
library(DSOpal)
library(testthat)


source("connection_to_datasets/init_login_data.R")
options(warn=-1)

context('ds.build.login.data.frame:incorrect format')
test_that('Incorrect format',
{
    expect_true(identical(init.incorrect.server(), "ERR:001"))
    expect_true(identical(init.incorrect.url(), "ERR:001"))
    expect_true(identical(init.incorrect.password(),"ERR:001"))
    expect_true(identical(init.incorrect.user(),"ERR:001"))
    expect_true(identical(init.incorrect.table(), "ERR:001"))
})

context('ds.build.login.data.frame:correct format')
test_that ('The number of columns is equal five -- correct data',
{
    login.info <- init.correct.data()
    expect_true(length(login.info) == 5)
    expect_that(login.info,is_a('data.frame'))
    expect_that(colnames(login.info)[1], equals('server'))
    expect_that(colnames(login.info)[2], equals('url'))
    expect_that(colnames(login.info)[3], equals('user'))
    expect_that(colnames(login.info)[4], equals('password'))
    expect_that(colnames(login.info)[5], equals('table'))
})

context('ds.build.login.data.frame:HTTPS connection')
test_that ('The url start with https',
{

   expect_true(identical(init.incorrect.url.http(),"ERR:002"))
    login.info <- init.correct.data()
    url <- as.vector(login.info$url)
    expect_true(all(startsWith(url,'https')))

})


