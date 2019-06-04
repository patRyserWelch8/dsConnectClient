library(opal)
library(testthat)


init.correct.data <- function()
{
  server <- c('study1', 'study2', 'study3')
  url <- c('https://datashield.example.org','https://datashield.example.org','https://datashield.example.org')
  user <- c('administrator','administrator','administrator')
  password <- c('datashield_test&','datashield_test&','datashield_test&')
  table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
  return(ds.build.login.data.frame.o(server,url,table,user,password))
}

init.incorrect.url.http <- function()
{
  server <- c('study1', 'study2', 'study3')
  url <- c('//192.168.56.100:8080','https://datashield.example.org','https://datashield.example.org')
  user <- c('administrator','administrator','administrator')
  password <- c('datashield_test&','datashield_test&','datashield_test&')
  table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
  return(ds.build.login.data.frame.o(server,url,table,user,password))
}


init.incorrect.server <- function()
{
  server <- c('study1')
  url <- c('https://datashield.example.org','https://datashield.example.org','https://datashield.example.org')
  user <- c('administrator','administrator','administrator')
  password <- c('datashield_test&','datashield_test&','datashield_test&')
  table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
  return(ds.build.login.data.frame.o(server,url,table,user,password))
}

init.incorrect.user <- function()
{
  server <- c('study1', 'study2', 'study3')
  url <- c('https://datashield.example.org','https://datashield.example.org','https://datashield.example.org')
  user <- c('administrator','administrator')
  password <- c('datashield_test&','datashield_test&','datashield_test&')
  table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
  return(ds.build.login.data.frame.o(server,url,table,user,password))
}


init.incorrect.url <- function()
{
  server <- c('study1', 'study2', 'study3')
  url <- c('//192.168.56.100:8080')
  user <- c('administrator','administrator','administrator')
  password <- c('datashield_test&','datashield_test&','datashield_test&')
  table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
  return(ds.build.login.data.frame.o(server,url,table,user,password))
}

init.incorrect.password <- function()
{
  server <- c('study1', 'study2', 'study3')
  url <- c('https://datashield.example.org','https://datashield.example.org','https://datashield.example.org')
  user <- c('administrator','administrator')
  password <- c('datashield_test&','datashield_test&','datashield_test&')
  table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
  return(ds.build.login.data.frame.o(server,url,table,user,password))
}

init.incorrect.table <- function()
{
  server <- c('study1', 'study2', 'study3')
  url <- c('https://datashield.example.org','https://datashield.example.org','https://datashield.example.org')
  user <- c('administrator','administrator','administrator')
  password <- c('datashield_test&','datashield_test&','datashield_test&')
  table <- c('DASIM.DASIM2', 'DASIM.DASIM3')
  return(ds.build.login.data.frame.o(server,url,table,user,password))
}


context('<START>ds.build.login.data.frame</START>')
context('Incorrect format')
test_that('Incorrect format',
          {

            expect_error(init.incorrect.server())
            expect_error(init.incorrect.url())
            expect_error(init.incorrect.password())
            expect_error(init.incorrect.user())
            expect_error(init.incorrect.table())
          })

context('correct format')
test_that ('The number of columns is equal five -- correct data',
           {
             login.info <- init.correct.data()
             expect_that(login.info,is_a('data.frame'))
             expect_that(length(login.info), equals(5))
             print(colnames(login.info)[1])
             expect_that(colnames(login.info)[1], equals('server'))
             expect_that(colnames(login.info)[2], equals('url'))
             expect_that(colnames(login.info)[3], equals('user'))
             expect_that(colnames(login.info)[4], equals('password'))
             expect_that(colnames(login.info)[5], equals('table'))
           })

context('HTTPS connection')
test_that ('The url start with http(s)',
           {

             expect_error(init.incorrect.url.http())
             login.info <- init.correct.data()
             url <- as.vector(login.info$url)
             expect_true(all(startsWith(url,'http')))

           })

context('<END>ds.build.login.data.frame</END>')
