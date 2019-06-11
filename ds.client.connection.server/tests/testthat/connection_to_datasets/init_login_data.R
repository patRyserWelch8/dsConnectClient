init.correct.data <- function()
{
  server <- c('study1', 'study2', 'study3')
  url <- c('https://datashield.example.org','https://datashield.example.org','https://datashield.example.org')
  user <- c('administrator','administrator','administrator')
  password <- c('datashield_test&','datashield_test&','datashield_test&')
  table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
  return(ds.build.login.data.frame.o(server,url,table,user,password))
}

init.incorrect.url <- function()
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

init.incorrect.url.http <- function()
{
  server <- c('study1', 'study2', 'study3')
  url <- c('http://192.168.56.100:8080','http://192.168.56.100:8080','http://192.168.56.100:8080')
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


