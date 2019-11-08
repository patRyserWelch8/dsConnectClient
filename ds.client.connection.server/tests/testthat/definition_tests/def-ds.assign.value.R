
source("connection_to_datasets/init_all_datasets.R")

.test.all.parameters.correct <- function()
{
  init.all.datasets()
  connection <- ds.login(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  ds.assign.value (connection, new.variable.name = "test.var", value  = "5", list.variables="INTEGER", symbol = "D")
}