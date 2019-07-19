#-------------------------------------------------------------------------------
# Copyright (c) 2019 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------
#
# Datashield test suite set up
#

library(DSI)
library(DSOpal)

library(dsBaseClient)
library(RCurl)

source("connection_to_datasets/login_details.R")
source("connection_to_datasets/init_all_datasets.R")
#source("libraries/load_libraries.R")

#options(show.error.messages = FALSE)


init.all.datasets()
#
print(ds.test_env$ping_address)
#connect to a server
context("VM problems")
test_that("The virtual machine is loaded. ",
{      
    expect_that(url.exists(ds.test_env$ping_address, timeout=5), is_true())
    print("A server is available")
})

#connect to an Opal or DSI ....
print ("connect to server")
if (ds.test_env$context == ds.test_env$contexts[1] 
   || ds.test_env$context == ds.test_env$contexts[2] )
{
  log.in.data.server()
}

test_that("The number of servers the same has setup",
{
  expect_true(length(ds.test_env$connection.DSI) == length(ds.test_env$server))
})

context("The number of rows of the test data are the same on the server and locally")
test_that("The of rows are the same",
{
  dimensions <- ds.dim(x='D',type='combine',datasources = ds.test_env$connection.DSI)
  expect_true(dimensions[[1]][1] == nrow(ds.test_env$local.values))
})

log.out.data.server()
