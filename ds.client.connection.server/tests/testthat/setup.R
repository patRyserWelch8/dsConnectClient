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
library(testthat)
library(httr)
library(dsBaseClient)

ds.test_env <- new.env()
source("connection_to_datasets/login_details.R")
source("connection_to_datasets/init_all_datasets.R")


#ds.test_env <- new.env()
#options(datashield.env=ds.test_env)

options(show.error.messages = FALSE)

print("setup - Check connections and server functions")

connections <- connect.all.datasets()
server.functions <- c("existsDS","removeDS")
aggregate.functions <- datashield.methods(connections,type="aggregate")
if(all(server.functions %in% aggregate.functions[,"name"]))
{ 
  print("All the functions have been uploaded on the server")
}
 
print("setup finished")