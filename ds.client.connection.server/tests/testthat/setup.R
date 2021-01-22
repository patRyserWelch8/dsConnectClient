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


options(show.error.messages = TRUE)


print("setup - Check connections and server functions")


connections <- connect.all.datasets(ds.test_env)

server.functions <- c("existsDS","removeDS","assignCoordinatesDS",
                      "assignDataDS", "assignParamSettingsDS",
                      "getDataDS", "getCoordinatesDS",
                      "assignSharingSettingsDS", "decryptDataDS",
                      "encryptDataDS", "decryptParamDS",
                      "encryptParamDS", "removeEncryptingDataDS")

aggregate.functions <- datashield.methods(connections,type="aggregate")

function.found <- server.functions %in% aggregate.functions[,"name"]
#print(function.found)
#print(!(server.functions %in% aggregate.functions[,"name"]))
#print(server.function[function.found]) 
if(all(function.found))
{ 
  print("All the functions have been uploaded on the server")
}


print("setup finished")