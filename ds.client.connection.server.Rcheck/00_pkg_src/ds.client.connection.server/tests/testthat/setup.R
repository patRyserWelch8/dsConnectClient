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

options(show.error.messages = FALSE)
options()
print("setup")
connect.all.datasets ()



print("setup finished")