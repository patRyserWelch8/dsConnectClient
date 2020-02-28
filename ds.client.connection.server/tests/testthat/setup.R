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



library(DSOpal)
library(DSI)
ds.test_env <- new.env()
source("connection_to_datasets/login_details.R")
source("connection_to_datasets/init_all_datasets.R")


#ds.test_env <- new.env()
#options(datashield.env=ds.test_env)

options(show.error.messages = FALSE)
options()
print("setup")


connections <- connect.all.datasets()
disconnect.all.datasets(connections)

print("setup finished")