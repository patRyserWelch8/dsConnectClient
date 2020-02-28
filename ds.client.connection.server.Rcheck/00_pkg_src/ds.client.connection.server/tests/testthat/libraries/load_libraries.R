
load.libraries <- function()
{
  print('Loading packages....')
 
  package.loaded = require('opalr')
  if (!package.loaded)
  {
     install.packages('opalr',repos='http://cran.obiba.org')
     library('opalr')
  }
  
  package.loaded = require('DSI')
  if (!package.loaded)
  {
    devtools::install_github('datashield/DSI', force=TRUE)
    library('DSI')
  }
  
  package.loaded = require('DSOpal')
  if (!package.loaded)
  {
    devtools::install_github('datashield/DSOpal', force=TRUE)
    library('DSOpal')
  }
  
  
 
  
  package.loaded = require('RCurl')
  if (!package.loaded)
  {
    install.packages('RCurl')
    library('RCurl')
  }
  
  #package.loaded = require('dsBase')
  #if (!package.loaded)
  #{
  #  install.packages('dsBase',repos='http://cran.obiba.org')
  #  library('dsBase')
  #}

  #print('dsModelling')
  #package.loaded = require('dsModelling')
  #if (!package.loaded)
  #{
  #  install.packages('dsModelling',repos='http://cran.obiba.org')
  #  library('dsModelling')
  #}
  
  #package.loaded = require('dsGraphics')
  #if (!package.loaded)
  #{
  #  install.packages('dsGraphics',repos='http://cran.obiba.org')
  #  library('dsGraphics')
  #}

  
  #package.loaded = require('dsStats')
  #if (!package.loaded)
  #{
  #  install.packages('dsStats',repos='http://cran.obiba.org')
  #  library('dsStats')
  #}
}