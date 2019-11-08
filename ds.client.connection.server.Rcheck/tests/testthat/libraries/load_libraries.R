
load.libraries <- function()
{
  print('Loading packages....')
  print('dsBase')

  package.loaded = require('dsBase')
  if (!package.loaded)
  {
    install.packages('dsBase',repos='http://cran.obiba.org')
    library('dsBase')
  }

  print('dsModelling')
  package.loaded = require('dsModelling')
  if (!package.loaded)
  {
    install.packages('dsModelling',repos='http://cran.obiba.org')
    library('dsModelling')
  }

  print('dsGraphics')
  package.loaded = require('dsGraphics')
  if (!package.loaded)
  {
    install.packages('dsGraphics',repos='http://cran.obiba.org')
    library('dsGraphics')
  }

  print('dsStats')
  package.loaded = require('dsStats')
  if (!package.loaded)
  {
    install.packages('dsStats',repos='http://cran.obiba.org')
    library('dsStats')
  }
}