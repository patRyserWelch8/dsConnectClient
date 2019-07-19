readUrl <- function(url) 
{
  out <- tryCatch(
    {.script(url)},
    warning = function(cond) {.warning(url)},
    error = function(cond) {.error(url)},
    finally = {.finally(url)}
  )    
  return(out)
}


.script <- function(url)
{
  readLines(con = url, warn = FALSE) 
}



.warning <- function(url)
{
  message(paste("Reading the URL caused a warning:", url))
  message("Here's the original warning message:")
  message(cond)
  
  # Choose a return value when such a type of condition occurs
  return(NULL)
}

.error <- function(url)
{
  message(paste("This seems to be an invalid URL:", url))
  message("Here's the original error message:")
  message(cond)
  
  # Choose a return value when such a type of condition occurs
  return(NA)
}

.finally <- function(url)
{
  message(paste("Processed URL:", url))
  message("Some message at the end\n")
}

y <- readUrl(urls[1])
y <- readUrl(urls[2])
y <- readUrl(urls[3])

urls <- c(
  "http://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html",
  "http://en.wikipedia.org/wiki/Xz",
  "I'm no URL"
)

print(y)