.onLoad <- function(libname, pkgname) 
  {
  error_message <- data.frame(error_code = c("ERR::001", "ERR::002"), error_message = c("blah", "blih"))
  assign("error_message", error_message, envir = parent.env(environment()))
}