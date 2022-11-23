.onLoad <- function(libname, pkgname){
}

.onAttach <- function(libname,pkgname){
  startMessage()
}

startMessage <- function() {
  packageStartupMessage("R Speaks Simulx!")
}