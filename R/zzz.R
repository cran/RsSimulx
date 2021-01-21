.onLoad <- function(libname, pkgname){
  if (length(find.package("lixoftConnectors", quiet = TRUE))) {
    op <- options()
    op$lixoft_notificationOptions$info <- 1
    options(op)
    eval.parent(parse(text = 'r <- lixoftConnectors::initializeLixoftConnectors(software = "simulx", force = TRUE)'))
  }
}

.onAttach <- function(libname,pkgname){
  startMessage()
}

startMessage <- function() {
  if (!length(find.package("lixoftConnectors", quiet = TRUE))) {
    packageStartupMessage(paste0(
"You need to install the lixoftConnectors package in order to use RsSimulx.
See http://monolix.lixoft.com/monolix-api/lixoftconnectors_installation/ for more information."
    ))
  } else {
    version <- eval.parent(parse(text = 'r <- lixoftConnectors::getLixoftConnectorsState()$version'))
    v <- regmatches(version, regexpr("^[0-9]*", version, perl = TRUE))
    if (length(v)) {
      if (as.numeric(v) < 2020) {
        packageStartupMessage("RsSimulx package does not work for lixoftConnectors package with a  version < 2020.")
      }
    }
  }
  packageStartupMessage("R Speaks Simulx!")
}