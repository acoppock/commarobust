.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Hello! The commarobust package has been entirely replaced by the estimatr package, which you can install with
    install.packages('estimatr').
    The commarobust package will no longer be maintained, but if you need the last working version, use
    remotes::install_github('acoppock/commarobust', ref = 'legacy')"
  )
}
