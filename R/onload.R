.onLoad <- function(libname, pkgname) {
  lemur::check_API_key()
  lemur::set_model()
}
