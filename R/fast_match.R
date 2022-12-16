
#' Fast matching
#' 
#' A faster version of `%in%`.
#' 
#' See [fastmatch::fmatch()]
#'
#' @param x values to be matched
#' @param table values to be matched against
#'
#' @return Logical indicating match or not
#' @export
#'
#' @keywords internal
#'
#' @examples
#' 
#' \dontrun{
#' 
#' x %fin% y
#' 
#' }
#' 
#' 
#' 
`%fin%` <- function(x, table) {
  
  fastmatch::fmatch(x, table, nomatch = 0L) > 0L
  
}