#' @export
`%X%` <- function(x,y) if(length(x)==1 | length(y)==1) x*y else x%*%y

#' @export
encol <- function(w) { if( is.null(dim(w))) 1 else ncol(w) }

#' @export
enrow <- function(w) { if( is.null(dim(w))) length(w) else nrow(w) }

