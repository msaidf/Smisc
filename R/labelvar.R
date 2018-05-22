#' Wrapper for write.dta 
#' 
#' Piping-ready and easy labelling variables in the resulted stata file 
#' @param labels vector with same length as number of columns, the default is using variables' label attributes that comes from Hmisc package
#' @examples 
#' write_dta(mtcars, file = '~/test.dta') # use Hmisc's variable labels
#' mtcars %>>% write_dta(file = '~/test.dta') # piping-ready, equivalent with above
#' write_dta(mtcars, file = '~/test.dta', labels = labelvar) # use own-supplied label vector
#' @export
write_dta <- function(dataframe, file, labels=NULL, ...) {
	if(is.null(labels)) labels = Hmisc::label(dataframe)
	data.temp = dataframe
	attributes(data.temp)$var.labels = labels
	foreign::write.dta(file = file,dataframe = data.temp, ...)
}

#' Clearing labels created by Hmisc's package that sometimes cause troubles for other functions
#' @export
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}
