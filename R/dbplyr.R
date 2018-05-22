#' @export 
selected <- function(.data, ...) UseMethod('selected')

#' @export 
selected.default <- function(.data, ...) {
	dplyr::select(.data, ...)
}

#' @export 
selected.tbl_sql <- function(.data, ...) {
	dplyr::collect(dplyr::select(.data, ...))
} 

#' @export 
selected.tbl_ffdf <- function(.data, ...) {
	.dots = lazyeval::lazy_dots(...)
	vars = auto_names(.dots)
	.data[,vars]
} 

#' @export 
selected.environment <- function(env, ...) {
	.dots = lazyeval::lazy_dots(...)
	vars = auto_names(.dots)
	data.frame(mget(vars, envir = env)) 
}

#' @export 
readnames <- function(df) UseMethod('readnames')

#' @export 
readnames.default <- function(df) {
	vars   = names(df)
	vars   = paste0(deparse(substitute(df)), '$', vars)
	cat(vars, sep ='\n', file = 'objbrowser.R')
	# label  = get_label(df)
	# layout = data.frame(vars, label)
	# layout = knitr::kable(layout,format = 'markdown')
	# cat(layout, sep ='\n', file = 'objbrowser.R')
}

#' @export 
readnames.tbl_sql <- function(df) {
	z = colnames(df)
	z = sort(z)
	z = paste0(deparse(substitute(df)), '$', z)
	cat(z, sep ='\n', file = 'objbrowser.R')
}

#' @export 
summary.tbl_sql <- function(env) {
	library(ggplot2)
	name = colnames(env)
	n_col = length(name)

	classes = vector('character', n_col)
	res = vector('list', n_col)
	gplot = vector('list', n_col)

	foreach::foreach(i = 1:n_col) %dopar% {
		vec = unlist(collect(select_(env,name[i])))  
		classes[i] = class(vec)
		res[[i]] = summary(vec)
		if(n_distinct(vec) <=10) {
			gplot[[i]] = qplot(as.factor(vec), xlab = name[i])
		} else {
			gplot[[i]] = qplot(vec, binwidth = (max(vec) - min(vec))/15, xlab = name[i])
		}
	}
	names(res) = names(gplot) = name

	resnum = res[classes == 'numeric' | classes == 'integer'] 
	if(length(resnum) != 0) {
		resnum  %<>% rbind.data.frame %>% t
		colnames(resnum) =  c('N', "NA's", 'Mean', 'Sd', 'Min', 'P10', 'Median', 'P90', 'Max')
	}

	reschar = res[classes == 'character'] 
	if(length(reschar) != 0) {
		reschar  %<>% rbind.data.frame %>% t
		colnames(reschar) =  c('Class', 'N', "NA's")
		class(reschar) = 'table'
	}

	resfac = res[classes == 'factor']
	list('byVariables' = res, 'Plots' = gplot, 'Numeric' = resnum , 'Character' = reschar, 'Factor' = resfac)
}

