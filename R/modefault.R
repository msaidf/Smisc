#' @export 
refine <- function(df) {
    name = deparse(substitute(df))
    filename = paste0(getwd(), '/', name , '.csv')
    rio::export(df, filename)
    rrefine::refine_upload(filename, name)
}

#' @export 
refdel <- function(df) {
    name = deparse(substitute(df))
    rrefine::refine_delete(name)
}

#' @export 
str_insert  <- function(x, pos, char) {
    stringr::str_c(
                   stringr::str_sub(x, end=pos-1), 
                   char, 
                   stringr::str_sub(x, start=pos)
                   )
}

#' @export 
tablenotes <- function(file, notes) {
	tex = readLines(file) 
	tex = c(  '\\begin{table}' , '\\begin{threeparttable}' , tex[3:(length(tex)-2)] , '\\begin{tablenotes}' , '\\scriptsize' , notes , '\\end{tablenotes}' , '\\end{threeparttable}' , '\\end{center}' , '\\end{table}')
	cat(tex, sep = '\n' , file = file) }

#' @export 
object.size <- function(x) format(object.size(x), 'auto')

#' @export 
saveobj <- function(x) save(list = deparse(substitute(x)) , file = paste0(deparse(substitute(x)), '.rda'))
{

}
#' @export 
loadobj <- function(x, type=c('fst','feather','rda')) {
    if(missing(type)) type=getOption('format')
    if(is.null(type)) type='rda'
    if(format=='fst') paste0(deparse(substitute(x))) <<- fst::read.fst(paste0(deparse(substitute(x)), '.fst'))
    else if(format=='feather') paste0(deparse(substitute(x))) <<- fst::read_feather(paste0(deparse(substitute(x)), '.feather'))
    else load(paste0(deparse(substitute(x)), '.rda'), envir = .GlobalEnv)
}

#' @export 
saveall <- function(path) {
    if(missing(path)) {
        if(!dir.exists('Rda')) dir.create('Rda')
        path='Rda'
    }
	for(obj in ls(.GlobalEnv)) {
		save(list=obj, file = file.path(path.expand(path), paste0(obj, '.rda')))
	}
}

#' @export 
loadrdas <- function(path) {
    if(missing(path)) path='Rda' 
	for(obj in system(paste('ls', file.path(path.expand(path), '*.rda')))) load(obj)
}

#' @export 
loadpack <- function(x) {
	z = deparse(substitute(x))
	if(!require(z, character.only=T)) 
	install.packages(z)
	library(z, character.only=T)
}

#' @export 
install.ver <- function(pkg, ver) {
    packageUrl = paste0("https://cran.r-project.org/src/contrib/Archive/", pkg, "/", pkg, "_", ver, ".tar.gz")
    install.packages(packageUrl, repos=NULL, type='source')
}


# #' @export 
# sum <- function (..., na.rm = T) base::sum(..., na.rm = na.rm)
# 
# #' @export 
# mean.default <- function (..., na.rm = T) base::mean.default(..., na.rm = na.rm)
# 
# #' @export 
# var <- function (..., na.rm = T) stats::var(..., na.rm = na.rm)
# 
# #' @export 
# cor <- function (..., use = "na.or.complete") stats::cor(..., use = use)
# 
# #' @export 
# cov <- function (..., use = "na.or.complete") stats::cov(..., use = use)
# 
# #' @export 
# sd <- function (..., na.rm = T) stats::sd(..., na.rm = na.rm)
# 
# #' @export 
# quantile <- function(..., probs = seq(0, 1, 0.1), na.rm = TRUE) 
# 	stats::quantile(..., probs = probs, na.rm = na.rm)

# Summary Functions ----------------------------------------
#' @export 
glance  <- function(df) {
    gldf=setNames(
             data.frame(format(names(df),justify='left'),
                        t(sapply(df, function(x) c(
                             class(x),
                             length(x) - sum(is.na(x)),
                             sum(is.na(x)),
                             length(unique(x)),
                             sum(duplicated(x))
                             )
                          )
                          )
						,format(try(sjmisc::get_label(df)),justify='left')
                        ),
             c('Var', 'Class', 'N', 'NA', 'Unique', 'Dups', 'Label' )
             )
	row.names(gldf) = NULL
	gldf
}

#' @export
quantile_factory <- function(x) {
	function(y, ...) quantile(y, probs = x, ... )
}

#' @export
q1 = quantile_factory(.1)

#' @export
q9 = quantile_factory(.9)


