#' Source and print the names of newly created objects
#'
#' will not show the change in old object
#' @export
process <- function(script) 
{
    old = ls()
    source(script)
    print('New Objects:')
    setdiff(ls(), old)
}

#' Toggle any lib path
#'
#' @export
lib_toggle <- function(path, show.lib=TRUE) {
    path = path.expand(path)
    suppressWarnings(if(path %in% .libPaths()) {
        assign('.lib.loc', 
               setdiff(.libPaths(), path), 
               envir = environment(.libPaths))
    } else {
        assign('.lib.loc', 
               c(path, .libPaths()), 
               envir = environment(.libPaths))
    })
    .libPaths()
}

#' Light replacement for devtools::dev_mode
#'
#' @export
lib_dev <- function(path) lib_toggle(ifelse(missing(path), '~/R-dev', path)) 

#' Light replacement for checkpoint and setSnapshot
#'
#' @export
lib_snapshot <- function(snapshotDate) {
    if(missing(snapshotDate)) snapshotDate = getOption('snapshotDate')
    if(is.null(snapshotDate)) stop('specify snapshotDate as a function argument or in options')

    lib_path = c(paste0('~/.checkpoint/', 
                        snapshotDate, 
                        '/lib/', 
                        version$platform, '/', 
                        version$major, '.', 
                        version$minor), 
                 paste0('~/.checkpoint/R-', 
                        version$major, '.', 
                        version$minor))

    suppressWarnings(if(path.expand(lib_path) %in% .libPaths()) options('repos' = 'https://cran.rstudio.com')
                     else options(repos = c(CRAN = paste0('https://mran.microsoft.com/snapshot/', snapshotDate))) 
                     )
    lib_toggle(lib_path, show.lib=FALSE) 
    cat('Package Local Paths:\n')
    print(.libPaths())
    cat('\nPackage Repository:\n')
    print(getOption('repos'))
}

#' @export
install_package <- function(pkg) {
    pkg = deparse(substitute(pkg))
    install.packages(pkg)
}

#' @export
pkg_ins <- function(pkg) {
    pkgs = as.data.frame(installed.packages())
    pkgs = pkgs[,c('Package', 'Version', 'LibPath')]
    rownames(pkgs) = NULL
    if(missing(pkg)) pkgs 
    else {
        pkg = deparse(substitute(pkg))
        pkgs[pkgs$Package %in% pkg,]
    } 
}

#' @export
pkg_move <- function(pkg, from, to) {
    system2(paste0('mv ', file.path(from, pkg), ' ', to)) 
}

#' @export
pkg_loaded <- function(pkg) {
    pkg = deparse(substitute(pkg))
    pkgs = devtools::session_info()$packages
    pkgs[pkgs$package %in% pkg,]
}

#' @export
pkg_unload <- function(pkg) {
    pkg = deparse(substitute)
    devtools::unload(pkg)
}

#' @export
lib_export <- function(csv) {
    pkgs = devtools::session_info()$packages
    write.csv(pkgs, file=csv)
}

#' @export
lib_import <- function(csv) 
{
    pkgs = read.csv(file=csv)
    install <- function(x) 
    {
        if(grepl(pattern = 'CRAN', x[['source']])) {
             devtools::install_version(x[['package']], x[['version']],
                                       "http://cran.us.r-project.org")
                } 
      else if(grepl(pattern = 'Github', x[['source']])) { 
              pkgurl = sub('Github \\((.*)\\)', 
                           '\\1', 
                           x[['source']])
              devtools::install_github(pkgurl)
      }
      else cat('not installed: ', x, '\n')
  }
    apply(pkgs, 1, install)
}

#' Locating rows containing string
#'
#' This function locating row numbers that contain string pattern.
#' @param string vectors containing string
#' @param pattern string pattern
#' @keywords string
#' @export
#' @examples
#' locate(mtcars$make, 'toyota | honda')
locate <- function(string, pattern) {
	require(stringr, quietly = T)
	which(str_detect(string,ignore.case(pattern)))
}

#' recoding based on rows containing string
#'
#' This function locating row numbers that contain string pattern.
#' @param x vector to recode
#' @param string vectors containing strings
#' @param pattern string pattern
#' @param value value to put on vector
#' @keywords string
#' @export
#' @examples
#' recodestring(mtcars$japan, mtcars$make, 'toyota | honda', 1)
recodestring <- function(x,strings,pattern, value) {
	require(stringr, quietly = T)
	x[locate(strings, pattern)] = value
}

#' Giving df with column names match with pattern
#'
#' @param df dataframes with column names containing string
#' @param pattern string pattern
#' @keywords string
#' @export
#' @examples
#' dfmatch(mtcars$make, 'to')
dfmatch = function(df, pattern) {
	df[,grep(pattern,names(df),value=T)] 
}

#' Giving column names match with pattern
#'
#' @param df dataframes with column names containing string
#' @param pattern string pattern
#' @keywords string
#' @export
#' @examples
#' colmatch(mtcars$make, 'to')
colmatch = function(df, pattern) {
	grep(pattern,names(df),value=T) 
}

#' Giving vector of string begin with pattern
#'
#' @param string vectors containing string
#' @param pattern string pattern
#' @keywords string
#' @export
#' @examples
#' begin(mtcars$make, 'to')
vbegin = function(string, pattern) {
	grep(paste0('^',pattern,collapse=''),string,value=T) 
}

#' Giving df with column names begin with pattern
#'
#' @param df dataframes with column names containing string
#' @param pattern string pattern
#' @keywords string
#' @export
#' @examples
#' dfbegin(mtcars$make, 'to')
dfbegin = function(df, pattern) {
	df[,grep(paste0('^',pattern,collapse=''),names(df),value=T)] 
}

#' Giving column names begin with pattern
#'
#' @param df dataframes with column names containing string
#' @param pattern string pattern
#' @keywords string
#' @export
#' @examples
#' colbegin(mtcars$make, 'to')
colbegin = function(df, pattern) {
	grep(paste0('^',pattern,collapse=''),names(df),value=T) 
}

#' Giving vector of string end with pattern
#'
#' @param string vectors containing string
#' @param pattern string pattern
#' @keywords string
#' @export
#' @examples
#' end(mtcars$make, 'to')
vend = function(string, pattern) {
	grep(paste0(pattern,'$',collapse=''),string,value=T) 
}

#' Giving column names end with pattern
#'
#' @param df dataframes with column names containing string
#' @param pattern string pattern
#' @keywords string
#' @export
#' @examples
#' colend(mtcars$make, 'to')
colend = function(df, pattern) {
	grep(paste0(pattern,'$',collapse=''),names(df),value=T) 
}

#' Giving df with column names end with pattern
#'
#' @param df dataframes with column names containing string
#' @param pattern string pattern
#' @keywords string
#' @export
#' @examples
#' dfend(mtcars$make, 'to')
dfend = function(df, pattern) {
	df[,grep(paste0(pattern,'$',collapse=''),names(df),value=T)] 
}

#' @export
stata <- function(src = stop("At least 'src' must be specified"),
                  data.in = NULL,
                  data.out = FALSE,
                  stata.path = getOption("RStata.StataPath", stop("You need to set up a Stata path; ?chooseStataBin")),
                  stata.version = getOption("RStata.StataVersion", stop("You need to specify your Stata version")),
                  stata.echo = getOption("RStata.StataEcho", TRUE),
                  stata.quiet = getOption("RStata.StataQuiet", TRUE),
                  ...
                  )
{ 
  ## -------------------------
  ## Data validation and setup
  ## -------------------------
  if (!is.character(src))
    stop("src must be a character")
  
  if (!(is.null(data.in) | is.data.frame(data.in)))
    stop("data.in must be NULL or a data.frame")
  
  if (!is.logical(data.out))
    stop("data.out must be logical")

  if (!is.numeric(stata.version))
    stop("stata.version must be logical")

  if (!is.logical(stata.echo))
    stop("stata.echo must be logical")

  if (!is.logical(stata.quiet))
    stop("stata.quiet must be logical")
  
  OS <- Sys.info()["sysname"]
  OS.type <- .Platform$OS.type
  dataIn <- is.data.frame(data.in)
  dataOut <- data.out[1L]
  stataVersion <- stata.version[1L]
  stataPath <- stata.path[1L]
  stataEcho <- stata.echo[1L]
  stataQuiet <- stata.quiet[1L]
 ## -----
  ## Files
  ## -----
  ## doFile <- tempfile("RStata", fileext = ".do")
  ## tempfile could be misleading if the do source other dos with relative paths
  # doFile <- "RStata.do"
  # on.exit(unlink(doFile), add = TRUE)

  if (dataIn){
    ## dtaInFile <- tempfile("RStataDataIn", fileext = ".dta") # Windows/Stata8 unhappy?
    dtaInFile <- "RStataDataIn.dta"
    on.exit(unlink(dtaInFile), add = TRUE)
    foreign::write.dta(data.in, file = dtaInFile, version = ifelse(stataVersion >= 7, 7L, 6L), ...)
  }  

  if (dataOut) {
    ## dtaOutFile <- tempfile("RStataDataOut", fileext = ".dta") # Windows/Stata8 unhappy?
    dtaOutFile <- "RStataDataOut.dta"
    on.exit(unlink(dtaOutFile), add = TRUE)
  }

  ## -------------------------
  ## Creating the .do file ...
  ## -------------------------
  ## External .do script 'support': KIS
  if (file.exists(src[1L]))
    src <- readLines(src[1L])

  ## put a use at the top of .do if a data.frame is passed to data.in
  if (dataIn)  src <- c(sprintf("use %s",  tools::file_path_sans_ext(dtaInFile)), src)

  ## put a save or saveold at the end of .do if data.out == TRUE
  ## for Stata 14, saveold defaults to a Stata 13 dta file
  ## -> use the (Stata 14 only) saveold option: "version(12)" to allow foreign::read.dta() read compatibility
  if (dataOut)  src <- c(src, sprintf("%s %s%s",
                                      ifelse(stataVersion >= 13, "saveold", "save"),
                                      tools::file_path_sans_ext(dtaOutFile),
                                      ifelse(stataVersion >= 14, ", version(12)", "") ))
	src = c('set more off', src, 'exit, clear STATA')
	con = file('_RStata.do', 'w')
	writeLines(src, con)
	close(con)
	# on.exit(unlink('_RStata.do'), add = TRUE)
	system(paste0('wine \"', stataPath, '\" /e /q do _RStata.do'))
	# stataLog = readLines(rd) # rd gives meaningful output for native unix command piping
	stataLog = readLines('_RStata.log') # windows stata gives log by default

 	if (stataEcho) cat(stataLog, sep = '\n') 
	# on.exit(unlink('_RStata.log'), add = TRUE)

	if (dataOut){
		res <- foreign::read.dta(dtaOutFile, ...)
		invisible(res) 
	}
}

#' a function to extract numeric vector from shiny text input

#' @export
numextract = function(string) {
	string %>%
	regmatches(gregexpr("[[:digit:]]+\\.*[[:digit:]]*",.)) %>%
	unlist(use.names=FALSE) %>%
	as.numeric
}


#' a function to show table using Shiny and DataTables

#' @export
stable = function(table) {
  library(shiny)
  runApp(list(
    ui = basicPage(
      dataTableOutput('mytable')
      ),
    server = function(input, output) {
      output$mytable = renderDataTable({
        table
      })
      }
  ))
}

#' rmvn is a function to take random values from multivariate normal distribution with 'n' number of observation, mean 'u', and variance-covariance matrix 'sigma'
#' @export
rmvn = function(n =1, u=0, sigma=1) { 
  k = length(u) 
  s = chol(sigma) 
  x = matrix(rnorm(n*k),nrow=n,ncol=k)
  x = x %*% s + rep(1,n) %*% t(u) 
  return(x)
  }
# EXAMPLE:
# rmvn( n = 10 , u = c(1,2) , sigma = diag(2))
#             [,1]       [,2]
#  [1,] -0.2462700 -0.1416372
#  [2,]  0.9486454  0.1489186
#  [3,]  1.1315778  2.2862960
#  [4,]  1.3605292  2.1125775
#
#' recoding from the old value to new value
#'
#' @param data is the dataframe
#' @param varvector is a vector with string elements of variable names.
#' @param lowbound is the lower bound of values, whatever above that bound will be recoded
#' @param upperbound is the upper bound of values, whatever below that bound will be recoded
#' @param not.value is the value which whatever that is not that value will be recoded
#' @param old.value is the value which will be recoded
#' @param new.value is the value which will become the replacement
#' @keywords recode
#' @export 
#' @examples 
#' new.data <- recoding(old.data,c('v1','v2'), 3,0)
#' new.data <- recode.na(old.data,c('v1','v2'), 96)

recode_eq <- function(data,varvector,old.value,new.value) {
x <- data
for(y in varvector) x[x[y]==old.value & !is.na(x[y]),y]<-new.value
return(x)
}

recode_inv <- function(data,varvector,not.value,new.value) {
x <- data
for(y in varvector) x[x[y]!=not.value & !is.na(x[y]),y]<-new.value
return(x)
}

recode_up <- function(data,varvector,upperbound,new.value) {
x <- data
for(y in varvector) x[x[y]<=upperbound & !is.na(x[y]),y]<-new.value
return(x)
}

recode_low <- function(data,varvector,lowbound,new.value) {
x <- data
for(y in varvector) x[x[y]>=lowbound & !is.na(x[y]),y]<-new.value
return(x)
}


#' recoding based on condition, otherwise keep the original value
#'
#' @param data vector to recode
#' @param condition condition for recoding
#' @param value value to put on vector
#' @keywords recode
#' @export
#' @examples
#' z = c(1,2,NA,3)
#' y = c(1,NA,2,3)
#' z = recode2(z,y<=2,10)
#' z # [1] 10  2 10  3

recode_ie = function(data, condition, value) {
	ifelse(condition & !is.na(condition), value, data)
}

#' ifelse but treating NA as FALSE
#'
#' @param condition condition for recoding
#' @param then value if condition is true
#' @param otherwise value if condition is false or NA due to missing values in vector which condition base
#' @keywords logical
#' @export
#' @examples
#' z = c(1,2,NA,3)
#' y = c(1,NA,2,3)
#' ifelse(y<2,'T','F')  # [1] "T" NA  "F" "F"
#' ifelse2(y<2,'T','F') # [1] "T" "F" "F" "F"
ifelse2 = function(condition, then, otherwise) {
	ifelse(condition & !is.na(condition), then, otherwise)
}

#' Read clipboard regardless of OS
#' 
#' Different operating systems have different ways of handling the clipboard. 
#' Given the frequency with which text is copied to the clipboard to place in
#' an answer on StackOverflow, this utility is provided.
#' 
#' @return character string containing text on the clipboard.
#' @export

readClip <- function(){
  OS <- Sys.info()["sysname"]
  
  cliptext <- switch(OS,
                   Darwin = {
                     con <- pipe("pbpaste")
                     text <- readLines(con)
                     close(con)
                     text
                   },
                   Windows = readClipboard(),
                   readLines("clipboard"))
    cliptext
}

#' Producing proportion table easily
#'
#' @param data the data to make cross table
#' @param by whether the proportion is calculated with regard to row total or column total
#' @keywords cross tabulation
#' @examples 
#' ptab(df %>% select(x,y), by=2)
#' @export 

ptab  <- function(..., margin = NULL, round = 1) ftable(...) %>% prop.table(margin) %>% multiply_by(100) %>% round(round)

plotab <- function(ctab, range = c(20,50), col = 'yellow') {
	grids =  expand.grid( attributes(ctab)$row.vars[[1]] %>% factor,
							attributes(ctab)$col.vars[[1]] %>% factor)
	grids$value = c(ctab) 
	print(ctab)
	ggplot(grids, aes(Var1, Var2)) + 
		geom_point(aes(size = value), col=col) + 
			scale_size_continuous(range=range) + 
				geom_text(aes(label =value)) + guides(size = F) + 
					xlab(names(attributes(ctab)$row.vars)) + 
						ylab(names(attributes(ctab)$col.vars))  
}

#' @export 
tebel <- function(...) table(... , useNA='always')
