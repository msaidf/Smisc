to_map <- function(func){
  function(list, column, ...){
    if(missing(column)){
        res <- purrr::map(list, (function(x) func(x, ...)))
      } else {
        res <- purrr::map(list, (function(x) func(x[column], ...)))
             }
    res
  }
}

n_dots <- function(...) nargs()

#' reshape dataset in wide form 
#' @export 
widen <- function(data, item, ...) {
    item_col = deparse(substitute(item))
    if (n_dots(...) == 0) {
        gather_cols <- setdiff(colnames(data), item_col)
    }
    else {
        gather_cols <- unname(dplyr::select_vars(colnames(data), ...))
    }
    tidyr::gather_(data, 'vars', 'val', gather_cols) %>%  
	tidyr::unite_(col = 'vars', c('vars', item_col)) %>%  
	tidyr::spread(key = vars, value = val) 
}

#' reshape dataset in long form 
#' @export 
longen <- function(df, item, ...) {
	tidyr::gather(df, vars, val, ...) %>% 
		tidyr::separate(df, col = vars, into = c('vars', deparse(substitute(item)))) %>% 
			tidyr::spread(vars, val) 
}

#' make dataset spread-ready, useful to inspect problem in spreading 
#' @export 
spready <- function(data, item, ...) {
    item_col = deparse(substitute(item))
    if (n_dots(...) == 0) {
        gather_cols <- setdiff(colnames(data), item_col)
    }
    else {
        gather_cols <- unname(dplyr::select_vars(colnames(data), ...))
    }
    tidyr::gather_(data, 'vars', 'val', gather_cols) %>%   
	tidyr::unite_(col = 'vars', c('vars', item_col)) 
}

#' inspect duplicate problem in spreading # working
#' @export 
spreadiag <- function(df, id, vars) {
	vars = deparse(substitute(vars))
	id = deparse(substitute(id))
	df = mutate(df, vars_id=paste0(vars, id)) 
	showdup(df, vars_id)
}

#' cure duplicate problem in spreading  # working
#' @export 
spreadcure <- function(df, id, vars) {
	vars = deparse(substitute(vars))
	id = deparse(substitute(id))
	df = mutate(df, vars_id=paste0(vars, id))
	df = {df[!duplicated(df$vars_id),]}
	select(df, -vars_id) 
}

#' @export
showdup <- function(df, id) {
	id = deparse(substitute(id))
	dup = df[[id]][duplicated(df[id])]
	df[df[[id]] %in% dup, ] 
}

#' @export 
pair_join <- function(df, group, id_many, id_one, ... , prefix=NULL, suffix=NULL, separator='_') {
	vars=arg2char(...)
	group = deparse(substitute(group))
	id_many = deparse(substitute(id_many))
	id_one = deparse(substitute(id_one))
	if(length(vars)==0) vars=setdiff(names(df), c(group, id_many, id_one))
	vars_many=c(group, id_many, vars)
	if(!is.null(prefix)) vars=paste(prefix, vars, sep=separator)
	if(!is.null(suffix)) vars=paste(vars, suffix, sep=separator)
	vars_one=c(group, id_one, vars) 
	dplyr::left_join(df, 
					 setNames(dplyr::select(df, one_of(vars_many)), vars_one),
					 by=c(group, id_one)
					 )
}

#' @export
`%<%` <- function(x, y) {
	if(!dir.exists('.rversioning')) dir.create('.rversioning')
	save(x, file=paste0('.rversioning/', deparse(substitute(x)), '.rda'))
	assign(deparse(substitute(x)), y, envir=.GlobalEnv)
}

#' @export
revert <- function(y) {
	load(paste0('.rversioning/', deparse(substitute(y)), '.rda'))
	assign(deparse(substitute(y)), x, envir=.GlobalEnv)
}

#' @export
versioned <- function() list.files('.rversioning/')

wipe <- function() {
	unlink('.rversioning', recursive=T)
	list.files('.rversioning/')
}

#' @export
rename_pattern <- function(data, pattern, replacement) {
	path = attributes(data)$dir

	old_names = names(data)[stringr::str_detect(names(data), pattern)]
	new_names = stringr::str_replace(old_names,pattern, replacement)

	cap =stringr::str_detect(old_names, '^[A-Z]' )
	old_names[cap] = paste0(path, '/@', old_names[cap])
	old_names[!cap] = paste0(path, '/', old_names[!cap])

	cap =stringr::str_detect(old_names, '^[A-Z]' )
	new_names[cap] = paste0(path, '/@', new_names[cap])
	new_names[!cap] = paste0(path, '/', new_names[!cap])
	file.rename(from = old_names, to = new_names)
}

#' @export
prefixadd <- function(data, prefix, ...) {
	prefix = deparse(substitute(prefix))
	path = attributes(data)$dir

	old_names = arg2char(...)
	new_names = paste0(prefix, old_names)

	cap =stringr::str_detect(old_names, '^[A-Z]' )
	old_names[cap] = paste0(path, '/@', old_names[cap])
	old_names[!cap] = paste0(path, '/', old_names[!cap])

	cap =stringr::str_detect(old_names, '^[A-Z]' )
	new_names[cap] = paste0(path, '/@', new_names[cap])
	new_names[!cap] = paste0(path, '/', new_names[!cap])
	file.rename(from = old_names, to = new_names)
}

#' @export
suffixadd <- function(data, suffix, ...) {
	suffix = deparse(substitute(suffix))
	path = attributes(data)$dir

	old_names = arg2char(...)
	new_names = paste0(old_names,suffix)

	cap =stringr::str_detect(old_names, '^[A-Z]' )
	old_names[cap] = paste0(path, '/@', old_names[cap])
	old_names[!cap] = paste0(path, '/', old_names[!cap])

	cap =stringr::str_detect(old_names, '^[A-Z]' )
	new_names[cap] = paste0(path, '/@', new_names[cap])
	new_names[!cap] = paste0(path, '/', new_names[!cap])
	file.rename(from = old_names, to = new_names)
}



#' @export
arg2char <- function(...) {
	.dots = lazyeval::lazy_dots(...)
	auto_names(.dots) 
}

#' @export
ezyload = function(...) {
	libraries=arg2char(...)
  for(i in 1:length(libraries)) {
    lib = libraries[i]
    if(!require(lib, character.only=T)) {
      install.packages(lib)
      library(lib, character.only = T)
    }
  }
}

#' @export
reload_package <- function(pkg) {
	detach_package(pkg)
	library(pkg)
}

#' detaching a loaded package
#' @export
detach_package <- function(pkg, character.only = FALSE) {
	if(!character.only) { 
		pkg <- deparse(substitute(pkg)) }
	search_item <- paste("package", pkg, sep = ":")
	while(search_item %in% search()) {
			  detach(search_item, unload = TRUE, character.only = TRUE) }
}

#' @export
vectorize <- function(table, keep.table = T) {
	if (is.matrix(table)) {
		for(i in 1:ncol(table)) {
			assign(colnames(table)[i] , table[,i] , .GlobalEnv) 
		}
	} 
	else {
		for(i in 1:length(table)) {
			assign(names(table)[i] , 
				   eval(parse(text = paste0(deparse(substitute(table)) 
											,'$',names(table)[i])))
				   , .GlobalEnv) 
		}
	}
	if(!keep.table) rm(table)
}

#' @export
tableize <- function(table, keep.vectors = T, labels = NULL, use.old.labels = T) {
	table.temp = data.frame(mget(names(table), envir = .GlobalEnv))  
	if(is.null(labels) & use.old.labels) labels = Hmisc::label(table)
	table.temp = Hmisc::cleanup.import(table.temp, labels = labels)
	rownames(table.temp) <- rownames(table)
	assign(deparse(substitute(table)), data.frame(table.temp),.GlobalEnv )  
	if(!keep.vectors) rm(list = names(table), envir = .GlobalEnv)
}

#' @export
orderize <- function(table, ...) { 
	table.temp = data.frame(mget(names(table), envir = .GlobalEnv))  
	table.temp =  dplyr::arrange(table.temp, ...)
	for(i in 1:length(table)) assign(names(table)[i], table.temp[,i], .GlobalEnv) 
}

#' @export
headize <- function(table, ...) { 
	stopifnot(!is.environment(table))
	table.temp = data.frame(mget(names(table), envir = .GlobalEnv))  
	rownames(table.temp) <- rownames(table)
	head(table.temp, ...)
}

#' @export 
applize <- function (FUN, ..., as.vector = FALSE) {
	.dots = lazyeval::lazy_dots(...)
	vars = auto_names(.dots)
	results = vector('list', length(vars))
	for (i in 1:length(vars)) {
		results[[i]] = do.call(FUN, list(get(vars[i],envir = .GlobalEnv ))) 
	}
	names(results) = vars
	if (as.vector) {
		return(cbind(results))
	} else {
		return(results)
	}
	return(results)
}

#' @export 
mutatize <- function (FUN, ...) {
	.dots = lazyeval::lazy_dots(...)
	vars = auto_names(.dots)
	for (i in 1:length(vars)) {
		x = do.call(FUN, list(get(vars[i],envir = .GlobalEnv ))) 
		assign(vars[i], x, envir = .GlobalEnv)
	}
}

#' @export 
which_binary  <- function(data) names(data)[which(unlist(lapply(data, function(x) length(unique(x))==2)))]

#' @export 
make_binary  <- function(data, zero, ...) {
    if(missing(zero)) stop('Need to specify what value to be set as zero')
	binary = arg2char(...)
	data %>% mutate_each_(funs(ifelse(.==zero, 0, 1)), binary)
}

#' @export 
Where <- function(x, env = parent.frame()) pryr::where(deparse(substitute(x)), env = env)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Grouped histogram
#'
#' Histogram density plots (in percentage, not counts) of a variable grouped by another variable
#' @export 
group_hist <- function(dataframe, group, vec) {
	grouped =do.call(group_by, list(dataframe, substitute(group), substitute(vec))) 
    summarised = grouped %>%  summarise(n=n()) 
	mutated = summarised %>% mutate(prop = n/sum(n)) 
	g = ggplot(mutated, do.call(aes, list(x = substitute(vec), y=mutated$prop)))
    h = g	+ scale_fill_grey() + xlab('') + theme(legend.title=element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = 12)) + geom_bar(stat='identity') + ylab('proportion') 
   i = h	+ do.call(facet_wrap, list(substitute(~group)))  
   return(i)
}

#' Grouped histogram (deprecated)
#' @export 
gplot <- group_hist

sumtab <- function(df) summary(df) %>% data.frame %>% select(variables = Var2, Freq) %>% separate(Freq, c('stats', 'val'), ':')  %>% spread(key = stats, val) %>%  .[,c(1,5,7,4,2,6,3,8)]

#' Grouped scatterpolot
#' @export 
group_scat <- function(data, formula)
{
    data = aggregate(formula, data, mean, na.rm=T)
    colname = as.character(formula)[-1]
    colname[2] = gsub(' ', '', colname[2])
    colname = c(colname[1], unlist(strsplit(colname[2], '\\+')))
    if(ncol(data) == 2) p = qplot(data[[1]], data[[2]]) + xlab(colname[2]) + ylab(colname[1])  
    if(ncol(data) == 3) p = qplot(data[[1]], data[[3]], col=data[[2]], geom='point') + geom_smooth() + xlab(colname[2]) + ylab(colname[1]) + guides(col = guide_legend(title = colname[3]))
    p
}

#' Grouped scatterpolot (deprecated)
#' @export 
group_plot = group_scat

group_plot_i <- function(data, formula)
{
    data = aggregate(formula, data, mean, na.rm=T)
    rCharts::rPlot(formula, data=data, type='point')
}

# below functions are taken from lazeval sources
auto_names <- function(x, max_width = 40) {
	x <- lazyeval::as.lazy_dots(x)
	nms <- names(x) %||% rep("", length(x))
	missing <- nms == ""
	expr <- lapply(x[missing], `[[`, "expr")
	nms[missing] <- vapply(expr, deparse_trunc, width = max_width,
						   FUN.VALUE = character(1), USE.NAMES = FALSE)
	nms
}

deparse_trunc <- function(x, width = getOption("width")) {
	if (is.symbol(x)) {
		return(as.character(x))
	}
	text <- deparse(x, width.cutoff = width)
	if (length(text) == 1 && nchar(text) < width) return(text)
	paste0(substr(text[1], 1, width - 3), "...")
}

"%||%" <- function(x, y) if(is.null(x)) y else x
