##-------------------------------------------------------------------------
# default symbols allowed to define rules or restrictions
.onLoad <- function(libname,pkgname){
  options(ruleSymbols = c(
    'if', 'else', 'is.na','is.finite',
    '==','<','<=','=','>=','>','!=','!', '%in%',
    'identical','sign','abs',
    '||', '|', '&&', '&', 
    '(','{','<-','=',
    '+', '-', '*', '^', '/', '%%', '%/%'
  ))
  options(validationSymbols = c(
    '<','<=','==','>','>=', '!=', '%in%', ":"
    , 'identical', 'all','any' 
    , '!', '|', '||', '&', '&&', 'xor'
  ))
}

.onUnload <- function(libpath){
  options(ruleSymbols=NULL)
  options(validationSymbols=NULL) 
}

#
# Read rules from ... or (list of) file(s).
# 
#
parse_rules <- function(...,file=NULL){
  if ( !is.null(file) && is.character(file) ){
    L <- list()
    ifile <- character(0)
    for ( f in file ){ 
      L <- c(L,read_resfile(f))
      ifile <- c(ifile,rep(f,length(L)))
    }
  } else {
    L <- as.list(substitute(list(...))[-1])
    names(L) <- extract_names(L)
    ifile <- rep("commandline",length(L))
  }
  names(ifile) <- names(L)
  list(files=ifile, calls=L)
}


read_resfile <- function(file){
  L <- tryCatch(parse(file=file)
                , error = function(e){
                  cat('Parsing failure at', file,"\n")
                  e
                })
  names(L) <- extract_names(L)
  lapply(L,as.call)
}


# Extract user-defined names from an object returned by 'parse'
extract_names <- function(L){
  i <- getSrcLocation(L)
  d <- getParseData(L)
  
  names <- paste0(rep("000",length(L)),1:length(L))
  names <- paste0("R",substring(names,nchar(names)-2))
  if ( !is.null(names(L)) ){
    iname <- names(L) != ""
    names[iname] <- names(L)[iname]
  }
  # select comments
  d <- d[d$token == "COMMENT",c('line1','text')]
  # select name definitions
  d <- d[grep("@restriction",d$text), ]
  srcloc <- d$line1 + 1
  
  srclab <- gsub("^.+@restriction ","",d$text)
  srclab <- gsub("\\s+.+$","",srclab)
  
  # merge names and definitions
  j <- match(i,srcloc,nomatch=0)
  names[which(i %in% srcloc)] <- srclab[j]
  names  
}












