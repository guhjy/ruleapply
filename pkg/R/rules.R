
###############################################################################
#' The 'rules' class.
#'
#' This class stores simple execution rules, to be applied to data.
#'
#' @seealso \code{\link{rule}}, \code{\link{vars}}, \code{\link{apply}}
#' @export
setClass("rules",
  representation(
    rule = 'list'
    , origin = 'character'
))


##-------------------------------------------------------------------------
#' Define execution rules
#'
#' @param ... a comma-separated list of assignment statements.
#' @param file A (vector of) file location(s) defining a set of execution rules.
#' @param strict check rules against a set of allowed symbols?
#' @param allowed vector of symbols, allowed to define an execution rule.
#' @export
rules <- function(..., file=NULL, strict=TRUE, allowed=getOption('ruleSymbols')){
  v <- parse_rules(...,file=file)
  if ( strict ){
    M <- check_rules(v$calls,allowed=allowed)
    if ( any(M$error) ){ 
      err <-  paste('Forbidden symbols found:\n', get_errors(v,M))
      stop(err)
    }
  }
  new('rules',rule=v$calls, origin=v$files)
}

# print method
setMethod('show','rules',function(object){
  cat("Object of class 'rules'")
  v <- as.character(object@rule)
  v <- gsub("^","  ",v)
  v <- gsub("\n","\n  ",v)
  cat(sprintf("\n## %s\n%s",names(object@rule),v),'\n')
})

setMethod('[', 'rules', function( x, i, j, ..., drop=FALSE ){
  new('rules',rule=x@rule[i],origin=x@origin[i])
})

setMethod('[[','rules',function(x,i,j,...) x@rule[[i]])

setMethod('names','rules', function(x) names(x@rule))

setMethod('as.data.frame','rules',function(x,row.names=NULL,optional=FALSE,...){
  data.frame(
    rule        = sapply(x@rule,deparse)
    , name      = names(x)
    , origin    = x@origin
    , row.names = row.names
    , stringsAsFactors=FALSE
    )
})

# TODO: onliner doesn't add ';' where appropriate...
setMethod('as.character','rules', function(x, oneliner=FALSE,...){ 
    # this seems the only way to retain formatting and re-parsibility (mvdl).
    v <- sapply(x@rule ,function(r) as.character(noquote(list(r))))
    if ( oneliner ){
      v <- gsub("\n"," ",v)      # remove line end
      v <- gsub("[ ]+"," ",v)    # collapse multiple spaces to one
    }
    v
})


##-------------------------------------------------------------------------
# check if expression contains forbidden symbols (boolean)
checkSymbols <- function(x, b=TRUE, allowed=getOption('allowedSymbols'), ...){
#   if (is.expression(x)){ 
#      x <- x[[1]]
#   }

   if (length(x) == 1 ) return(TRUE)
   if (  is.symbol(x[[1]]) && 
         !(as.character(x[[1]]) %in% allowed) ){
            return(FALSE)
   }
   for ( i in 1:length(x) ){b <- b & checkSymbols(x[[i]],b)}
   b
}

##-------------------------------------------------------------------------
# extract forbidden symbols from an expression
extractSymbols <- function(x, allowed, L=character(0), ...){
   if ( length(x) == 1 ) return(NULL)
   if ( is.symbol(x[[1]]) && 
      !( as.character(x[[1]]) %in% allowed ) ){
      return(as.character(x[[1]]))
   }
   for ( i in 1:length(x) )  L <- c(L, extractSymbols(x[[i]], allowed, L))
   L
}

check_rules <- function(x, allowed, ...){
   M <- lapply(x,extractSymbols, allowed=allowed,...)
   list(error = sapply(M,function(m) length(m) > 0), symbols=M)
}


##-------------------------------------------------------------------------
# print rules and their errors. 
# x : list of rules, M result of check_rules
get_errors <- function(x, M){
   ix <- which(M$error)
   v <- sapply(x[ix],as.character)
   v <- gsub("^","  ",v)
   v <- gsub("\n","\n  ",v)
   S <- lapply(M$symbols[ix], paste, collapse=", ")
   sprintf('\n## ERR %2d ------\nForbidden symbols: %s\n%s',1:sum(M$error),S,v[ix])
}


setMethod('vars','rules', function(x,type=c('all','assigned'),...){
  type <- match.arg(type)
  switch(type
   , 'all'       = unique(do.call(c,lapply(x@rule,getvrs)))
   , 'assigned'  = unique(do.call(c,lapply(x@rule,assignee)))
  ) 
})


##-------------------------------------------------------------------------'
# get variables from expression (helper function, returnes list)
getvrs <- function(x, L=character(0), ...){
   if ( length(x) == 1){
      if ( is.symbol(x) ) return(as.character(x))
      return(NULL)
   }

   for ( i in 2:length(x) ) L <- c(L, getvrs(x[[i]],L))
   unique(L)
}

##-------------------------------------------------------------------------'
# get assigned variables from expression (helper function, returns list)
assignee <- function(e){
  f <- function(e,L=list()){
    x <- if (is.expression(e)) e[[1]] else e
    if ( as.character(x)[1] %in% c('<-','=') ) return(c(L,as.character(x[2])))
    if (length(x) > 1) return(sapply(x[-1],f,L))
  }
  unique(rapply(f(e),function(x) x))
}


##-------------------------------------------------------------------------'
# Code of the old 'correctWithRules function'
rowbyrow <- function(rules, dat, strict=TRUE){
  if (strict){
    vars <- get_vars(rules)
    I <- vars %in% names(dat)
    if (!all(I)) stop(
      sprintf("Variables '%s' in rules do not occur in data",paste(vars[!I],sep=", "))
    )
  }
  
  out <- dat
  m <- nrow(dat)
  n <- length(rules)
  row <- numeric(0)
  variable <- character(0)
  old <- character(0)
  new <- character(0)
  how <- character(0)
  vars <- colnames(dat)
  tr <- as.character(rules,oneliner=TRUE)
  
  
  
  for ( i in 1:m ){
    for ( j in 1:n ){
      d <- within(out[i,,drop=FALSE],eval(rules[[j]]))
      
      if ( !all(equal(d,out[i,])) ){
        rule <- tr[j]
        w <- which(!equal(d,out[i,]))
        row <- c(row, rep(i,length(w)))
        variable <- c(variable,vars[w])
        old <- c(old, format(unlist(out[i,w])) )
        new <- c(new, format(unlist(d[1,w])) )
        how <- c(how,rep(rule,length(w)))
        out[i,] <- d
      }
    }
  }
  list(
    corrected=out, 
    corrections=data.frame(
      row=row,
      variable=variable,
      old=old,
      new=new,
      how=how,
      stringsAsFactors=FALSE
    )
  )
}




##-------------------------------------------------------------------------'
# NA-robust pairwise comparison of data.frames
equal <- function(d,e){
   dNA <- is.na(d)
   eNA <- is.na(e)
   d == e & !(dNA != eNA) | (dNA & eNA) 
}

##-------------------------------------------------------------------------'
# helper function for vectorize 
set_guards <- function(x){
  e <- x[[1]]
  if ( class(e) == "{" ){
    v <- lapply(e[min(2,length(e)):length(e)], function(ex){
      ex <- as.expression(ex)
      attr(ex,'guard') <- guard(x)
      ex
    })
    return(lapply(v,set_guards))
  }
  
  if(class(e) == 'if'){
    v <- as.expression(e[[3]]) # expression
    attr(v,'guard') <- guard(x) %&% condition(e)
    v <- list(v)
    if (length(e)==4){ # there is an 'else'
      w <- as.expression(e[[4]])
      attr(w,'guard') <- guard(x) %&% not(condition(e))
      v <- list(v[[1]],w)
    }
    return(lapply(v,set_guards))
  }
  return(x) 
}

##-------------------------------------------------------------------------'
# vectorize an expression.
vectorize_rules <- function(x){
  L <- lapply(x,function(e) set_guards(as.expression(e)))  
  expr <- unlist(L)
  guard <- rapply(L, function(e) if (is.null(attr(e,'guard'))) expression(TRUE) else attr(e,'guard') )
  list(rule=as.expression(expr), guard=as.expression(guard))
}

##-------------------------------------------------------------------------'
# apply a single vectorized rule to a dataset
apply_rule <- function(rule, dat, sdat=NULL, subset=expression(TRUE),na.condition=FALSE){

  I <- if (is.null(sdat)) eval(subset,dat) else eval(subset,sdat)
  I[is.na(I)] <- na.condition
  if ( any (I) ) dat[I, ] <- within(dat[I,,drop=FALSE],eval(rule))
  dat
}  

##-------------------------------------------------------------------------'
# replace shortcircuited operators by vectorized ones
replace_shortcircuit <- function(x){
  y <- as.character(x)
  if ( any(grepl('&&|\\|\\|',y)) ){
    warning("Short-circuited operators '&&' and/or '||' replaced by vectorized operators '&' and/or '|'")
    y <- gsub('&&','&',y,fixed=TRUE)
    y <- gsub('||','|',y,fixed=TRUE)
    x <- parse(text=y)
  } 
  x
}
##-------------------------------------------------------------------------'
#' Generic execution function
#' @param x object to be executed
#' @param dat data
#' @param ... extra options, depending on the method.
#' @export 
setGeneric("execute", function(x,dat,...) standardGeneric("execute"))

#'
#' Apply simple replacement and derivation rules to a \code{data.frame}.
#'
#' @section Details:
#' This function applies the the \code{rules} one by one to \code{dat} and logs
#' their actions. Rules are vectorized by default for speedy execution. Vectorisation means
#' that the rule expessions are split up into assignments and 'guarding expressions' that
#' determine on which subset of the data an assignment should take place.
#' 
#' By default, assignments are excuted in order of occurrence in the \code{\link{rules}}
#' so order may matter for the final result. If \code{sequential=FALSE} the guarding expressions
#' are always evaluated on the original dataset, so the order of assignment execution is 
#' unimportant. Also see the example section.
#' 
#' See \code{\link{rules}} for details on the type of rules that are allowed.
#'
#' @param rules object of class \code{\link{rules}} 
#' @param dat \code{data.frame}
#' @param addnew If \code{TRUE}, newly assigned variables are added to the data, only when \code{vectorized=TRUE}.
#' @param sequential Make rule execution order-dependent? See 'Details'
#' @param na.condition If a conditional expression evaluates to \code{NA}, replace it with \code{TRUE} or \code{FALSE}? Only when \code{vectorized=TRUE}.
#' @param vectorize Vectorize rules before applying them? If \code{FALSE}, the rules are applied row-by-row, which can be significantly slower.
#' @seealso \code{\link{rules}}
#'
#' @return list with altered data (\code{$dat}) and a list of modifications (\code{$log}). 
#' 
setMethod("execute",signature(x="rules",dat="data.frame"), 
 function(x, dat, addnew=FALSE, vectorize=TRUE, sequential=TRUE, na.condition=FALSE, ...){
  if (addnew && !vectorize) stop("New variables can only be added in vectorized mode")
  
  rules <- x@rule
  # should we get rid of this?
  if (!vectorize) return(rowbyrow(rules,dat,strict=TRUE))
  
  vrs <- vars(x)
  asgn <- vars(x,type='assigned')
  
  if (addnew) vrs <- vrs[!vrs %in% asgn]
  
  if (any( I <- !vrs %in% names(dat))){
    stop("Variables "
         , paste(vrs[I],collapse=',')
         , ' used in rules but not present in data'
    )
  }  
  # add empty columns
  
  if (addnew) dat[asgn[!asgn %in% names(dat)]] <- NA
  
  rules <- replace_shortcircuit(rules)
  R <- vectorize_rules(rules)

  if (!sequential) inidat <- dat
  
  # init log
  log <- logframe(dat,dat,'')
  
  # apply rules
  for ( i in 1:length(R[[1]]) ){
    dat1 <- if(sequential){
      apply_rule(rule=R$rule[i], dat=dat, subset=R$guard[i],na.condition=na.condition)
    } else {
      apply_rule(rule=R$rule[i], dat=dat, sdat=inidat, subset=R$guard[i],na.condition=na.condition)      
    }
    if ( TRUE ){ # TODO: make logging optional/customizable
      g <- ifelse(as.character(R$guard[i])=="TRUE","",paste(' @',R$guard[i])) 
      log <- rbind(log,logframe(
        dat
        , dat1
        , paste0(R$rule[i],g)
        , as.character(R$rule[i])
      ))
    }
    dat <- dat1
  }
  list(dat=dat,log=log)  
})



##-------------------------------------------------------------------------'
# Get guarding expression
guard <- function(x) attr(x,'guard')

##-------------------------------------------------------------------------'
# Conjugate two expressions
`%&%` <- function(e,f){
  if ( is.null(e) ) return(f)
  if ( is.null(f) ) return(e)
  parse(text=paste('(',e,') & (',f,')'))
}

##-------------------------------------------------------------------------'
# Negate an expression
not <- function(e){
  parse(text=paste0('!(',e,')'))
}

##-------------------------------------------------------------------------'
# return the conditional expression from an 'if' object
condition <- function(e){
  stopifnot(class(e)=='if')
  as.expression(e[[2]])
}

##-------------------------------------------------------------------------'
# row-variable-old-new-remark log
logframe <- function(dat1, dat2, remark,...){
  A <- !equal(dat1,dat2)
  rc <- which(A,arr.ind=TRUE)
  data.frame(
      row = rc[,'row']
    , variable = names(dat1)[rc[,'col']]
    , old = format(as.character(dat1[A]))
    , new = format(as.character(dat2[A]))
    , how=rep(remark,nrow(rc))
    , stringsAsFactors=FALSE
  )
}





