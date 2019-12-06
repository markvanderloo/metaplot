
#' Extract rank probability matrix
#'
#' A rank probability matrix is a matrix with all entries between 0 and 1. The
#' rows represent treatments while the columns represent ranks.
#'
#' @param x A model object holding rank probabilities.
#' @param ... currently not used
#'
#' @return A \code{matrix}
#' @export
rank_probabilities <- function(x,...){
  UseMethod("rank_probabilities")
}

#' @rdname rank_probabilities
#'
#' @export
rank_probabilities.nma.ab <- function(x,...){
  rp <- x$TrtRankProb
  array(as.numeric(rp)
      , dim      = c(nrow(rp),ncol(rp))
      , dimnames = list(treatment=rownames(rp), rank=seq_len(ncol(rp)))
  )
}

#' Extract scra values
#'
#' @param x a \code{matrix} or a model object holding rank probabilities
#' @param ... Passed to \code{\link{rank_probabilities}}
#'
#' @export
sucra_values <- function(x,...){
  UseMethod("sucra_values")
}

#' @rdname sucra_values
#' @export
sucra_values.matrix <- function(x,...){
  t(apply(x,1,cumsum))
}

#' @rdname sucra_values
#' @export
sucra_values.default <- function(x,...){
  x <- rank_probabilities(x,...)
  sucra_values.matrix(x)
}




