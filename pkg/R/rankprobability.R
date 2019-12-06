
#' Extract rank probability matrix
#'
#' A rank probability matrix is a matrix with all entries between 0 and 1. The
#' rows represent treatments while the columns represent ranks. 
#'
#' @details
#' This function creats a rank probability matrix by reading information from a
#' model object. For example if \code{nma} is the result from a call to
#' \code{nma.ab.bin} from the \code{pcnetmeta} package, calling
#' \code{rank_probabilities(nma)} returns a matrix.
#'
#' @param x A model object holding rank probabilities.
#' @param ... currently not used
#'
#'
#' @family extractors
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
#' @param ... currently not used
#'
#' @family extractors
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
  x <- rank_probabilities(x)
  sucra_values.matrix(x)
}




