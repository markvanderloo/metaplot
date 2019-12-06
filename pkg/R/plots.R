#' Create rank probability or SUCRA plot
#'
#'
#' @param x An object holding rank probabilites (matrix or model object)
#' @param ...  options (see Details)
#'
#' @export 
rank_probability_plot <- function(x,...){
  UseMethod("rank_probability_plot")
}

#' @rdname rank_probability_plot
#' @export 
rank_probability_plot.matrix <- function(x,...){
  rp_plot(x, cumulative=FALSE, ...)
}

#' @rdname rank_probability_plot
#' @export 
rank_probability_plot.nma.ab <- function(x,...){
  rp <- rank_probabilities(x)
  rp_plot(rp,...)
}

#' @rdname rank_probability_plot
#' @export
sucra_plot <- function(x,...){
  UseMethod("sucra_plot")
}

#' @rdname rank_probability_plot
#' @export 
sucra_plot.matrix <- function(x, ...){
  rp_plot(x, cumulative=TRUE,...)
}


#' @rdname rank_probability_plot
#' @export 
sucra_plot.nma.ab <- function(x, ...){
  rp <- rank_probabilities(x)
  sucra_plot(rp,...)
}



rp_plot <- function(rp
        , lwd=2
        , ymax = 1
        , palette = "Dark 2"
        , cumulative = FALSE
        , legend=TRUE
        , legend.loc   = c("topleft","topright","bottomleft","bottomright")
        , legend.horiz = TRUE, ...){
  

  ranks <- seq_len(ncol(rp))
  
  if (cumulative){
    rp <- t(apply(rp, 1, cumsum))
    ylab <- "cumulative probability"
    main <- "SUCRA"
  } else {
    ylab <- "probability"
    main <- "Rank probability"
  }
  
  pal <- hcl.colors(n = nrow(rp), pal=palette)
  plot(x=ranks, y = rp[1,], type='b',lwd=lwd, las=1,ylim=c(0,ymax)
       , pch=16, col=pal[1]
       , xlab="rank", ylab=ylab
       , main=main, ...) 
  for ( r in ranks[-1]){
    lines(x=ranks, y=rp[r,],type='b',pch=16,col=pal[r], lwd=lwd)
  }
  
  if (legend){
    i <- order(rp[,1], decreasing = TRUE)
    legend.loc <- match.arg(legend.loc)
    legend(legend.loc
           , legend=rownames(rp)[i]
           , col=pal[i], lwd=lwd, pch=16, bty="n"
           , horiz=legend.horiz)
  }
}



