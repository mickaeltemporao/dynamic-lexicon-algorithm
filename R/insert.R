#' Insert function
#'
#' @param x
#' @param index
#' @param replacement
#' @param how
#'
#' @return
#' @export
#'
#' @examples
insert <- function(x, index, replacement, how = c("replace","left","right")) {
  how <- match.arg(how)
  repl <- switch (how,
                  replace = replacement,
                  left = Map("c", index, replacement),
                  right = Map("c", replacement, index)
  )
  x[index] <- repl
  unlist(x)
}
