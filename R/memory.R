#' Functions to check memory handling

devtools::use_package("utils")

#' improved list of objects
#' @rdname lsos
ls.objects <- function (pos = 1, pattern, order_by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(obj_names, fn) sapply(obj_names, function(x) fn(get(x, pos = pos)))
  obj_names <- ls(pos = pos, pattern = pattern)

  if (length(obj_names) == 0) {
    print("No Objects Loaded")
    return(invisible(data.frame()))
  }

  obj_class <- napply(obj_names, function(x) as.character(class(x))[1])
  obj_mode  <- napply(obj_names, mode)
  obj_type  <- ifelse(is.na(obj_class), obj_mode, obj_class)
  obj_prettysize <- napply(obj_names, function(x) {
    format(utils::object.size(x), units = "auto") })

  obj_size  <- napply(obj_names, utils::object.size)
  obj_dim   <- t(napply(obj_names, function(x) as.numeric(dim(x))[1:2]))
  vec <- is.na(obj_dim)[, 1] & (obj_type != "function")

  obj_dim[vec, 1] <- napply(obj_names, length)[vec]

  out <- data.frame(obj_type, obj_size, obj_prettysize, obj_dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")

  if (!missing(order_by))
    out <- out[order(out[[order_by]], decreasing = decreasing), ]
  if (head)
    out <- head(out, n)
  out
}


#' @title Memory check
#'
#' @description
#' Describe objects in memory
#'
#' @param n     Numeric of how many objects to return
#' @param ...   Other optional parameters:
#' \itemize{
#' \item \code{pos(integer)}: position in search list
#' \item \code{pattern(character)}: an optional regular expression
#' \item \code{order.by(character or integer)}: which column to order by
#' \item \code{decreasing(logical)}: whether to sort by decreasing
#' \item \code{head(logical)}: whether to return the first part only
#' }
#'
#' @return printed description of objects in memory
#'
#' @source Tony Breyal's post attributed to JD Long at:
#' @source https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#'
#' @examples
#'
#' df1 <- iris
#' df2 <- CO2
#' lsos()
#'
#' @export
lsos <- function(..., n=10) {
  ls.objects(..., order_by = "Size", decreasing = TRUE, head = TRUE, n = n)
}

