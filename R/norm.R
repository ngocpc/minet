norm <- function(x)
{
      x <- x-min(x)
      x <- x/max(x)
      x
}
