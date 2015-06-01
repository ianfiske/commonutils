rpart.plot <- function (model, main = "", sub, type = 4, extra = 4, ...) {
  if (missing(sub))
    sub <- paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"),
                 Sys.info()["user"])
  num.classes <- length(attr(model, "ylevels"))
  numpals <- 6
  palsize <- 5
  pals <- c(RColorBrewer::brewer.pal(9, "Greens")[1:5], RColorBrewer::brewer.pal(9,
                                                                                 "Blues")[1:5], RColorBrewer::brewer.pal(9, "Oranges")[1:5],
            RColorBrewer::brewer.pal(9, "Purples")[1:5], RColorBrewer::brewer.pal(9,
                                                                                  "Reds")[1:5], RColorBrewer::brewer.pal(9, "Greys")[1:5])
  if (model$method == "class") {
    yval2per <- -(1:num.classes) - 1
    per <- apply(model$frame$yval2[, yval2per], 1, function(x) x[1 +
                                                                   x[1]])
  }
  else {
    per <- model$frame$yval/max(model$frame$yval)
  }
  per <- as.numeric(per)
  if (model$method == "class")
    col.index <- ((palsize * (model$frame$yval - 1) + trunc(pmin(1 +
                                                                   (per * palsize), palsize)))%%(numpals * palsize))
  else col.index <- round(per * (palsize - 1)) + 1
  col.index <- abs(col.index)
  if (model$method == "class")
    extra <- 104
  else extra <- 101
  rpart.plot::prp(model, box.col = pals[col.index], type = type, extra = extra,
                  nn = TRUE, varlen = 0, faclen = 0, shadow.col = "grey",
                  fallen.leaves = TRUE, branch.lty = 3, ...)
  title(main = main, sub = sub)
}


pgconnect <- function(uri) {
  parsed_uri <- httr::parse_url(uri)
  dplyr::src_postgres(host = parsed_uri$hostname, dbname = parsed_uri$path, port = parsed_uri$port,
                      user = parsed_uri$username, password = parsed_uri$password)
}
