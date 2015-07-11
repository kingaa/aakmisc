bake <- function (file, expr) {
  if (file.exists(file)) {
    readRDS(file)
  } else {
    val <- eval(expr)
    saveRDS(val,file=file)
    val
  }
}
