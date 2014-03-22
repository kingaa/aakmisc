if (.Platform$OS.type == "unix") {

  library(aakmisc)

  options(aakmisc.dbname="ouchsim")

  startTunnel()

  getQuery("select painting,seed,id from fits where id < 10")

  stopTunnel()

}
