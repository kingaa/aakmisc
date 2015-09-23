if (.Platform$OS.type == "unix" && Sys.getenv("FULL_TESTS")=="yes") {

  library(aakmisc)

  options(aakmisc.dbname="ouchsim",
          aakmisc.remotehost="kinglab.eeb.lsa.umich.edu")

  startTunnel()

  print(getQuery("select painting,seed,id from fits where id < 10"))

  stopTunnel()

}
