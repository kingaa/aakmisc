if (.Platform$OS.type == "unix" && Sys.getenv("FULL_TESTS")=="yes") {

  library(aakmisc)

  options(
    aakmisc.dbname="vandy",
    aakmisc.remotehost="kinglab.eeb.lsa.umich.edu"
  )

  startTunnel()

  x <- getQuery("select day,count from paramecia where experiment='BlPc'")
  print(mean(x$count))

  stopTunnel()

}
