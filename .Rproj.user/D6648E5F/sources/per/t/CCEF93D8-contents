
#=====================================================
# DEPLOY APP
#=====================================================
library(shinylive)
library(httpuv)

shinylive::export(
  appdir = "app",
  destdir = "docs"
)

#preview link
httpuv::runStaticServer("docs/", port = 8008)
