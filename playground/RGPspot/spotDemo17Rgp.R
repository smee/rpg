## Load package
require("SPOT")
## (for calls from demo():) disable interactive confirmation of plots
old.demo.ask <- options("demo.ask" = FALSE)
## get path of test project
testPath<-.find.package("SPOT")
testPath<-file.path("~/repos/rgp/playground/RGPspot","")
## show path
testPath
## remember workdir
#workdir <- getwd();
## source SPOT from repos
#setwd("~/workspace/SvnSpot.d/trunk/SPOT/R")
#for (f in dir()) source(f)
#setwd(workdir);
## Path to demo configuration:
testFile<-file.path(testPath,"rgp0001.conf")
## run example
spotConfig=spot(testFile)
## (for calls from demo():) reset confirmation of plots to previous setting
options("demo.ask" = old.demo.ask)
