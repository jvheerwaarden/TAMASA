#' TAMASA Ethiopia maize yield data prediction
#' 2016 Ethiopia maize yield data courtesy TAMASA
#' M. Walsh, J. Chamberlin, J. v. Heerwaarden, March 2017

# install.packages(c("downloader","rgdal","raster"), dependencies=T)
suppressPackageStartupMessages({
  require(caret)
  require(doParallel)
  require(rgdal)
  require(raster)
})

# Data setup --------------------------------------------------------------
# Run this first: https://github.com/mgwalsh/TAMASA/blob/master/TAMASA_trial_setup.R
# or run ...
# SourceURL <- "https://raw.githubusercontent.com/mgwalsh/TAMASA/master/TAMASA_trial_setup.R"
# source_url(SourceURL)

# Variable selection
y <- yield$yield
x <- yield[c(3:6,10:22)]

# bartMachine -------------------------------------------------------------
options(java.parameters = "-Xmx8000m")
require(bartMachine)

# Start doParallel to parallelize model fitting
mc <- makeCluster(detectCores())
registerDoParallel(mc)

# Control setup
tc <- trainControl(method = "cv", returnResamp = "all", allowParallel = T)

# Fit model
typ.bar <- train(x, y,
                 method = "bartMachine", 
                 preProc = c("center", "scale"),
                 trControl = tc,
                 tuneLength = 3,
                 verbose = FALSE,
                 seed = 123)
print(typ.bar) ## training results
typ.imp <- varImp(typ.bar)
plot(typ.imp, col="black", cex=1.2, xlab="Model importance in BART prediction")

# Model predictions
# bar.pred <- predict(grids, typ.bar) ## spatial prediction, not run!

stopCluster(mc)
detach("package:bartMachine", unload=TRUE)
