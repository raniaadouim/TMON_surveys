# Load packagaes ---------------------------------------------------------------
library(tidyverse)
library(rgee)
library(raster)
library(terra)
library(reticulate)

# Setting up Python connection

use_python("/Library/Frameworks/Python.framework/Versions/3.13/bin/python3", required = TRUE)
py_config()
Sys.setenv(SSL_CERT_FILE = system("python3 -m certifi", intern = TRUE))
# ee_Initialize()

# Downloading GEE maps
classified19 <- ee$Image("users/admr/classified19")
classified20 <- ee$Image("users/admr/classified20")
classified21 <- ee$Image("users/admr/classified21")
classified22 <- ee$Image("users/admr/classified22")
classified23 <- ee$Image("users/admr/classified23")
classified24 <- ee$Image("users/admr/classified24")
roi <- ee$Geometry$Rectangle(c(xmin, ymin, xmax, ymax))  # optional
classified_r <- ee_as_raster(
  image = classified$clip(roi),
  region = roi,
  scale = 30,            
  via = "drive"          
)

plot(classified_r)