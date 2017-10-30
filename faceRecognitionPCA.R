##### INSTALL PACKAGES #####
install.packages("jpeg")

##### READ IMAGES #####
require(jpeg)

# Specify path to folder containing files 
pixel.vectors <- filereader("C:/Users/Neelotpal/Dropbox/MIT_xPRO_DataScience/Module 1/instructors")
pixel.vectors <- lapply(pixel.vectors, as.vector)

# Function to read all files from location
filereader = function(mypath){
  filenames <- list.files(path = mypath, full.names = TRUE)
  data.list <- lapply(filenames, function(x) {readJPEG(x)})
  return(data.list)
}

##### PRE PROCESS IMAGES #####
# Normalize each image
pixel.normal <- lapply(pixel.vectors, scale)

# Obtain Mean Face
pixel.dat <- do.call(cbind, pixel.normal)
mean.vector <- rowMeans(pixel.dat)

##### COMPUTE EIGENFACES #####
cov.mat <- cov(pixel.dat)
eigen.dat <- eigen(cov.mat)
for(i in 1:5) {
  img <- pixel.dat %*% eigen.dat[[2]][,i]
  # plotter(img)
  file.name <- paste("C:/Users/Neelotpal/Dropbox/MIT_xPRO_DataScience/Module 1/instructors/eigenface", i, ".jpg",sep = "")
  img <- matrix(img, nrow = 300, ncol = 300)
  writeJPEG(img, target = file.name, quality = 1)
}

##### PLOT AN IMAGE #####
plotter = function(img.vector) {
  img <- matrix(img.vector, nrow = 300, ncol = 300)
  up.range <- max(img.vector)
  low.range <- min(img.vector)
  img <- apply(img, MARGIN = c(1,2), function(x) (x-low.range)/(up.range-low.range))
  plot(c(-1,1),c(-1,1), type = "n")
  rasterImage(img, -0.75, -0.75, 0.75, 0.75)
}
