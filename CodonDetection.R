###### IMPORT THE FASTA FILE ######
# Install package required to read fasta file 
install.packages("seqinr")
require(seqinr)

# Read fasta file into a list of char 
dna.seq <- read.fasta(file = "/Users/kshukla/Documents/data_science_files/ccrescentus.fa", set.attributes = FALSE)

##### PRE-PROCESS DATA TO OBTAIN NUMERICAL FEATURE TABLE ######
# Create fragments of length 300
frag.length <- 300
m <- matrix(dna.seq[[1]], nrow=frag.length)
dna.frag <- apply(m, 2, paste, collapse='')
dna.frag <- as.list(dna.frag)

# Create numeric table of short word freq
wordCounter = function(dna.frag, word.combo, word.length) {
  x <- grep(word.combo,substring(dna.frag, seq(1, nchar(dna.frag)-word.length+1, word.length), 
                                             seq(word.length, nchar(dna.frag), word.length)))
  y <- length(lengths(x))
  return(y)
}

tableCreator = function(word.length, dna.frag) {
  # Create all permutations
  x <- expand.grid(rep(list(c('a', 't', 'g', 'c')), word.length))
  word.combos <- do.call(paste0, x)
  # Create a dataframe and populate it 
  freq.table <- matrix(, nrow=length(dna.frag), ncol=length(word.combos))
  for(i in 1:length(word.combos)) {
    freq.table[,i] <- sapply(dna.frag, wordCounter, word.combos[i], word.length)
  }
  freq.table = data.frame(freq.table)
  return(freq.table)
}

freq.table <- tableCreator(3, dna.frag)

##### PERFORM PCA ##### 
require(stats)
# Scale data 
dat.scaled <- scale(freq.table)
# Obtain principal components
dna.pc <- prcomp(dat.scaled)
# Plot principal components
plot(dna.pc, type = 'l')
# Plot data along 2 main principal components
install.packages("devtools")
require(devtools)
install_github("ggbiplot", "vqv")

require(ggbiplot)
g <- ggbiplot(dna.pc, obs.scale = 1, var.scale = 1, ellipse = TRUE, var.axes = FALSE)
print(g)

##### CLUSTER DATA ######
# Project data on principal component space
proj.dat <- predict(dna.pc,freq.table)

# Perform k-means clustering
cluster.num = 7
word.cluster <- kmeans(proj.dat[,1:2], cluster.num, nstart = 5) 
word.grps <- as.factor(word.cluster[[1]])

# Plot clusters in principal component space
g <- ggbiplot(dna.pc, obs.scale = 1, var.scale = 1, groups = word.grps, ellipse = TRUE, var.axes = FALSE)
print(g)