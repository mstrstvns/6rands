# Basic code taken from "Parallel Random Number Generation in C++ and R Using RngStream.pdf"
# See https://www.researchgate.net/publication/228712701_Parallel_Random_Number_Generation_Using_RngStream?ev=prf_pub
install.packages("rstream")
require(rstream)
#Download package
library(rstream)

# Create a new thing using the Multiple Recursive Generator rstream.mrg32k3a
gen0<- new("rstream.mrg32k3a")
rstream.sample(gen0,1)
mrg_dataset<- 1 
# Loop to create 3000000 elements for this vector
for(i in 1:3000000){
  gen <-new("rstream.mrg32k3a")

  # Save the vector as a variable for plotting 
  mrg_dataset[i] <- (rstream.sample(gen,1))
}

# save dataset to a file.
write.table(mrg_dataset, file="2.0.MRG_dataset.raw", append = FALSE, quote = FALSE, sep = " ",
            eol = "\n", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"),
            fileEncoding = "")