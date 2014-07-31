#combined lcg and mrg using mrg to set seed for lcg.
# Basic code taken from "Parallel Random Number Generation in C++ and R Using RngStream.pdf"
# See https://www.researchgate.net/publication/228712701_Parallel_Random_Number_Generation_Using_RngStream?ev=prf_pub

#Download package
require(rstream)
library(rstream)

# Create a new thing using the Multiple Recursive Generator rstream.mrg32k3a
gen0<- new("rstream.mrg32k3a")
rstream.sample(gen0,1)

#Initialize two new vectors
streamset<-1
streamset1<-1

# Loop to create two streams with 101 elements for each vector
for(i in 1:3000000){
  gen <-new("rstream.mrg32k3a")
  gen1 <-new("rstream.mrg32k3a")
  
  # Save the vector as a variable for plotting 
  streamset[i] <- (rstream.sample(gen0,1))
  streamset1[i] <- (rstream.sample(gen1,1))
}
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


# ##############################################################################
# FUNCTION:                 DESCRIPTION:
#  set.lcgseed               Set initial random seed
#  get.lcgseed               Get the current valus of the random seed
#  runif.lcg                 Uniform linear congruational generator
#  run1if.lcg               Another Uniform linear congruational generator
#  rnorm.lcg                 Normal linear congruational generator
#  rt.lcg                    Student-t linear congruational generator
################################################################################
# DESCRIPTION:
#  A Simple Portable Random Number Generator for Use in R and Splus
#  Use this generator only for comparisons of Programs in R and Splus !!!
#  Method: A linear congruential generator with
#    LCG(a=13445, c=0, m=2^31-1, X0)
#    Note, this is a random number generator which passes the bitwise 
#    randomness test.
#  Reference: 
#    http://csep1.phy.ornl.gov/rn/node13.html 
#    N. S. Altman. ``Bitwise Behavior of Random Number Generators,'' 
#    SIAM J. Sci. Stat. Comput., 9(5), September, pps. 941-949, 1988
#  Example:
#    set.lcgseed(4711)
#    cbind(runif.lcg(100), rnorm.lcg(100), rt.lcg(100, df=4))
################################################################################


set.lcgseed = 
  function(seed = 905291) 
  {   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Sets the random seed for the linear congruential 
    #   random number generator
    
    # FUNCTION
    
    # Return Value:
    lcg.seed <<- seed
  }


# ------------------------------------------------------------------------------


get.lcgseed = 
  function() 
  {   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the random seed for the linear congruential 
    #   random number generator
    
    # FUNCTION
    
    # Return Value:
    lcg.seed
  }


# ------------------------------------------------------------------------------
runif.lcg = 
  function(n, min = 0, max = 1)
  {   # A function implemented by Diethelm Wuertz
    
    # Description:
    #    A linear congruential generator for uniform distributed
    #    random numbers
    
    # Notes: 
    #    Important - Use this generator only for comparisons of 
    #      Programs in R and SPlus (and not for production) !!!
    #    Portable Random Numbers:
    #      A linear congruential generator
    #      LCG(a=13445, c=0, m=2^31-1, X0)
    #      This is a random number generator which 
    #      passes the bitwise randomness test
    
    # References: 
    #    http://csep1.phy.ornl.gov/rn/node13.html 
    #    N. S. Altman. ``Bitwise Behavior of Random Number Generators,'' 
    #    SIAM J. Sci. Stat. Comput., 9(5), September, pps. 941-949, 1988
    
    # FUNCTION stream 1
    
    # Initialize:
     if(!exists("lcg.seed")) lcg.seed <<- streamset[1]
    
    # Generate:
    r0.lcg = rep(0, times = n)
    a = 13445
    c = 992129
    m = 2^31-1
    for (i in 1:n) {
      lcg.seed <<- (a*streamset[i] + c) %% m 
      r0.lcg[i] = lcg.seed/m }
    r0.lcg = (max-min)*r0.lcg + min
    
    # Return Value:
    r0.lcg
}
# ------------------------------------------------------------------------------
run1if.lcg = 
  function(n, min = 0, max = 1)
  {   # A second modified function from the previous one implemented by Diethelm Wuertz
    
    # Description:
    #    A linear congruential generator for uniform distributed
    #    random numbers
    
    # Notes: 
    #    Important - Use this generator only for comparisons of 
    #      Programs in R and SPlus (and not for production) !!!
    #    Portable Random Numbers:
    #      A linear congruential generator
    #      LCG(a=13445, c=0, m=2^31-1, X0)
    #      This is a random number generator which 
    #      passes the bitwise randomness test
    
    # References: 
    #    http://csep1.phy.ornl.gov/rn/node13.html 
    #    N. S. Altman. ``Bitwise Behavior of Random Number Generators,'' 
    #    SIAM J. Sci. Stat. Comput., 9(5), September, pps. 941-949, 1988
    
    # FUNCTION stream 1
    
    # Initialize:
    if(!exists("lcg.seed")) lcg.seed <<- streamset1[1]
    
    # Generate:
    r1.lcg = rep(0, times = n)
    a = 13445
    c = 992317
    m = 2^31-1
    for (i in 1:n) {
      lcg.seed <<- (a*streamset1[i] + c) %% m 
      r1.lcg[i] = lcg.seed/m }
    r1.lcg = (max-min)*r1.lcg + min
    
    # Return Value:
    r1.lcg
  }

# ------------------------------------------------------------------------------
run2if.lcg = 
  function(n, min = 0, max = 1)
  {   # A third function to combine the two sets.
    
    # FUNCTION stream 1
    
    # Initialize:
    if(!exists("lcg.seed")) lcg.seed <<- streamset1[1]
    
    # Generate using provided value for n:
    r2.lcg = rep(0, times = n)
    for (i in 1:n) {
      r2.lcg[i] = cmb_datasetx[i]+cmb_datasety[i] }
    
    # Return Value:
    r2.lcg
  }
# Save the data into three sets and graph the third set that is the first two sets added together. if the data ever is the same it should plot out as a straight diagonal line.
 cmb_datasetx<-(.125*runif.lcg(3000000))
 cmb_datasety<-(10.3*run1if.lcg(3000000))
 cmb_datasetz<-((100000*run2if.lcg(3000000))-481.5)
write.table(cmb_datasetz, file="3.0.CLMRG_dataset.raw", append = FALSE, quote = FALSE, sep = " ",
            eol = "\n", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"),
            fileEncoding = "")
print(cmb_datasetz[2999000:3000000])
plot(cmb_datasetz[2999000:3000000])

