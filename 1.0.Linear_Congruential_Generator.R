# This  library is free software; you can redistribute it and/or
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
install.packages("rstream")
require(rstream)
library(rstream)
set.lcgseed = 
  function(seed = 930499) 
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
  function(n, min = 0, max = 30000000)
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
    
    # FUNCTION
    
    # Initialize:
    if(!exists("lcg.seed")) lcg.seed <<- 930637
    
    # Generate:
    r.lcg = rep(0, times = n)
    a = 13445
    c = 921497
    m = 2^31-1
    for (i in 1:n) {
      lcg.seed <<- (a*lcg.seed + c) %% m 
      r.lcg[i] = lcg.seed/m }
    r.lcg = (max-min)*r.lcg + min
    
    # Return Value:
    r.lcg
  }
# Save the data into two sets and graph them in x,y format. if the data ever is the same it should plot out as a straight diagonal line.
lcg_dataset<-(runif.lcg(3000000))
#datasety<-(100*runif.lcg(1000))
# write.table(datasety, file="datasetY", append = FALSE, quote = TRUE, sep = " ",
#           eol = "\n", na = "NA", dec = ".", row.names = TRUE,
#            col.names = TRUE, qmethod = c("escape", "double"),
#            fileEncoding = "")
#print(lcg_dataset)
write.table(lcg_dataset, file="1.0.LCG_dataset.raw", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"),
            fileEncoding = "")
print(lcg_dataset[2999000:3000000])
plot(lcg_dataset[2999000:3000000])