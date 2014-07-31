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
#
# Modified for use as an Inversive Congruential Generator

# ##############################################################################
# FUNCTION:                 DESCRIPTION:
#  set.icgseed               Set initial random seed
#  get.icgseed               Get the current valus of the random seed
#  runif.icg                 Uniform inversive congruational generator
#  rnorm.icg                 Normal inversive congruational generator
#  rt.icg                    Student-t inversive congruational generator
################################################################################
# DESCRIPTION:
#  A Simple Portable Random Number Generator for Use in R and Splus
#  Use this generator only for comparisons of Programs in R and Splus !!!
#  Method: A inversive congruential generator with
#    icg(a=13445, c=0, m=2^31-1, X0)
#    Note, this is a random number generator which passes the bitwise 
#    randomness test.
#  Reference: 
#    http://csep1.phy.ornl.gov/rn/node13.html 
#    N. S. Altman. ``Bitwise Behavior of Random Number Generators,'' 
#    SIAM J. Sci. Stat. Comput., 9(5), September, pps. 941-949, 1988
#  Example:
#    set.icgseed(4711)
#    cbind(runif.icg(100), rnorm.icg(100), rt.icg(100, df=4))
################################################################################
install.packages("lawstat")
require(lawstat)
library(lawstat)
set.icgseed = 
  function(seed = 999863) 
  {   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Sets the random seed for the inversive congruential 
    #   random number generator
    
    # FUNCTION
    
    # Return Value:
    icg.seed <<- seed
  }


# ------------------------------------------------------------------------------


get.icgseed = 
  function() 
  {   # A function originally implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the random seed for the inversive congruential 
    #   random number generator
    
    # FUNCTION
    
    # Return Value:
    icg.seed
  }


# ------------------------------------------------------------------------------
runif.icg = 
  function(n)
  {   # A function implemented by Diethelm Wuertz
    
    # Description:
    #    A inversive congruential generator for uniform distributed
    #    random numbers
    
    # Notes: 
    #    Important - Use this generator only for comparisons of 
    #      Programs in R and SPlus (and not for production) !!!
    #    Portable Random Numbers:
    #      A inversive congruential generator
    #      icg(a=13445, c=0, m=2^31-1, X0)
    #      This is a random number generator which 
    #      passes the bitwise randomness test
    
    # References: 
    #    http://csep1.phy.ornl.gov/rn/node13.html 
    #    N. S. Altman. ``Bitwise Behavior of Random Number Generators,'' 
    #    SIAM J. Sci. Stat. Comput., 9(5), September, pps. 941-949, 1988
    
    # FUNCTION
    
    # Initialize:
    if(!exists("icg.seed")) icg.seed <<- 999631
    
    # Generate:
    r.icg = rep(0, times = n)
    a = 13445
    c = 12345
    m = 930637
    for (i in 1:n) {
  #    r.icg[i] = icg.seed 
      r.icg[(i+1)] <- (a*(r.icg[(i)]+r.icg[1]) + c) %% m }
  #    r.icg = (max-min)*r.icg + min
    
    # Return Value:
    r.icg
  }
dataset_icg<-runif.icg(3000000)
# datasety<-(100*runif.icg(1000))
write.table(dataset_icg, file="4.0.ICG_dataset.raw", append = FALSE, quote = FALSE, sep = " ",
            eol = "\n", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"),
            fileEncoding = "")