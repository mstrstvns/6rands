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
#  set.qcgseed               Set initial random seed
#  get.qcgseed               Get the current valus of the random seed
#  runif.qcg                 Uniform linear congruational generator
#  rnorm.qcg                 Normal linear congruational generator
#  rt.qcg                    Student-t linear congruational generator
################################################################################
# DESCRIPTION:
#  A Simple Portable Random Number Generator for Use in R and Splus
#  Use this generator only for comparisons of Programs in R and Splus !!!
#  Method: A linear congruential generator with
#    qcg(a=13445, c=0, m=2^31-1, X0)
#    Note, this is a random number generator which passes the bitwise 
#    randomness test.
#  Reference: 
#    http://csep1.phy.ornl.gov/rn/node13.html 
#    N. S. Altman. ``Bitwise Behavior of Random Number Generators,'' 
#    SIAM J. Sci. Stat. Comput., 9(5), September, pps. 941-949, 1988
#  Example:
#    set.qcgseed(4711)
#    cbind(runif.qcg(100), rnorm.qcg(100), rt.qcg(100, df=4))
################################################################################
install.packages("rstream")
install.packages("doMC")
require(rstream)
library(rstream)
require(doMC)
library(doMC)
registerDoMC(4)

set.qcgseed = 
  function(seed = 9883) 
  {   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Sets the random seed for the linear congruential 
    #   random number generator
    
    # FUNCTION
    
    # Return Value:
    qcg.seed <<- seed
  }


# ------------------------------------------------------------------------------


get.qcgseed = 
  function() 
  {   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the random seed for the linear congruential 
    #   random number generator
    
    # FUNCTION
    
    # Return Value:
    qcg.seed
  }


# ------------------------------------------------------------------------------
runif.qcg = 
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
    #      qcg(a=13445, c=0, m=2^31-1, X0)
    #      This is a random number generator which 
    #      passes the bitwise randomness test
    
    # References: 
    #    http://csep1.phy.ornl.gov/rn/node13.html 
    #    N. S. Altman. ``Bitwise Behavior of Random Number Generators,'' 
    #    SIAM J. Sci. Stat. Comput., 9(5), September, pps. 941-949, 1988
    
    # FUNCTION
    
    # Initialize:
    if(!exists (qcg.seed)) qcg.seed <<- 992153
    
    # Generate:
    r.qcg = rep(0, times = n)
    a = 13445
    d = 9883
    c = 92147
    m = 2^31-1
    foreach (i = 1:n) %dopar% {
      qcg.seed <<- (a*(qcg.seed*qcg.seed) + d*qcg.seed + c) %% m 
      r.qcg[i] = qcg.seed/m }
    r.qcg = (max-min)*r.qcg + min
    
    # Return Value:
    r.qcg
  }
datasetqcg<-(runif.qcg(300000))
write.table(datasetqcg, file="5.0.QCG_dataset.raw", append = FALSE, quote = FALSE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"),
            fileEncoding = "")
