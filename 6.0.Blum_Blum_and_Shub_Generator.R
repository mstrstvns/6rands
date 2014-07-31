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
# 2014, Matthew Stevens GPL
#  Matthew Stevens <e1328375@student.tuwien.ac.at>
# ##############################################################################
# FUNCTION:                 DESCRIPTION:
#  set.bbsseed               Set initial random seed for X0
#  get.bbsseed               Get the current valus of the random seed
#  r.bbs                     Blum Blum and Shub generator
#  rt.bbs                    Student-t Blum, Blum, and Shub generator
################################################################################
# DESCRIPTION:
#  A Simple Blum Blum and Shub Random Number Generator for Use in R
#  Example:
#    set.bbsseed(4711)
#    cbind(r.bbs(100), rt.bbs)
################################################################################
install.packages("rstream")
install.packages("bitops")
install.packages("highr")
install.packages("foreach")
require(bitops)
require(rstream)
require(highr)
require(foreach)
set.bbsseed = 
  function(seed = 1367) 
  {   # A function implemented by Blum Blum and Shub
    
    # Description:
    #   Sets the random seed for the Blum Blum and Shub 
    #   random number generator
    
    # FUNCTION
    
    # Return Value:
    bbs.seed <<- seed
  }


# ------------------------------------------------------------------------------


get.bbsseed = 
  function() 
  {   # A function implemented by Matthew Stevens
    
    # Description:
    #   Returns the random seed for the Blum Blum and Shub number generator
    
    # FUNCTION
    
    # Return Value:
    bbs.seed
  }


# ------------------------------------------------------------------------------
r.bbs = 
  function(n)
  {   # A function implemented by Matthew Stevens
    
    # Description:
    #    A Blum Blum and Shub generator for uniform distributed
    #    random numbers
        
    # FUNCTION
    
    # Initialize:
    if(!exists("bbs.seed")) bbs.seed <<- 10111
    
    # Generate:
    r.bbs = rep(0, times = n)
    p = 6359
    q = 7919 
    m = p*q
    for (i in 1:n) {
      bbs.seed <<- (bbs.seed*bbs.seed) %% m 
      r.bbs[i] = bitAnd(bbs.seed, 1) }
    # Return Value:
    r.bbs
  }


bbs_dataset<-(r.bbs(3000000))
write.table(bbs_dataset, file="6.0.bbs_dataset.raw", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"),
            fileEncoding = "")
 print(bbs_dataset[2999900:3000000])
 #plot(bbs_dataset[2999900:3000000])