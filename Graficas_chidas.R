###################  Correlación #################



# Presidential data up to and including 2008; data from Stulp et al. 2013
rm(list=ls())
# height of president divided by height of most successful opponent: 
height.ratio <- c(0.924324324, 1.081871345, 1, 0.971098266, 1.029761905,
                  0.935135135, 0.994252874, 0.908163265, 1.045714286, 1.18404908,
                  1.115606936, 0.971910112, 0.97752809, 0.978609626, 1,
                  0.933333333, 1.071428571, 0.944444444, 0.944444444, 1.017142857,
                  1.011111111, 1.011235955, 1.011235955, 1.089285714, 0.988888889,
                  1.011111111, 1.032967033, 1.044444444, 1, 1.086705202,
                  1.011560694, 1.005617978, 1.005617978, 1.005494505, 1.072222222,
                  1.011111111, 0.983783784, 0.967213115, 1.04519774, 1.027777778,
                  1.086705202, 1, 1.005347594, 0.983783784, 0.943005181, 1.057142857)

# proportion popular vote for president vs most successful opponent
# NB can be lower than .5 because popolar vote does not decide election
pop.vote <- c(0.427780852, 0.56148981, 0.597141922, 0.581254292, 0.530344067,
              0.507425996, 0.526679292, 0.536690951, 0.577825976, 0.573225387,
              0.550410082, 0.559380032, 0.484823958, 0.500466176, 0.502934212,
              0.49569636, 0.516904414, 0.522050547, 0.531494442, 0.60014892, 
              0.545079801, 0.604274986, 0.51635906, 0.63850958, 0.652184407, 
              0.587920412, 0.5914898, 0.624614752, 0.550040193, 0.537771958, 
              0.523673642, 0.554517134, 0.577511576, 0.500856251, 0.613444534, 
              0.504063153, 0.617883695, 0.51049949, 0.553073235, 0.59166415, 
              0.538982024, 0.53455133, 0.547304058, 0.497350649, 0.512424242, 
              0.536914796)

# cor.test(height.ratio,pop.vote)
library(plotrix) # package plotrix is needed for function "ablineclip""
# if the following line and the line containing "dev.off()" are executed, the plot will be saved as a png file in the current working directory
# png("Presidental.png", width = 18, height = 18, units = "cm", res = 800, pointsize = 10) 
op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
plot(height.ratio, pop.vote, col = "black", pch = 21, bg = "grey", cex = 2,
     xlim = c(.90,1.20), ylim = c(.40,.70), ylab = "", xlab = "", axes = FALSE)
axis(1)
axis(2) 
reg1 <- lm(pop.vote ~ height.ratio)
ablineclip(reg1, lwd = 2,x1 = .9, x2 = 1.2) 
par(las = 0)
mtext("Presidential Height Ratio", side = 1, line = 2.5, cex = 1.5)
mtext("Relative Support for President", side = 2, line = 3.7, cex = 1.5)
text(1.15, .65, "r = .39", cex = 1.5)
# dev.off()
# For comparison, consider the default plot:
# par(op) # reset to default "par" settings
# plot(height.ratio, pop.vote) #yuk!


###############   Histograms

# rm(list = ls())
# Data: Proportion of choices from the good decks as reported in 39 studies
good.choices <- c(.43, .47, .47, .48, .50, .52, .53, .53, .54, .54, .54, .54, .55, .55, .55, .56, .56, .57, .57, .57, .57, .58, .58, .58, .59, .59, .60, .62, .63, .63, .64, .64, .66, .66, .67, .67, .68, .70, .70)
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
hist(good.choices, main = "", xlab = "", ylab = " ", ylim = c(0, 13), xlim = c(.30, .80), axes = FALSE, col = "grey")
axis(1, seq(.30, .80, by = .1))
axis(2, seq(.00,  12, by = 2))
rug(jitter(good.choices))
mtext("Prop. Choices from Good Decks", side = 1, line = 2.5, cex = 1.5, font = 2)
mtext("Number of Studies", side = 2, line = 3, cex = 1.5, font = 2, las = 0)

################# Including a Density Estimato
good.choices <- c(.43, .47, .47, .48, .50, .52, .53, .53, .54, .54, .54, .54, .55, .55, .55, .56, .56, .57, .57, .57, .57, .58, .58, .58, .59, .59, .60, .62, .63, .63, .64, .64, .66, .66, .67, .67, .68, .70, .70)
yhigh <- 8
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
h <- hist(good.choices, freq = FALSE, main = "", xlab = "", ylab = " ", ylim = c(0, yhigh), xlim = c(.30, .80), axes = FALSE, col = "grey")
axis(1, seq(.30, .80, by = .1))
axis(2, labels = FALSE, lwd.ticks = 0)
rug(jitter(good.choices))
mtext("Prop. Choices from Good Decks", side = 1, line = 2.5, cex = 1.5, font = 2)
mtext("Density of Studies", side = 2, line = 1, cex = 1.5, font = 2, las = 0)
lines(density(good.choices), lwd = 2)



############### Including Numbers on Top

# rm(list = ls())
library(plyr)  # needed for function 'l_ply'
# Data: Proportion of choices from the good decks as reported in 39 studies
good.choices <- c(0.43, 0.47, 0.47, 0.48, 0.5, 0.52, 0.53, 0.53, 0.54, 0.54, 
                  0.54, 0.54, 0.55, 0.55, 0.55, 0.56, 0.56, 0.57, 0.57, 0.57, 0.57, 0.58, 0.58, 
                  0.58, 0.59, 0.59, 0.6, 0.62, 0.63, 0.63, 0.64, 0.64, 0.66, 0.66, 0.67, 0.67, 
                  0.68, 0.7, 0.7)
yhigh <- 8
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
h <- hist(good.choices, freq = FALSE, main = "", xlab = "", ylab = " ", ylim = c(0, 
                                                                                 yhigh), xlim = c(0.3, 0.8), axes = FALSE, col = "grey")
l_ply(seq_along(h$density), function(x) text(h$mids[x], h$density[x] + 0.32, 
                                             round(h$density[x], 2), cex = 1.2))
axis(1, seq(0.3, 0.8, by = 0.1))
axis(2, labels = FALSE, lwd.ticks = 0)
rug(jitter(good.choices))
mtext("Prop. Choices from Good Decks", side = 1, line = 2.5, cex = 1.5, font = 2)
mtext("Density of Studies", side = 2, line = 1, cex = 1.5, font = 2, las = 0)

################ Line Plots

plotsegraph <- function(loc, value, sterr, wiskwidth, color = "grey", linewidth = 2) {
  
  w <- wiskwidth/2
  segments(x0 = loc, x1 = loc, y0 = value - sterr, y1 = value + sterr, col = color, 
           lwd = linewidth)
  segments(x0 = loc - w, x1 = loc + w, y0 = value + sterr, y1 = value + sterr, 
           col = color, lwd = linewidth)  # upper whiskers
  segments(x0 = loc - w, x1 = loc + w, y0 = value - sterr, y1 = value - sterr, 
           col = color, lwd = linewidth)  # lower whiskers
}

RT.hf.sp <- 0.41
RT.lf.sp <- 0.43
RT.vlf.sp <- 0.425
se.RT.hf.sp <- 0.01
se.RT.lf.sp <- 0.015
se.RT.vlf.sp <- 0.02
RT.hf.ac <- 0.46
RT.lf.ac <- 0.51
RT.vlf.ac <- 0.52
se.RT.hf.ac <- 0.01
se.RT.lf.ac <- 0.015
se.RT.vlf.ac <- 0.02

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
x <- c(1, 2, 3, 4)
plot(x, c(-10, -10, -10, -10), type = "p", ylab = "", xlab = " ", cex = 1.5, 
     ylim = c(0.3, 0.6), xlim = c(1, 4), lwd = 2, pch = 5, axes = F, main = " ")
axis(1, at = c(1.5, 2.5, 3.5), labels = c("HF", "LF", "VLF"))
mtext("Word Frequency", side = 1, line = 3, cex = 1.5, font = 2)
axis(2, pos = 1.2, )
par(las = 0)
mtext(expression(paste("Mean ", mu)), side = 2, line = 2, cex = 1.5, font = 2)
x <- c(1.5, 2.5, 3.5)
points(x, c(RT.hf.sp, RT.lf.sp, RT.vlf.sp), cex = 1.5, lwd = 2, pch = 19)
plot.errbars <- plotsegraph(x, c(RT.hf.sp, RT.lf.sp, RT.vlf.sp), c(se.RT.hf.sp, 
                                                                   se.RT.lf.sp, se.RT.vlf.sp), 0.1, color = "black")  #0.1 = wiskwidth
lines(c(1.5, 2.5, 3.5), c(RT.hf.sp, RT.lf.sp, RT.vlf.sp), lwd = 2, type = "c")
points(x, c(RT.hf.ac, RT.lf.ac, RT.vlf.ac), cex = 1.5, lwd = 2, pch = 21)
plot.errbars <- plotsegraph(x, c(RT.hf.ac, RT.lf.ac, RT.vlf.ac), c(se.RT.hf.ac, 
                                                                   se.RT.lf.ac, se.RT.vlf.ac), 0.1, color = "black")  #0.1 = wiskwidth
lines(c(1.5, 2.5, 3.5), c(RT.hf.ac, RT.lf.ac, RT.vlf.ac), lwd = 2, type = "c")
points(1.5, 0.6, pch = 21, lwd = 2, cex = 1.5)
text(1.7, 0.6, "Accuracy", cex = 1.2, font = 1, adj = 0)
points(1.5, 0.57, pch = 19, lwd = 2, cex = 1.5)
text(1.7, 0.57, "Speed", cex = 1.2, font = 1, adj = 0)


############### Box Plot

boxplot.ej <- function(y, xloc = 1, width.box = 0.25, lwd.box = 2, width.hor = 0.25, 
                       lwd.hor = 2, range.wisk = 1.5, lwd.wisk = 2, pch.box = 16, cex.boxpoint = 2, 
                       plot.outliers = FALSE, pch.out = 1, cex.out = 1, color = "black") {
  
  # makes boxplot with dot as median and solid whisker Interquartile range =
  # (.75 quantile) - (.25 quantile).  Note: Wiskers are not always symmetrical;
  # top wisker extends up to max(y) constrained by y <= (.75 quantile) +
  # range.wisk*Interquartile range bottom whisker is determined by min(y)
  # constrained by y >= (.25 quantile) - range.wisk*Interquartile range
  
  Q <- quantile(y, c(0.25, 0.5, 0.75))
  names(Q) <- NULL  # gets rid of percentages
  IQ.range <- Q[3] - Q[1]
  low <- Q[1] - range.wisk * IQ.range
  high <- Q[3] + range.wisk * IQ.range
  index <- which((y >= low) & (y <= high))
  wisk.low <- min(y[index])
  wisk.high <- max(y[index])
  outliers <- y[which((y < low) | (y > high))]
  
  # plot median:
  points(xloc, Q[2], pch = pch.box, cex = cex.boxpoint, col = color)
  
  # plot box:
  xleft <- xloc - width.box/2
  xright <- xloc + width.box/2
  ybottom <- Q[1]
  ytop <- Q[3]
  rect(xleft, ybottom, xright, ytop, lwd = lwd.box, border = color)
  
  # plot whiskers:
  segments(xloc, wisk.low, xloc, Q[1], lwd = lwd.wisk, col = color)
  segments(xloc, Q[3], xloc, wisk.high, lwd = lwd.wisk, col = color)
  
  # plot horizontal segments:
  x0 <- xloc - width.hor/2
  x1 <- xloc + width.hor/2
  segments(x0, wisk.low, x1, wisk.low, lwd = lwd.hor, col = color)
  segments(x0, wisk.high, x1, wisk.high, lwd = lwd.hor, col = color)
  
  # plot outliers:
  if (plot.outliers == TRUE) {
    xloc.p <- rep(xloc, length(outliers))
    points(xloc.p, outliers, pch = pch.out, cex = cex.out, col = color)
  }
}

RT.hf.sp <- rnorm(1000, mean = 0.41, sd = 0.008)
RT.lf.sp <- rnorm(1000, mean = 0.43, sd = 0.01)
RT.vlf.sp <- rnorm(1000, mean = 0.425, sd = 0.012)
RT.hf.ac <- rnorm(1000, mean = 0.46, sd = 0.008)
RT.lf.ac <- rnorm(1000, mean = 0.51, sd = 0.01)
RT.vlf.ac <- rnorm(1000, mean = 0.52, sd = 0.012)

ps <- 1  # size of boxpoint
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
x <- c(1, 2, 3, 4)
plot(x, c(-10, -10, -10, -10), type = "p", ylab = " ", xlab = " ", cex = 1.5, 
     ylim = c(0.3, 0.6), xlim = c(1, 4), lwd = 2, pch = 5, axes = FALSE, main = " ")
axis(1, at = c(1.5, 2.5, 3.5), labels = c("HF", "LF", "VLF"))
mtext("Word Frequency", side = 1, line = 3, cex = 1.5, font = 2)
axis(2, pos = 1.1)
par(las = 0)
mtext("Group Mean M", side = 2, line = 2.9, cex = 1.5, font = 2)

x <- c(1.5, 2.5, 3.5)
boxplot.ej(RT.hf.sp, xloc = 1.5, cex.boxpoint = ps)
boxplot.ej(RT.hf.ac, xloc = 1.5, cex.boxpoint = ps, color = "grey")
boxplot.ej(RT.lf.sp, xloc = 2.5, cex.boxpoint = ps)
boxplot.ej(RT.lf.ac, xloc = 2.5, cex.boxpoint = ps, color = "grey")
boxplot.ej(RT.vlf.sp, xloc = 3.5, cex.boxpoint = ps)
boxplot.ej(RT.vlf.ac, xloc = 3.5, cex.boxpoint = ps, color = "grey")

text(2.5, 0.35, "Speed", cex = 1.4, font = 1, adj = 0.5)
text(2.5, 0.57, "Accuracy", cex = 1.4, font = 1, col = "grey", adj = 0.5)


############### Violin Plot

RT.hf.sp <- rnorm(1000, mean = 0.41, sd = 0.008)
RT.lf.sp <- rnorm(1000, mean = 0.43, sd = 0.01)
RT.vlf.sp <- rnorm(1000, mean = 0.425, sd = 0.012)
RT.hf.ac <- rnorm(1000, mean = 0.46, sd = 0.008)
RT.lf.ac <- rnorm(1000, mean = 0.51, sd = 0.01)
RT.vlf.ac <- rnorm(1000, mean = 0.52, sd = 0.012)

library(sm)

# by Henrik Singmann customized violinplot function (singmann.org) the
# original violinplot function stems from the 'vioplot' package Copyright (c)
# 2004, Daniel Adler. All rights reserved.  Redistribution and use in source
# and binary forms, with or without modification, are permitted provided that
# the following conditions are met: * Redistributions of source code must
# retain the above copyright notice, this list of conditions and the
# following disclaimer.  * Redistributions in binary form must reproduce the
# above copyright notice, this list of conditions and the following
# disclaimer in the documentation and/or other materials provided with the
# distribution.  * Neither the name of the University of Goettingen nor the
# names of its contributors may be used to endorse or promote products
# derived from this software without specific prior written permission.  THIS
# SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

vioplot.singmann <- function(x, ..., range = 1.5, h = NULL, ylim = NULL, names = NULL, 
                             horizontal = FALSE, col = NULL, border = "black", lty = 1, lwd = 1, rectCol = "black", 
                             colMed = "white", pchMed = 19, at, add = FALSE, wex = 1, mark.outlier = TRUE, 
                             pch.mean = 4, ids = NULL, drawRect = TRUE, yaxt = "s") {
  
  # process multiple datas
  datas <- list(x, ...)
  n <- length(datas)
  if (missing(at)) 
    at <- 1:n
  # pass 1 - calculate base range - estimate density setup parameters for
  # density estimation
  upper <- vector(mode = "numeric", length = n)
  lower <- vector(mode = "numeric", length = n)
  q1 <- vector(mode = "numeric", length = n)
  q3 <- vector(mode = "numeric", length = n)
  med <- vector(mode = "numeric", length = n)
  base <- vector(mode = "list", length = n)
  height <- vector(mode = "list", length = n)
  outliers <- vector(mode = "list", length = n)
  baserange <- c(Inf, -Inf)
  
  # global args for sm.density function-call
  args <- list(display = "none")
  
  if (!(is.null(h))) 
    args <- c(args, h = h)
  for (i in 1:n) {
    data <- datas[[i]]
    if (!is.null(ids)) 
      names(data) <- ids
    if (is.null(names(data))) 
      names(data) <- as.character(1:(length(data)))
    
    # calculate plot parameters 1- and 3-quantile, median, IQR, upper- and
    # lower-adjacent
    data.min <- min(data)
    data.max <- max(data)
    q1[i] <- quantile(data, 0.25)
    q3[i] <- quantile(data, 0.75)
    med[i] <- median(data)
    iqd <- q3[i] - q1[i]
    upper[i] <- min(q3[i] + range * iqd, data.max)
    lower[i] <- max(q1[i] - range * iqd, data.min)
    
    # strategy: xmin = min(lower, data.min)) ymax = max(upper, data.max))
    est.xlim <- c(min(lower[i], data.min), max(upper[i], data.max))
    
    # estimate density curve
    smout <- do.call("sm.density", c(list(data, xlim = est.xlim), args))
    
    # calculate stretch factor the plots density heights is defined in range 0.0
    # ... 0.5 we scale maximum estimated point to 0.4 per data
    hscale <- 0.4/max(smout$estimate) * wex
    
    # add density curve x,y pair to lists
    base[[i]] <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1], t[1])
    baserange[2] <- max(baserange[2], t[2])
    min.d <- boxplot.stats(data)[["stats"]][1]
    max.d <- boxplot.stats(data)[["stats"]][5]
    height[[i]] <- height[[i]][(base[[i]] > min.d) & (base[[i]] < max.d)]
    height[[i]] <- c(height[[i]][1], height[[i]], height[[i]][length(height[[i]])])
    base[[i]] <- base[[i]][(base[[i]] > min.d) & (base[[i]] < max.d)]
    base[[i]] <- c(min.d, base[[i]], max.d)
    outliers[[i]] <- list(data[(data < min.d) | (data > max.d)], names(data[(data < 
                                                                               min.d) | (data > max.d)]))
    
    # calculate min,max base ranges
  }
  # pass 2 - plot graphics setup parameters for plot
  if (!add) {
    xlim <- if (n == 1) 
      at + c(-0.5, 0.5) else range(at) + min(diff(at))/2 * c(-1, 1)
    
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }
  if (is.null(names)) {
    label <- 1:n
  } else {
    label <- names
  }
  boxwidth <- 0.05 * wex
  
  # setup plot
  if (!add) 
    plot.new()
  if (!horizontal) {
    if (!add) {
      plot.window(xlim = xlim, ylim = ylim)
      axis(2)
      axis(1, at = at, label = label)
    }
    
    box()
    for (i in 1:n) {
      # plot left/right density curve
      polygon(c(at[i] - height[[i]], rev(at[i] + height[[i]])), c(base[[i]], 
                                                                  rev(base[[i]])), col = col, border = border, lty = lty, lwd = lwd)
      
      if (drawRect) {
        # browser() plot IQR
        boxplot(datas[[i]], at = at[i], add = TRUE, yaxt = yaxt, pars = list(boxwex = 0.6 * 
                                                                               wex, outpch = if (mark.outlier) "" else 1))
        if ((length(outliers[[i]][[1]]) > 0) & mark.outlier) 
          text(rep(at[i], length(outliers[[i]][[1]])), outliers[[i]][[1]], 
               labels = outliers[[i]][[2]])
        # lines( at[c( i, i)], c(lower[i], upper[i]) ,lwd=lwd, lty=lty) plot 50% KI
        # box rect( at[i]-boxwidth/2, q1[i], at[i]+boxwidth/2, q3[i], col=rectCol)
        # plot median point points( at[i], med[i], pch=pchMed, col=colMed )
      }
      points(at[i], mean(datas[[i]]), pch = pch.mean, cex = 1.3)
    }
  } else {
    if (!add) {
      plot.window(xlim = ylim, ylim = xlim)
      axis(1)
      axis(2, at = at, label = label)
    }
    
    box()
    for (i in 1:n) {
      # plot left/right density curve
      polygon(c(base[[i]], rev(base[[i]])), c(at[i] - height[[i]], rev(at[i] + 
                                                                         height[[i]])), col = col, border = border, lty = lty, lwd = lwd)
      
      if (drawRect) {
        # plot IQR
        boxplot(datas[[i]], yaxt = yaxt, at = at[i], add = TRUE, pars = list(boxwex = 0.8 * 
                                                                               wex, outpch = if (mark.outlier) "" else 1))
        if ((length(outliers[[i]][[1]]) > 0) & mark.outlier) 
          text(rep(at[i], length(outliers[[i]][[1]])), outliers[[i]][[1]], 
               labels = outliers[[i]][[2]])
        # lines( at[c( i, i)], c(lower[i], upper[i]) ,lwd=lwd, lty=lty)
      }
      points(at[i], mean(datas[[i]]), pch = pch.mean, cex = 1.3)
    }
  }
  invisible(list(upper = upper, lower = lower, median = med, q1 = q1, q3 = q3))
}

# plot
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
x <- c(1, 2, 3, 4)
plot(x, c(-10, -10, -10, -10), type = "p", ylab = " ", xlab = " ", cex = 1.5, 
     ylim = c(0.3, 0.6), xlim = c(1, 4), lwd = 2, pch = 5, axes = F, main = " ")
axis(1, at = c(1.5, 2.5, 3.5), labels = c("HF", "LF", "VLF"))
axis(2, pos = 1.1)
mtext("Word Frequency", side = 1, line = 3, cex = 1.5, font = 2)

par(las = 0)
mtext("Group Mean M", side = 2, line = 2.9, cex = 1.5, font = 2)

x <- c(1.5, 2.5, 3.5)

vioplot.singmann(RT.hf.sp, RT.lf.sp, RT.vlf.sp, add = TRUE, mark.outlier = FALSE, 
                 at = c(1.5, 2.5, 3.5), wex = 0.4, yaxt = "n")
vioplot.singmann(RT.hf.ac, RT.lf.ac, RT.vlf.ac, add = TRUE, mark.outlier = FALSE, 
                 at = c(1.5, 2.5, 3.5), wex = 0.4, col = "grey", border = "grey", rectCol = "grey", 
                 colMed = "grey", yaxt = "n")

text(2.5, 0.35, "Speed", cex = 1.4, font = 1, adj = 0.5)
text(2.5, 0.58, "Accuracy", cex = 1.4, font = 1, col = "grey", adj = 0.5)



####################  Combined Line and Bar Plot


### Plot 1: RTs on first y-axis, errors on second y-axis

plotsebargraph = function(loc, value, sterr, wiskwidth, color = "grey", linewidth = 2) {
  w = wiskwidth/2
  segments(x0 = loc, x1 = loc, y0 = value, y1 = value + sterr, col = color, 
           lwd = linewidth)
  segments(x0 = loc - w, x1 = loc + w, y0 = value + sterr, y1 = value + sterr, 
           col = color, lwd = linewidth)  # upper whiskers
}
plotsegraph = function(loc, value, sterr, wiskwidth, color = "grey", linewidth = 2) {
  w = wiskwidth/2
  segments(x0 = loc, x1 = loc, y0 = value - sterr, y1 = value + sterr, col = color, 
           lwd = linewidth)
  segments(x0 = loc - w, x1 = loc + w, y0 = value + sterr, y1 = value + sterr, 
           col = color, lwd = linewidth)  # upper whiskers
  segments(x0 = loc - w, x1 = loc + w, y0 = value - sterr, y1 = value - sterr, 
           col = color, lwd = linewidth)  # lower whiskers
}

# =======================================================

# Data; order = Speed, neutral, accuracy
MRT <- c(429, 515, 555)
MRT.se <- c(25, 25, 30)
Er <- c(0.23, 0.14, 0.13)
Er.se <- c(0.022, 0.021, 0.021)

# ======================================================

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
# mpg = c(3, 1, 0) is default. first = axis labels!; middle = tick labels mar
# = c(5, 4, 4, 2) + 0.1 is default

digitsize <- 1.2
x <- c(1, 2, 3, 4)
plot(x, c(-10, -10, -10, -10), type = "p", ylab = " Mean Response Time (ms.)", 
     xlab = " ", cex = 1.5, ylim = c(200, 800), xlim = c(1, 4), lwd = 2, pch = 5, 
     axes = F, main = " ")

axis(1, at = c(1.5, 2.5, 3.5), labels = c("Speed", "Neutral", "Accuracy"))
mtext("Cue", side = 1, line = 3, cex = 1.5, font = 2)
axis(2, at = c(300, 400, 500, 600, 700))

x = c(1.5, 2.5, 3.5)
points(x, MRT, cex = 1.5, lwd = 2, pch = 19)
plot.errbars = plotsegraph(x, MRT, MRT.se, 0.1, color = "black")  #0.1 = wiskwidth

lines(c(1.5, 2.5, 3.5), MRT, lwd = 2, type = "c")
text(1.5, MRT[1] + 60, "429", adj = 0.5, cex = digitsize)
text(2.5, MRT[2] + 60, "515", adj = 0.5, cex = digitsize)
text(3.5, MRT[3] + 60, "555", adj = 0.5, cex = digitsize)

par(new = TRUE)

x <- c(1, 2, 3, 4)
plot(x, c(-10, -10, -10, -10), type = "p", ylab = " ", xlab = " ", cex = 1.5, 
     ylim = c(0, 1), xlim = c(1, 4), lwd = 2, axes = FALSE, main = " ")
axis(4, at = c(0, 0.1, 0.2, 0.3, 0.4), las = 1)
grid::grid.text("Mean Proportion of Errors", 0.97, 0.5, rot = 270, gp = grid::gpar(cex = 1.5, 
                                                                                   font = 2))

width <- 0.25
linewidth <- 2
x0 <- 1.5 - width
x1 <- 1.5 + width
y0 <- 0
y1 <- Er[1]
segments(x0, y0, x0, y1, lwd = linewidth)
segments(x0, y1, x1, y1, lwd = linewidth)
segments(x1, y1, x1, y0, lwd = linewidth)
segments(x1, y0, x0, y0, lwd = linewidth)
x0 <- 2.5 - width
x1 <- 2.5 + width
y0 <- 0
y1 <- Er[2]
segments(x0, y0, x0, y1, lwd = linewidth)
segments(x0, y1, x1, y1, lwd = linewidth)
segments(x1, y1, x1, y0, lwd = linewidth)
segments(x1, y0, x0, y0, lwd = linewidth)
x0 <- 3.5 - width
x1 <- 3.5 + width
y0 <- 0
y1 <- Er[3]
segments(x0, y0, x0, y1, lwd = linewidth)
segments(x0, y1, x1, y1, lwd = linewidth)
segments(x1, y1, x1, y0, lwd = linewidth)
segments(x1, y0, x0, y0, lwd = linewidth)

loc.errbars <- c(1.5, 2.5, 3.5)
plot.errbars <- plotsebargraph(loc.errbars, Er, Er.se, 0.2, color = "black")  # 0.2 = wiskwidth

text(1.5, 0.9, "Behavioral Data", font = 2, cex = 2, pos = 4)

text(1.5, 0.05, "0.23", adj = 0.5, cex = digitsize)
text(2.5, 0.05, "0.14", adj = 0.5, cex = digitsize)
text(3.5, 0.05, "0.13", adj = 0.5, cex = digitsize)



########################## Bar Plots


library(plyr)

mean.prop.sw <- c(0.7, 0.6, 0.67, 0.5, 0.45, 0.48, 0.41, 0.34, 0.5, 0.33)
sd.prop.sw <- c(0.3, 0.4, 0.2, 0.35, 0.28, 0.31, 0.29, 0.26, 0.21, 0.23)
N <- 100
b <- barplot(mean.prop.sw, las = 1, xlab = " ", ylab = " ", col = "grey", cex.lab = 1.7, 
             cex.main = 1.5, axes = FALSE, ylim = c(0, 1))

axis(1, c(0.8, 2, 3.2, 4.4, 5.6, 6.8, 8, 9.2, 10.4, 11.6), 1:10, cex.axis = 1.3)
axis(2, seq(0, 0.8, by = 0.2), cex.axis = 1.3, las = 1)
mtext("Block", side = 1, line = 2.5, cex = 1.5, font = 2)
mtext("Proportion of Switches", side = 2, line = 3, cex = 1.5, font = 2)
l_ply(seq_along(b), function(x) arrows(x0 = b[x], y0 = mean.prop.sw[x], x1 = b[x], 
                                       y1 = mean.prop.sw[x] + 1.96 * sd.prop.sw[x]/sqrt(N), code = 2, length = 0.1, 
                                       angle = 90, lwd = 1.5))





######################## Densities


op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
          font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

yhigh <- 1
xlow <- -3
xhigh <- 3
postmean <- 0.5
postsd <- 0.8
priormean <- 0
priorsd <- 1

plot(function(x) dnorm(x, mean = postmean, sd = postsd), xlow, xhigh, ylim = c(0, 
                                                                               yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 1, ylab = "", xlab = "", main = "Inference for Mu", 
     axes = FALSE)
lines(c(0, 0), c(0, 1.25), lwd = 2, col = "grey")

par(new = TRUE)

plot(function(x) dnorm(x, mean = priormean, sd = priorsd), xlow, xhigh, ylim = c(0, 
                                                                                 yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 2, ylab = "", xlab = "", axes = FALSE)
axis(1)
axis(2)
par(las = 0)
mtext("Mu", side = 1, line = 2.5, cex = 1.5)
mtext("Density", side = 2, line = 3, cex = 1.8)

par(op)


################################ With a Histogram on Top

library(polspline)
Gen.p.within = function(n.draws = 1000, n.data = 20, d = 0, s = 1) {
  
  # Generates p-values from a within-subject (paired) t-test
  
  p <- array(dim = n.draws)
  for (i in 1:n.draws) {
    # yes I know, vectorize is better
    dat <- rnorm(n.data, mean = d, sd = s)
    p[i] <- as.numeric(t.test(dat)$p.value)
  }
  return(p)
}

n.draws <- 20000
n.data <- 20
dfr <- n.data - 1
s <- 1

p.observed <- 0.045
t.observed <- qt(1 - (p.observed/2), dfr)

set.seed(1)
pvalues <- Gen.p.within(n.draws, n.data, d = s * t.observed/sqrt(n.data), s)

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

# Start with probit-transformed uniform distribution, that is, N(0,1):
y.high <- 1
x.high <- 4
x.low <- min(qnorm(pvalues))
curve(dnorm(x), from = -4, to = 4, add = FALSE, col = "black", lwd = 2, ylim = c(0, 
                                                                                 y.high), xlim = c(x.low, x.high), ylab = "Density", xlab = " ", main = " ", 
      axes = FALSE)
axis(1)
axis(2)
mtext("Probit-transformed p Value", side = 1, line = 2.8, cex = 1.5)
greycolhist <- rgb(0, 0, 0, alpha = 0.7)
greycol <- rgb(red = 190, green = 190, blue = 190, alpha = 280, maxColorValue = 280)
# For transparent lines set, set 'alpha' between 0 (invisible) and 255
# (opaque)
lines(c(qnorm(p.observed), qnorm(p.observed)), c(0, y.high), lwd = 2, col = greycol)  #at observed p
points(qnorm(p.observed), dnorm(qnorm(p.observed)), pch = 21, cex = 2, bg = "grey")  # height under H0
height.H0 <- dnorm(qnorm(p.observed))

# Now for probit-transformed distribution of p-values under H1:
par(new = TRUE)
Nbreaks <- 20
small.y <- 0.05

y <- hist(qnorm(pvalues), Nbreaks, plot = FALSE)
plot(c(y$breaks, max(y$breaks)), c(0, y$density, 0), col = greycolhist, type = "S", 
     lwd = 2, lty = 1, ylim = c(0, y.high), xlim = c(x.low, x.high), xlab = " ", 
     ylab = "Density", main = " ")
pvalues.denspp <- logspline(qnorm(pvalues))
par(new = TRUE)
plot(pvalues.denspp, xlim = c(x.low, x.high), ylim = c(0, y.high), col = greycol, 
     lwd = 2)
height.H1 <- dlogspline(qnorm(p.observed), pvalues.denspp)  # height under H1
points(qnorm(p.observed), height.H1, pch = 21, cex = 2, bg = "grey")


##################### Including tex 

NormBF10 <- function(dat, mu = 0, m = 1, priordat = NULL, plot = F, xwide = 3) {
  
  # dat ~ N(theta,1); theta ~ N(mu, 1/m); mu is prior mean, m is prior precision
  if (is.null(priordat)) {
    # no prior data
    priormean <- mu
    priorprec <- m
  }
  if (!is.null(priordat)) {
    # prior data
    n <- length(priordat)
    priormean <- (m * mu + n * mean(priordat))/(m + n)
    priorprec <- m + n
  }
  n <- length(dat)
  posteriormean <- (priorprec * priormean + n * mean(dat))/(priorprec + n)
  posteriorprec <- priorprec + n
  
  prior.height <- dnorm(0, mean = priormean, sd = priorprec^(-0.5))
  posterior.height <- dnorm(0, mean = posteriormean, sd = posteriorprec^(-0.5))
  BF10 <- prior.height/posterior.height
  if (plot == TRUE) {
    par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
        font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
    yhigh <- 1.5
    xlow <- -3
    xhigh <- 3
    plot(function(x) dnorm(x, mean = posteriormean, sd = posteriorprec^(-0.5)), 
         xlow, xhigh, ylim = c(0, yhigh), xlim = c(xlow, xhigh), lwd = 2, 
         lty = 1, ylab = "", xlab = "", axes = FALSE)
    lines(c(0, 0), c(0, 1.25), lwd = 2, col = "grey")
    par(new = TRUE)
    plot(function(x) dnorm(x, mean = priormean, sd = priorprec^(-0.5)), xlow, 
         xhigh, ylim = c(0, yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 2, 
         ylab = "", xlab = "", axes = FALSE)
    axis(1)
    axis(2)
    par(las = 0)
    mtext("Mu", side = 1, line = 2.5, cex = 1.5)
    mtext("Density", side = 2, line = 3, cex = 1.8)
    # Show Savage-Dickey density ratio:
    points(0, prior.height, cex = 2, pch = 21, bg = "grey")
    points(0, posterior.height, cex = 2, pch = 21, bg = "grey")
  }
  invisible(BF10)
}
dat <- c(0, 1, -1)
# dat <- c(-1,1,0)

#### simultaneous #### 1/NormBF10(dat, plot = TRUE) #2 text(-3, 1.4,
#### expression(BF[0][1](y[1],y[2],y[3]) == 2), cex = 1.5, pos = 4)

##### y1 #### 1/NormBF10(dat = dat[1], plot = TRUE) #sqrt(2) text(-3, 1.4,
##### expression(BF[0][1](y[1]) == sqrt(2)), cex = 1.5, pos = 4)

##### y2, given y1 #### 1/NormBF10(dat = dat[2], plot = TRUE, priordat = dat[1]) #1.04
##### composite.expression <- expression(paste(BF[0][1], '(', y[2], ' | ', y[1],
##### ')' %~~% 1.04)) text(-3, 1.4, composite.expression, cex = 1.5, pos = 4)

##### y3, given y1 and y2 ####
BF01 <- 1/NormBF10(dat = dat[3], plot = TRUE, priordat = dat[1:2])  #1.36
composite.expression <- expression(paste(BF[0][1], "(", y[3], " | ", y[1], ",", 
                                         y[2], ")" %~~% 1.36))
text(-3, 1.4, composite.expression, cex = 1.5, pos = 4)



######################## Another Example

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

yhigh <- 3
xi <- c(1, 0.5, 0.1)
plot(function(x) dbeta(x, xi[1], 1), 0, 1, ylim = c(0, yhigh), xlim = c(0, 1), 
     lwd = 2, lty = 1, xlab = " ", ylab = " ")
mtext("p value", 1, line = 2.5, cex = 1.5, font = 2)
mtext("Density", 2, line = 3, cex = 1.5, font = 2, las = 0)

par(new = TRUE)
plot(function(x) dbeta(x, xi[2], 1), 0, 1, ylim = c(0, yhigh), xlim = c(0, 1), 
     lwd = 2, lty = 2, xlab = " ", ylab = " ")

par(new = TRUE)
plot(function(x) dbeta(x, xi[3], 1), 0, 1, ylim = c(0, yhigh), xlim = c(0, 1), 
     lwd = 2, lty = 3, xlab = " ", ylab = " ")

cexsize <- 1.5
text(0.5, 1.15, expression(xi == 1(i.e., H[0])), cex = cexsize, pos = 4)
text(0.1, 1.6, expression(xi == 0.5), cex = cexsize, pos = 4)
text(0, 0.2, expression(xi == 0.1), cex = cexsize, pos = 4)


########################### Highlighting Specific Areas

x <- seq(0, 1, 0.001)
y <- dbeta(x, 2, 4)

y1 <- 0.25
y2 <- 0.62

par(cex.main = 2, mar = c(4, 2, 4, 2) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 2, 
    font.lab = 2, cex.axis = 2, bty = "n", las = 1, lwd = 3)

layout(matrix(c(1, 2), 1, 2))

########################################################## UNBIASED THRESHOLDS ######

plot(x, y, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
     main = "Unbiased Thresholds", bty = "n", yaxt = "n", xaxt = "n", xpd = FALSE)
polygon(c(x[which(x < 0.2)], 0.2, 0), c(y[which(x < 0.2)], 0, 0), col = "#E0E0E0", 
        xpd = FALSE)
polygon(c(x[which(x >= 0.2 & x < 0.4)], 0.4, 0.2), c(y[which(x >= 0.2 & x < 0.4)], 
                                                     0, 0), col = "#C0C0C0", xpd = FALSE)
polygon(c(x[which(x >= 0.4 & x < 0.6)], 0.6, 0.4), c(y[which(x >= 0.4 & x < 0.6)], 
                                                     0, 0), col = "#A0A0A0", xpd = FALSE)
polygon(c(x[which(x >= 0.6 & x < 0.8)], 0.8, 0.6), c(y[which(x >= 0.6 & x < 0.8)], 
                                                     0, 0), col = "#808080", xpd = FALSE)
polygon(c(x[which(x >= 0.8)], 1, 0.8), c(y[which(x >= 0.8)], 0, 0), col = "#606060", 
        xpd = FALSE)
axis(1, c(0, 1), c("0", "1"), lwd = 0, cex = 2, pos = -0.09)
abline(v = 0)
abline(v = 1)
l <- seq(0, 3.2, 0.01)
x1 <- rep(0.2, length(l))
par(new = TRUE)
plot(x1, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
     main = "", bty = "n", yaxt = "n", xaxt = "n")
x2 <- rep(0.4, length(l))
par(new = TRUE)
plot(x2, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
     main = "", bty = "n", yaxt = "n", xaxt = "n")
x3 <- rep(0.6, length(l))
par(new = TRUE)
plot(x3, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
     main = "", bty = "n", yaxt = "n", xaxt = "n")
x4 <- rep(0.8, length(l))
par(new = TRUE)
plot(x4, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
     main = "", bty = "n", yaxt = "n", xaxt = "n")
text(0.2, 3.4, expression(gamma[1]), cex = 2)
text(0.4, 3.4, expression(gamma[2]), cex = 2)
text(0.6, 3.4, expression(gamma[3]), cex = 2)
text(0.8, 3.4, expression(gamma[4]), cex = 2)
text(0.1, 2.6, "1", cex = 2)
text(0.3, 2.6, "2", cex = 2)
text(0.5, 2.6, "3", cex = 2)
text(0.7, 2.6, "4", cex = 2)
text(0.9, 2.6, "5", cex = 2)

########################################################## BIASED THRESHOLDS ######

a <- 0.75  #scaling parameter
b <- 1.5  #shifting parameter
thr_1 <- (b * (0.2^a))/((1 - 0.2)^a + b * (0.2^a))
thr_2 <- (b * (0.4^a))/((1 - 0.4)^a + b * (0.4^a))
thr_3 <- (b * (0.6^a))/((1 - 0.6)^a + b * (0.6^a))
thr_4 <- (b * (0.8^a))/((1 - 0.8)^a + b * (0.8^a))

plot(x, y, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
     main = "Biased Thresholds", bty = "n", yaxt = "n", xaxt = "n", xpd = FALSE)
polygon(c(x[which(x < thr_1)], thr_1, 0), c(y[which(x < thr_1)], 0, 0), col = "#E0E0E0", 
        xpd = FALSE)
polygon(c(x[which(x >= thr_1 & x < thr_2)], thr_2, thr_1), c(y[which(x >= thr_1 & 
                                                                       x < thr_2)], 0, 0), col = "#C0C0C0", xpd = FALSE)
polygon(c(x[which(x >= thr_2 & x < thr_3)], thr_3, thr_2), c(y[which(x >= thr_2 & 
                                                                       x < thr_3)], 0, 0), col = "#A0A0A0", xpd = FALSE)
polygon(c(x[which(x >= thr_3 & x < thr_4)], thr_4, thr_3), c(y[which(x >= thr_3 & 
                                                                       x < thr_4)], 0, 0), col = "#808080", xpd = FALSE)
polygon(c(x[which(x >= thr_4)], 1, thr_4), c(y[which(x >= thr_4)], 0, 0), col = "#606060", 
        xpd = FALSE)
axis(1, c(0, 1), c("0", "1"), lwd = 0, cex = 2, pos = -0.09)
abline(v = 0)
abline(v = 1)
l <- seq(0, 3.2, 0.01)
x1 <- rep(thr_1, length(l))
par(new = TRUE)
plot(x1, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
     main = "", bty = "n", yaxt = "n", xaxt = "n")
x2 <- rep(thr_2, length(l))
par(new = TRUE)
plot(x2, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
     main = "", bty = "n", yaxt = "n", xaxt = "n")
x3 <- rep(thr_3, length(l))
par(new = TRUE)
plot(x3, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
     main = "", bty = "n", yaxt = "n", xaxt = "n")
x4 <- rep(thr_4, length(l))
par(new = TRUE)
plot(x4, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
     main = "", bty = "n", yaxt = "n", xaxt = "n")
text(thr_1, 3.4, expression(delta[i1]), cex = 2)
text(thr_2, 3.4, expression(delta[i2]), cex = 2)
text(thr_3, 3.4, expression(delta[i3]), cex = 2)
text(thr_4, 3.4, expression(delta[i4]), cex = 2)
text(thr_1/2, 2.6, "1", cex = 2)
text((thr_1 + thr_2)/2, 2.6, "2", cex = 2)
text((thr_2 + thr_3)/2, 2.6, "3", cex = 2)
text((thr_3 + thr_4)/2, 2.6, "4", cex = 2)
text((thr_4 + 1)/2, 2.6, "5", cex = 2)


################## More Highlighting of Specific Areas

xbar.therapy <- 92
s.therapy <- 8.5
xbar.placebo <- 85
s.placebo <- 9.1
n <- 15
xdiff <- xbar.therapy - xbar.placebo
sdiff <- sqrt((s.therapy^2 + s.placebo^2)/2) * sqrt(2/n)
sdiff <- sqrt(s.therapy^2 + s.placebo^2)/sqrt(n)

muH0 <- 0
muH1 <- 8

t0 <- (xdiff - muH0)/sdiff

# H0 distribution with p-value shaded:
par(cex.main = 1.5, mar = c(4, 4.5, 4.5, 1), mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)
par(mar = c(4, 4.5, 4.5, 1))

x <- seq(-15, 15, by = 0.001)
y <- dt(x/sdiff, df = 28)
plot(x, y, type = "l", axes = FALSE, xlab = NA, ylab = NA, xlim = c(-15, 20), 
     lwd = 2)
axis(side = 1, at = seq(-15, 15, by = 5), pos = 0, lwd = 2)
axis(side = 1, at = 7, pos = 0, col = "red4", col.axis = "red4", lwd = 2)

# shade area to right of obtained test statistic:
t0 <- xdiff/sdiff
cord.x <- c(t0, seq(t0, 4, 0.001), 4) * sdiff
cord.y <- c(0, dt(seq(t0, 4, 0.001), df = 28), 0)
polygon(cord.x, cord.y, col = "grey")
cord.x <- c(-4, seq(-4, -t0, 0.001), -t0) * sdiff
cord.y <- c(0, dt(seq(-4, -t0, 0.001), df = 24), 0)
polygon(cord.x, cord.y, col = "grey")

# add lines and text:
abline(v = xdiff, col = "red4", lwd = 2)
text(-15, 0.25, expression(paste(H[0], " : ", mu[diff], " = 0", sep = "")), adj = 0, 
     cex = 1.8)
text(10, 0.08, paste("p = .04"), adj = 0, col = "red4", cex = 1.8)
lines(c(10, 8), c(0.05, 0.01), col = "red4", lwd = 2)
lines(c(10, -8), c(0.05, 0.01), col = "red4", lwd = 2)
mtext(expression(bar(x)[diff]), side = 1, line = 2, at = 6.5, adj = 0, col = "red4", 
      cex = 1.8)

################  Still More Highlighting

xbar.therapy <- 92
s.therapy <- 8.5
xbar.placebo <- 85
s.placebo <- 9.1
n <- 15
xdiff <- xbar.therapy - xbar.placebo
sdiff <- sqrt((s.therapy^2 + s.placebo^2)/2) * sqrt(2/n)
sdiff <- sqrt(s.therapy^2 + s.placebo^2)/sqrt(n)

muH0 <- 0
muH1 <- 8

t0 <- (xdiff - muH0)/sdiff
par(cex.main = 1.5, mar = c(4, 4.5, 4.5, 1), mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)

x <- seq(-15, 30, by = 0.001)
y <- dt(x/sdiff, df = 28)
y3 <- dt((x - 9)/sdiff, df = 28)
plot(x, y, type = "l", axes = FALSE, xlab = NA, ylab = NA, xlim = c(-15, 30), 
     lwd = 2)
lines(x, y3, lwd = 2)
axis(side = 1, at = seq(-15, 30, by = 5), labels = seq(-15, 30, by = 5), cex.axis = 1.6, 
     lwd = 2)
axis(side = 1, at = 7, pos = 0, col = "red4", col.axis = "red4", lwd = 2, cex.axis = 1.6, 
     padj = 0.6)

# shade critical regions:
tcrit <- qt(0.975, df = 28)
cord.x <- c(tcrit, seq(tcrit, 4, 0.001), 4) * sdiff
cord.y <- c(0, dt(seq(tcrit, 4, 0.001), df = 28), 0)
polygon(cord.x, cord.y, col = "grey")
cord.x <- c(-4, seq(-4, -tcrit, 0.001), -tcrit) * sdiff
cord.y <- c(0, dt(seq(-4, -tcrit, 0.001), df = 24), 0)
polygon(cord.x, cord.y, col = "grey")

# shade type-II error region
xcrit <- tcrit * sdiff
cord.x <- c(-5, seq(-5, xcrit, 0.001), xcrit)
cord.y <- c(0, dt(((seq(-5, xcrit, 0.001) - 9)/sdiff), df = 28), 0)
polygon(cord.x, cord.y, col = "grey90")

# add lines and text:
abline(v = xdiff, col = "red4", lwd = 2)
text(-16.3, 0.3, expression(paste(H[0], " : ", mu[diff], " = 0", sep = "")), 
     adj = 0, cex = 1.8)
text(13, 0.3, expression(paste(H[1], " : ", mu[diff], "" >= 9, , sep = "")), 
     adj = 0, cex = 1.8)
text(10, 0.08, expression(paste(alpha)), adj = 0, col = "red4", cex = 1.8)
text(-11, 0.08, expression(paste(alpha)), adj = 0, col = "red4", cex = 1.8)
text(1, 0.08, expression(paste(beta)), adj = 0, col = "red4", cex = 1.8)
mtext(expression(bar(x)[diff]), side = 1, line = 2, at = 6.5, adj = 0, col = "red4", 
      cex = 1.8, padj = 0.4)
lines(c(10, 8), c(0.05, 0.01), col = "red4", lwd = 2)
lines(c(-10, -8), c(0.05, 0.01), col = "red4", lwd = 2)
lines(c(2, 4), c(0.05, 0.01), col = "red4", lwd = 2)


##################   Density Ratios


xbar.therapy <- 92
s.therapy <- 8.5
xbar.placebo <- 85
s.placebo <- 9.1
n <- 15
xdiff <- xbar.therapy - xbar.placebo
sdiff <- sqrt((s.therapy^2 + s.placebo^2)/2) * sqrt(2/n)
sdiff <- sqrt(s.therapy^2 + s.placebo^2)/sqrt(n)

muH0 <- 0
muH1 <- 8

t0 <- (xdiff - muH0)/sdiff
par(cex.main = 1.5, mar = c(4, 4.5, 4.5, 1), mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)
par(mar = c(4, 4.5, 4.5, 1))
x <- seq(-15, 30, by = 0.001)
y <- dt(x/sdiff, df = 28)
y3 <- dt((x - 9)/sdiff, df = 28)

plot(x, y, type = "l", axes = FALSE, xlab = NA, ylab = NA, xlim = c(-15, 25), 
     lwd = 2)
lines(x, y3, lwd = 2)
axis(side = 1, at = seq(-15, 30, by = 5), pos = 0, lwd = 2, cex.axis = 1.7)
axis(side = 1, at = 7, pos = 0, col = "red4", col.axis = "red4", lwd = 2, padj = 0.1)
abline(v = xdiff, col = "red4", lwd = 2)
L0 <- dt((xdiff/sdiff), df = 28)
L2 <- dt(((xdiff - 9)/sdiff), df = 28)
lines(c(6.7, 7.3), y = rep(L0, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L2, 2), col = "red4", lwd = 2)
text(8, L0, expression(paste(italic("L"), " = .04")), adj = 0, col = "red4", 
     cex = 1.8)
text(7.5, L2, expression(paste(italic("L"), " = .32")), adj = 0, col = "red4", 
     cex = 1.8)
text(-16, 0.35, expression(paste(H[0], " : ", mu[diff], " = 0", sep = "")), adj = 0, 
     cex = 1.8)
text(-16, 0.3, expression(paste(H[1], " : ", mu[diff], " = 9", sep = "")), adj = 0, 
     cex = 1.8)
mtext(expression(bar(x)[diff]), side = 1, line = 2, at = 6.5, adj = 0, col = "red4", 
      cex = 1.8, padj = 0.1)
text(14, 0.2, expression(paste("LR = ", frac(".32", ".04") %~~% 8, sep = "")), 
     adj = 0, col = "red4", cex = 1.8)



#################  Many Density Ratios

xbar.therapy <- 92
s.therapy <- 8.5
xbar.placebo <- 85
s.placebo <- 9.1
n <- 15
xdiff <- xbar.therapy - xbar.placebo
sdiff <- sqrt((s.therapy^2 + s.placebo^2)/2) * sqrt(2/n)
sdiff <- sqrt(s.therapy^2 + s.placebo^2)/sqrt(n)

muH0 <- 0
muH1 <- 8

t0 <- (xdiff - muH0)/sdiff
par(cex.main = 1.5, mar = c(4, 4.5, 4.5, 1), mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)
par(mar = c(4, 4.5, 4.5, 1))

# Bayes Factor (or, ugliest code ever and I DO NOT CARE TO FIX IT)
x <- seq(-15, 30, by = 0.001)
y <- dt(x/sdiff, df = 28)

y1 <- dt((x - 1)/sdiff, df = 28)
y2 <- dt((x - 2)/sdiff, df = 28)
y3 <- dt((x - 3)/sdiff, df = 28)
y4 <- dt((x - 4)/sdiff, df = 28)
y5 <- dt((x - 5)/sdiff, df = 28)
y6 <- dt((x - 6)/sdiff, df = 28)
y7 <- dt((x - 7)/sdiff, df = 28)
y8 <- dt((x - 8)/sdiff, df = 28)
y9 <- dt((x - 9)/sdiff, df = 28)
y10 <- dt((x - 10)/sdiff, df = 28)

plot(x, y, type = "l", axes = FALSE, xlab = NA, ylab = NA, xlim = c(-15, 25), 
     lwd = 2)

lines(x, y1, col = "grey70")
lines(x, y2, col = "grey70")
lines(x, y3, col = "grey70", lwd = 2)
lines(x, y4, col = "grey70")
lines(x, y5, col = "grey70", lwd = 2)
lines(x, y6, col = "grey70")
lines(x, y7, col = "grey70")
lines(x, y8, col = "grey70", lwd = 2)
lines(x, y9, col = "grey70")
lines(x, y10, col = "grey70", lwd = 2)

axis(side = 1, at = seq(-15, 30, by = 5), pos = 0, lwd = 2, cex.axis = 1.7)
axis(side = 1, at = 7, pos = 0, col = "red4", col.axis = "red4", lwd = 2, padj = 0.1)
abline(v = xdiff, col = "red4", lwd = 2)

L0 <- dt((xdiff/sdiff), df = 28)
L1 <- dt(((xdiff - 1)/sdiff), df = 28)
L2 <- dt(((xdiff - 2)/sdiff), df = 28)
L3 <- dt(((xdiff - 3)/sdiff), df = 28)
L4 <- dt(((xdiff - 4)/sdiff), df = 28)
L5 <- dt(((xdiff - 5)/sdiff), df = 28)
L6 <- dt(((xdiff - 6)/sdiff), df = 28)
L7 <- dt(((xdiff - 7)/sdiff), df = 28)
L8 <- dt(((xdiff - 8)/sdiff), df = 28)
L9 <- dt(((xdiff - 9)/sdiff), df = 28)
L10 <- dt(((xdiff - 10)/sdiff), df = 28)

lines(c(6.7, 7.3), y = rep(L0, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L1, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L2, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L3, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L4, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L5, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L6, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L7, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L8, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L9, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L10, 2), col = "red4", lwd = 2)

text(-16.8, 0.35, expression(paste(H[0], " : ", mu[diff], " = 0", sep = "")), 
     adj = 0, cex = 1.6)
text(-16.8, 0.3, expression(paste(H[1], " : 0", "" <= mu[diff], "" <= 10), sep = ""), 
     adj = 0, cex = 1.6)
text(15, 0.35, expression(paste(italic("L"), "(", H[0], ") = .04")), adj = 0, 
     col = "red4", cex = 1.6)
text(15, 0.3, expression(paste(italic("L"), "(", H[1], ") = .10")), adj = 0, 
     col = "red4", cex = 1.6)
text(14.2, 0.22, expression(paste("BF = ", frac(".10", ".04"), " = ", 2.5, sep = "")), 
     adj = 0, col = "red4", cex = 1.6)
mtext(expression(bar(x)[diff]), side = 1, line = 2, at = 6.5, adj = 0, col = "red4", 
      cex = 1.8, padj = 0.1)



####################### Stacked Densities

op <- par(mar = c(4, 0, 0, 4))

x <- seq(-12, 12, 0.1)
x.ticks <- seq(-12, 12, 2)
y <- x
z <- matrix(0, ncol = length(x), nrow = length(y))
z[, 1] <- dnorm(x)
zcol <- matrix(0, ncol = length(x), nrow = length(y))
zcol[, 1] <- "black"

res <- persp(x, y, z, theta = 0, phi = 0, expand = 0.4, xlab = "", ylab = "", 
             ticktype = "detailed", cex.lab = 0.8, zlab = "", box = FALSE, border = FALSE, 
             xlim = c(-13, 13))
polygon(trans3d(c(x, rev(x)), y = rep(y[1], 2 * length(x)), z = c(dnorm(y, 3.8, 
                                                                        2), rep(0, length(x))), pmat = res), col = rgb(red = 190, green = 190, blue = 190, 
                                                                                                                       alpha = 100, maxColorValue = 300), border = NA)
polygon(trans3d(c(x, rev(x)), y = rep(y[41], 2 * length(x)), z = c(dnorm(y, 6.8), 
                                                                   rep(0, length(x))), pmat = res), col = rgb(red = 190, green = 190, blue = 190, 
                                                                                                              alpha = 140, maxColorValue = 300), border = NA)
polygon(trans3d(c(x, rev(x)), y = rep(y[81], 2 * length(x)), z = c(dnorm(y, -1, 
                                                                         2.5), rep(0, length(x))), pmat = res), col = rgb(red = 190, green = 190, 
                                                                                                                          blue = 190, alpha = 180, maxColorValue = 300), border = NA)
polygon(trans3d(c(x, rev(x)), y = rep(y[121], 2 * length(x)), z = c(dnorm(y, 
                                                                          -5), rep(0, length(x))), pmat = res), col = rgb(red = 190, green = 190, blue = 190, 
                                                                                                                          alpha = 220, maxColorValue = 300), border = NA)
polygon(trans3d(c(x, rev(x)), y = rep(y[161], 2 * length(x)), z = c(dnorm(y, 
                                                                          2.5, 1.5), rep(0, length(x))), pmat = res), col = rgb(red = 190, green = 190, 
                                                                                                                                blue = 190, alpha = 260, maxColorValue = 300), border = NA)
polygon(trans3d(c(x, rev(x)), y = rep(y[201], 2 * length(x)), z = c(dnorm(y, 
                                                                          -9, 0.8), rep(0, length(x))), pmat = res), col = rgb(red = 190, green = 190, 
                                                                                                                               blue = 190, alpha = 300, maxColorValue = 300), border = NA)

### draw x-axis
lines(trans3d(x[which(x == -8):which(x == 10)], min(y) - 2, min(z), res), col = "black", 
      lwd = 1.4)

# tick marks
tick.start <- trans3d(seq(-8, 10, 2), min(y) - 2, min(z), res)
tick.end <- trans3d(seq(-8, 10, 2), min(y) - 2, min(z - 0.01), res)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y, lwd = 2.6)

# labels
labels <- seq(-8, -2, 2)
label.pos <- trans3d(seq(-8, -2, 2), min(y) - 2, min(z - 0.035), res)
text(label.pos$x, label.pos$y, labels = labels, cex = 1.6, adj = 0.65)
labels <- seq(0, 10, 2)
label.pos <- trans3d(seq(0, 10, 2), min(y) - 2, min(z - 0.035), res)
text(label.pos$x, label.pos$y, labels = labels, cex = 1.6, adj = 0.5)

### add labels to distributions
text(trans3d(3.8, y[1], dnorm(3.8, 3.8, 2) + 0.02, res), "a", cex = 1.7)
text(trans3d(6.8, y[41], dnorm(6.8, 6.8) + 0.024, res), "b", cex = 1.7)
text(trans3d(-1, y[81], dnorm(-1, -1, 2.5) + 0.027, res), "c", cex = 1.7)
text(trans3d(-5, y[121], dnorm(-5, -5) + 0.029, res), "d", cex = 1.7)
text(trans3d(2.5, y[161], dnorm(2.5, 2.5, 1.5) + 0.033, res), "e", cex = 1.7)
text(trans3d(-9, y[201], dnorm(-9, -9, 0.8) + 0.039, res), "f", cex = 1.7)

par(op)





###################  Plotting a Function

Max.BF10 = function(p) {
  # Computes the upper bound on the Bayes factor As in Sellke, Bayarri, &
  # Berger, 2001
  Max.BF10 <- -1/(exp(1) * p * log(p))
  return(Max.BF10)
}

# Plot this function for p in .001 to .1
xlow <- 0.001
xhigh <- 0.1
p1 <- 0.0373
p2 <- 0.00752
p3 <- 0.001968
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
plot(function(p) Max.BF10(p), xlow, xhigh, xlim = c(xlow, xhigh), lwd = 2, xlab = " ", 
     ylab = " ")
mtext("Two-sided p value", 1, line = 2.5, cex = 1.5, font = 2)
mtext("Maximum Bayes factor for H1", 2, line = 2.8, cex = 1.5, font = 2, las = 0)
lines(c(0, p1), c(3, 3), lwd = 2, col = "grey")
lines(c(0, p2), c(10, 10), lwd = 2, col = "grey")
lines(c(0, p3), c(30, 30), lwd = 2, col = "grey")
lines(c(p1, p1), c(0, 3), lwd = 2, col = "grey")
lines(c(p2, p2), c(0, 10), lwd = 2, col = "grey")
lines(c(p3, p3), c(0, 30), lwd = 2, col = "grey")

cexsize <- 1.2
text(0.005, 31, expression(max((BF[10])) == 30 %<->% p %~~% 0.002), cex = cexsize, 
     pos = 4)
text(0.01, 11, expression(max((BF[10])) == 10 %<->% p %~~% 0.008), cex = cexsize, 
     pos = 4)
text(p1 - 0.005, 5, expression(max((BF[10])) == 3 %<->% p %~~% 0.037), cex = cexsize, 
     pos = 4)


##############  Time Series
#############   A Diffusion Process


gendat = function(ndat = 1000, dt = 0.1) {
  # Outputs a sequence of Brownian motion data
  dat <- array()
  dat[1] <- rnorm(1, mean = 0, sd = sqrt(dt))
  
  for (j in 1:(ndat - 1)) {
    drift <- 0
    diffvar <- 1
    
    error <- rnorm(1, 0, sqrt(diffvar * dt))
    dat[j + 1] <- dat[j] + drift * dt + error  # Cobb & Zacks (1985), Eq. 1.14
  }
  
  invisible(dat)  # same as 'return', but without printing to console    
}

## General settings:
dt <- 0.1
ntime <- 1000
times <- c(1:ntime)
nsims <- 1000

# Plot settings:
ylow <- -40
yhigh <- 40
xhigh <- 1.3 * ntime
greycol <- rgb(red = 190, green = 190, blue = 190, alpha = 170, maxColorValue = 255)
# For transparent lines set, set 'alpha' between 0 (invisible) and 255
# (opaque)

op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
          font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
plot(times, gendat(ndat = ntime, dt), type = "l", lwd = 1, main = "", ylab = "", 
     xlab = "", axes = FALSE, ylim = c(ylow, yhigh), xlim = c(0, xhigh), cex.lab = 1, 
     font.lab = 2, cex.axis = 0.9, col = greycol, bty = "n")
axis(1, at = c(0, 200, 400, 600, 800, 1000), lab = c("0", "20", "40", "60", "80", 
                                                     "100"))
# NB the labels are divided by 10 because dt = .1
axis(2)
par(las = 0)
mtext("Time", side = 1, line = 2.5, cex = 1.5, at = 500)
mtext("Evidence", side = 2, line = 2.8, cex = 1.5)
lines(c(1, ntime), c(0, 0), lwd = 1, lty = 2, col = "black")

for (i in 1:9) {
  par(new = TRUE)
  plot(times, gendat(ndat = ntime, dt), type = "l", lwd = 1, main = "", ylab = "", 
       xlab = "", axes = FALSE, ylim = c(ylow, yhigh), xlim = c(0, xhigh), cex.lab = 1, 
       font.lab = 2, cex.axis = 0.9, col = greycol, bty = "n")
}

std <- sqrt(ntime * dt)
c <- 7000  # A multiplication factor so the Normal density is visible
df1 <- data.frame(yval = seq(from = -35, to = 35, by = 0.1), xval = (dnorm(seq(from = -35, 
                                                                               to = 35, by = 0.1), 0, std) * c))
with(df1, lines(xval + ntime + 20, yval, lwd = 2))

# Optional: Check by simulation and plot density estimate
do.sim <- FALSE
if (do.sim == TRUE) {
  x <- array()
  for (i in 1:nsims) {
    x[i] <- gendat(ntime, dt)[ntime]  # end point of simulation process
  }
  startx <- ntime + 20
  yhigh.c <- yhigh * c
  y <- density(x)
  lines((y$y * c) + startx, y$x, lwd = 2, col = greycol)
}

par(op)



##########################  A Sequence of Choices  

# rm(list = ls())

IndividualPerformance <- function(choice, lo, show.losses = FALSE) {
  # Plots the choice profile Args: choice: A vector containing the choices on
  # each trial lo: A vector containing the losses on each trial show.losses:
  # logical: Should the losses be indicated by filled dots?
  
  par(mar = c(4, 4.5, 0.5, 1))
  plot(choice, type = "b", axes = FALSE, xlab = "Trial", ylab = "Deck", cex.lab = 2)
  axis(1, seq(0, 100, length = 6), cex.axis = 1.8)
  axis(2, 1:4, labels = c("A", "B", "C", "D"), cex.axis = 1.8, las = 1)
  if (show.losses == TRUE) {
    index.losses <- which(lo < 0)
    points(matrix(c(index.losses, choice[index.losses]), byrow = FALSE, nrow = length(index.losses)), 
           pch = 19, lwd = 1.5)
  }
}

# Synthetic data
choice <- sample(1:4, 100, replace = TRUE)
lo <- sample(c(-1250, -250, -50, 0), 100, replace = TRUE)

# postscript('DiversePerformance.eps', width = 7, height = 7)
IndividualPerformance(choice, lo, show.losses = TRUE)
# dev.off()



#########################   The Electoral Advantage of Being Tall Revisited

# rm(list=ls())

# height of president divided by height of most successful opponent:
height.ratio <- c(0.924324324, 1.081871345, 1, 0.971098266, 1.029761905, 
                  0.935135135, 0.994252874, 0.908163265, 1.045714286, 1.18404908, 1.115606936, 
                  0.971910112, 0.97752809, 0.978609626, 1, 0.933333333, 1.071428571, 
                  0.944444444, 0.944444444, 1.017142857, 1.011111111, 1.011235955, 1.011235955, 
                  1.089285714, 0.988888889, 1.011111111, 1.032967033, 1.044444444, 1, 
                  1.086705202, 1.011560694, 1.005617978, 1.005617978, 1.005494505, 1.072222222, 
                  1.011111111, 0.983783784, 0.967213115, 1.04519774, 1.027777778, 1.086705202, 
                  1, 1.005347594, 0.983783784, 0.943005181, 1.057142857)

# proportion popular vote for president vs most successful opponent
pop.vote <- c(0.427780852, 0.56148981, 0.597141922, 0.581254292, 0.530344067, 
              0.507425996, 0.526679292, 0.536690951, 0.577825976, 0.573225387, 0.550410082, 
              0.559380032, 0.484823958, 0.500466176, 0.502934212, 0.49569636, 0.516904414, 
              0.522050547, 0.531494442, 0.60014892, 0.545079801, 0.604274986, 0.51635906, 
              0.63850958, 0.652184407, 0.587920412, 0.5914898, 0.624614752, 0.550040193, 
              0.537771958, 0.523673642, 0.554517134, 0.577511576, 0.500856251, 0.613444534, 
              0.504063153, 0.617883695, 0.51049949, 0.553073235, 0.59166415, 0.538982024, 
              0.53455133, 0.547304058, 0.497350649, 0.512424242, 0.536914796)

## now calculate BF sequentially; two-sided test
library("hypergeo")
BF10.HG.exact = function(n, r) {
  # Jeffreys' test for whether a correlation is zero or not Jeffreys
  # (1961), pp. 289-292 Note that if the means are subtracted, n needs to
  # be replaced by n-1
  hypgeo = hypergeo((0.25 + n/2), (-0.25 + n/2), (3/2 + n/2), r^2)
  BF10 = (sqrt(pi) * gamma(n/2 + 1) * (hypgeo))/(2 * gamma(3/2 + n/2))
  return(as.numeric(BF10))
}

BF10 <- array()
BF10[1] <- 1
BF10[2] <- 1

for (i in 3:length(height.ratio)) {
  BF10[i] <- BF10.HG.exact(n = i - 1, r = cor(height.ratio[1:i], pop.vote[1:i]))
}

# We wish to plot this Bayes factor sequentially, as it unfolds as more
# elections become available: ============ Plot log Bayes factors ================

par(cex.main = 1.3, mar = c(4.5, 6, 4, 7) + 0.1, mgp = c(3, 1, 0), cex.lab = 1.3, 
    font.lab = 2, cex.axis = 1.3, las = 1)
xhigh <- 60
plot(log(BF10), xlim = c(1, xhigh), ylim = c(-1 * log(200), log(200)), 
     xlab = "", ylab = "", cex.lab = 1.3, cex.axis = 1.3, las = 1, yaxt = "n", 
     bty = "n", type = "p", pch = 21, bg = "grey")

labelsUpper = log(c(100, 30, 10, 3, 1))
labelsLower = -1 * labelsUpper
criticalP = c(labelsLower, 0, labelsUpper)
for (idx in 1:length(criticalP)) {
  abline(h = criticalP[idx], col = "darkgrey", lwd = 1, lty = 2)
}
abline(h = 0)
axis(side = 4, at = criticalP, tick = TRUE, las = 2, cex.axis = 1, labels = FALSE)
axis(side = 4, at = labelsUpper + 0.602, tick = FALSE, cex.axis = 1, labels = c("Extreme", 
                                                                                "Very strong", "Strong", "Moderate", "Anecdotal"))
axis(side = 4, at = labelsLower - 0.602, tick = FALSE, cex.axis = 1, labels = c("Extreme", 
                                                                                "Very strong", "Strong", "Moderate", "Anecdotal"))

axis(side = 2, at = c(criticalP), tick = TRUE, las = 2, cex.axis = 1, labels = c("1/100", 
                                                                                 "1/30", "1/10", "1/3", "1", "", "100", "30", "10", "3", ""))

mtext(expression(BF[1][0]), side = 2, line = 2.5, las = 0, cex = 1.3)
grid::grid.text("Evidence", 0.97, 0.5, rot = 270, gp = grid::gpar(cex = 1.3))
mtext("No. of Elections", side = 1, line = 2.5, las = 1, cex = 1.3)

arrows(20, -log(10), 20, -log(100), length = 0.25, angle = 30, code = 2, 
       lwd = 2)
arrows(20, log(10), 20, log(100), length = 0.25, angle = 30, code = 2, 
       lwd = 2)
text(25, -log(70), "Evidence for H0", pos = 4, cex = 1.3)
text(25, log(70), "Evidence for H1", pos = 4, cex = 1.3)




######################   A Sequential Test on ??


library(plotrix)

### plot multinomial BF

load("BFMultiPi.rda")
load("maxBFMultiPi.rda")
load("exBFMultiPi.rda")

N <- seq(1000, 1e+08, 1000)

par(cex.main = 1.3, mar = c(4.5, 6, 4, 7) + 0.1, mgp = c(3, 1, 0), cex.lab = 1.3, 
    font.lab = 2, cex.axis = 1.3, las = 1)
plot(c(0, log(BFMultiPi)), xlim = c(0, 100001), ylim = c(-20, 80), xlab = "", 
     ylab = "", cex.lab = 1.3, cex.axis = 1.3, las = 1, yaxt = "n", bty = "n", 
     type = "n", pch = 21, bg = "grey", axes = FALSE, lwd = 4, main = expression(paste("Multinomial", 
                                                                                       ~logBF[0][1], ~"for", ~pi, sep = " ")), cex.main = 2)
axis(2, at = seq(-20, 80, 20), labels = seq(-20, 80, 20))
options(scipen = 100, digits = 4)
axis(1, at = seq(from = 0, to = 1e+05, by = 10000), labels = seq(from = 0, 
                                                                 to = 1e+05, by = 10000))
ablineclip(h = 0, lty = 2, x2 = 1e+05, y2 = 0)
mtext(expression(logBF[0][1]), side = 2, line = 3.1, las = 0, cex = 1.7)
mtext(expression("No. of Decimal Places of" ~ pi ~ "(No./1000)"), side = 1, 
      line = 3.1, las = 1, cex = 1.3)
greycol <- rgb(red = 190, green = 190, blue = 190, alpha = 170, maxColorValue = 255)
points(c(0, seq_along(N)), c(0, log(maxBFMultiPi)), type = "l", lwd = 4, 
       col = "red")
points(c(0, seq_along(N)), c(0, log(exBFMultiPi)), type = "l", lwd = 3, 
       col = greycol)
yy <- c(log(maxBFMultiPi), rev(log(exBFMultiPi)))
xx <- c(N/1000, rev(N/1000))
polygon(xx, yy, col = greycol)
text(10000, 7, "Evidence for H0", pos = 4, cex = 1.3)
text(10000, -7, "Evidence for H1", pos = 4, cex = 1.3)
arrows(7000, -2, 7000, -14, length = 0.25, angle = 30, code = 2, lwd = 2)
arrows(7000, 2, 7000, 14, length = 0.25, angle = 30, code = 2, lwd = 2)
points(c(0, seq_along(N)), c(0, log(BFMultiPi)), type = "l", lwd = 3, 
       col = "black")
op <- par(lend = 1)
legend(x = 54000, 45, legend = c(expression(max.BF[0][1]), expression(BF[0][1]), 
                                 expression("95%" ~ BF[0][1] ~ "|" ~ H[0])), lty = c(1, 1, 1), lwd = c(3, 
                                                                                                       3, 20), col = c("red", "black", greycol), bty = "n", x.intersp = 0.5, 
       cex = 1.2)
par(op)
text(39600, 76, "D(a=1)", cex = 1.3)
text(40000, 34.5, "D(a=50)", cex = 1.3)

# add Dirichlet a=50 prior

load("BFMultiPi50.rda")
load("maxBFMultiPi50.rda")
load("exBFMultiPi50.rda")

greycol2 <- rgb(red = 190, green = 190, blue = 190, alpha = 60, maxColorValue = 255)
yy <- c(log(maxBFMultiPi50), rev(log(exBFMultiPi50)))
xx <- c(N/1000, rev(N/1000))
polygon(xx, yy, col = greycol2)
red2 <- rgb(red = 255, green = 0, blue = 0, alpha = 80, maxColorValue = 255)
points(c(0, seq_along(N)), c(0, (log(maxBFMultiPi50))), type = "l", lwd = 4, 
       col = red2, lty = 1)
black2 <- red2 <- rgb(red = 100, green = 100, blue = 100, alpha = 200, 
                      maxColorValue = 300)
points(c(0, seq_along(N)), c(0, (log(BFMultiPi50))), type = "l", lwd = 4, 
       lty = 1, col = black2)


 ################### Multiple Panels
######################### Two panel plot

library(plotrix)

# mix of 2 normal distributions
mixedNorm <- function(x) {
  return(0.5 * dnorm(x, 0.25, 0.13) + 0.5 * dnorm(x, 0.7, 0.082))
}
### normalize so that area [0,1] integrates to 1; k = normalizing constant
k <- 1/integrate(mixedNorm, 0, 1)$value

# normalized
pdfmix <- function(x, k) {
  return(k * (0.5 * dnorm(x, 0.25, 0.13) + 0.5 * dnorm(x, 0.7, 0.082)))
}

# integrate(pdfmix, 0.0790321,0.4048)$value # 0.4

op <- par(mfrow = c(1, 2), mar = c(5.9, 6, 4, 2) + 0.1)

barplot(height = c(0.2, 0.25, 0.1, 0.05, 0.35, 0.05), names.arg = c(1, 
                                                                    2, 3, 4, 5, 6), axes = FALSE, ylim = c(0, 1), width = 1, cex.names = 1.5)
arrows(x0 = 0.6, x1 = 0.6, y0 = 0.38, y1 = 0.23, length = c(0.2, 0.2), 
       lwd = 2)
text(0.6, 0.41, "0.2", cex = 1.3)
ablineclip(v = 1.9, y1 = 0.28, y2 = 0.375, lwd = 2)
ablineclip(v = 4.2, y1 = 0.28, y2 = 0.375, lwd = 2)
ablineclip(h = 0.375, x1 = 1.9, x2 = 4.2, lwd = 2)
arrows(x0 = 3.05, x1 = 3.05, y0 = 0.525, y1 = 0.375, length = c(0.2, 0.2), 
       lwd = 2)
text(3.05, 0.555, "0.4", cex = 1.3)
ablineclip(v = 5.5, y1 = 0.38, y2 = 0.43, lwd = 2)
arrows(x0 = 6.7, x1 = 6.7, y0 = 0.43, y1 = 0.09, length = c(0.2, 0.2), 
       lwd = 2)
ablineclip(h = 0.43, x1 = 5.5, x2 = 6.7, lwd = 2)
text(6.1, 0.46, "7 x", cex = 1.3)
par(las = 1)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), lwd = 2, cex.axis = 1.3)
par(las = 0)
mtext("Probability Mass", side = 2, line = 3.7, cex = 2)
mtext("Value", side = 1, line = 3.7, cex = 2)

par(mar = c(4.6, 6, 3.3, 2) + 0.1)
xx <- c(0.0790321, 0.079031, seq(0.08, 0.4, 0.01), 0.4084, 0.4084)
yy <- c(0, pdfmix(0.079031, k = k), pdfmix(seq(0.08, 0.4, 0.01), k = k), pdfmix(0.4084, k = k), 
        0)
plot(1, type = "n", axes = FALSE, ylab = "", xlab = "", xlim = c(0, 1), 
     ylim = c(0, 3))
polygon(xx, yy, col = "grey", border = NA)
curve(pdfmix(x, k = k), from = 0, to = 1, lwd = 2, ylab = "", xlab = "", xlim = c(0, 
                                                                                  1), ylim = c(0, 3), add = TRUE)
text(0.25, 0.7, "0.4", cex = 1.3)
par(las = 1)
axis(2, at = seq(0, 3, 0.5), labels = seq(0, 3, 0.5), lwd = 2, cex.axis = 1.3)
points(0.539580297, pdfmix(0.539580297, k = k), pch = 21, bg = "white", cex = 1.4, 
       lwd = 2.7)
points(uniroot(function(x) pdfmix(x, k = k) - 5 * pdfmix(0.539580297, k = k), interval = c(0.56, 
                                                                                           0.7))$root, pdfmix(uniroot(function(x) pdfmix(x, k = k) - 5 * pdfmix(0.539580297, k = k), 
                                                                                                                      interval = c(0.56, 0.7))$root, k = k), pch = 21, bg = "white", cex = 1.4, 
       lwd = 2.7)
arrows(x0 = 0.539580297, x1 = 0.539580297, y0 = 2.7, y1 = 0.7, length = c(0.17, 
                                                                          0.17), angle = 19, lwd = 2)
ablineclip(h = 2.7, x1 = 0.539580297, x2 = 0.6994507, lwd = 2)
ablineclip(v = 0.6994507, y1 = 2.55, y2 = 2.7, lwd = 2)
text(0.6194593, 2.79, "5 x", cex = 1.3)
axis(1, at = seq(0, 1, 0.1), labels = c("0", ".1", ".2", ".3", ".4", ".5", 
                                        ".6", ".7", ".8", ".9", "1"), line = -1.2, lwd = 2, cex.axis = 1.37)
par(las = 0)
mtext("Probability Density", side = 2, line = 3.7, cex = 2)
mtext("Value", side = 1, line = 2.4, cex = 2)

par(op)


##################  Buffon's Needle


library(plotrix)
source("HDIofMCMC.R")

load("mcmcChain1.rda")
load("mcmcChain2.rda")

op <- par(mfrow = c(1, 2))

par(cex.main = 1.5, mar = c(5.5, 5.5, 5.9, 3) + 0.1, mgp = c(3.5, 1, 0), 
    cex.lab = 1.5, font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)
y <- hist(mcmcChain1[, "pihat"], freq = F, main = "", xlab = "", ylab = " ", 
          xlim = c(2.1, 5.1), axes = FALSE, breaks = 24, ylim = c(0, 2), yaxt = "n", 
          col = "grey")
axis(1, at = c(2.1, 2.6, pi, 3.6, 4.1, 4.6, 5.1), labels = c(2.1, 2.6, 
                                                             expression(pi), 3.6, 4.1, 4.6, 5.1), lwd = 2, lwd.ticks = 2, line = -0.1)
ablineclip(h = 0, x1 = 5.1, col = "white")
axis(2, at = seq(0, 2, 0.5), line = -0.2, lwd = 2, lwd.ticks = 2)
mtext(expression(hat(pi)), side = 1, line = 4, cex = 2.4, font = 2, adj = 0.5)
mtext("Density", side = 2, line = 3.7, cex = 2.4, font = 2, las = 0)
lines(density(mcmcChain1[, "pihat"], from = 2.1, to = 5.1), lwd = 4)
HDI <- HDIofMCMC(mcmcChain1[, "pihat"])
arrows(x0 = HDI[1], y0 = 1.4, x1 = HDI[2], y1 = 1.4, angle = 90, length = 0.1, 
       code = 3, lwd = 2.2)
text("95% HDI", x = mean(HDI), y = 1.48, cex = 1.8)
text(expression(P(cross) ~ "= .5"), x = 3.99, y = 1, cex = 1.5)


par(cex.main = 1.5, mar = c(5.5, 5.5, 5.9, 3) + 0.1, mgp = c(3.5, 1, 0), 
    cex.lab = 1.5, font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)
h <- hist(mcmcChain2[, "pihat"], freq = F, main = "", xlab = "", ylab = " ", 
          xlim = c(2.1, 5.1), axes = FALSE, col = "grey", breaks = 17, ylim = c(0, 
                                                                                2), , xaxt = "n")
axis(1, at = c(2.1, 2.6, pi, 3.6, 4.1, 4.6, 5.1), labels = c(2.1, 2.6, 
                                                             expression(pi), 3.6, 4.1, 4.6, 5.1), lwd = 2, lwd.ticks = 2, line = -0.1)
axis(2, at = seq(0, 2, 0.5), lwd = 2, lwd.ticks = 2, line = -0.2)
mtext(expression(hat(pi)), side = 1, line = 4, cex = 2.4, font = 2, adj = 0.5)
mtext("Density", side = 2, line = 3.7, cex = 2.4, font = 2, las = 0)
lines(density(mcmcChain2[, "pihat"], from = 2.1, to = 5.1), lwd = 4)
HDI <- HDIofMCMC(mcmcChain2[, "pihat"])
arrows(x0 = HDI[1], y0 = 1.73, x1 = HDI[2], y1 = 1.73, angle = 90, length = 0.1, 
       code = 3, lwd = 2.2)
text("95% HDI", x = mean(HDI), y = 1.81, cex = 1.8)
text(expression(P(cross) ~ "= .63"), x = 4.12, y = 1, cex = 1.5)
mtext(expression("Posterior of" ~ hat(pi)), side = 3, line = -4.6, outer = TRUE, 
      cex = 3.3)

par(op)


###########################  Anscombe's Quartet

# rm(list = ls())

library(stats)
library(graphics)
library(plotrix)

# summary(anscombe) -- now some 'magic' to do the 4 regressions in a
# loop:
ff <- y ~ x
for (i in 1:4) {
  ff[2:3] <- lapply(paste(c("y", "x"), i, sep = ""), as.name)
  ## or ff[[2]] <- as.name(paste('y', i, sep='')) ff[[3]] <-
  ## as.name(paste('x', i, sep=''))
  assign(paste("lm.", i, sep = ""), lmi <- lm(ff, data = anscombe))
  # print(anova(lmi))
}

op <- par(mfrow = c(2, 2), mar = 0.1 + c(4, 4, 1, 1), oma = c(0, 0, 2, 
                                                              0), cex.lab = 1.5, font.lab = 1.5, cex.axis = 1.3, bty = "n", las = 1, 
          cex.main = 1.5)

for (i in 1:4) {
  ff[2:3] <- lapply(paste(c("y", "x"), i, sep = ""), as.name)
  plot(ff, data = anscombe, col = "black", pch = 21, bg = "grey", cex = 2, 
       xlim = c(3, 21), ylim = c(3, 13), ylab = "", xlab = "", axes = F)
  axis(1, at = seq(3, 21, 3))
  axis(2)
  text(15, 6, "r = 0.816", cex = 1.5)
  # mtext(ff[2:3][[1]], side=2, line=2.5, cex=1.3) #y-labels
  # mtext(ff[2:3][[2]], side=1, line=2.5, cex=1.3) #x-labels
  ablineclip(get(paste("lm.", i, sep = "")), x1 = 3, x2 = 21, col = "black", 
             lwd = 2)
}

mtext("Anscombe's Quartet", outer = TRUE, cex = 1.5)
par(op)


################ Four Quite Different Panels


################################## load data #############

# Social Priming Research, all between-subject studies, N=159:
dataSocialPriming <- read.csv("SocialPriming.csv", sep = ";")
# Control Studies, between-subject, N=130:
dataControlsSocialPriming <- read.csv("ControlsSocialPriming.csv", sep = ";")

# p values
pValuesSocialPriming <- dataSocialPriming$pvalue
pValuesControlsSocialPriming <- dataControlsSocialPriming$pvalue

################################## 4-panel plot ###########

source("HDIofMCMC.R")

plotphi <- function(samples, Nbreaks = 80, xlow = 0, xhigh = 1, ylow = 0, 
                    yhigh = 10) {
  
  # Plots the mixture proportion p(H0)
  
  phi <- samples$BUGSoutput$sims.list$phi
  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), 
      cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
  y <- hist(phi, Nbreaks, plot = F)
  plot(c(y$breaks, max(y$breaks)), c(0, y$density, 0), type = "S", lwd = 2, 
       lty = 1, xlim = c(xlow, xhigh), ylim = c(ylow, yhigh), xlab = "", 
       ylab = "", col = "black")
  mtext("H0 Assignment Rate", side = 1, line = 2.7, cex = 1.5)
  par(las = 0)
  mtext("Posterior Density", side = 2, line = 2.5, cex = 1.5)
  lines(c(0.5, 0.5), c(ylow, yhigh), cex = 2, lty = 2)
}

plotz <- function(pvals, samples) {
  
  # Plots the p-value against the probability of being classified as from H0
  
  z <- samples$BUGSoutput$mean$ind
  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), 
      cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
  plot(sort(pvals, dec = TRUE), z, col = "black", pch = 21, bg = "grey", 
       cex = 2, xlim = c(0, 0.05), ylim = c(0, 1), ylab = "", xlab = "", 
       axes = F)
  axis(1)
  axis(2)
  par(las = 0)
  mtext("Significant P Values", side = 1, line = 2.9, cex = 1.5)
  mtext("Probability H0 Assignment", side = 2, line = 3.5, cex = 1.5)
  lines(c(0, 0.05), c(0.5, 0.5), cex = 2, lty = 2)
}

plotpredqq <- function(pvals, samples, ks.test = FALSE) {
  
  # Draws qq plot; computes ks test
  
  predp <- pnorm(samples$BUGSoutput$sims.list$predqp)
  par(cex.main = 1.5, mar = c(5, 6, 4, 4) + 0.1, mgp = c(3.5, 1, 0), 
      cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
  qqplot(pvals, predp, xlab = "", ylab = "", axes = F, col = "black")
  axis(1)
  axis(2)
  mtext("Observed P Value Quantiles", side = 1, line = 2.9, cex = 1.5)
  par(las = 0)
  mtext("Predicted Quantiles ", side = 2, line = 3.7, cex = 1.7)
  abline(a = 0, b = 1, lty = 3)
  if (ks.test == TRUE) 
    ks.test(pvals, predp)
}

histP <- function(pvals, samples, yhigh = 100, col = "lightblue") {
  
  # Plots histogram of the p-values
  
  predp <- pnorm(samples$BUGSoutput$sims.list$predqp)
  par(cex.main = 1.5, mar = c(5, 6, 4, 4) + 0.1, mgp = c(3.5, 1, 0), 
      cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
  hist(pvals, freq = TRUE, xlab = "", ylab = "", col = col, main = "", 
       ylim = c(0, yhigh), xlim = c(0, 0.05), axes = F)
  axis(1)
  par(las = 1)
  axis(2)
  mtext("Significant P Values", side = 1, line = 2.9, cex = 1.5)
  par(las = 0)
  mtext("Number of P Values", side = 2, line = 3.3, cex = 1.6)
  rug(pvals)
}

### load samples

load("samplesSocialPriming_DirichletJags.Rdata")
load("samplesControlsSocialPriming_DirichletJags.Rdata")

pvals <- pValuesSocialPriming
pvals2 <- pValuesControlsSocialPriming
samplesP <- samplesSocialPriming_DirichletJags
samplesP2 <- samplesControlsSocialPriming_DirichletJags

### create plot

op <- par(mfrow = c(2, 2))

yhigh <- 80
greycol2 <- rgb(0, 0, 0, alpha = 0.7)
histP(pvals, samplesP, yhigh, col = greycol2)
greycol <- rgb(red = 190, green = 190, blue = 190, alpha = 95, maxColorValue = 255)
hist(pvals2, freq = TRUE, xlab = "", ylab = "", main = "", ylim = c(0, 
                                                                    yhigh), xlim = c(0, 0.05), col = greycol, axes = F, add = TRUE, lty = 2)
op2 <- par(lend = 1)
legend(x = 0.006, 45, legend = c("Social Priming Studies (N=159)", "Control Studies (N=130)"), 
       lty = c(1, 1, 1), lwd = c(15, 15), col = c(greycol2, greycol), bty = "n", 
       x.intersp = 0.5, cex = 1.4)
par(op2)

plotphi(samplesP, yhigh = 6)
Nbreaks <- 80
xlow <- 0
xhigh <- 1
ylow <- 0
yhigh <- 10
phi1 <- samplesP$BUGSoutput$sims.list$phi
phi2 <- samplesP2$BUGSoutput$sims.list$phi
y <- hist(phi2, Nbreaks, plot = F)
greycol2 <- rgb(0, 0, 0, alpha = 0.5)
lines(c(y$breaks, max(y$breaks)), c(0, y$density, 0), lwd = 2, lty = 1, 
      col = greycol2)
par(las = 1)
text(0.91, 2.7, c("Social \nPriming \nStudies"), cex = 1.3)
text(0.09, 3.5, c("Control \nStudies"), cex = 1.3)
HDI1 <- HDIofMCMC(phi1)  # [0.414, 0.865]
HDI2 <- HDIofMCMC(phi2)  # [0.061, 0.446]
arrows(x0 = HDI1[1], y0 = 4, x1 = HDI1[2], y1 = 4, angle = 90, code = 3, 
       length = 0.08)
mtext("95%", side = 3, at = mean(HDI1), line = -5)
arrows(x0 = HDI2[1], y0 = 4.8, x1 = HDI2[2], y1 = 4.8, angle = 90, code = 3, 
       length = 0.08, col = greycol2)
mtext("95%", side = 3, at = mean(HDI2), line = -3.1, col = greycol2)

plotz(pvals, samplesP)
z <- samplesP2$BUGSoutput$mean$ind
greycol <- rgb(red = 190, green = 190, blue = 190, alpha = 40, maxColorValue = 255)
greycol2 <- rgb(0, 0, 0, alpha = 0.12)
points(sort(pvals2, dec = TRUE), z, col = greycol2, pch = 21, bg = greycol, 
       cex = 2)
text(0.02, 0.92, "Social Priming Studies", cex = 1.4, pos = 4)
text(0.025, 0.38, "Control Studies", cex = 1.4, pos = 4)

plotpredqq(pvals, samplesP)
predp <- pnorm(samplesP2$BUGSoutput$sims.list$predqp)
d <- qqplot(pvals2, predp, xlab = "", ylab = "", axes = FALSE, plot = FALSE)
greycol2 <- rgb(0, 0, 0, alpha = 0.3)
points(d$x, d$y, col = greycol2)
legend(0.019, 0.019, legend = c("Social Priming Studies", "Control Studies"), 
       pch = rep(1, 2), col = c("black", greycol2), lwd = c(2.3, 2.3), lty = c(NA, 
                                                                               NA), bty = "n", x.intersp = 0, cex = 1.4)

par(op)




###################   Nine-Panel Posterior Predictives

library(ggmcmc)
library(gridExtra)
library(grid)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

load("PostPredCheck.Rdata")

############################################## GRAPHS ###

postpred <- list()
for (i in 1:3) postpred[[i]] <- list()

for (i in 1:3) {
  for (j in 1:3) {
    postpred[[i]][[j]] <- ggplot(data[[i]][[j]], aes(x = ypred, y = yobs, 
                                                     size = factor(nrow))) + geom_point() + coord_flip() + 
      theme(panel.grid.minor = element_blank(), 
            lot.title = element_blank(), panel.grid.major = element_blank(), legend.position = "none", axis.title.x = element_blank(), 
                                                                                                                 axis.title.y = element_blank(), axis.text.x = element_text(size = 14), 
                                                                                                                 axis.text.y = element_text(size = 14), panel.background = element_rect(fill = "white", 
                                                                                                                                                                                        colour = "white"), panel.border = element_blank(), axis.line = element_line(size = 1.1), 
                                                                                                                 plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) + geom_abline(size = 1.1)
  }
}

############################################## PLOT ALL GRAPHS IN ONE FIGURE ###

T1 <- textGrob("1 Truth", gp = gpar(cex = 1.5))
T2 <- textGrob("2 Truths", gp = gpar(cex = 1.5))
T3 <- textGrob("3 Truths", gp = gpar(cex = 1.5))

xlab <- textGrob("Predicted Ranking", gp = gpar(cex = 2))
ylab <- textGrob("Observed Ranking", gp = gpar(cex = 2), rot = 90)

Tlab <- textGrob("Data Generated Using", gp = gpar(cex = 2), rot = 270)
Mlab <- textGrob("Model Assumption", gp = gpar(cex = 2))
blank <- textGrob("", gp = gpar(cex = 1.5))

grid.arrange(arrangeGrob(blank, blank, ylab, blank, nrow = 4, heights = c(0.2, 
                                                                          0.2, 3, 0.3)), arrangeGrob(arrangeGrob(blank, Mlab, blank, ncol = 3, 
                                                                                                                 widths = c(0.3, 3, 0.1)), arrangeGrob(blank, T1, blank, T2, blank, 
                                                                                                                                                       T3, ncol = 6, widths = c(0.15, 1, 0.15, 1, 0.15, 1)), arrangeGrob(postpred[[1]][[1]], 
                                                                                                                                                                                                                         postpred[[1]][[2]], postpred[[1]][[3]], ncol = 3), arrangeGrob(postpred[[2]][[1]], 
                                                                                                                                                                                                                                                                                        postpred[[2]][[2]], postpred[[2]][[3]], ncol = 3), arrangeGrob(postpred[[3]][[1]], 
                                                                                                                                                                                                                                                                                                                                                       postpred[[3]][[2]], postpred[[3]][[3]], ncol = 3), arrangeGrob(blank, 
                                                                                                                                                                                                                                                                                                                                                                                                                      xlab, blank, ncol = 3, widths = c(0.3, 3, 0.1)), nrow = 6, heights = c(0.2, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             0.2, 1, 1, 1, 0.2)), arrangeGrob(blank, blank, T1, T2, T3, blank, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              nrow = 6, heights = c(0.2, 0.2, 1, 1, 1, 0.3)), arrangeGrob(blank, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          blank, Tlab, blank, nrow = 4, heights = c(0.2, 0.2, 3, 0.3)), ncol = 4, 
             widths = c(0.2, 2.8, 0.4, 0.2))