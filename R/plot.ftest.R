#' Illustrate an F Test graphically.
#'
#' This function plots the density probability distribution of an F statistic, with a vertical cutline at the observed F value specified. A p-value and the observed F value are plotted. Although largely customizable, only three arguments are required (the observed F and the degrees of freedom).
#'
#' @param f A numeric value indicating the observed F statistic. Alternatively, you can pass an object of class \code{lm} created by the function \code{lm()}.
#' @param dfnum A numeric value indicating the degrees of freedom of the numerator. This argument is optional if you are using an \code{lm} object as the \code{f} argument.
#' @param dfdenom A numeric value indicating the degrees of freedom of the denominator. This argument is optional if you are using an \code{lm} object as the \code{f} argument.
#' @param blank A logical that indicates whether to hide (\code{blank = TRUE}) the test statistic value, p value and cutline. The corresponding colors are actually only made transparent when \code{blank = TRUE}, so that the output is scaled exactly the same (this is useful and especially intended for step-by-step explanations).
#' @param xmax A numeric including the maximum for the x-axis. Defaults to \code{"auto"}, which scales the plot automatically (optional).
#' @param title A character or expression indicating a custom title for the plot (optional).
#' @param xlabel A character or expression indicating a custom title for the x axis (optional).
#' @param ylabel A character or expression indicating a custom title for the y axis (optional).
#' @param fontfamily A character indicating the font family of all the titles and labels (e.g. \code{"serif"} (default), \code{"sans"}, \code{"Helvetica"}, \code{"Palatino"}, etc.) (optional).
#' @param colorleft A character indicating the color for the "left" area under the curve (optional).
#' @param colorright A character indicating the color for the "right" area under the curve (optional).
#' @param colorleftcurve A character indicating the color for the "left" part of the curve (optional).
#' @param colorrightcurve A character indicating the color for the "right" part of the curve (optional). By default, for color consistency, this color is also passed to the label, but this can be changed by providing an argument for the \code{colorlabel} parameter.
#' @param colorcut A character indicating the color for the cut line at the observed test statistic (optional).
#' @param colorplabel A character indicating the color for the label of the p-value (optional). By default, for color consistency, this color is the same as color of \code{colorright}.
#' @param theme A character indicating one of the predefined color themes. The themes are \code{"default"} (light blue and red), \code{"blackandwhite"}, \code{"whiteandred"}, \code{"blueandred"}, \code{"greenandred"} and \code{"goldandblue"}) (optional). Supersedes \code{colorleft} and \code{colorright} if another argument than \code{"default"} is provided.
#' @param signifdigitsf A numeric indicating the number of desired significant figures reported for the F (optional).
#' @param curvelinesize A numeric indicating the size of the curve line (optional).
#' @param cutlinesize A numeric indicating the size of the cut line (optional). By default, the size of the curve line is used.
#' @return A plot with the density of probability of F under the null hypothesis, annotated with the observed test statistic and the p-value.
#' @export plotftest
#' @examples
#' #Making an F plot with an F of 3, and degrees of freedom of 1 and 5.
#' plotftest(f = 4, dfnum = 3, dfdenom = 5)
#'
#' #Note that the same can be obtained even quicker with:
#' plotftest(4,3,5)
#'
#' #The same plot without the f or p value
#' plotftest(4,3,5, blank = TRUE)
#'
#' #Passing an "lm" object
#' x <- rnorm(10) ; y <- x + rnorm(10)
#' fit <- lm(y ~ x)
#' plotftest(fit)
#' plotftest(summary(fit)) # also works
#'
#' @author Nils Myszkowski <nmyszkowski@pace.edu>
plotftest <- function(f, dfnum = f$fstatistic[2], dfdenom = f$fstatistic[3], blank = FALSE, xmax = "auto", title = "F Test", xlabel = "F", ylabel = "Density of probability\nunder the null hypothesis", fontfamily = "serif", colorleft = "aliceblue", colorright = "firebrick3", colorleftcurve = "black", colorrightcurve = "black", colorcut = "black", colorplabel = colorright, theme = "default", signifdigitsf = 3, curvelinesize = .4, cutlinesize = curvelinesize) {
  x=NULL


  # If f is a "summary.lm" object, take values from it
  if (class(f) == "summary.lm") {
    dfnum <- f$fstatistic[2]
    dfdenom <- f$fstatistic[3]
    f <- f$fstatistic[1]
  }

  # If f is a "lm" object, take values from it
  if (class(f) == "lm") {
    dfnum <- summary(f)$fstatistic[2]
    dfdenom <- summary(f)$fstatistic[3]
    f <- summary(f)$fstatistic[1]
  }

  #Unname inputs (can cause issues)
  f <- unname(f)
  dfnum <- unname(dfnum)
  dfdenom <- unname(dfdenom)

  #Create a function to restrict plotting areas to specific bounds of x
  area_range <- function(fun, min, max) {
    function(x) {
      y <- fun(x)
      y[x < min | x > max] <- NA
      return(y)
    }
  }

  # Function to format p value
  p_value_format <- function(p) {
    if (p < .001) {"< .001"} else
      if (p > .999) {"> .999"} else
        paste0("= ", substr(sprintf("%.3f", p), 2, 5))
  }

  #Calculate the p value
  pvalue <- stats::pf(q = f, df1 = dfnum, df2 = dfdenom, lower.tail = FALSE)
  #Label for p value
  plab <- paste0("p ", p_value_format(pvalue))
  #Label for F value
  flab <- paste("F =", signif(f, digits = signifdigitsf), sep = " ")
  #Define x axis bounds as the maximum between 1.5*f or 3 (this avoids only the tip of the curve to be plotted when F is small, and keeps a nice t curve shape display)
  if (xmax == "auto") {
    xbound <- max(1.5*f, 3)
  } else {xbound <- xmax}
  #To ensure lines plotted by stat_function are smooth
  precisionfactor <-  5000
  #To define the function to plot in stat_function
  density <- function(x) stats::df(x, df1 = dfnum, df2 = dfdenom)
  #Use the maximum density (top of the curve) to use as maximum y axis value (start finding maximum at .2 to avoid very high densities values when the density function has a y axis asymptote)
  maxdensity <- stats::optimize(density, interval=c(0.2, xbound), maximum=TRUE)$objective
  #Use the density corresponding to the given f to place the label above (if this density is too high places the label lower in order to avoid the label being out above the plot)
  y_plabel <- min(density(f)+maxdensity*.1, maxdensity*.7)
  #To place the p value labels on the x axis, at the middle of the part of the curve they correspond to
  x_plabel <- f+(xbound-f)/2
  #Define the fill color of the labels as white
  colorlabelfill <- "white"
  #Theme options
  if (theme == "default") {
    colorleft <- colorleft
    colorright <- colorright
    colorplabel <- colorplabel
  } else if (theme == "blackandwhite"){
    colorleft <- "grey96"
    colorright <- "darkgrey"
    colorplabel <- "black"
  } else if (theme == "whiteandred") {
    colorleft <- "grey96"
    colorright <- "firebrick3"
    colorplabel <- "firebrick3"
  } else if (theme == "blueandred") {
    colorleft <- "#104E8B"
    colorright <- "firebrick3"
    colorplabel <- "firebrick3"
  } else if (theme == "greenandred") {
    colorleft <- "seagreen"
    colorright <- "firebrick3"
    colorplabel <- "firebrick3"
  }else if (theme == "goldandblue") {
    colorleft <- "#FFC61E"
    colorright <- "#00337F"
    colorplabel <- "#00337F"
  }else warning("The ",'"', "theme", '"', " argument was not recognized. See documentation for a list of available color themes. Reverting to default.")
  #To make some colors transparent when `blank` parameter is TRUE (to only plot de probability density function in that case)
  if (blank == TRUE) {
    colorright <- grDevices::adjustcolor("white", alpha.f = 0)
    colorcut <- grDevices::adjustcolor("white", alpha.f = 0)
    colorplabel <- grDevices::adjustcolor("white", alpha.f = 0)
    colorlabelfill <- grDevices::adjustcolor("white", alpha.f = 0)
  }
  else {
    #Do nothing
  }
  #Plotting with ggplot2
    ggplot2::ggplot(data.frame(x = c(0, xbound)), ggplot2::aes(x)) +
    #Left side area
    ggplot2::stat_function(fun = area_range(density, 0, xbound), geom="area", fill=colorleft, n=precisionfactor) +
    #Right side area
    ggplot2::stat_function(fun = area_range(density, f, xbound), geom="area", fill=colorright, n=precisionfactor) +
    #Right side curve
    ggplot2::stat_function(fun = density, xlim = c(f,xbound), colour = colorrightcurve,size=curvelinesize) +
    #Left side curve
    ggplot2::stat_function(fun = density, xlim = c(0,f), colour = colorleftcurve, n=1000, size=curvelinesize) +
    #Define plotting area for extraspace (proportional to the max y plotted) below the graph to place f label
    ggplot2::coord_cartesian(xlim=c(0,xbound),ylim=c(maxdensity*(-.08), maxdensity)) +
    #Cut line
    ggplot2::geom_vline(xintercept = f*1, colour = colorcut, size = cutlinesize) +
    #p label
    ggplot2::geom_label(ggplot2::aes(x_plabel,y_plabel,label = plab), colour=colorplabel, fill = colorlabelfill, family=fontfamily) +
    #f label
    ggplot2::geom_label(ggplot2::aes(f,maxdensity*(-.05),label = flab),colour=colorcut, fill = colorlabelfill, family=fontfamily) +
    #Add the title
    ggplot2::ggtitle(title) +
    #Axis labels
    ggplot2::labs(x=xlabel,y=ylabel, size=10) +
    #Apply black and white ggplot theme to avoid grey background, etc.
    ggplot2::theme_bw() +
    #Remove gridlines and pass fontfamily argument to ggplot2
    ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(family = fontfamily),
        axis.text = ggplot2::element_text(family = fontfamily),
        axis.text.x = ggplot2::element_text(family = fontfamily),
        axis.text.y = ggplot2::element_text(family = fontfamily),
        plot.title = ggplot2::element_text(family = fontfamily, hjust = .5),
        legend.text = ggplot2::element_text(family = fontfamily),
        legend.title = ggplot2::element_text(family = fontfamily))
}
