#' Illustrate a one- or two-tailed z test graphically.
#'
#' This function plots the density probability distribution of a z statistic, with appropriate vertical cutlines at the z value. The p-value and the observed z value are plotted. Although largely customizable, only one argument is required (the observed z statistic) for a two-tailed z test. The optional argument \code{tails = "one"} plots a one-tailed test plot (the tail is on the left or right, depending on the sign of the z statistic).
#'
#' @param z A numeric value indicating the observed t statistic.
#' @param tails A character that indicates whether to plot a one (\code{"one"}) or two (\code{"two"}) tailed z-test (optional). By default, a two-tailed test is plotted.
#' @param blank A logical that indicates whether to hide (\code{blank = TRUE}) the test statistic value, p value and cutline. The corresponding colors are actually only made transparent when \code{blank = TRUE}, so that the output is scaled exactly the same (this is useful and especially intended for step-by-step explanations).
#' @param xmax A numeric including the maximum for the x-axis. Defaults to \code{"auto"}, which scales the plot automatically (optional).
#' @param title A character or expression indicating a custom title for the plot (optional).
#' @param xlabel A character or expression indicating a custom title for the x axis (optional).
#' @param ylabel A character or expression indicating a custom title for the y axis (optional).
#' @param fontfamily A character indicating the font family of all the titles and labels (e.g. \code{"serif"} (default), \code{"sans"}, \code{"Helvetica"}, \code{"Palatino"}, etc.) (optional).
#' @param colormiddle A character indicating the color for the "middle" area under the curve (optional).
#' @param colorsides A character indicating the color for the "side(s)" area(s) under the curve (optional).
#' @param colormiddlecurve A character indicating the color for the "middle" part of the curve (optional).
#' @param colorsidescurve A character indicating the color for the "side(s)" part of the curve (optional).
#' @param colorcut A character indicating the color for the cut line at the observed test statistic (optional).
#' @param colorplabel A character indicating the color for the label of the p-value (optional). By default, for color consistency, this color is the same as color of \code{colorright}.
#' @param theme A character indicating one of the predefined color themes. The themes are \code{"default"} (light blue and red), \code{"blackandwhite"}, \code{"whiteandred"}, \code{"blueandred"}, \code{"greenandred"} and \code{"goldandblue"}) (optional). Supersedes \code{colormiddle} and \code{colorsides} if another argument than \code{"default"} is provided.
#' @param signifdigitsz A numeric indicating the number of desired significant figures reported for the z label (optional).
#' @param curvelinesize A numeric indicating the size of the curve line (optional).
#' @param cutlinesize A numeric indicating the size of the cut line(s) (optional). By default, the size of the curve line is used.
#' @return Returns a plot with the density of probability of z under the null hypothesis, annotated with the observed z statistic and the p-value.
#' @examples
#' #Making a z test plot with a z value of 2.
#' plotztest(z = 2)
#'
#' #Note that the same can be obtained even quicker with:
#' plotztest(2)
#'
#' #The same plot without the z or p value
#' plotztest(2, blank = TRUE)
#'
#' #Plotting a one-tailed test using the "tails" parameter.
#' plotztest(z = 2, tails = "one")
#'
#' @author Nils Myszkowski <nmyszkowski@pace.edu>
#' @export plotztest
plotztest <- function(z, tails = "two", blank = FALSE, xmax = "auto", title = "z Test", xlabel = "z", ylabel = "Density of probability\nunder the null hypothesis", fontfamily = "serif", colormiddle = "aliceblue", colorsides = "firebrick3", colormiddlecurve = "black", colorsidescurve = "black", colorcut = "black", colorplabel = colorsides, theme = "default", signifdigitsz = 3, curvelinesize = .4, cutlinesize = curvelinesize) {
  x=NULL

  #Unname inputs (can cause issues)
  z <- unname(z)

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

  #Store the z value provided as argument, used only for one tailed tests, to decide whether to plot left tailed or right tailed
  originalz <- z
  #Use the absolute value of the t provided for the graph
  z <- abs(z)
  #Calculate the p value
  pvalue <- stats::pnorm(z, mean = 0, sd = 1, lower.tail = FALSE)*2
  #Label for half the p value (two tailed)
  phalflab <- as.character(as.expression(bquote(frac(p,2)~.(p_value_format(pvalue/2)))))
  #Label for p value (one tailed)
  plab <- as.character(as.expression(bquote(p~.(p_value_format(pvalue/2)))))
  #Labels for z value and - z value (two tailed)
  zlableft <- as.character(as.expression(bquote(- group("|",z,"|") == .(signif(-z, digits=signifdigitsz)))))
  zlabright <- as.character(as.expression(bquote( + group("|",z,"|") == .(signif(z, digits=signifdigitsz)))))
  #Label for z value (one tailed)
  zlab <- as.character(as.expression(bquote(z == .(signif(originalz, digits=signifdigitsz)))))
  #Define x axis bounds as the maximum between t*3 or 3 (this avoids only the tip of the curve to be plotted when t is small, and keeps a nice t curve shape display)
  if (xmax == "auto") {
    xbound <- max(3*z, 2)
  } else {xbound <- xmax}
  #To ensure lines plotted by stat_function are smooth
  precisionfactor <-  5000
  #To define the function to plot in stat_function
  density <- function(x) stats::dnorm(x, mean = 0, sd = 1)
  #Use the maximum density (top of the curve) to scale the y axis
  maxdensity <- density(0)
  #Use the density corresponding to the z value to place the label above (if this density is too high places the label lower in order to avoid the label being out above the plot)
  y_plabel <- min(density(z)+maxdensity*.1, maxdensity*.7)
  #To place the p value labels on the x axis, at the middle of the part of the curve they correspond to
  x_plabel <- z+(xbound-z)/2
  #To place t labels on the x axis where their cutline is, avoiding that they overlap if the two cutlines are too close
  x_zlabel <- max(z, .5)
  #Define the fill color of the labels as white
  colorlabelfill <- "white"
  #Theme options
  if (theme == "default") {
    colormiddle <- colormiddle
    colorsides <- colorsides
    colorplabel <- colorplabel
  } else if (theme == "blackandwhite"){
    colormiddle <- "grey96"
    colorsides <- "darkgrey"
    colorplabel <- "black"
  } else if (theme == "whiteandred") {
    colormiddle <- "grey96"
    colorsides <- "firebrick3"
    colorplabel <- "firebrick3"
  } else if (theme == "blueandred") {
    colormiddle <- "#104E8B"
    colorsides <- "firebrick3"
    colorplabel <- "firebrick3"
  } else if (theme == "greenandred") {
    colormiddle <- "seagreen"
    colorsides <- "firebrick3"
    colorplabel <- "firebrick3"
  }else if (theme == "goldandblue") {
    colormiddle <- "#FFC61E"
    colorsides <- "#00337F"
    colorplabel <- "#00337F"
  }else warning("The ",'"', "theme", '"', " argument was not recognized. See documentation for a list of available color themes. Reverting to default.")
  #To make some colors transparent when `blank` parameter is TRUE (to only plot de probability density function in that case)
  if (blank == TRUE) {
    colorsides <- grDevices::adjustcolor("white", alpha.f = 0)
    colorcut <- grDevices::adjustcolor("white", alpha.f = 0)
    colorplabel <- grDevices::adjustcolor("white", alpha.f = 0)
    colorlabelfill <- grDevices::adjustcolor("white", alpha.f = 0)
  }
  else {
    #Do nothing
  }
  #Start if else statement relative to the two vs. one tail argument
  if (tails == "two") {
    ggplot2::ggplot(data.frame(x = c(-xbound*2, xbound*2)), ggplot2::aes(x)) +
      #Middle area
      ggplot2::stat_function(fun = area_range(density, -xbound, xbound), geom="area", fill=colormiddle, n=precisionfactor) +
      #Left side area
      ggplot2::stat_function(fun = area_range(density, -xbound, -z), geom="area", fill=colorsides, n=precisionfactor) +
      #Right side area
      ggplot2::stat_function(fun = area_range(density, z, xbound), geom="area", fill=colorsides, n=precisionfactor) +
      #Middle curve
      ggplot2::stat_function(fun = density, xlim = c(-z,z), colour = colormiddlecurve, n=precisionfactor, size=curvelinesize) +
      #Left side curve
      ggplot2::stat_function(fun = density, xlim = c(-xbound,-z), colour = colorsidescurve,size=curvelinesize,n=precisionfactor) +
      #Right side curve
      ggplot2::stat_function(fun = density, xlim = c(z,xbound), colour = colorsidescurve,size=curvelinesize,n=precisionfactor) +
      #Axis labels
      ggplot2::labs(x=xlabel,y=ylabel, size=10) +
      #Define plotting area for extraspace below the graph to place t label
      ggplot2::coord_cartesian(xlim=c(-xbound,xbound),ylim=c(-.05, maxdensity)) +
      #Left cut line
      ggplot2::geom_vline(xintercept = -z, colour = colorcut, size = cutlinesize) +
      #Right cut line
      ggplot2::geom_vline(xintercept = z, colour = colorcut, size = cutlinesize) +
      #Left p label
      ggplot2::geom_label(ggplot2::aes(-x_plabel,y_plabel,label = phalflab), parse = T, colour=colorplabel, fill = colorlabelfill, family = fontfamily) +
      #Right p label
      ggplot2::geom_label(ggplot2::aes(x_plabel,y_plabel,label = phalflab), parse = T, fill = colorlabelfill, colour=colorplabel,family = fontfamily) +
      #Left z label
      ggplot2::geom_label(ggplot2::aes(-x_zlabel,-.05,label = zlableft),colour=colorcut, fill = colorlabelfill, parse = T, family=fontfamily) +
      #Right z label
      ggplot2::geom_label(ggplot2::aes(x_zlabel,-.05,label = zlabright), parse = T, colour=colorcut, fill = colorlabelfill, family=fontfamily) +
      #Add the title
      ggplot2::ggtitle(label = title) +
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



    #If "one" is passed as the tails argument, then we distinguish between a positive and a negative z value
  } else if (tails == "one") {
    if (originalz > 0) {
      ggplot2::ggplot(data.frame(x = c(-xbound*2, xbound*2)), ggplot2::aes(x)) +
        #Left and middle area
        ggplot2::stat_function(fun = area_range(density, -xbound, xbound), geom="area", fill=colormiddle, n=precisionfactor) +
        #Right side area
        ggplot2::stat_function(fun = area_range(density, z, xbound), geom="area", fill=colorsides, n=precisionfactor) +
        #Left and middle curve
        ggplot2::stat_function(fun = density, xlim = c(-xbound,z), colour = colormiddlecurve,size=curvelinesize,n=precisionfactor) +
        #Right side curve
        ggplot2::stat_function(fun = density, xlim = c(z,xbound), colour = colorsidescurve,size=curvelinesize,n=precisionfactor) +
        #Axis labels
        ggplot2::labs(x=xlabel,y=ylabel, size=10) +
        #Define plotting area for extraspace below the graph to place z label
        ggplot2::coord_cartesian(xlim=c(-xbound,xbound),ylim=c(-.05, maxdensity)) +
        #Right cut line
        ggplot2::geom_vline(xintercept = z, colour = colorcut, size = cutlinesize) +
        #Right p label
        ggplot2::geom_label(ggplot2::aes(x_plabel,y_plabel,label = plab), parse = T, fill = colorlabelfill, colour=colorplabel,family = fontfamily) +
        #Right z label
        ggplot2::geom_label(ggplot2::aes(x_zlabel,-.05,label = zlab), parse = T, fill = colorlabelfill, colour=colorcut, family=fontfamily) +
        #Add the title
        ggplot2::ggtitle(label = title) +
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
    } else {
      #Plot left tailed t test plot
      ggplot2::ggplot(data.frame(x = c(-xbound*2, xbound*2)), ggplot2::aes(x)) +
        #Middle and right area
        ggplot2::stat_function(fun = area_range(density, -xbound, xbound), geom="area", fill=colormiddle, n=precisionfactor) +
        #Left side area
        ggplot2::stat_function(fun = area_range(density, -xbound, -z), geom="area", fill=colorsides, n=precisionfactor) +
        #Left side curve
        ggplot2::stat_function(fun = density, xlim = c(-xbound,-z), colour = colorsidescurve,size=curvelinesize,n=precisionfactor) +
        #Middle and right curve
        ggplot2::stat_function(fun = density, xlim = c(-z,xbound), colour = colormiddlecurve, n=precisionfactor, size=curvelinesize) +
        #Axis labels
        ggplot2::labs(x=xlabel,y=ylabel, size=10) +
        #Define plotting area for extraspace below the graph to place t label
        ggplot2::coord_cartesian(xlim=c(-xbound,xbound),ylim=c(-.05, maxdensity)) +
        #Left cut line
        ggplot2::geom_vline(xintercept = -z, colour = colorcut, size = cutlinesize) +
        #Left p label
        ggplot2::geom_label(ggplot2::aes(-x_plabel,y_plabel,label = plab), parse = T, fill = colorlabelfill, colour=colorplabel, family = fontfamily) +
        #Left z label
        ggplot2::geom_label(ggplot2::aes(-x_zlabel,-.05,label = zlab, vjust = 0), fill = colorlabelfill, colour=colorcut, parse = T, family=fontfamily) +
        #Add the title
        ggplot2::ggtitle(label = title) +
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
  } else
    #Stop function is invalid tails argument is provided
    warning("Please specify the number of tails as ", '"', "two", '"', " or ", '"', "one", '"', " for the ", '"', "tails", '"', " argument. Reverting to default (two-tailed).", sep="")
}
