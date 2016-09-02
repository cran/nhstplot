#' Illustrate a one- or two-tailed Student's t test graphically.
#'
#' This function plots the density probability distribution of a Student's t statistic, with appropriate vertical cutlines at the t value. The p-value and the observed t value are plotted. Although largely customizable, only two arguments are required (the observed t statistic and the degrees of freedom) for a two-tailed t test. The optional argument tails = "one" plots a one-tailed test plot (the tail is on the left or right, depending on the sign of the t statistic).
#'
#' @param t A numeric value indicating the observed t statistic.
#' @param df A numeric value indicating the degrees of freedom.
#' @param tails A string that indicates whether to plot a one ("one") or two ("two") tailed t-test (optional). By default, a two-tailed test is plotted.
#' @param blank A logical that indicates whether to hide (blank = TRUE) the test statistic value, p value and cutline. The corresponding colors are actually only made transparent when blank = TRUE, so that the output is scaled exactly the same (this is useful and especially intended for step-by-step explanations).
#' @param title A string or expression indicating a custom title for the plot (optional).
#' @param xlabel A string or expression indicating a custom title for the x axis (optional).
#' @param ylabel A string or expression indicating a custom title for the y axis (optional).
#' @param fontfamily A string indicating the font family of all the titles and labels (e.g. "serif" (default), "sans", "Helvetica", "Palatino", etc.) (optional).
#' @param colormiddle A string indicating the color for the "middle" area under the curve (optional).
#' @param colorsides A string indicating the color for the "side(s)" area(s) under the curve (optional).
#' @param colormiddlecurve A string indicating the color for the "middle" part of the curve (optional).
#' @param colorsidescurve A string indicating the color for the "side(s)" part of the curve (optional).
#' @param colorcut A string indicating the color for the cut line at the observed test statistic (optional).
#' @param colorplabel A string indicating the color for the label of the p-value (optional). By default, for color consistency, this color is the same as color of "colorright".
#' @param theme A string indicating one of the predefined color themes. The themes are "default" (light blue and red), "blackandwhite", "whiteandred", "blueandred", "greenandred" and "goldandblue") (optional). Supersedes "colormiddle" and "colorsides" if another argument than "default" is provided.
#' @param signifdigitsp A numeric indicating the number of desired significant figures reported for the p-value label (optional).
#' @param signifdigitst A numeric indicating the number of desired significant figures reported for the t label (optional).
#' @param curvelinesize A numeric indicating the size of the curve line (optional).
#' @param cutlinesize A numeric indicating the size of the cut line(s) (optional). By default, the size of the curve line is used.
#' @return Returns a plot with the density of probability of t under the null hypothesis, annotated with the observed test statistic and the p-value.
#' @examples
#' #Making a t test plot with a t value of 2 and df of 10
#' plotttest(t = 2, df = 10)
#'
#' #Note that the same can be obtained even quicker with:
#' plotttest(2,10)
#'
#' #The same plot without the t or p value
#' plotttest(2,10, blank = TRUE)
#'
#' #Plotting a one-tailed test using the "tails" parameter.
#' plotttest(t = 2, df = 10, tails = "one")
#'
#' #If a negative t is provided, the tail is on the left.
#' plotttest(t = -2, df = 10, tails = "one")
#'
#' #Changing the fontfamily to "sans" and changing the color theme to "blackandwhite".
#' plotttest(t = 2, df = 10, fontfamily = "sans", theme = "blackandwhite")
#'
#' #Using specific colors and changing the curve line size
#' plotttest(t = 2, df = 10, colormiddle = "grey96", colorsides = "indianred", curvelinesize=1)
#'
#' @author Nils Myszkowski <nmyszkowski@pace.edu>
#' @export plotttest

plotttest <- function(t, df, tails = "two", blank = FALSE, title = "t Test", xlabel = "t", ylabel = "Density of probability\nunder the null hypothesis", fontfamily = "serif", colormiddle = "aliceblue", colorsides = "firebrick3", colormiddlecurve = "black", colorsidescurve = "black", colorcut = "black", colorplabel = colorsides, theme = "default", signifdigitsp = 3, signifdigitst = 3, curvelinesize = .4, cutlinesize = curvelinesize) {
  x=NULL
  #Create a function to restrict plotting areas to specific bounds of x
  area_range <- function(fun, min, max) {
    function(x) {
      y <- fun(x)
      y[x < min | x > max] <- NA
      return(y)
    }
  }
  #Store the t value provided as argument, used only for one tailed tests, to decide whether to plot left tailed or right tailed
  originalt <- t
  #Use the absolute value of the t provided for the graph
  t <- abs(t)
  #Calculate the p value
  pvalue <- stats::pt(t, df = df, lower.tail = FALSE)*2
  #Label for half the p value (two tailed)
  phalflab <- as.character(as.expression(bquote(frac(p,2) == .(signif(pvalue/2, digits=signifdigitsp)))))
  #Label for p value (one tailed)
  plab <- as.character(as.expression(bquote(p == .(signif(pvalue/2, digits=signifdigitsp)))))
  #Labels for t value and - t value (two tailed)
  tlableft <- as.character(as.expression(bquote(- group("|",t,"|") == .(signif(-t, digits=signifdigitst)))))
  tlabright <- as.character(as.expression(bquote( + group("|",t,"|") == .(signif(t, digits=signifdigitst)))))
  #Label for t value (one tailed)
  tlab <- as.character(as.expression(bquote(t == .(signif(originalt, digits=signifdigitst)))))
  #Define x axis bounds as the maximum between t*3 or 3 (this avoids only the tip of the curve to be plotted when t is small, and keeps a nice t curve shape display)
  xbound <- max(3*t, 2)
  #To ensure lines plotted by stat_function are smooth
  precisionfactor <-  5000
  #To define the function to plot in stat_function
  density <- function(x) stats::dt(x, df)
  #Use the maximum density (top of the curve) to scale the y axis
  maxdensity <- density(0)
  #Use the density corresponding to the t value to place the label above (if this density is too high places the label lower in order to avoid the label being out above the plot)
  y_plabel <- min(density(t)+maxdensity*.1, maxdensity*.7)
  #To place the p value labels on the x axis, at the middle of the part of the curve they correspond to
  x_plabel <- t+(xbound-t)/2
  #To place t labels on the x axis where their cutline is, avoiding that they overlap if the two cutlines are too close
  x_tlabel <- max(t, .5)
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
          #Axis labels
          ggplot2::labs(x=xlabel,y=ylabel, size=10) +
          #Middle area
          ggplot2::stat_function(fun = area_range(density, -xbound, xbound), geom="area", fill=colormiddle, n=precisionfactor) +
          #Right side area
          ggplot2::stat_function(fun = area_range(density, t, xbound), geom="area", fill=colorsides, n=precisionfactor) +
          #Left side area
          ggplot2::stat_function(fun = area_range(density, -xbound, -t), geom="area", fill=colorsides, n=precisionfactor) +
          #Left side curve
          ggplot2::stat_function(fun = density, xlim = c(-xbound,-t), colour = colorsidescurve,size=curvelinesize,n=precisionfactor) +
          #Right side curve
          ggplot2::stat_function(fun = density, xlim = c(t,xbound), colour = colorsidescurve,size=curvelinesize,n=precisionfactor) +
          #middle curve
          ggplot2::stat_function(fun = density, xlim = c(-t,t), colour = colormiddlecurve, n=precisionfactor, size=curvelinesize) +
          #Define plotting area for extraspace below the graph to place t label
          ggplot2::coord_cartesian(xlim=c(-xbound,xbound),ylim=c(-.05, maxdensity)) +
          #Left cut line
          ggplot2::geom_vline(xintercept = -t, colour = colorcut, size = cutlinesize) +
          #Right cut line
          ggplot2::geom_vline(xintercept = t, colour = colorcut, size = cutlinesize) +
          #Left p label
          ggplot2::geom_label(ggplot2::aes(-x_plabel,y_plabel,label = phalflab), parse = T, fill = colorlabelfill, colour=colorplabel, family = fontfamily) +
          #Right p label
          ggplot2::geom_label(ggplot2::aes(x_plabel,y_plabel,label = phalflab), parse = T, fill = colorlabelfill, colour=colorplabel, family = fontfamily) +
          #Left t label
          ggplot2::geom_label(ggplot2::aes(-x_tlabel,-.04,label = tlableft), fill = colorlabelfill, colour=colorcut, parse = T, family=fontfamily) +
          #Right t label
          ggplot2::geom_label(ggplot2::aes(x_tlabel,-.04,label = tlabright), parse = T, fill = colorlabelfill, colour=colorcut, family=fontfamily) +
          #Add the title
          ggplot2::ggtitle(title) +
          #Apply black and white ggplot theme to avoid grey background, etc.
          ggplot2::theme_bw() +
          #Remove gridlines and pass fontfamily argument to ggplot2
          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(linetype = "solid"),
            axis.title = ggplot2::element_text(family = fontfamily),
            axis.text = ggplot2::element_text(family = fontfamily),
            axis.text.x = ggplot2::element_text(family = fontfamily),
            axis.text.y = ggplot2::element_text(family = fontfamily),
            plot.title = ggplot2::element_text(family = fontfamily),
            legend.text = ggplot2::element_text(family = fontfamily),
            legend.title = ggplot2::element_text(family = fontfamily))



     #If "one" is passed as the tails argument, then we distinguish between a positive and a negative t value
  } else if (tails == "one") {
    if (originalt > 0) {
          ggplot2::ggplot(data.frame(x = c(-xbound*2, xbound*2)), ggplot2::aes(x)) +
          #Axis labels
          ggplot2::labs(x=xlabel,y=ylabel, size=10) +
          #Left and middle area
          ggplot2::stat_function(fun = area_range(density, -xbound, xbound), geom="area", fill=colormiddle, n=precisionfactor) +
          #Right side area
          ggplot2::stat_function(fun = area_range(density, t, xbound), geom="area", fill=colorsides, n=precisionfactor) +
          #Left and middle curve
          ggplot2::stat_function(fun = density, xlim = c(-xbound,t), colour = colormiddlecurve,size=curvelinesize,n=precisionfactor) +
          #Right side curve
          ggplot2::stat_function(fun = density, xlim = c(t,xbound), colour = colorsidescurve,size=curvelinesize,n=precisionfactor) +
          #Define plotting area for extraspace below the graph to place t label
          ggplot2::coord_cartesian(xlim=c(-xbound,xbound),ylim=c(-.05, maxdensity)) +
          #Right cut line
          ggplot2::geom_vline(xintercept = t, colour = colorcut, size = cutlinesize) +
          #Right p label
          ggplot2::geom_label(ggplot2::aes(x_plabel,y_plabel,label = plab), parse = T, fill = colorlabelfill, colour=colorplabel,family = fontfamily) +
          #Right t label
          ggplot2::geom_label(ggplot2::aes(x_tlabel,-.05,label = tlab), parse = T, fill = colorlabelfill, colour=colorcut, family=fontfamily) +
          #Add the title
          ggplot2::ggtitle(title) +
          #Apply black and white ggplot theme to avoid grey background, etc.
          ggplot2::theme_bw() +
          #Remove gridlines and pass fontfamily argument to ggplot2
          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(linetype = "solid"),
            axis.title = ggplot2::element_text(family = fontfamily),
            axis.text = ggplot2::element_text(family = fontfamily),
            axis.text.x = ggplot2::element_text(family = fontfamily),
            axis.text.y = ggplot2::element_text(family = fontfamily),
            plot.title = ggplot2::element_text(family = fontfamily),
            legend.text = ggplot2::element_text(family = fontfamily),
            legend.title = ggplot2::element_text(family = fontfamily))
    } else {
    #Plot left tailed t test plot
          ggplot2::ggplot(data.frame(x = c(-xbound*2, xbound*2)), ggplot2::aes(x)) +
          #Axis labels
          ggplot2::labs(x=xlabel,y=ylabel, size=10) +
          #Middle and right area
          ggplot2::stat_function(fun = area_range(density, -xbound, xbound), geom="area", fill=colormiddle, n=precisionfactor) +
          #Left side area
          ggplot2::stat_function(fun = area_range(density, -xbound, -t), geom="area", fill=colorsides, n=precisionfactor) +
          #Left side curve
          ggplot2::stat_function(fun = density, xlim = c(-xbound,-t), colour = colorsidescurve,size=curvelinesize,n=precisionfactor) +
          #Middle and right curve
          ggplot2::stat_function(fun = density, xlim = c(-t,xbound), colour = colormiddlecurve, n=precisionfactor, size=curvelinesize) +
          #Define plotting area for extraspace below the graph to place t label
          ggplot2::coord_cartesian(xlim=c(-xbound,xbound),ylim=c(-.05, maxdensity)) +
          #Left cut line
          ggplot2::geom_vline(xintercept = -t, colour = colorcut, size = cutlinesize) +
          #Left p label
          ggplot2::geom_label(ggplot2::aes(-x_plabel,y_plabel,label = plab), parse = T, fill = colorlabelfill, colour=colorplabel, family = fontfamily) +
          #Left t label
          ggplot2::geom_label(ggplot2::aes(-x_tlabel,-.05,label = tlab),fill = colorlabelfill, colour=colorcut, parse = T, family=fontfamily) +
          #Add the title
          ggplot2::ggtitle(title) +
          #Apply black and white ggplot theme to avoid grey background, etc.
          ggplot2::theme_bw() +
          #Remove gridlines and pass fontfamily argument to ggplot2
          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(linetype = "solid"),
            axis.title = ggplot2::element_text(family = fontfamily),
            axis.text = ggplot2::element_text(family = fontfamily),
            axis.text.x = ggplot2::element_text(family = fontfamily),
            axis.text.y = ggplot2::element_text(family = fontfamily),
            plot.title = ggplot2::element_text(family = fontfamily),
            legend.text = ggplot2::element_text(family = fontfamily),
            legend.title = ggplot2::element_text(family = fontfamily))
    }
  } else
    #Stop function is invalid tails argument is provided
    warning("Please specify the number of tails as ", '"', "two", '"', " or ", '"', "one", '"', " for the ", '"', "tails", '"', " argument. Reverting to default (two-tailed).", sep="")
   }