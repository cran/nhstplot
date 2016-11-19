#' Illustrate a Chi-Squared Test graphically.
#'
#' This function plots the density probability distribution of a Chi-Squared statistic, with a vertical cutline at the observed Chi-Squared value specified. The p-value and the observed Chi-Squared value are plotted. Although largely customizable, only two arguments are required (the observed Chi-Squared and the degrees of freedom).
#'
#' @param chisq A numeric value indicating the observed Chi-squared statistic.
#' @param df A numeric value indicating the degrees of freedom.
#' @param blank A logical that indicates whether to hide (blank = TRUE) the test statistic value, p value and cutline. The corresponding colors are actually only made transparent when blank = TRUE, so that the output is scaled exactly the same (this is useful and especially intended for step-by-step explanations).
#' @param title A string or expression indicating a custom title for the plot (optional).
#' @param xlabel A string or expression indicating a custom title for the x axis (optional).
#' @param ylabel A string or expression indicating a custom title for the y axis (optional).
#' @param fontfamily A string indicating the font family of all the titles and labels (e.g. "serif" (default), "sans", "Helvetica", "Palatino", etc.) (optional).
#' @param colorleft A string indicating the color for the "left" area under the curve (optional).
#' @param colorright A string indicating the color for the "right" area under the curve (optional).
#' @param colorleftcurve A string indicating the color for the "left" part of the curve (optional).
#' @param colorrightcurve A string indicating the color for the "right" part of the curve (optional). By default, for color consistency, this color is also passed to the label, but this can be changed by providing an argument for the "colorlabel" parameter.
#' @param colorcut A string indicating the color for the cut line at the observed test statistic (optional).
#' @param colorplabel A string indicating the color for the label of the p-value (optional). By default, for color consistency, this color is the same as color of "colorright".
#' @param theme A string indicating one of the predefined color themes. The themes are "default" (light blue and red), "blackandwhite", "whiteandred", "blueandred", "greenandred" and "goldandblue") (optional). Supersedes "colorleft" and "colorright" if another argument than "default" is provided.
#' @param signifdigitsp A numeric indicating the number of desired significant figures reported for the p-value label (optional).
#' @param signifdigitschisq A numeric indicating the number of desired significant figures reported for the Chi-Squared label (optional).
#' @param curvelinesize A numeric indicating the size of the curve line (optional).
#' @param cutlinesize A numeric indicating the size of the cut line (optional). By default, the size of the curve line is used.
#' @return Returns a plot with the density of probability of Chi-Squared under the null hypothesis, annotated with the observed test statistic and the p-value.
#' @export plotchisqtest
#' @examples
#' #Making a chi-squared plot with Chi-squared of 8 and df of 4
#' plotchisqtest(chisq = 8, df = 4)
#'
#' #Note that the same can be obtained even quicker with:
#' plotchisqtest(8,4)
#'
#' #The same plot without the Chi-Squared or p value
#' plotchisqtest(8,4, blank = TRUE)
#'
#' #Changing the fontfamily to "sans" and changing the color theme to "blackandwhite"
#' plotchisqtest(chisq = 8, df = 4, fontfamily = "sans", theme = "blackandwhite")
#'
#' #Using specific colors and changing the curve line size
#' plotchisqtest(chisq = 8, df = 4, colorleft = "grey", colorright = "indianred", curvelinesize = 1.2)
#'
#' #Changing the title to "Chi-Squared Test of Independence"
#' plotchisqtest(chisq = 8, df = 4, title = "Chi-Squared Test of Independence")
#'
#' #Changing the title to "Chi-Squared Test of Independence" with greek chi-squared
#' plotchisqtest(chisq = 8, df = 4, title = expression(chi^2 ~ "Test" ~ "of" ~ "Independence"))
#'
#' @author Nils Myszkowski <nmyszkowski@pace.edu>
plotchisqtest <- function(chisq, df, blank = FALSE, title = parse(text = expression(chi^2 ~ "Test")), xlabel = parse(text = expression(chi^2)), ylabel = "Density of probability\nunder the null hypothesis", fontfamily = "serif", colorleft = "aliceblue", colorright = "firebrick3", colorleftcurve = "black", colorrightcurve = "black", colorcut = "black", colorplabel = colorright, theme = "default", signifdigitsp = 3, signifdigitschisq = 3, curvelinesize = .4, cutlinesize = curvelinesize) {
  x=NULL
  #Create a function to restrict plotting areas to specific bounds of x
    area_range <- function(fun, min, max) {
      function(x) {
        y <- fun(x)
        y[x < min | x > max] <- NA
        return(y)
      }
    }
  #Calculate the p value
  pvalue <- stats::pchisq(chisq, df, lower.tail = FALSE)
  #Label for p value
  plab <- paste("p =", signif(pvalue, digits = signifdigitsp), sep = " ")
  #Label for chi square value
  chisqlab <- as.character(as.expression(bquote(chi^2== .(signif(chisq, digits=signifdigitschisq)))))
  #Define x axis bounds as the maximum between 1.5*f or 3 (this avoids only the tip of the curve to be plotted when F is small, and keeps a nice t curve shape display)
  xbound <- max(1.5*chisq, 5)
  #To ensure lines plotted by stat_function are smooth
  precisionfactor <-  5000
  #To define the function to plot in stat_function
  density <- function(x) stats::dchisq(x, df)
  #Use the maximum density (top of the curve) to use as maximum y axis value (start finding maximum at .2 to avoid very high densities values when the density function has a y axis asymptote)
  maxdensity <- stats::optimize(density, interval=c(0.2, xbound), maximum=TRUE)$objective
  #To place the p value labels on the x axis, at the middle of the part of the curve they correspond to
  x_plabel <- chisq+(xbound-chisq)/2
  #Use the density corresponding to the given chisquare to place the label above (if this density is too high places the label lower in order to avoid the label being out above the plot)
  y_plabel <- min(density(chisq)+maxdensity*.1, maxdensity*.7)
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
  } else if (theme == "goldandblue") {
    colorleft <- "#FFC61E"
    colorright <- "#00337F"
    colorplabel <- "#00337F"
  } else warning("The ",'"', "theme", '"', " argument was not recognized. See documentation for a list of available color themes. Reverting to default.")
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
     ggplot2::stat_function(fun = area_range(density, chisq, xbound), geom="area", fill=colorright, n=precisionfactor) +
     #Left side curve
     ggplot2::stat_function(fun = density, xlim = c(0,chisq), colour = colorleftcurve, n=precisionfactor, size=curvelinesize) +
     #Right side curve
     ggplot2::stat_function(fun = density, xlim = c(chisq,xbound), colour = colorrightcurve,size=curvelinesize) +
     #Define plotting area for extraspace (proportional to the max y plotted) below the graph to place chisquared label
     ggplot2::coord_cartesian(xlim=c(0,xbound),ylim=c(maxdensity*(-.08), maxdensity)) +
     #Cut line
     ggplot2::geom_vline(xintercept = chisq, colour = colorcut, size = cutlinesize) +
     #p label
     ggplot2::geom_label(ggplot2::aes(x_plabel,y_plabel,label = plab), fill = colorlabelfill, colour=colorplabel, family = fontfamily) +
     #Chi squared label
     ggplot2::geom_label(ggplot2::aes(chisq,maxdensity*(-.05),label = chisqlab), fill = colorlabelfill, colour=colorcut, family=fontfamily, parse = TRUE) +
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
