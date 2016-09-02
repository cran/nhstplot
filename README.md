
<!-- README.md is generated from README.Rmd. Please edit that file -->
Purpose
=======

`nhstplot` is a simple and convenient package to represent graphically the most common Null Hypothesis Significance Tests (NHST).

One plot being worth 1000 words, it does this...

![](README-unnamed-chunk-2-1.png)

With a very minimal amount of (intuitive) coding (here `plotftest(f = 4, dfnum = 3, dfdenom = 5)`, and even `plotftest(4,3,5)` works the same if you're in a hurry).

In other words, it plots the density functions of \(z\), \(t\), \(F\) and \(\chi^2\), adding a cutline at the observed statistic value, scaling it all conveniently, and plotting a label for the \(p\) value.

How to use it
=============

First, install the library with `install.packages("nhstplot")`

Then, load the library.

``` r
library(nhstplot)
```

Basic functions
---------------

'nhstplot' is composed of 4 functions, one for each major NHST test "family" :

-   \(\chi^2\) tests (with the `plotchisqtest` function)
-   \(F\) tests (with the `plotftest` function)
-   \(t\) tests (with the `plotttest` function)
-   \(z\) tests (with the `plotztest` function)

They all work quite the same (with minor differences, see the vignette for more info), with very few required arguments:

-   The first required argument is the value of the test statistic \(z\), \(t\), \(F\) and \(\chi^2\)
-   The other required arguments are the degrees of freedom (except for \(z\) of course)

That's it.

Going further
-------------

One tailed tests
----------------

`nhstplot`is very flexible, but its strength is its helpful defaults and easy options.

For example, by default, when appropriate, two-tailed \(z\) and \(t\) tests are performed, but just add `tails = "one"` to get a one-tailed test that adapts to the sign of the test statistics:

``` r
plotttest(-2, 10, tails = "one")
```

![](README-unnamed-chunk-4-1.png)

``` r
plotttest(2, 10, tails = "one")
```

![](README-unnamed-chunk-5-1.png)

"Blanking"
----------

To explain NHST in successive steps (and look good doing it), you may be tempted to "blank the plot" with `blank = TRUE`, which outputs the exact same graphs as before, but without the "cutting" part:

``` r
plotztest(1, blank = TRUE)
```

![](README-unnamed-chunk-6-1.png)

It's especially useful for any "step-by-step" explanation.

Colors
------

And finally, if you don't like the default theme, I've added others, that can be called with `theme`, that can probably accomodate you (see the documentation or vignette for a list).

``` r
plotztest(1, theme = "blackandwhite")
```

![](README-unnamed-chunk-7-1.png)

``` r
plotztest(1, theme = "whiteandred")
```

![](README-unnamed-chunk-7-2.png)

There are other options. See the vignette for further customizations.

Disclaimer
----------

This package is neither for or against NHST. It's meant to help explain the process, should you want to explain it. A lot of students (and scholars) have no choice but to read articles with \(p\) values, so they might as well have a better understanding of what it is and what it's not anyway, right?

Bug reports
-----------

I will try to implement new features soon, so check that you have the newest version.
