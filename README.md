
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Purpose

`nhstplot` is a simple and convenient package to represent graphically
the most common Null Hypothesis Significance Tests (NHST).

In other words, it plots the density functions of z, t, F and
Chi-squared, adding a cutline at the observed statistic value, scaling
it all conveniently, and plotting a label for the p value.

# How to use it

First, install the library with `install.packages("nhstplot")`, load the
library with `library(nhstplot)`.

`nhstplot` is composed of 4 functions, one for each major NHST test
“family” :

  - Chi-squared tests (with the `plotchisqtest` function)
  - F tests (with the `plotftest` function)
  - t tests (with the `plotttest` function)
  - z tests (with the `plotztest` function)

They all work quite the same (with minor differences, see the vignette
for more info), with very few required arguments:

  - The first required argument is the value of the test statistic z, t,
    F and Chi-squared
  - The other required arguments are the degrees of freedom (except for
    z of course)

See the vignette for examples and options.

# Disclaimer

This package is neither for or against NHST. It’s meant to help explain
the process, should you want to explain it. A lot of students (and
scholars) have no choice but to read articles with \(p\) values, so they
might as well have a better understanding of what it is and what it’s
not anyway, right?
