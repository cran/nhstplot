# nhstplot 1.4.0
- Improved error messages when passing invalid arguments to the plotting functions.
- The `plotftest()` function now returns an error when the `anova()` does not contain exactly one test.
- The `plotchisqtest()` function now accepts `glm` objects as input (a likelihood ratio test is used).

# nhstplot 1.3.0
- The p-value position can now be adjusted with the `p_value_position` argument (see vignette for examples).
- Fixing incompatibilities with the latest version of `ggplot2`.

# nhstplot 1.2.0
- You can now pass nested model comparisons ("F-change" tests, or Deviance/Likelihood Ratio Tests) F and chi-square tests created with the `anova()` directly for plotting (see vignette for examples).
- Code improvements and updates.

# nhstplot 1.1.0
- You can now directly pass objects created with functions `lm()`, `t.test()`, `cor.test()` and `chisq.test()` for plotting (see vignette for examples).
- x-axis limits can now be manually specified with argument `xmax` (see vignette for example).
- p-value formatting was improved to match typical reporting standards.
- Improvements to documentation for clarity.

# nhstplot 1.0.1
- Improved support for new `ggplot2` version (horizontal alignment of the plot titles)
- Gold and blue theme now works in Chi-squared function

# nhstplot 1.0.0
Initial release
