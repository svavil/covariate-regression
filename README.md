# covariate-regression
An R function to calculate and plot scatter plot and linear regression line, starting from a column name string

Sample use:
```
data("mtcars")
covariate.regression("mpg", "cyl", mtcars %>% mutate(Nsub = 1))
```
invisibly returns (and saves to file) the following plot: 

![a linear regression plot of miles per gallon versus cylinder count](https://github.com/svavil/covariate-regression/blob/main/mpg_vs_cyl.png?raw=true)
