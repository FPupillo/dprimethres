This package contains a function that outputs the smallest d' value that is above a certain
probability level over noise. The way it does is by computing a null distribution
of d' values and selects the value that sits at the proportion level passed
as an argument.

To install, just run:

```
{
install.packages('devtools')
library(devtools)
install_github('FPupillo/dprimethres')
}
```