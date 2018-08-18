# 2018-09-23_Joint_Models_Workshop_Cincinnati2018


This repository consist of all the material (slides, practicals and app) for the workshop on *"Joint Modelling of Longitudinal 
and Survival Data: Tools to Evaluate Exposures and Predict Outcome Across the Lifespan"*.
\
The packages that are needed are: \
- survival
- nlme
- JMbayes
- lattice
- shiny\
These packages can be installed using the following function call:

```r
install.packages(c("survival", "nlme", "JMbayes", "lattice", "shiny"), dependencies = TRUE)
```

To run the app you will need to run the following:

```r
runGitHub("2018-09-23_Joint_Models_Workshop_Cincinnati2018", "ERandrinopoulou", subdir = "shiny app/")
 ```