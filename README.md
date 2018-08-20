# Joint Models Workshop Cincinnati2018


This repository consist of all the material (slides, practicals and app) for the workshop on **"Joint Modelling of Longitudinal 
and Survival Data: Tools to Evaluate Exposures and Predict Outcome Across the Lifespan"**.
\
The packages that are needed for this workshop are: 
- [survival](http://cran.r-project.org/package=survival)
- [nlme](http://cran.r-project.org/package=nlme)
- [JMbayes](http://cran.r-project.org/package=JMbayes)
- [lattice](http://cran.r-project.org/package=lattice)
- [splines](http://cran.r-project.org/) 
- [shiny](http://cran.r-project.org/package=shiny)


These packages can be installed using the following function call:

```r
install.packages(c("survival", "nlme", "JMbayes", "lattice", "splines", "shiny"), dependencies = TRUE)
```

To run the app you will need to run the following:

```r
runGitHub("JointModelsWorkshopCincinnati2018", "ERandrinopoulou", subdir = "shiny app/")
 ```