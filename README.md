flacco: Feature-Based Landscape Analysis of Continuous and Constraint Optimization Problems
===========================================================================================

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/flacco)](http://cran.r-project.org/web/packages/flacco)
[![GitHub Status Badge](https://img.shields.io/badge/GitHub-1.6-green.svg)](https://github.com/kerschke/flacco)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/flacco)](http://cran.rstudio.com/web/packages/flacco/index.html)
[![Tutorial](https://img.shields.io/badge/tutorial-flacco-ff69b5.svg)](http://kerschke.github.io/flacco-tutorial/site/)
[![Web-GUI](https://img.shields.io/badge/GUI-flacco-bb6fbf.svg)](https://flaccogui.shinyapps.io/flaccogui/)

[![Build Status](https://travis-ci.org/kerschke/flacco.svg?branch=master)](https://travis-ci.org/kerschke/flacco)
[![Build status](https://ci.appveyor.com/api/projects/status/cd170v2xlpw8db47/branch/master?svg=true)](https://ci.appveyor.com/project/kerschke/flacco/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/kerschke/flacco/badge.svg?branch=master)](https://coveralls.io/r/kerschke/flacco?branch=master)
[![codecov.io](http://codecov.io/github/kerschke/flacco/coverage.svg?branch=master)](http://codecov.io/github/kerschke/flacco?branch=master)

* *If you like our package, please [star](https://github.com/blog/1204-notifications-stars) it on Github.*

Introduction
------------

**flacco** is a collection of features for *Explorative Landscape Analysis (ELA)* of single-objective, continuous *(Black-Box-)Optimization Problems*.
It allows the user to quantify characteristics of an (unknown) optimization problem's landscape.

Features, which used to be spread over different packages and platforms (R, Matlab, python, etc.), are now combined within this single package. Amongst others, this package contains feature sets, such as *ELA*, *Information Content*, *Dispersion*, *(General) Cell Mapping* or *Barrier Trees*.

Furthermore, the package provides a unified interface for all features -- using a so-called *feature object* and (if required) *control* arguments. In total, the current release (1.4) consists of 17 different feature sets, which sum up to approximately 300 features.

In addition to the features themselves, this package also provides visualizations, e.g. of the cell mappings, barrier trees or information content:

- 3D-Barrier Tree:

![Examplary Barrier Tree](https://raw.githubusercontent.com/kerschke/flacco/master/images/example_bt_3d.png)


- Cell Mapping:

![Examplary Cell Mapping](https://raw.githubusercontent.com/kerschke/flacco/master/images/example_cm.png)

- Information Content Plot:

![Examplary Info Content](https://raw.githubusercontent.com/kerschke/flacco/master/images/example_info.png)


Quickstart
----------

If you want to get started quickly, have a look at the [flacco tutorial](http://kerschke.github.io/flacco-tutorial/site/).

```splus
library(flacco)

## (1) Create some example-data
X = createInitialDesign(n.obs = 500, dim = 2)
f = function(x) sum(sin(x) * x^2 + (x - 0.5)^3)
y = apply(X, 1, f)

## (2) Compute the feature object
feat.object = createFeatureObject(X = X, y = y)

## (3) Have a look at feat.object
print(feat.object)

## (4) Check, which feature sets are available
listAvailableFeatureSets()

## (5) Calculate a specific feature set, e.g. the ELA meta model
featureSet = calculateFeatureSet(feat.object, set = "ela_meta")
```

Installation Instructions
-------------------------

* If you want to use the version from CRAN, you should download it from [here](https://cran.r-project.org/web/packages/flacco/).
* However, if you want to use the development version, you can download it with the following command:

    ```splus
    devtools::install_github("kerschke/flacco")
    ```


flacco-GUI
----------

We have started to implement a graphical user interface (GUI) for our package. Its online-version is hosted [here](https://flaccogui.shinyapps.io/flaccogui/).

Alternatively, you can start it from within R:

```splus
library(flacco)
runFlaccoGUI()
```


News
----

* April 28, 2017: flacco 1.5 (including its GUI) has been submitted to CRAN.
* February 27, 2017: flacco 1.4 has been submitted to CRAN.
* April 15, 2016: flacco 1.3 has been submitted to CRAN.
* January 22, 2016: flacco 1.2 has been submitted to CRAN.
* November 26, 2015: flacco 1.1 has been submitted to CRAN.
* October 28, 2015: It is done! flacco 1.0 is released on CRAN!
* September 2015: Fixing the last minor issues before submitting version 1.0 to CRAN.


Contact
-------

If you have any suggestions or ideas (e.g. for new features), or if you encounter any problems while running the code, please use the issue tracker or send me an e-mail (kerschke@uni-muenster.de).
