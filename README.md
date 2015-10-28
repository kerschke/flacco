flacco: Feature-Based Landscape Analysis of Continuous and Constraint Optimization Problems
===========================================================================================

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/flacco)](http://cran.r-project.org/web/packages/flacco)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/flacco)](http://cran.rstudio.com/web/packages/flacco/index.html)
[![Tutorial](https://img.shields.io/badge/tutorial-flacco-ff69b5.svg)](http://kerschke.github.io/flacco-tutorial/site/)

[![Build Status](https://travis-ci.org/kerschke/flacco.svg?branch=master)](https://travis-ci.org/kerschke/flacco)
[![Build status](https://ci.appveyor.com/api/projects/status/cd170v2xlpw8db47/branch/master?svg=true)](https://ci.appveyor.com/project/kerschke/flacco/branch/master)
[![Coverage Status](https://coveralls.io/repos/kerschke/flacco/badge.svg?branch=master)](https://coveralls.io/r/kerschke/flacco?branch=master)
[![codecov.io](http://codecov.io/github/kerschke/flacco/coverage.svg?branch=master)](http://codecov.io/github/kerschke/flacco?branch=master)

**ATTENTION: This package is not yet ready for release.**

* Install the development version

    ```splus
    devtools::install_github("kerschke/flacco")
    ```

* [Further installation instructions](https://githubkagesInfo/wiki/Installation-Information)

* *If you like our package, please [star](https://github.com/blog/1204-notifications-stars) it on Github.*

Introduction
------------

**flacco** is a collection of features for *Explorative Landscape Analysis (ELA)* of *(Black-Box-)Optimization Problems*.
It allows the user to quantify characteristics of an (unknown) optimization problem's landscape.

Features, which used to be spread over different packages and platforms (R, Matlab, python, etc.), are now combined within this single package. Amongst others, this package contains feature sets, such as *ELA*, *Information Content*, *Dispersion*, *(General) Cell Mapping* or *Barrier Trees*.

Furthermore, the package provides a unified interface for all features -- using a so-called *feature object* and (if required) *control* arguments. In total, the current release (1.0) consists of 17 different feature sets, which sum up to approximately 300 features.

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

```{r}
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


News
----

Fixing the last minor issues before submitting version 1.0 to CRAN.


Contact
-------

If you have any suggestions or ideas (e.g. for new features), or if you encounter any problems while running the code, please use the issue tracker or send me an e-mail (kerschke@uni-muenster.de).
