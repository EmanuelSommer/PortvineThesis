
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Estimation and Backtesting of the Expected Shortfall and Value at Risk using Vine Copulas

### An unconditional and conditional rolling window approach

This repository collects the code for all theoretical visualizations and
case studies presented in [my master
thesis](https://mediatum.ub.tum.de/1658240) at the chair of Mathematical
Statistics at the TUM with the title given above.

The heavily used `portvine` R package that I developed also while
working on this project can be found
[**HERE**](https://emanuelsommer.github.io/portvine/).
<a href='https://github.com/EmanuelSommer/portvine'><img src='logo.png' align="right" width="20%" /></a>

<br><br><br><br>

The repo is structured as follows:

-   The theoretical visualizations for the thesis were created using the
    code in [`theoretical_viz.R`](theoretical_viz.R).
-   The `data` folder is not shared publicly as we partly use not freely
    available services. However as we use only daily log returns of
    publicly traded stocks one can easily and free of charge obtain the
    same data through services like *Yahoo finance*. Moreover the data
    folder holds all the results of the case study and performance
    measurement scripts that were run on a Linux Cluster of the Leibniz
    Supercomputing Centre in order to exploit the strong parallel
    processing capabilities of the `portvine` package. The provided
    computational resources are gratefully acknowledged.
-   The code in [`raw_data_preprocessing.R`](raw_data_preprocessing.R)
    provides tidy data to base the further analyses on.
-   The code in [`profile_runtime.R`](profile_runtime.R) was used to
    detect and then fix computational bottlenecks in the implementation
    of the `portvine` package.
-   The scripts for the performance measurements of the `portvine`
    package are collected in the [`performance`](performance) folder.
    The results of these scripts are visualized using the code in the
    [`performance_viz.R`](performance_viz.R) script.
-   Many utility functions for the case studies are collected in the
    script [`analysis_utils.R`](analysis_utils.R). Please note that
    these functions are not intended for the general usage as they do
    not cover input checks, unit tests and are documented quite minimal.
    If you would like to use these functions do this with care.
-   The code for the case studies regarding the portfolio of Spanish
    stocks from the MSCI Spain is collected in the
    [`analysis/msci_spain`](analysis/msci_spain) folder. There you can
    find all the scripts for the risk measure estimation that were
    submitted to the Linux cluster as well as the Rmarkdown document
    [`analysis_msci_spain.Rmd`](analysis/msci_spain/analysis_msci_spain.Rmd)
    and corresponding html document
    [`analysis_msci_spain.html`](analysis/msci_spain/analysis_msci_spain.html)
    where all results are collected, visualized and commented. For a
    more rigorous and structured interpretation of the result you should
    consult my master thesis. There the case studies are embedded in the
    theoretical context.
-   The [`img`](img) folder contains the final theoretical
    visualizations and the performance measurement visualizations.
