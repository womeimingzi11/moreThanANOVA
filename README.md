# moreThanANOVA

## Why ANOVA

Once you want to compare some data between different treatments, what you learned from plenty of papers, articles and theses is applying **ANOVAs (Analysis of variance)**, even we don't really know what is ANOVA.

*Quiz*: Why do you choose ANOVA? Here are probably your answers:

1. Others used it in their own works.
2. It can compare something.
3. Becasue I know why.

However, what others do is not always right, is it? At least, it is not always suitable for some situations.

## Beyond ANOVA

Here, we don't state what is ANOVA, there are amount of articles and videos about this topic.

From what I can tell, there is a **premise** of ANOVA. In fact, it is a premise for all t-test family, which is **Normal Distribution**(also Known as **Gaussian Distribution**). Once your data do not meet normal distribution, maybe you want to transform them, like log(x+1), square-root, log, etc.

However, we are not always lucky dogs. Literally, it even never works for me in my current research.

We choose nonparametric tests, **Wilcoxon tests** for two treatments, and **Kruskal-Wallis rank sum test** for multiple treatments.

Their are conceptions about signed-rank test and signed-rank sum test. We also don't state what are these conceptions, but I highly recommend you to Google them before you choose a test. Here is a article that posted by **Stats and R** entitled [Wilcoxon test in R: how to compare 2 groups under the non-normality assumption](https://www.statsandr.com/blog/wilcoxon-test-in-r-how-to-compare-2-groups-under-the-non-normality-assumption/).

Besides, the permutation test is also used as a fancy method to evaluate the significant level especially for data with unknown distribution. For more information, there is an article from [R-Bloger](https://www.r-bloggers.com/what-is-a-permutation-test/), and here is [another Chinese article](https://www.r-bloggers.com/what-is-a-permutation-test/) about it.

## Features

- [X] Data View
- [X] Data Distribution Detect
  - [X] Data Distributions
  - [X] Automatically determine methods
  - [X] Data density plot
- [X] Significant Comparisons
  - [X] Significant Table
  - [X] Mean, Median and Sig-Level
  - [X] Post Hoc test plot

## How to use it?

### 1. EZ way

[Click here](https://hanchen.shinyapps.io/moreThanANOVA/). moreThanANOVA is hosted at [Shinyapps.io](https://Shinyapps.io).

### 2. Hardcore way

To make sure that you can control everything, you are welcomed to [fork my code](https://github.com/womeimingzi11/moreThanANOVA/fork) to your own repo (and leave me a star please).

Then what you can do is to oepn `moreThanANOVA.Rproj` file in RStudio, following open `app.R` file, install all the packages which will be loaded.

At the least, click `run.app` at the right top of the code editor panel, **rdaWithStep** will run locally.

![](resource/figure/runApp.png)

#### Upload Data

You can upload your file with following structure.
![](resource/figure/table_str.png)

## Privacy Statements

We guarantee that all your data won't be kept once you leave the Shiny app. There is no code and won't have any code to record your cilentID, uploaded file or any other data.

## Known Issues

You tell me.

## Contact Me

You are welcomed to commit any feature about this and even any other features in my [repo on GitHub](https://github.com/womeimingzi11/moreThanANOVA).

You are also welcomed to visit my [Blog (in Chinese)](https://womeimingzi11.github.io) or contact me by [mail](mailto://chenhan28@gmail.com).
