## Why ANOVA

Once you want to compare some data between different treatments, what you learned from plenty of papers, articles and thesises is using **ANOVA (Analysis of variance)**, even we don't really know what is ANOVA.

*Quiz*: Why do you choose ANOVA? Here are probably your answers:

1. Others use it in their own works.
2. It can compare something.
3. Becasue I know why.

Howver, what others do are not always right, are they? At least, are not always suitable for any situations.

## Beyond ANOVA

Here, we don't state what is ANOVA, there are amount of articles and videos about this topic.

For what I can tell, there is an **premise** of ANOVA, in fact it is a premise for all t-test family, is **Normal Distribution**(also Known as **Gaussian Distribution**). Once your data do not meet normal distribution, maybe you want to transform them, like log(x+1), square-root, log, etc.

However, we are not always lucky dogs, literally, it even never works for me in my current research.

We choose nonparametric tests, **Wilcoxon tests** for two treatments, and **Kruskal-Wallis rank sum test** for multiple treatments.

Their are conceptions about signed-rank test and signed-rank sum test. We also don't state what are these conceptions, but I highly recommand you to Google them before you choose a test. Here is a article that posted by **Stats and R** entitled [Wilcoxon test in R: how to compare 2 groups under the non-normality assumption](https://www.statsandr.com/blog/wilcoxon-test-in-r-how-to-compare-2-groups-under-the-non-normality-assumption/).

Besides, the permutation test is also used as a fancy method to evaluate the significant level especially for data with unknown distribution. For more information, there is an article from [R-Bloger](https://www.r-bloggers.com/what-is-a-permutation-test/), and here is [another Chinese article](https://www.r-bloggers.com/what-is-a-permutation-test/) about it.

## Privacy Statements

We gurantee that all your data won't be kept once you leave the Shiny app. There is no code and won't have any code to record your cilentID, uploaded file or any other data.
