# library(samplyzer)
# context("plot")

# test_that("split violin plot", {
#   set.seed(0)
#   my_data = data.frame(
#     y=c(rnorm(1000), rnorm(1000, 0.5), rnorm(1000, 1), rnorm(1000, 1.5)),
#     x=c(rep('a', 2000), rep('b', 2000)),
#     m=c(rep('i', 1000), rep('j', 2000), rep('i', 1000))
#   )
#   (ggplot2::ggplot(my_data, ggplot2::aes(x, y, fill=m))
#     + samplyzer::geom_split_violin())
# })

