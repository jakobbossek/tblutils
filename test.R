library(tidyverse)
library(ggplot2)
library(devtools)
library(kableExtra)

load_all()

# import
data(sampleDataset)
tbl = sampleDataset

# perform statistical tests
tbl = tblutils::test.pairwise(tbl, by = c("n", "mu", "alpha"),
  split.col = "max.k", value.col = "diversity", testresult.col = "diversity.test",
  show.positive.only = FALSE,
  show.numbers.only = FALSE,
  colors = c("red!50", "blue!50", "violet!50"),
  alternative = "greater", alpha = 0.05)

## Results for constrained optimization
## ===

# aggregate
tblaggr = tbl %>%
  group_by(type, n, mu, alpha, max.k) %>%
  dplyr::summarise(
    diversity.mean = mean(diversity),
    diversity.sd   = sd(diversity),
    diversity.test = diversity.test[1L]
    ) %>%
  ungroup()

tblaggr = tblutils::highlight(tblaggr, by = c("type", "n", "mu", "alpha"),
  which = c("diversity.mean", "diversity.sd"),
  order.fun = c("max", "min"),
  highlight.fun = tblutils::baseLaTeXHighlighter, bg.color = "white", bg.saturation.max = 20)
#tblaggr$diversity.test = sprintf("\\tiny{%s}", tblaggr$diversity.test)


# move multiple columns side by side
tblaggr = tblutils::widen(tblaggr, split.col = "max.k", widen.cols = c("diversity.mean", "diversity.sd", "diversity.test"))
tblaggr$max.k = NULL # FIXME: drop this automatically in widen

tblaggr = tblaggr %>% arrange(type, n, mu, alpha)
tblaggr$type = NULL

cns = c("$n$", "$\\mu$", "$\\alpha$", rep(c("\\textbf{mean}", "\\textbf{std}", "\\textbf{stat}"), 3))
align = rep("r", ncol(tblaggr))
ktbl = kableExtra::kable(tblaggr, "latex", col.names = cns, align = align, longtable = TRUE, escape = FALSE, booktabs = TRUE, caption = "test") %>%
  kable_styling() %>%
  #pack_rows(index = c("Euclidean" = 84, "Uniform Random" = 84), bold = TRUE) %>%
  #row_spec(row = seq(4, nrow(tbl), by = 4), extra_latex_after = "\\cline{2-9}") %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle") %>%
  #collapse_rows(columns = 3, latex_hline = "major", valign = "middle") %>%
  add_header_above(c(" ", " ", " ", "1-EX (1)" = 3, "2-EX (2)" = 3, "3-EX (3)" = 3), bold = TRUE) %>%
  kable_styling(latex_options = c("repeat_header"))

fn = "test.tex"
cat(ktbl, file = fn)

BBmisc::pause()
preview(ktbl, ktbl)
