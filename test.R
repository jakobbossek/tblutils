library(tidyverse)
library(ggplot2)
library(devtools)
library(kableExtra)

load_all()

## EDGE-DIVERSITY AND QUALITY
## ===

# import
tbl = readr::read_delim("testdata/mst_diversity.csv", delim = " ")

# tidy
tbl = filter(tbl, sampling.fun %in% c("uniform1", "poisson"))

# add maximum diversity
tbl$MPD = tbl$mu * (tbl$mu - 1) * (tbl$n - 1)
tbl$diversity = round((tbl$PD / tbl$MPD) * 100, digits = 2L)

tbl$sampling.fun2 = factor(tbl$sampling.fun, levels = c("uniform1", "poisson"), ordered = TRUE)
tbl = arrange(tbl, sampling.fun2)

res = texify(tbl,
  split.cols = c("type", "n", "mu", "alpha"),
  widen.col = c("sampling.fun2"),
  measure.cols = "diversity")

ktbl = kableExtra::kable(res, "latex", longtable = TRUE, escape = FALSE, booktabs = TRUE, caption = "test")
preview(ktbl)



re::stopf("DONE")


### COPY AND PASTE FROM MST-DIVERSITY PAPER

tbl = tblutils::test.pairwise(tbl, by = c("type", "n", "mu", "alpha"),
  split.col = "sampling.fun2", value.col = "diversity", testresult.col = "diversity.test",
  alternative = "greater", alpha = 0.05)
tbl$sampling.fun2 = NULL

tbl.aggr = ddivc %>%
  group_by(type, n, mu, alpha, sampling.fun) %>%
  dplyr::summarise(
    diversity.mean  = round(mean(diversity), 2L),
    diversity.sd = round(sd(diversity), 2L),
    diversity.test = diversity.test[1L]
  ) %>%
  ungroup() %>%
  arrange(n, mu, alpha, desc(sampling.fun))

tbl = ddivc.aggr
tbl = tblutils::highlight(tbl, by = c("type", "n", "mu", "alpha"), which = "diversity.mean",
  order.fun = "max",
  highlight.fun = tblutils::baseLaTeXHighlighter, bg.color = "gray", bg.saturation.max = 20)

tbl = tblutils::widen(tbl,
  split.col = "sampling.fun",
  widen.cols = c("diversity.mean", "diversity.sd", "diversity.test"))
tbl = tbl %>% arrange(n, mu, alpha)
tbl$sampling.fun = NULL
#tbl$type = NULL

# filtering (table is too large for PPSN)
tbl.final = tbl %>%
  filter(!((mu == 3) & (n == 50)) & !((n == 100) & (mu %in% c(3, 10))), type == "euclidean")

cns = c("\\textbf{Type}", "$n$", "$\\mu$", "$\\alpha$", rep(c("\\textbf{mean}", "\\textbf{std}", "\\textbf{stat}"), 2))
align = rep("r", length(cns))
ktbl = kableExtra::kable(tbl.final, "latex", col.names = cns, align = align, longtable = FALSE, escape = FALSE, booktabs = TRUE, caption = "test") %>%
  kable_styling() %>%
  row_spec(row = c(12), extra_latex_after = "\\cline{2-10}") %>%
  row_spec(row = c(4, 8, 16, 20), extra_latex_after = "\\cline{3-10}") %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle") %>%
  add_header_above(c(" ", " ", " ", " ", "Uniform (1)" = 3, "Poisson (2)" = 3), bold = TRUE) %>%
  kable_styling(latex_options = c("repeat_header"))



#stop()


# import
data(sampleDataset)
tbl = sampleDataset

# perform statistical tests
tbl = tblutils::test.pairwise(tbl, by = c("n", "mu", "alpha"),
  split.col = "max.k", value.col = "diversity", testresult.col = "diversity.test",
  show.positive.only = FALSE,
  show.numbers.only = FALSE,
  show.intervals = TRUE,
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
