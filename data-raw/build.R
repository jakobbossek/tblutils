library(tidyverse)

sampleDataset = readr::read_delim("data-raw/sampleData.csv", delim = " ")
sampleDataset$MPD = sampleDataset$mu * (sampleDataset$mu - 1) * (sampleDataset$n - 1)
sampleDataset$diversity = round((sampleDataset$PD / sampleDataset$MPD) * 100, digits = 2L)
sampleDataset$job.id = NULL

usethis::use_data(sampleDataset, overwrite = TRUE)
