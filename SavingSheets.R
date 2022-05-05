library(googlesheets4)
library(tidyverse)

Jan22 <- read_sheet("1PlRAXOgZOHI3FhXitp1Y7pfbD7lbM4jZZItZkXXENJU", sheet = "Jan22")

write.csv(read_sheet("1PlRAXOgZOHI3FhXitp1Y7pfbD7lbM4jZZItZkXXENJU", sheet = "Jan22"), "~/RProjects/Jan22.csv")


