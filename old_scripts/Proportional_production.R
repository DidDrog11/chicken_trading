Proportional_production_meat <- function (x, y) {
  x %>%
    mutate(Proportion_meat = (x$`1000 Heads`[x$Year == 1992]/y$`1000 Heads`[y$Year == 1992])*x$`1000 Heads`)
}