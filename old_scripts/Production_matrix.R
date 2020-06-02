## Dependencies Data_cleaning and Country_compiling
# Empty matrix ------------------------------------------------------------
NodeList_Production <- sort(unique(colnames(Producing_country_compiled)))
Production_matrix <- matrix(data = NA, nrow = length(NodeList_Production), ncol = length(NodeList_Production),
                            dimnames = list(c(NodeList_Production), c(NodeList_Production)))
Year_list <- c(1961:2018)

for (year in 1:length(Year_list)) {
  d <- paste('Production_matrix_', Year_list[year], sep = '')
}

Production_matrices <- as.list(1:length(Year_list))
names(Production_matrices) <- names(c(Year_list))
  as.list(Year_list)
