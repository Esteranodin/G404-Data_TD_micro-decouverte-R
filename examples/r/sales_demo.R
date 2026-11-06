# Guided demonstration: base R only. Independent exercise not solved here.
# Run from the course folder, or pass explicit input and output paths.
args <- commandArgs(trailingOnly = TRUE)
input_path <- if (length(args) >= 1L) args[1L] else {
  "data/sales_synthetic.csv"
}
output_dir <- if (length(args) >= 2L) args[2L] else {
  "examples/r/output"
}
sales <- read.csv(
  input_path, na.strings = "", stringsAsFactors = FALSE,
  colClasses = c("integer", "character", "character", "character", "numeric")
)
expected_names <- c("order_id", "order_date", "store", "channel", "amount_eur")
stopifnot(identical(names(sales), expected_names))
sales$order_date <- as.Date(sales$order_date, format = "%Y-%m-%d")
stopifnot(nrow(sales) > 0L, !anyNA(sales[1:4]))
stopifnot(!anyDuplicated(sales$order_id), all(sales$order_id > 0L))
stopifnot(all(sales$store %in% c("Centre", "Rive")))
stopifnot(all(sales$channel %in% c("web", "boutique")))
stopifnot(all(is.na(sales$amount_eur) |
              (is.finite(sales$amount_eur) & sales$amount_eur >= 0)))
str(sales)
print(head(sales))

# A vector has one atomic type; numeric indexing starts at 1.
amounts <- c(120, 80, NA, 0)
print(amounts[1])
print(amounts * 1.1)
print(is.na(amounts))
print(mean(amounts))
print(mean(amounts, na.rm = TRUE))

# Missing amounts are excluded explicitly from this selection.
keep <- !is.na(sales$amount_eur) & sales$amount_eur >= 100
selected <- sales[keep, c("order_id", "store", "amount_eur")]
print(selected)
sales$amount_plus_10pct <- sales$amount_eur * 1.1

# Return an explicit missing result when no amount is known.
known_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}
store_mean <- tapply(sales$amount_eur, sales$store, known_mean)
store_orders <- table(sales$store)
store_known <- tapply(!is.na(sales$amount_eur), sales$store, sum)
store_sum <- tapply(sales$amount_eur, sales$store, sum, na.rm = TRUE)
summary <- data.frame(
  store = names(store_mean),
  orders = as.integer(store_orders[names(store_mean)]),
  known_amounts = as.integer(store_known),
  known_sum_eur = as.numeric(store_sum),
  known_mean_eur = as.numeric(store_mean)
)
print(summary, row.names = FALSE)
cat("Orders:", nrow(sales), "\n")
cat("Known amounts:", sum(!is.na(sales$amount_eur)), "\n")
cat("Known sum EUR:", sum(sales$amount_eur, na.rm = TRUE), "\n")
cat("Known mean EUR:", known_mean(sales$amount_eur), "\n")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(summary, file.path(output_dir, "store_summary.csv"), row.names = FALSE)
png(file.path(output_dir, "store_mean.png"), width = 1200, height = 800, res = 150)
barplot(
  store_mean, ylim = c(0, 120), col = c("#145A75", "#B36B27"),
  ylab = "Montant moyen connu (EUR)", xlab = "Magasin",
  main = "Commandes fictives : moyenne des montants connus",
  sub = "Centre : 5 montants connus / 6 commandes ; Rive : 6 / 6"
)
dev.off()
writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))
cat("Outputs:", normalizePath(output_dir), "\n")
