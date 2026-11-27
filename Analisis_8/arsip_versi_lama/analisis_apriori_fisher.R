# ==============================================================================
# MARKET BASKET ANALYSIS DENGAN ALGORITMA APRIORI DAN UJI EKSAK FISHER
# Mata Kuliah : Riset Pemasaran
# Sumber Data : Matriks_Biner_Transaksi.csv / MBA_Riset_Pemasaran.xlsx (N = 224)
# ==============================================================================

# 1. LOAD LIBRARY
suppressPackageStartupMessages({
  library(arules)
  library(arulesViz)
  library(ggplot2)
  library(ggraph)
})

# 2. INPUT DATA
csv_path <- "Matriks_Biner_Transaksi.csv"
df_kantin <- read.csv(csv_path, check.names = FALSE, stringsAsFactors = FALSE)

# 3. EKSPLORASI DATA
cat("Dimensi data awal :", nrow(df_kantin), "baris x", ncol(df_kantin), "kolom\n")

# 4. PRE-PROCESSING DATA
# Menghapus atribut penanda non-item (ID transaksi, Baris Excel, Tanggal)
# serta mengonversi matriks biner 0/1 ke tipe logika (boolean TRUE/FALSE)
item_mat_all <- as.matrix(df_kantin[, 4:ncol(df_kantin)])
storage.mode(item_mat_all) <- "logical"

# 5. KONVERSI KE OBJEK TRANSACTIONS
trans_all <- as(item_mat_all, "transactions")
cat("\n=== RINGKASAN OBJEK TRANSAKSI (SELURUH 32 ITEM, N = 224) ===\n")
print(summary(trans_all))

# 6. ANALISIS FREKUENSI ITEM
item_freq <- sort(itemFrequency(trans_all, type = "relative"), decreasing = TRUE)
cat("\n=== 10 ITEM DENGAN FREKUENSI RELATIF TERTINGGI ===\n")
print(round(head(item_freq, 10), 4))

# 7. IDENTIFIKASI ITEM WAJIB / DOMINAN (FREKUENSI RELATIF > 60%)
item_wajib <- names(item_freq[item_freq > 0.60])
cat("\nItem wajib/dominan (> 60%):", paste(item_wajib, collapse = ", "), "\n")

# 8. PEMBENTUKAN ATURAN ASOSIASI AWAL (DENGAN ITEM DOMINAN, N = 224)
rules_all <- apriori(
  trans_all,
  parameter = list(supp = 0.03, conf = 0.55, minlen = 2, maxlen = 2, target = "rules"),
  control = list(verbose = FALSE)
)
rules_all <- sort(rules_all, by = "lift", decreasing = TRUE)
fisher_all <- interestMeasure(rules_all, measure = "fishersExactTest", transactions = trans_all)
qual_all <- quality(rules_all)

res_all <- data.frame(
  Rule = labels(rules_all),
  Count = qual_all$count,
  Support = round(qual_all$support, 4),
  Confidence = round(qual_all$confidence, 4),
  Coverage = round(qual_all$coverage, 4),
  Lift = round(qual_all$lift, 4),
  Fisher_p_1sided = signif(fisher_all, 4),
  Signif_0.05 = ifelse(fisher_all < 0.05, "Signifikan", "Tidak")
)
cat("\n=== HASIL ATURAN ASOSIASI AWAL (SEBELUM ELIMINASI ITEM DOMINAN) ===\n")
print(res_all, row.names = FALSE)

# 9. SELEKSI ATURAN ASOSIASI TANPA ITEM DOMINAN 'NASI' & UJI EKSAK FISHER
item_mat_non_nasi <- item_mat_all[, !(colnames(item_mat_all) %in% item_wajib)]
trans_non_nasi <- as(item_mat_non_nasi, "transactions")

rules_non_nasi <- apriori(
  trans_non_nasi,
  parameter = list(supp = 0.03, conf = 0.55, minlen = 2, maxlen = 2, target = "rules"),
  control = list(verbose = FALSE)
)

fisher_one_sided <- interestMeasure(
  rules_non_nasi,
  measure = "fishersExactTest",
  transactions = trans_non_nasi
)

lhs_clean <- gsub("^\\{|\\}$", "", labels(lhs(rules_non_nasi)))
rhs_clean <- gsub("^\\{|\\}$", "", labels(rhs(rules_non_nasi)))

p_two_sided <- numeric(length(rules_non_nasi))
odds_ratio <- numeric(length(rules_non_nasi))

for (i in seq_along(rules_non_nasi)) {
  x_vec <- item_mat_non_nasi[, lhs_clean[i]]
  y_vec <- item_mat_non_nasi[, rhs_clean[i]]
  tbl <- matrix(c(
    sum(x_vec & y_vec), sum(x_vec & !y_vec),
    sum(!x_vec & y_vec), sum(!x_vec & !y_vec)
  ), nrow = 2, byrow = TRUE)
  ft <- fisher.test(tbl, alternative = "two.sided")
  p_two_sided[i] <- ft$p.value
  odds_ratio[i] <- unname(ft$estimate)
}

qual <- quality(rules_non_nasi)
res_non_nasi <- data.frame(
  LHS = lhs_clean,
  RHS = rhs_clean,
  Count = qual$count,
  Support = round(qual$support, 4),
  Confidence = round(qual$confidence, 4),
  Coverage = round(qual$coverage, 4),
  Lift = round(qual$lift, 4),
  Odds_Ratio = round(odds_ratio, 3),
  Fisher_p_arules_1sided = signif(fisher_one_sided, 4),
  Fisher_p_2sided = signif(p_two_sided, 4),
  Signif_0.05 = ifelse(fisher_one_sided < 0.05 & p_two_sided < 0.05, "Signifikan", "Tidak")
)
res_non_nasi <- res_non_nasi[order(-res_non_nasi$Lift, res_non_nasi$Fisher_p_arules_1sided), ]

cat("\n=== HASIL SELEKSI ATURAN ASOSIASI TERKUAT & UJI EKSAK FISHER (N = 224) ===\n")
print(res_non_nasi, row.names = FALSE)

out_csv <- res_non_nasi[, c(
  "LHS", "RHS", "Count", "Support", "Confidence", "Lift",
  "Odds_Ratio", "Fisher_p_arules_1sided", "Fisher_p_2sided", "Signif_0.05"
)]
write.csv(out_csv, "Hasil_Apriori_Fisher_NonNasi.csv", row.names = FALSE)

# 10. VISUALISASI JARINGAN ASOSIASI (ARULESVIZ)
dir.create("visualisasi", showWarnings = FALSE)
set.seed(42)
p_all <- plot(
  rules_all,
  method = "graph",
  engine = "ggplot2",
  control = list(
    layout = "stress",
    edges = ggraph::geom_edge_link(
      end_cap = ggraph::circle(3.5, "mm"),
      start_cap = ggraph::circle(3.5, "mm"),
      color = "gray55",
      alpha = 0.75,
      arrow = grid::arrow(length = grid::unit(2.5, "mm"), type = "closed")
    )
  )
) +
  scale_color_gradient(low = "#1D4ED8", high = "#DC2626", name = "lift") +
  scale_size_continuous(range = c(3.8, 9.8), name = "support") +
  ggtitle("Graf Jaringan Seluruh 16 Aturan Asosiasi Awal (Sebelum Eliminasi Item Dominan)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11.5, family = "serif"),
    legend.title = element_text(face = "bold", size = 9.5, family = "serif"),
    legend.text = element_text(size = 8.5, family = "serif"),
    plot.margin = margin(12, 16, 12, 16)
  )
ggsave("visualisasi/Gambar_4_3_Plot_Jaringan_Asosiasi.png", plot = p_all, width = 9.0, height = 6.0, dpi = 300, bg = "white")
