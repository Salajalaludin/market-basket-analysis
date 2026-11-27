# ================================================================
# ANALISIS 8 - VERSI RINGKAS (224 TRANSAKSI, 2-18 NOVEMBER 2026)
# Market Basket Analysis dengan Algoritma Apriori
#
# Isi dan hasil sama persis dengan MBA_Analisis_8_Data_Diperbarui.R
# (file CSV dan gambar di folder hasil/ identik). Perbedaannya hanya
# penulisan: percabangan if/else dan ifelse diganti fungsi bantu dan
# operasi vektor, perhitungan yang berulang dijadikan satu fungsi.
#
# Cara menjalankan (RStudio):
# Session > Set Working Directory > To Source File Location, lalu
# Source. File data dibaca dari subfolder data/ di folder ini.
# ================================================================

# 1. LOAD LIBRARY & FUNGSI BANTU
library(readxl)
library(arules)
library(arulesViz)

FOLDER_HASIL <- "hasil"
dir.create(FOLDER_HASIL, showWarnings = FALSE)
simpan <- function(x, nama) write.csv(x, file.path(FOLDER_HASIL, nama), row.names = FALSE)

ya_tidak    <- function(p) c("Tidak", "Ya")[(p < 0.05) + 1]
nama_item   <- function(s) gsub("^\\{|\\}$", "", s)                       # "{a}" -> "a"
kunci_pasang <- function(l, r) paste(pmin(l, r), pmax(l, r), sep = " & ") # X->Y = Y->X
p_fisher    <- function(r, tx) interestMeasure(r, "fishersExactTest", transactions = tx)

# tabel aturan: lhs, rhs, dan ukuran kualitas
tabel_rules <- function(r) {
  q <- quality(r)
  data.frame(lhs = labels(lhs(r)), rhs = labels(rhs(r)),
             support = round(q$support, 4), confidence = round(q$confidence, 4),
             coverage = round(q$coverage, 4), lift = round(q$lift, 4),
             count = q$count)
}

# sel a, b, c, d tabel kontingensi 2 x 2 untuk tiap pasangan kolom x[, k] -> y[, k]
sel_2x2 <- function(x, y) unname(cbind(a = colSums(x & y), b = colSums(x & !y),
                                       c = colSums(!x & y), d = colSums(!x & !y)))
fisher_sel <- function(sel, ambil, ...) unname(apply(sel, 1, function(v)
  fisher.test(matrix(v, 2, byrow = TRUE), ...)[[ambil]]))

# 2. INPUT DATA
df_kantin <- as.data.frame(read_excel("data/MBA_Riset_Pemasaran.xlsx", sheet = "Matriks_Biner"))

# 3. EKSPLORASI DATA
dim(df_kantin)     # 224 x 35
str(df_kantin)

# 4. PRE-PROCESSING: kolom 1-3 (ID, baris Excel, tanggal) bukan item
df_item <- df_kantin[, -(1:3)]
df_item[] <- lapply(df_item, function(x) as.logical(as.numeric(x)))

# 4.1 PENGELOMPOKAN DASAR ITEM
minuman <- c("air putih", "es air putih",
             grep("^tea jus|^jasjus", colnames(df_item), value = TRUE),
             "good day freeze", "kopi", "teh biasa", "susu kental frisian flag")
makanan <- setdiff(colnames(df_item), c(minuman, "nasi"))
lauk    <- setdiff(makanan, c("nasi goreng ayam", "nasi goreng cabe ijo"))

# 4.2 NASI SEBAGAI ITEM TERSENDIRI ("nasi putih")
df_analisis <- df_item[, c(makanan, "nasi", minuman)]
colnames(df_analisis)[colnames(df_analisis) == "nasi"] <- "nasi putih"
print(table(nasi = df_item$nasi, lauk = rowSums(df_item[, lauk]) > 0))

# 5. KONVERSI KE TRANSACTIONS
# 5.1 hanya transaksi kosong yang dihapus; versi minimal 2 item sebagai pembanding
transaksi_semua <- as(df_analisis, "transactions")
transaksi       <- transaksi_semua[size(transaksi_semua) > 0]
transaksi_min2  <- transaksi_semua[size(transaksi_semua) > 1]
panjang <- size(transaksi)
summary(transaksi)

simpan(data.frame(
  karakteristik = c("Transaksi awal", "Transaksi 1 item (dipertahankan)",
                    "Transaksi dianalisis", "Jumlah item",
                    "Item yang muncul", "Kepadatan data (%)"),
  nilai = c(length(transaksi_semua), sum(size(transaksi_semua) == 1),
            length(transaksi), ncol(df_analisis),
            sum(itemFrequency(transaksi, type = "absolute") > 0),
            round(100 * sum(panjang) / (length(transaksi) * ncol(df_analisis)), 2))
), "01_karakteristik_data.csv")
simpan(as.data.frame(table(jumlah_item = panjang), responseName = "jumlah_transaksi"),
       "02_distribusi_jumlah_item.csv")
simpan(data.frame(statistik = c("Minimum", "Kuartil 1", "Median",
                                "Rata-rata", "Kuartil 3", "Maksimum"),
                  nilai = round(as.numeric(summary(panjang)), 3)),
       "03_statistik_panjang_transaksi.csv")

# 5.2 KOMPOSISI KERANJANG
ada_makan <- rowSums(df_item[, makanan]) >= 1
ada_minum <- rowSums(df_item[, minuman]) >= 1
pola <- c("Minuman saja", "Makanan tanpa minuman", "Makanan dan minuman")[1 + ada_makan * (1 + ada_minum)]
komposisi <- as.data.frame(table(pola), responseName = "jumlah_transaksi")
komposisi$persen <- round(100 * komposisi$jumlah_transaksi / nrow(df_item), 1)
simpan(komposisi, "04_komposisi_keranjang.csv")

# 6. ANALISIS FREKUENSI ITEM
item_freq <- sort(itemFrequency(transaksi, type = "relative"), decreasing = TRUE)
simpan(data.frame(item = names(item_freq),
                  frekuensi_relatif = round(as.numeric(item_freq), 3),
                  jumlah_transaksi = itemFrequency(transaksi, type = "absolute")[names(item_freq)]),
       "05_frekuensi_item.csv")

# 7. VISUALISASI ITEM PALING LAKU
png(file.path(FOLDER_HASIL, "Gambar_1_frekuensi_item.png"), width = 1800, height = 1100, res = 170)
par(mar = c(9, 4, 3, 1))
itemFrequencyPlot(transaksi, topN = 10, col = "aquamarine",
                  main = "10 Item Makanan dan Minuman Paling Sering Dibeli")
dev.off()

# 8. IDENTIFIKASI ITEM WAJIB (frekuensi > 60%)
item_wajib <- names(item_freq[item_freq > 0.6])
item_wajib

# fungsi apriori + eliminasi item wajib pada LHS dan RHS (langkah 11)
buang_wajib <- function(r) subset(r, subset = !(rhs %in% item_wajib) & !(lhs %in% item_wajib))
aturan <- function(tx, supp = 0.03, conf = 0.6, ...)
  buang_wajib(apriori(tx, parameter = list(supp = supp, conf = conf, minlen = 2, ...),
                      control = list(verbose = FALSE)))

# 9-10. ASSOCIATION RULES, DIURUTKAN BERDASARKAN LIFT
rules <- sort(apriori(transaksi, parameter = list(supp = 0.03, conf = 0.6, minlen = 2)),
              by = "lift", decreasing = TRUE)
inspect(head(rules, 10))

semua <- tabel_rules(rules)
semua$memuat_item_wajib <- c("Tidak", "LHS", "RHS")[
  1 + pmax(2 * (rhs(rules) %in% item_wajib), lhs(rules) %in% item_wajib)]
simpan(semua, "06_semua_aturan.csv")

q <- quality(rules)[c("support", "confidence", "coverage", "lift", "count")]
simpan(data.frame(ukuran = c("Support", "Confidence", "Coverage", "Lift", "Jumlah transaksi"),
                  minimum = sapply(q, min), rata_rata = sapply(q, mean), maksimum = sapply(q, max)),
       "07_statistik_kualitas_aturan.csv")

# 11. ELIMINASI ATURAN BIAS
rules_nobias <- buang_wajib(rules)
simpan(tabel_rules(rules_nobias), "08_aturan_tanpa_bias.csv")

# 12. SELEKSI ATURAN KUAT
rules_strong <- sort(subset(rules_nobias, subset = confidence >= 0.6 & lift >= 1.5 & coverage >= 0.05),
                     by = "lift", decreasing = TRUE)
inspect(rules_strong)
simpan(tabel_rules(rules_strong), "09_aturan_kuat.csv")

# 13. VISUALISASI ATURAN TANPA ITEM WAJIB
png(file.path(FOLDER_HASIL, "Gambar_2_graph_aturan.png"), width = 1600, height = 1200, res = 170)
print(plot(
  head(rules_nobias, 10), method = "graph",
  control = list(edges = ggraph::geom_edge_link(
    end_cap = ggraph::circle(4, "mm"), start_cap = ggraph::circle(4, "mm"),
    color = "gray40", alpha = 0.8,
    arrow = grid::arrow(length = grid::unit(3, "mm"), type = "closed")))
) + ggplot2::scale_color_gradient(low = "#f4a582", high = "#b2182b") +
  ggplot2::scale_size(range = c(4, 10)))
dev.off()

# 14. UJI FISHER EXACT UNTUK SETIAP ATURAN (+ sel 2x2 dan odds ratio)
m   <- as(transaksi, "matrix")
sel <- sel_2x2(m[, labels(lhs(rules_nobias), setStart = "", setEnd = ""), drop = FALSE],
               m[, labels(rhs(rules_nobias), setStart = "", setEnd = ""), drop = FALSE])
uji <- tabel_rules(rules_nobias)
uji$p_value_fisher     <- signif(p_fisher(rules_nobias, transaksi), 4)
uji$signifikan_5persen <- ya_tidak(uji$p_value_fisher)
uji$p_holm             <- signif(p.adjust(uji$p_value_fisher, method = "holm"), 4)
uji$signifikan_holm    <- ya_tidak(uji$p_holm)
uji[c("a", "b", "c", "d")] <- sel
uji$odds_ratio <- round(fisher_sel(sel, "estimate"), 3)
simpan(uji, "10_uji_fisher.csv")
print(uji)

# 14.1 UJI FISHER PADA SELURUH PASANGAN ITEM (support >= 3%, tanpa batas confidence)
# - X -> Y dan Y -> X ber-p-value sama, jadi Holm dihitung sekali per pasangan
rules_pasangan <- aturan(transaksi, conf = 0, maxlen = 2)
pasangan <- tabel_rules(rules_pasangan)
p_mentah <- p_fisher(rules_pasangan, transaksi)
kunci    <- kunci_pasang(pasangan$lhs, pasangan$rhs)
unik     <- !duplicated(kunci)
pasangan$p_value_fisher <- signif(p_mentah, 4)
pasangan$p_holm <- signif(unname(setNames(p.adjust(p_mentah[unik], method = "holm"), kunci[unik])[kunci]), 4)
pasangan$jumlah_pasangan_diuji <- sum(unik)
pasangan <- pasangan[order(pasangan$p_value_fisher, -pasangan$confidence), ]
simpan(pasangan, "13_uji_fisher_seluruh_pasangan.csv")
print(head(pasangan, 10))

# 15. SENSITIVITAS PARAMETER
grid <- expand.grid(support = c(0.02, 0.03, 0.05), confidence = c(0.4, 0.5, 0.6, 0.7),
                    data = c("Semua transaksi", "Minimal 2 item"), stringsAsFactors = FALSE)
data_tx <- list("Semua transaksi" = transaksi, "Minimal 2 item" = transaksi_min2)
hitung <- t(mapply(function(s, k, d) {
  q <- quality(aturan(data_tx[[d]], supp = s, conf = k))
  c(jumlah_aturan = nrow(q), aturan_lift_di_atas_1 = sum(q$lift > 1),
    aturan_kuat = sum(q$lift >= 1.5 & q$coverage >= 0.05))
}, grid$support, grid$confidence, grid$data))
grid <- cbind(grid, hitung)
simpan(grid, "11_sensitivitas_parameter.csv")
print(grid)

# 15.1 PEMBANDING: ATURAN PADA DATA MINIMAL 2 ITEM (CARA KATING)
rules_min2 <- sort(aturan(transaksi_min2), by = "lift", decreasing = TRUE)
uji_min2 <- tabel_rules(rules_min2)
uji_min2$p_value_fisher <- signif(p_fisher(rules_min2, transaksi_min2), 4)
uji_min2$p_holm <- signif(p.adjust(uji_min2$p_value_fisher, method = "holm"), 4)
simpan(uji_min2, "12_aturan_minimal_2_item.csv")
print(uji_min2)

# 15.2 PERBANDINGAN ANTARPERIODE PENCATATAN
# - dibandingkan: aturan tanpa bias + pasangan signifikan Holm (satu arah, confidence tertinggi)
awal <- as.Date(df_kantin$Tanggal) <= as.Date("2026-11-13")
sig <- pasangan[pasangan$p_holm < 0.05, ]
sig <- sig[order(-sig$confidence), ]
sig <- sig[!duplicated(kunci_pasang(sig$lhs, sig$rhs)), ]
dibandingkan <- unique(rbind(tabel_rules(rules_nobias)[, c("lhs", "rhs")], sig[, c("lhs", "rhs")]))

per_periode <- list("2-13 November" = awal, "16-18 November" = !awal,
                    "Seluruh periode" = rep(TRUE, nrow(df_analisis)))
banding <- do.call(rbind, Map(function(p, baris) {
  d <- df_analisis[baris, ]
  x <- as.matrix(d[, nama_item(dibandingkan$lhs), drop = FALSE])
  y <- as.matrix(d[, nama_item(dibandingkan$rhs), drop = FALSE])
  s <- sel_2x2(x, y)
  conf <- s[, 1] / pmax(colSums(x), 1)
  data.frame(periode = p, jumlah_transaksi = nrow(d),
             jumlah_aturan_periode = length(aturan(as(d, "transactions"))),
             lhs = dibandingkan$lhs, rhs = dibandingkan$rhs,
             count_lhs = colSums(x), count = s[, 1],
             confidence = round(conf, 4), lift = round(conf / colMeans(y), 4),
             p_value_fisher = signif(fisher_sel(s, "p.value", alternative = "greater"), 4))
}, names(per_periode), per_periode))
simpan(banding, "14_perbandingan_periode.csv")
print(banding)

# 16. SIMPAN MATRIKS TRANSAKSI YANG DIANALISIS
simpan(data.frame(df_kantin[, 1:3], df_analisis * 1L, check.names = FALSE),
       "00_matriks_transaksi_analisis.csv")
cat("Selesai. Hasil tersimpan di folder:", normalizePath(FOLDER_HASIL), "\n")
