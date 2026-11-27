# ================================================================
# ANALISIS 8 - DATA DIPERBARUI (224 TRANSAKSI, 2-18 NOVEMBER 2026)
# Market Basket Analysis dengan Algoritma Apriori
#
# Alur kode mengikuti Lampiran 3 (Kode R Studio) laporan kelompok K1
# "Penerapan MBA dengan Pendekatan Algoritma Apriori ... Kantin FT
# UNTIRTA" (2025). Nomor langkah 1-13 sama dengan kode tersebut,
# langkah 14-16 adalah tambahan.
#
# Perlakuan khusus analisis ini:
# - Data diperbarui: 184 transaksi periode 2-13 November (data yang
#   dipakai Analisis 1-7) ditambah 40 transaksi 16-18 November,
#   sehingga N = 224.
# - Tingkat menu (32 item). Nasi dicatat sebagai item tersendiri
#   ("nasi putih") seperti Analisis 2 dan data kating. Nasi terdeteksi
#   sebagai item wajib pada langkah 8.
# - Langkah 5.1: transaksi 1 item TIDAK dihapus (sama dengan
#   Analisis 6). Support dihitung terhadap seluruh basis transaksi
#   (Agrawal, Imielinski & Swami, 1993). Versi minimal 2 item (cara
#   kating) disimpan sebagai pembanding.
# - Langkah 11: aturan yang memuat item wajib dieliminasi, baik pada
#   RHS (cara kating) maupun pada LHS. Nasi selalu menyertai lauk dan
#   tidak pernah dibeli tanpa lauk, sehingga nasi pada LHS hanya
#   menandai "membeli lauk" dan menghasilkan aturan duplikat.
# - Langkah 14.1: uji Fisher pada seluruh pasangan item (seperti
#   Analisis 7). Langkah 15.2: perbandingan antarperiode pencatatan.
#
# Cara menjalankan (RStudio):
# Session > Set Working Directory > To Source File Location, lalu
# Source. File data dibaca dari subfolder data/ di folder ini.
# ================================================================

# 1. LOAD LIBRARY
library(readxl)
library(arules)
library(arulesViz)

FOLDER_HASIL <- "hasil"
dir.create(FOLDER_HASIL, showWarnings = FALSE)
simpan <- function(x, nama) {
  write.csv(x, file.path(FOLDER_HASIL, nama), row.names = FALSE)
}

# 2. INPUT DATA
data_mba <- read_excel("data/MBA_Riset_Pemasaran.xlsx", sheet = "Matriks_Biner")
df_kantin <- as.data.frame(data_mba)

# 3. EKSPLORASI DATA
dim(df_kantin)     # 224 x 35
str(df_kantin)
head(df_kantin)
colnames(df_kantin)

# 4. PRE-PROCESSING
# - tiga kolom pertama (ID transaksi, baris Excel, tanggal) bukan item
# - kolom item dikonversi ke logical
df_item <- df_kantin[, -(1:3)]
df_item[] <- lapply(
  df_item,
  function(x) as.logical(as.numeric(x))
)

# cek nilai unik
lapply(df_item, unique)

# 4.1 PENGELOMPOKAN DASAR ITEM (UNTUK KOMPOSISI KERANJANG)
minuman <- c("air putih", "es air putih",
             grep("^tea jus|^jasjus", colnames(df_item), value = TRUE),
             "good day freeze", "kopi", "teh biasa",
             "susu kental frisian flag")
makanan <- setdiff(colnames(df_item), c(minuman, "nasi"))
nasi_goreng <- c("nasi goreng ayam", "nasi goreng cabe ijo")
lauk <- setdiff(makanan, nasi_goreng)

# 4.2 NASI SEBAGAI ITEM TERSENDIRI
# - kolom nasi dipertahankan dan diberi nama "nasi putih"
# - cek: nasi selalu bersama lauk dan lauk selalu bersama nasi
df_analisis <- df_item[, c(makanan, "nasi", minuman)]
colnames(df_analisis)[colnames(df_analisis) == "nasi"] <- "nasi putih"
ada_lauk <- rowSums(df_item[, lauk]) > 0
print(table(nasi = df_item$nasi, lauk = ada_lauk))

# 5. KONVERSI KE TRANSACTIONS
transaksi_semua <- as(df_analisis, "transactions")
summary(transaksi_semua)
which(size(transaksi_semua) == 0)

# 5.1 PEMBERSIHAN TRANSAKSI
# - hanya transaksi kosong yang dihapus; transaksi 1 item DIPERTAHANKAN
# - versi minimal 2 item (cara kating) disimpan sebagai pembanding
transaksi <- transaksi_semua[size(transaksi_semua) > 0]
transaksi_min2 <- transaksi_semua[size(transaksi_semua) > 1]
summary(transaksi)

# ringkasan untuk Tabel 4.1 - 4.3
panjang <- size(transaksi)
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

# 5.2 KOMPOSISI KERANJANG (SELURUH 224 TRANSAKSI)
n_makan <- rowSums(df_item[, makanan])
n_minum <- rowSums(df_item[, minuman])
pola <- ifelse(n_makan >= 1 & n_minum >= 1, "Makanan dan minuman",
        ifelse(n_makan >= 1, "Makanan tanpa minuman", "Minuman saja"))
komposisi <- as.data.frame(table(pola), responseName = "jumlah_transaksi")
komposisi$persen <- round(100 * komposisi$jumlah_transaksi / nrow(df_item), 1)
simpan(komposisi, "04_komposisi_keranjang.csv")

# 6. ANALISIS FREKUENSI ITEM
item_freq <- itemFrequency(transaksi, type = "relative")
item_freq <- sort(item_freq, decreasing = TRUE)
# 15 item paling sering muncul
head(item_freq, 15)
simpan(data.frame(item = names(item_freq),
                  frekuensi_relatif = round(as.numeric(item_freq), 3),
                  jumlah_transaksi = itemFrequency(transaksi, type = "absolute")[names(item_freq)]),
       "05_frekuensi_item.csv")

# 7. VISUALISASI ITEM PALING LAKU
png(file.path(FOLDER_HASIL, "Gambar_1_frekuensi_item.png"),
    width = 1800, height = 1100, res = 170)
par(mar = c(9, 4, 3, 1))
itemFrequencyPlot(
  transaksi,
  topN = 10,
  col = "aquamarine",
  main = "10 Item Makanan dan Minuman Paling Sering Dibeli"
)
dev.off()

# 8. IDENTIFIKASI ITEM WAJIB (DOMINAN)
# item dengan frekuensi > 60%
item_wajib <- names(item_freq[item_freq > 0.6])
item_wajib

# 9. PEMBENTUKAN ASSOCIATION RULES
rules <- apriori(
  transaksi,
  parameter = list(
    supp = 0.03,      # minimal 3% transaksi
    conf = 0.6,       # confidence minimal 60%
    minlen = 2
  )
)
summary(rules)

# 10. SORTING ATURAN BERDASARKAN LIFT
rules <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(rules, 10))

tabel_rules <- function(r) {
  if (length(r) == 0) return(data.frame(lhs = character(), rhs = character()))
  q <- quality(r)
  data.frame(lhs = labels(lhs(r)), rhs = labels(rhs(r)),
             support = round(q$support, 4), confidence = round(q$confidence, 4),
             coverage = round(q$coverage, 4), lift = round(q$lift, 4),
             count = q$count)
}
semua <- tabel_rules(rules)
semua$memuat_item_wajib <- ifelse(rhs(rules) %in% item_wajib, "RHS",
                           ifelse(lhs(rules) %in% item_wajib, "LHS", "Tidak"))
simpan(semua, "06_semua_aturan.csv")
q <- quality(rules)
simpan(data.frame(ukuran = c("Support", "Confidence", "Coverage", "Lift", "Jumlah transaksi"),
                  minimum = sapply(q[c("support", "confidence", "coverage", "lift", "count")], min),
                  rata_rata = sapply(q[c("support", "confidence", "coverage", "lift", "count")], mean),
                  maksimum = sapply(q[c("support", "confidence", "coverage", "lift", "count")], max)),
       "07_statistik_kualitas_aturan.csv")

# 11. ELIMINASI ATURAN BIAS
# - buang RHS item wajib (cara kating)
# - buang juga aturan dengan item wajib pada LHS (perlakuan analisis ini)
buang_wajib <- function(r) {
  if (length(item_wajib) == 0 || length(r) == 0) return(r)
  subset(r, subset = !(rhs %in% item_wajib) & !(lhs %in% item_wajib))
}
rules_nobias <- buang_wajib(rules)
simpan(tabel_rules(rules_nobias), "08_aturan_tanpa_bias.csv")

# 12. SELEKSI ATURAN KUAT (FINAL)
rules_strong <- subset(
  rules_nobias,
  subset =
    confidence >= 0.6 &
    lift >= 1.5 &
    coverage >= 0.05
)
rules_strong <- sort(rules_strong, by = "lift", decreasing = TRUE)
inspect(rules_strong)
simpan(tabel_rules(rules_strong), "09_aturan_kuat.csv")

# 13. VISUALISASI ATURAN TERBAIK
# - aturan tanpa item wajib yang divisualisasikan, karena 10 aturan
#   teratas berdasarkan lift masih memuat duplikat dengan nasi pada LHS
rules_graf <- if (length(rules_nobias) > 0) rules_nobias else rules
if (length(rules_graf) > 0) {
  png(file.path(FOLDER_HASIL, "Gambar_2_graph_aturan.png"),
      width = 1600, height = 1200, res = 170)
  print(plot(
    head(rules_graf, 10),
    method = "graph",
    control = list(edges = ggraph::geom_edge_link(
      end_cap = ggraph::circle(4, "mm"), start_cap = ggraph::circle(4, "mm"),
      color = "gray40", alpha = 0.8,
      arrow = grid::arrow(length = grid::unit(3, "mm"), type = "closed")))
  ) + ggplot2::scale_color_gradient(low = "#f4a582", high = "#b2182b") +
    ggplot2::scale_size(range = c(4, 10)))
  dev.off()
}

# 14. TAMBAHAN: UJI FISHER EXACT UNTUK SETIAP ATURAN
# H0: LHS dan RHS saling bebas. p < 0,05 berarti asosiasi signifikan.
# - sel tabel kontingensi 2 x 2 (a, b, c, d) dan odds ratio ikut disimpan
kontingensi <- function(r, tx) {
  m <- as(tx, "matrix")
  t(sapply(seq_along(r), function(i) {
    x <- m[, labels(lhs(r[i]), setStart = "", setEnd = "")]
    y <- m[, labels(rhs(r[i]), setStart = "", setEnd = "")]
    a <- sum(x & y); b <- sum(x & !y); c <- sum(!x & y); d <- sum(!x & !y)
    c(a = a, b = b, c = c, d = d,
      odds_ratio = unname(fisher.test(matrix(c(a, b, c, d), 2, byrow = TRUE))$estimate))
  }))
}
if (length(rules_nobias) > 0) {
  uji <- tabel_rules(rules_nobias)
  uji$p_value_fisher <- signif(interestMeasure(rules_nobias, "fishersExactTest",
                                               transactions = transaksi), 4)
  uji$signifikan_5persen <- ifelse(uji$p_value_fisher < 0.05, "Ya", "Tidak")
  uji$p_holm <- signif(p.adjust(uji$p_value_fisher, method = "holm"), 4)
  uji$signifikan_holm <- ifelse(uji$p_holm < 0.05, "Ya", "Tidak")
  sel <- kontingensi(rules_nobias, transaksi)
  uji <- cbind(uji, sel[, 1:4])
  uji$odds_ratio <- round(sel[, "odds_ratio"], 3)
  simpan(uji, "10_uji_fisher.csv")
  print(uji)
}

# 14.1 TAMBAHAN: UJI FISHER PADA SELURUH PASANGAN ITEM
# - semua aturan 2 item dengan support >= 3% dibentuk TANPA batas confidence
# - pasangan yang memuat item wajib dikeluarkan (sama dengan langkah 11)
# - setiap pasangan diuji dua arah (X -> Y dan Y -> X memiliki p-value sama),
#   sehingga hanya satu arah per pasangan yang dihitung dalam koreksi Holm
rules_pasangan <- apriori(transaksi,
                          parameter = list(supp = 0.03, conf = 0, minlen = 2, maxlen = 2),
                          control = list(verbose = FALSE))
rules_pasangan <- buang_wajib(rules_pasangan)
pasangan <- tabel_rules(rules_pasangan)
pasangan$p_value_fisher <- interestMeasure(rules_pasangan, "fishersExactTest",
                                           transactions = transaksi)
kunci <- apply(cbind(pasangan$lhs, pasangan$rhs), 1, function(z) paste(sort(z), collapse = " & "))
unik <- !duplicated(kunci)
pasangan$p_holm <- NA_real_
pasangan$p_holm[unik] <- p.adjust(pasangan$p_value_fisher[unik], method = "holm")
pasangan$p_holm <- ave(pasangan$p_holm, kunci, FUN = function(v) max(v, na.rm = TRUE))
pasangan$jumlah_pasangan_diuji <- sum(unik)
pasangan$p_value_fisher <- signif(pasangan$p_value_fisher, 4)
pasangan$p_holm <- signif(pasangan$p_holm, 4)
pasangan <- pasangan[order(pasangan$p_value_fisher, -pasangan$confidence), ]
simpan(pasangan, "13_uji_fisher_seluruh_pasangan.csv")
print(head(pasangan, 10))

# 15. TAMBAHAN: SENSITIVITAS PARAMETER
# - menguji apakah temuan berubah jika ambang support/confidence digeser
#   dan jika transaksi 1 item dikeluarkan (cara kating)
grid <- expand.grid(support = c(0.02, 0.03, 0.05),
                    confidence = c(0.4, 0.5, 0.6, 0.7),
                    data = c("Semua transaksi", "Minimal 2 item"),
                    stringsAsFactors = FALSE)
for (i in seq_len(nrow(grid))) {
  tx <- if (grid$data[i] == "Semua transaksi") transaksi else transaksi_min2
  r <- apriori(tx, parameter = list(supp = grid$support[i],
                                    conf = grid$confidence[i], minlen = 2),
               control = list(verbose = FALSE))
  r <- buang_wajib(r)
  grid$jumlah_aturan[i] <- length(r)
  grid$aturan_lift_di_atas_1[i] <- if (length(r)) sum(quality(r)$lift > 1) else 0
  grid$aturan_kuat[i] <- if (length(r)) sum(quality(r)$lift >= 1.5 &
                                            quality(r)$coverage >= 0.05) else 0
}
simpan(grid, "11_sensitivitas_parameter.csv")
print(grid)

# 15.1 PEMBANDING: ATURAN PADA DATA MINIMAL 2 ITEM (CARA KATING)
# - parameter sama (support 3%, confidence 60%), hanya penyebutnya berbeda
rules_min2 <- apriori(transaksi_min2,
                      parameter = list(supp = 0.03, conf = 0.6, minlen = 2),
                      control = list(verbose = FALSE))
rules_min2 <- buang_wajib(rules_min2)
if (length(rules_min2) > 0) {
  rules_min2 <- sort(rules_min2, by = "lift", decreasing = TRUE)
  uji_min2 <- tabel_rules(rules_min2)
  uji_min2$p_value_fisher <- signif(interestMeasure(rules_min2, "fishersExactTest",
                                                    transactions = transaksi_min2), 4)
  uji_min2$p_holm <- signif(p.adjust(uji_min2$p_value_fisher, method = "holm"), 4)
} else {
  uji_min2 <- data.frame(keterangan = "Tidak ada aturan yang terbentuk")
}
simpan(uji_min2, "12_aturan_minimal_2_item.csv")
print(uji_min2)

# 15.2 TAMBAHAN: PERBANDINGAN ANTARPERIODE PENCATATAN
# - periode awal (2-13 November, data Analisis 1-7) dan periode tambahan
#   (16-18 November) dianalisis terpisah dengan prosedur yang sama
# - aturan yang dibandingkan: aturan tanpa bias (langkah 11) ditambah
#   pasangan yang signifikan setelah koreksi Holm pada langkah 14.1
periode <- ifelse(as.Date(df_kantin$Tanggal) <= as.Date("2026-11-13"),
                  "2-13 November", "16-18 November")
sig_pasangan <- pasangan[pasangan$p_holm < 0.05, ]
sig_pasangan <- sig_pasangan[order(-sig_pasangan$confidence), ]
sig_pasangan <- sig_pasangan[!duplicated(apply(cbind(sig_pasangan$lhs, sig_pasangan$rhs), 1,
                                               function(z) paste(sort(z), collapse = " & "))), ]
dibandingkan <- unique(rbind(tabel_rules(rules_nobias)[, c("lhs", "rhs")],
                             sig_pasangan[, c("lhs", "rhs")]))
banding <- do.call(rbind, lapply(c("2-13 November", "16-18 November", "Seluruh periode"), function(p) {
  baris <- if (p == "Seluruh periode") rep(TRUE, nrow(df_analisis)) else periode == p
  d <- df_analisis[baris, ]
  tx <- as(d, "transactions")
  r_p <- buang_wajib(apriori(tx, parameter = list(supp = 0.03, conf = 0.6, minlen = 2),
                             control = list(verbose = FALSE)))
  do.call(rbind, lapply(seq_len(nrow(dibandingkan)), function(k) {
    x <- d[, gsub("^\\{|\\}$", "", dibandingkan$lhs[k])]
    y <- d[, gsub("^\\{|\\}$", "", dibandingkan$rhs[k])]
    data.frame(periode = p, jumlah_transaksi = nrow(d),
               jumlah_aturan_periode = length(r_p),
               lhs = dibandingkan$lhs[k], rhs = dibandingkan$rhs[k],
               count_lhs = sum(x), count = sum(x & y),
               confidence = round(sum(x & y) / max(sum(x), 1), 4),
               lift = round((sum(x & y) / max(sum(x), 1)) / mean(y), 4),
               p_value_fisher = signif(fisher.test(
                 matrix(c(sum(x & y), sum(x & !y), sum(!x & y), sum(!x & !y)), 2, byrow = TRUE),
                 alternative = "greater")$p.value, 4))
  }))
}))
simpan(banding, "14_perbandingan_periode.csv")
print(banding)

# 16. SIMPAN MATRIKS TRANSAKSI YANG DIANALISIS
simpan(data.frame(df_kantin[, 1:3], df_analisis * 1L, check.names = FALSE),
       "00_matriks_transaksi_analisis.csv")
cat("Selesai. Hasil tersimpan di folder:", normalizePath(FOLDER_HASIL), "\n")
