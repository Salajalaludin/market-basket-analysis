# ================================================================
# ANALISIS 7 - KELOMPOK OLAHAN MAKANAN X KELOMPOK RASA MINUMAN
# Market Basket Analysis dengan Algoritma Apriori
#
# Alur kode mengikuti Lampiran 3 (Kode R Studio) laporan kelompok K1
# "Penerapan MBA dengan Pendekatan Algoritma Apriori ... Kantin FT
# UNTIRTA" (2025). Nomor langkah 1-13 sama dengan kode tersebut,
# langkah 14-16 adalah tambahan.
#
# Perlakuan khusus analisis ini:
# Lauk (nasi melekat di dalamnya) dikelompokkan menurut cara
# pengolahan menjadi 5 kelompok seperti Analisis 4, sedangkan minuman
# dikelompokkan menurut rasa menjadi 2 kelompok seperti Analisis 5
# (air putih vs minuman manis). Prosedur utama sama dengan kating.
# Dasar: generalized/multilevel association rules (Srikant & Agrawal,
# 1995; Han & Fu, 1995).
# Tambahan langkah 14.1: uji Fisher pada SELURUH pasangan item tanpa
# batas confidence (significant pattern discovery, Webb 2007), dengan
# koreksi Holm atas semua pasangan yang diuji.
#
# Cara menjalankan (RStudio):
# Session > Set Working Directory > To Source File Location, lalu
# Source. File data dibaca dari folder induk (../MBA_Data_Bersih.xlsx).
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
data_mba <- read_excel("../MBA_Data_Bersih.xlsx", sheet = "Matriks_Biner")
df_kantin <- as.data.frame(data_mba)

# 3. EKSPLORASI DATA
dim(df_kantin)     # 184 x 35
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

# 4.1 PENGELOMPOKAN DASAR ITEM
minuman <- c("air putih", "es air putih",
             grep("^tea jus|^jasjus", colnames(df_item), value = TRUE),
             "good day freeze", "kopi", "teh biasa",
             "susu kental frisian flag")
makanan <- setdiff(colnames(df_item), c(minuman, "nasi"))
nasi_goreng <- c("nasi goreng ayam", "nasi goreng cabe ijo")
lauk <- setdiff(makanan, nasi_goreng)
huruf_awal <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))

# 4.2 PENGELOMPOKAN MAKANAN BERDASARKAN CARA PENGOLAHAN LAUK
# - nasi melekat pada paket lauk, sehingga tidak menjadi item
kelompok_makanan <- list(
  "Lauk goreng sambal" = c("ayam penyet cabe ijo", "ayam sambal seruit",
                           "pecel ayam", "pecel lele", "kulit ayam", "omlet"),
  "Lauk bakar"         = c("ayam bakar"),
  "Lauk bumbu masak"   = c("ayam rica rica", "ayam bumbu hitam"),
  "Lauk berkuah"       = c("soto ayam", "tongseng ayam"),
  "Nasi goreng"        = nasi_goreng
)
stopifnot(setequal(unlist(kelompok_makanan), makanan))

# 4.3 PENGELOMPOKAN MINUMAN MENURUT RASA (SAMA DENGAN ANALISIS 5)
kelompok_minuman <- list(
  "Minuman air putih" = c("air putih", "es air putih"),
  "Minuman manis"     = c(grep("^tea jus|^jasjus", colnames(df_item), value = TRUE),
                          "good day freeze", "kopi", "teh biasa",
                          "susu kental frisian flag")
)
stopifnot(setequal(unlist(kelompok_minuman), minuman))

kelompok <- c(kelompok_makanan, kelompok_minuman)
df_analisis <- as.data.frame(lapply(kelompok, function(k)
  apply(df_item[, k, drop = FALSE], 1, any)), check.names = FALSE)
simpan(data.frame(kelompok = rep(names(kelompok), lengths(kelompok)),
                  item_asal = unlist(kelompok, use.names = FALSE)),
       "00_tabel_pengelompokan_item.csv")

# 5. KONVERSI KE TRANSACTIONS
transaksi_semua <- as(df_analisis, "transactions")
summary(transaksi_semua)
which(size(transaksi_semua) == 0)

# 5.1 PEMBERSIHAN TRANSAKSI (WAJIB)
# - hapus transaksi kosong & 1 item
transaksi <- transaksi_semua[size(transaksi_semua) > 1]
summary(transaksi)

# ringkasan untuk Tabel 4.1 - 4.3
panjang <- size(transaksi)
simpan(data.frame(
  karakteristik = c("Transaksi awal", "Transaksi 1 item (dikeluarkan)",
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

# 5.2 KOMPOSISI KERANJANG (SELURUH 184 TRANSAKSI)
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
simpan(tabel_rules(rules), "06_semua_aturan.csv")
q <- quality(rules)
simpan(data.frame(ukuran = c("Support", "Confidence", "Coverage", "Lift", "Jumlah transaksi"),
                  minimum = sapply(q[c("support", "confidence", "coverage", "lift", "count")], min),
                  rata_rata = sapply(q[c("support", "confidence", "coverage", "lift", "count")], mean),
                  maksimum = sapply(q[c("support", "confidence", "coverage", "lift", "count")], max)),
       "07_statistik_kualitas_aturan.csv")

# 11. ELIMINASI ATURAN BIAS
# - buang RHS item wajib
if (length(item_wajib) > 0) {
  rules_nobias <- subset(rules, subset = !(rhs %in% item_wajib))
} else {
  rules_nobias <- rules
}
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
if (length(rules) > 0) {
  png(file.path(FOLDER_HASIL, "Gambar_2_graph_aturan.png"),
      width = 1600, height = 1200, res = 170)
  print(plot(
    head(rules, 10),
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
if (length(rules_nobias) > 0) {
  uji <- tabel_rules(rules_nobias)
  uji$p_value_fisher <- signif(interestMeasure(rules_nobias, "fishersExactTest",
                                               transactions = transaksi), 4)
  uji$signifikan_5persen <- ifelse(uji$p_value_fisher < 0.05, "Ya", "Tidak")
  uji$p_holm <- signif(p.adjust(uji$p_value_fisher, method = "holm"), 4)
  uji$signifikan_holm <- ifelse(uji$p_holm < 0.05, "Ya", "Tidak")
  simpan(uji, "10_uji_fisher.csv")
  print(uji)
}

# 14.1 TAMBAHAN: UJI FISHER PADA SELURUH PASANGAN ITEM
# - semua aturan 2 item dengan support >= 3% dibentuk TANPA batas confidence
# - setiap pasangan diuji dua arah (X -> Y dan Y -> X memiliki p-value sama),
#   sehingga hanya satu arah per pasangan yang dihitung dalam koreksi Holm
rules_pasangan <- apriori(transaksi,
                          parameter = list(supp = 0.03, conf = 0, minlen = 2, maxlen = 2),
                          control = list(verbose = FALSE))
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
#   dan jika transaksi 1 item tetap dimasukkan
grid <- expand.grid(support = c(0.02, 0.03, 0.05),
                    confidence = c(0.4, 0.5, 0.6, 0.7),
                    data = c("Minimal 2 item", "Semua transaksi"),
                    stringsAsFactors = FALSE)
for (i in seq_len(nrow(grid))) {
  tx <- if (grid$data[i] == "Minimal 2 item") transaksi else transaksi_semua
  r <- apriori(tx, parameter = list(supp = grid$support[i],
                                    conf = grid$confidence[i], minlen = 2),
               control = list(verbose = FALSE))
  if (length(item_wajib) > 0 && length(r) > 0) r <- subset(r, subset = !(rhs %in% item_wajib))
  grid$jumlah_aturan[i] <- length(r)
  grid$aturan_lift_di_atas_1[i] <- if (length(r)) sum(quality(r)$lift > 1) else 0
  grid$aturan_kuat[i] <- if (length(r)) sum(quality(r)$lift >= 1.5 &
                                            quality(r)$coverage >= 0.05) else 0
}
simpan(grid, "11_sensitivitas_parameter.csv")
print(grid)

# 15.1 ATURAN PADA SELURUH TRANSAKSI (TERMASUK TRANSAKSI 1 ITEM)
# - parameter sama (support 3%, confidence 60%), hanya penyebutnya berbeda
rules_semua <- apriori(transaksi_semua,
                       parameter = list(supp = 0.03, conf = 0.6, minlen = 2),
                       control = list(verbose = FALSE))
if (length(item_wajib) > 0 && length(rules_semua) > 0) {
  rules_semua <- subset(rules_semua, subset = !(rhs %in% item_wajib))
}
if (length(rules_semua) > 0) {
  rules_semua <- sort(rules_semua, by = "lift", decreasing = TRUE)
  uji_semua <- tabel_rules(rules_semua)
  uji_semua$p_value_fisher <- signif(interestMeasure(rules_semua, "fishersExactTest",
                                                     transactions = transaksi_semua), 4)
} else {
  uji_semua <- data.frame(keterangan = "Tidak ada aturan yang terbentuk")
}
simpan(uji_semua, "12_aturan_semua_transaksi.csv")
print(uji_semua)

# 16. SIMPAN MATRIKS TRANSAKSI YANG DIANALISIS
simpan(data.frame(df_kantin[, 1:3], df_analisis * 1L, check.names = FALSE),
       "00_matriks_transaksi_analisis.csv")
cat("Selesai. Hasil tersimpan di folder:", normalizePath(FOLDER_HASIL), "\n")
