# ================================================================
# ANALISIS 9 - PENGARUH ELIMINASI NASI DAN DATA BARU
# Market Basket Analysis dengan Algoritma Apriori
#
# Prosedur sama dengan Analisis 8 (MBA_Analisis_8_Ringkas.R), dijalankan
# pada enam skenario:
#   0. data lama (224) + nasi dibuang      -> sama dengan Analisis 8
#   1. data lama (224) + nasi dipertahankan
#   2. data baru (222) + nasi dipertahankan
#   3. data baru (222) + nasi dibuang      -> prosedur Analisis 8 pada data baru
#   4. data lama (224) + nasi paket
#   5. data baru (222) + nasi paket
#
# Tiga perlakuan nasi:
# - "dibuang"       : nasi = item "nasi putih"; langkah 11 mengeliminasi aturan
#                     yang memuat item wajib (nasi putih, frekuensi > 60%)
#                     pada LHS atau RHS.
# - "dipertahankan" : nasi = item "nasi putih"; langkah 11 dilewati.
# - "paket"         : seperti Analisis 1, kolom nasi dihapus dan setiap lauk
#                     dibaca sebagai paket (nasi + lauk), mis. "paket ayam
#                     bakar". Nasi goreng tetap menu tersendiri. Langkah 11
#                     tetap dijalankan, tetapi tidak ada item wajib.
#
# Perbedaan data baru terhadap data lama:
# - TRX-177 dan TRX-178 (ayam bakar, 13 November) dihapus -> 222 transaksi
# - 40 transaksi yang semula bertanggal 16-18 November diberi tanggal
#   2-13 November; isi item setiap ID tidak berubah
# Karena itu pembanding langkah 15.2 memakai nomor ID (TRX-001 s.d. 184 =
# pencatatan awal, TRX-185 s.d. 224 = pencatatan tambahan), bukan tanggal.
#
# Hasil: hasil/<skenario>/ dan hasil/ringkasan_skenario.csv
#
# Cara menjalankan (RStudio):
# Session > Set Working Directory > To Source File Location, lalu Source.
# ================================================================

library(readxl)
library(arules)
library(arulesViz)

DATA_LAMA <- "data/MBA_Riset_Pemasaran_lama_224.xlsx"
DATA_BARU <- "data/MBA_Riset_Pemasaran_baru_222.xlsx"
SKENARIO <- data.frame(
  folder    = c("0_lama_nasi_dibuang", "1_lama_nasi_dipertahankan",
                "2_baru_nasi_dipertahankan", "3_baru_nasi_dibuang",
                "4_lama_nasi_paket", "5_baru_nasi_paket"),
  file_data = c(DATA_LAMA, DATA_LAMA, DATA_BARU, DATA_BARU, DATA_LAMA, DATA_BARU),
  nasi      = c("dibuang", "dipertahankan", "dipertahankan", "dibuang", "paket", "paket"))
BATAS_ID_AWAL <- 184   # TRX-001 s.d. TRX-184 = pencatatan awal (2-13 November)

# ---------------- FUNGSI BANTU ----------------
ya_tidak     <- function(p) c("Tidak", "Ya")[(p < 0.05) + 1]
nama_item    <- function(s) gsub("^\\{|\\}$", "", s)                       # "{a,b}" -> "a,b"
kunci_pasang <- function(l, r) paste(pmin(l, r), pmax(l, r), sep = " & ")  # X->Y = Y->X
p_fisher     <- function(r, tx) interestMeasure(r, "fishersExactTest", transactions = tx)

tabel_rules <- function(r) {
  q <- quality(r)
  data.frame(lhs = labels(lhs(r)), rhs = labels(rhs(r)),
             support = round(q$support, 4), confidence = round(q$confidence, 4),
             coverage = round(q$coverage, 4), lift = round(q$lift, 4),
             count = q$count)
}

# matriks logical transaksi x itemset: TRUE jika transaksi memuat SEMUA item
# pada itemset (berlaku juga untuk LHS lebih dari satu item, mis. {nasi putih,air putih})
memuat <- function(d, label) sapply(label, function(l) {
  it <- strsplit(nama_item(l), ",")[[1]]
  rowSums(d[, it, drop = FALSE] == 1) == length(it)
})

# sel a, b, c, d tabel kontingensi 2 x 2 untuk tiap kolom x[, k] -> y[, k]
sel_2x2 <- function(x, y) unname(cbind(a = colSums(x & y), b = colSums(x & !y),
                                       c = colSums(!x & y), d = colSums(!x & !y)))
fisher_sel <- function(sel, ambil, ...) unname(apply(sel, 1, function(v)
  fisher.test(matrix(v, 2, byrow = TRUE), ...)[[ambil]]))

# ---------------- PROSEDUR MBA (LANGKAH 2-16) ----------------
analisis_mba <- function(folder, file_data, nasi) {
  cat("\n================ SKENARIO", folder, "================\n")
  folder <- file.path("hasil", folder)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  simpan <- function(x, nama) write.csv(x, file.path(folder, nama), row.names = FALSE)

  # 2. INPUT DATA
  df_kantin <- as.data.frame(read_excel(file_data, sheet = "Matriks_Biner"))

  # 4. PRE-PROCESSING: kolom 1-3 (ID, baris Excel, tanggal) bukan item
  df_item <- df_kantin[, -(1:3)]
  df_item[] <- lapply(df_item, function(x) as.logical(as.numeric(x)))

  # 4.1 PENGELOMPOKAN DASAR ITEM
  minuman <- c("air putih", "es air putih",
               grep("^tea jus|^jasjus", colnames(df_item), value = TRUE),
               "good day freeze", "kopi", "teh biasa", "susu kental frisian flag")
  makanan <- setdiff(colnames(df_item), c(minuman, "nasi"))
  lauk    <- setdiff(makanan, c("nasi goreng ayam", "nasi goreng cabe ijo"))

  # 4.2 PERLAKUAN NASI
  # - dibuang / dipertahankan: nasi menjadi item tersendiri ("nasi putih")
  # - paket: kolom nasi dihapus, lauk diberi awalan "paket"
  df_analisis <- df_item[, c(makanan, "nasi", minuman)]
  colnames(df_analisis)[colnames(df_analisis) == "nasi"] <- "nasi putih"
  if (nasi == "paket") {
    df_analisis$`nasi putih` <- NULL
    colnames(df_analisis)[colnames(df_analisis) %in% lauk] <- paste("paket", lauk)
  }

  # 5. TRANSACTIONS (hanya transaksi kosong yang dihapus)
  transaksi_semua <- as(df_analisis, "transactions")
  transaksi       <- transaksi_semua[size(transaksi_semua) > 0]
  transaksi_min2  <- transaksi_semua[size(transaksi_semua) > 1]
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

  # 5.2 KOMPOSISI KERANJANG
  ada_makan <- rowSums(df_item[, makanan]) >= 1
  ada_minum <- rowSums(df_item[, minuman]) >= 1
  pola <- c("Minuman saja", "Makanan tanpa minuman", "Makanan dan minuman")[1 + ada_makan * (1 + ada_minum)]
  komposisi <- as.data.frame(table(pola), responseName = "jumlah_transaksi")
  komposisi$persen <- round(100 * komposisi$jumlah_transaksi / nrow(df_item), 1)
  simpan(komposisi, "04_komposisi_keranjang.csv")

  # 6. FREKUENSI ITEM
  item_freq <- sort(itemFrequency(transaksi, type = "relative"), decreasing = TRUE)
  simpan(data.frame(item = names(item_freq),
                    frekuensi_relatif = round(as.numeric(item_freq), 3),
                    jumlah_transaksi = itemFrequency(transaksi, type = "absolute")[names(item_freq)]),
         "05_frekuensi_item.csv")

  # 7. VISUALISASI ITEM PALING LAKU
  png(file.path(folder, "Gambar_1_frekuensi_item.png"), width = 1800, height = 1100, res = 170)
  par(mar = c(9, 4, 3, 1))
  itemFrequencyPlot(transaksi, topN = 10, col = "aquamarine",
                    main = "10 Item Makanan dan Minuman Paling Sering Dibeli")
  dev.off()

  # 8. ITEM WAJIB (frekuensi > 60%)
  item_wajib <- names(item_freq[item_freq > 0.6])
  cat("Item wajib:", item_wajib, "| nasi", toupper(nasi), "\n")

  # langkah 11 sebagai fungsi: dilewati hanya jika nasi dipertahankan
  saring <- function(r) {
    if (nasi == "dipertahankan") return(r)
    subset(r, subset = !(rhs %in% item_wajib) & !(lhs %in% item_wajib))
  }
  aturan <- function(tx, supp = 0.03, conf = 0.6, ...)
    saring(apriori(tx, parameter = list(supp = supp, conf = conf, minlen = 2, ...),
                   control = list(verbose = FALSE)))

  # 9-10. ASSOCIATION RULES, DIURUTKAN BERDASARKAN LIFT
  rules <- sort(apriori(transaksi, parameter = list(supp = 0.03, conf = 0.6, minlen = 2),
                        control = list(verbose = FALSE)),
                by = "lift", decreasing = TRUE)
  semua <- tabel_rules(rules)
  semua$memuat_item_wajib <- c("Tidak", "LHS", "RHS")[
    1 + pmax(2 * (rhs(rules) %in% item_wajib), lhs(rules) %in% item_wajib)]
  simpan(semua, "06_semua_aturan.csv")

  q <- quality(rules)[c("support", "confidence", "coverage", "lift", "count")]
  simpan(data.frame(ukuran = c("Support", "Confidence", "Coverage", "Lift", "Jumlah transaksi"),
                    minimum = sapply(q, min), rata_rata = sapply(q, mean), maksimum = sapply(q, max)),
         "07_statistik_kualitas_aturan.csv")

  # 11. ATURAN FINAL (nasi dibuang -> tanpa item wajib; dipertahankan -> semua aturan)
  rules_final <- saring(rules)
  simpan(tabel_rules(rules_final), "08_aturan_final.csv")

  # 12. ATURAN KUAT
  rules_strong <- sort(subset(rules_final, subset = confidence >= 0.6 & lift >= 1.5 & coverage >= 0.05),
                       by = "lift", decreasing = TRUE)
  inspect(rules_strong)
  simpan(tabel_rules(rules_strong), "09_aturan_kuat.csv")

  # 13. VISUALISASI 10 ATURAN FINAL TERATAS
  png(file.path(folder, "Gambar_2_graph_aturan.png"), width = 1600, height = 1200, res = 170)
  print(plot(
    head(rules_final, 10), method = "graph",
    control = list(edges = ggraph::geom_edge_link(
      end_cap = ggraph::circle(4, "mm"), start_cap = ggraph::circle(4, "mm"),
      color = "gray40", alpha = 0.8,
      arrow = grid::arrow(length = grid::unit(3, "mm"), type = "closed")))
  ) + ggplot2::scale_color_gradient(low = "#f4a582", high = "#b2182b") +
    ggplot2::scale_size(range = c(4, 10)))
  dev.off()

  # 14. UJI FISHER EXACT UNTUK SETIAP ATURAN FINAL (+ sel 2x2 dan odds ratio)
  m   <- as(transaksi, "matrix")
  sel <- sel_2x2(memuat(m, labels(lhs(rules_final))), memuat(m, labels(rhs(rules_final))))
  uji <- tabel_rules(rules_final)
  uji$p_value_fisher     <- signif(p_fisher(rules_final, transaksi), 4)
  uji$signifikan_5persen <- ya_tidak(uji$p_value_fisher)
  uji$p_holm             <- signif(p.adjust(uji$p_value_fisher, method = "holm"), 4)
  uji$signifikan_holm    <- ya_tidak(uji$p_holm)
  uji[c("a", "b", "c", "d")] <- sel
  uji$odds_ratio <- round(fisher_sel(sel, "estimate"), 3)
  simpan(uji, "10_uji_fisher.csv")

  # 14.1 UJI FISHER PADA SELURUH PASANGAN ITEM (support >= 3%, tanpa batas confidence)
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

  # 15. SENSITIVITAS PARAMETER
  grid <- expand.grid(support = c(0.02, 0.03, 0.05), confidence = c(0.4, 0.5, 0.6, 0.7),
                      data = c("Semua transaksi", "Minimal 2 item"), stringsAsFactors = FALSE)
  data_tx <- list("Semua transaksi" = transaksi, "Minimal 2 item" = transaksi_min2)
  hitung <- t(mapply(function(s, k, d) {
    q <- quality(aturan(data_tx[[d]], supp = s, conf = k))
    c(jumlah_aturan = nrow(q), aturan_lift_di_atas_1 = sum(q$lift > 1),
      aturan_kuat = sum(q$lift >= 1.5 & q$coverage >= 0.05))
  }, grid$support, grid$confidence, grid$data))
  simpan(cbind(grid, hitung), "11_sensitivitas_parameter.csv")

  # 15.1 PEMBANDING: DATA MINIMAL 2 ITEM (CARA KATING)
  rules_min2 <- sort(aturan(transaksi_min2), by = "lift", decreasing = TRUE)
  uji_min2 <- tabel_rules(rules_min2)
  uji_min2$p_value_fisher <- signif(p_fisher(rules_min2, transaksi_min2), 4)
  uji_min2$p_holm <- signif(p.adjust(uji_min2$p_value_fisher, method = "holm"), 4)
  simpan(uji_min2, "12_aturan_minimal_2_item.csv")

  # 15.2 PERBANDINGAN PENCATATAN AWAL vs TAMBAHAN (berdasarkan nomor ID)
  awal <- as.integer(sub("TRX-", "", df_kantin$`ID transaksi`)) <= BATAS_ID_AWAL
  sig <- pasangan[pasangan$p_holm < 0.05, ]
  sig <- sig[order(-sig$confidence), ]
  sig <- sig[!duplicated(kunci_pasang(sig$lhs, sig$rhs)), ]
  dibandingkan <- unique(rbind(tabel_rules(rules_final)[, c("lhs", "rhs")], sig[, c("lhs", "rhs")]))

  kelompok <- list("Pencatatan awal (TRX-001 s.d. 184)" = awal,
                   "Pencatatan tambahan (TRX-185 s.d. 224)" = !awal,
                   "Seluruh data" = rep(TRUE, nrow(df_analisis)))
  banding <- do.call(rbind, Map(function(p, baris) {
    d <- df_analisis[baris, ]
    x <- memuat(d, dibandingkan$lhs)
    y <- memuat(d, dibandingkan$rhs)
    s <- sel_2x2(x, y)
    conf <- s[, 1] / pmax(colSums(x), 1)
    data.frame(kelompok_data = p, jumlah_transaksi = nrow(d),
               jumlah_aturan_kelompok = length(aturan(as(d, "transactions"))),
               lhs = dibandingkan$lhs, rhs = dibandingkan$rhs,
               count_lhs = colSums(x), count = s[, 1],
               confidence = round(conf, 4), lift = round(conf / colMeans(y), 4),
               p_value_fisher = signif(fisher_sel(s, "p.value", alternative = "greater"), 4))
  }, names(kelompok), kelompok))
  simpan(banding, "14_perbandingan_pencatatan.csv")

  # 16. MATRIKS TRANSAKSI YANG DIANALISIS
  simpan(data.frame(df_kantin[, 1:3], df_analisis * 1L, check.names = FALSE),
         "00_matriks_transaksi_analisis.csv")

  # ringkasan skenario
  ada_nasi <- grepl("nasi putih", labels(rules_final))
  data.frame(skenario = basename(folder), data = basename(file_data), nasi = nasi,
             transaksi_dianalisis = length(transaksi), jumlah_item = ncol(df_analisis),
             item_teratas = names(item_freq)[1],
             frekuensi_item_teratas = round(item_freq[[1]], 3),
             jumlah_item_wajib = length(item_wajib),
             aturan_terbentuk = length(rules), aturan_final = length(rules_final),
             aturan_final_memuat_nasi = sum(ada_nasi), aturan_kuat = length(rules_strong),
             signifikan_fisher_5persen = sum(uji$p_value_fisher < 0.05),
             signifikan_holm = sum(uji$p_holm < 0.05),
             pasangan_item_diuji = sum(unik), pasangan_signifikan_holm = nrow(sig),
             aturan_min2_item = length(rules_min2))
}

# ---------------- JALANKAN SEMUA SKENARIO ----------------
ringkasan <- do.call(rbind, Map(analisis_mba, SKENARIO$folder, SKENARIO$file_data, SKENARIO$nasi))
write.csv(ringkasan, file.path("hasil", "ringkasan_skenario.csv"), row.names = FALSE)
print(ringkasan, row.names = FALSE)
cat("Selesai. Hasil tersimpan di folder:", normalizePath("hasil"), "\n")
