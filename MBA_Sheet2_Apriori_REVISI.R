# ================================================================
# MARKET BASKET ANALYSIS - APRIORI (DATA TERSTANDARISASI)
# Sumber utama : RISET PEMASARAN(1).xlsx / Sheet2
# Referensi    : Lampiran kode R K1_LAPORAN AKHIR_MBA_PROJECT_UAS
# Versi        : 13 November 2026 - revisi penyajian
#
# PENTING:
# - Tidak mengubah workbook Excel asli.
# - Satu baris Sheet2 = satu transaksi (sudah dikonfirmasi).
# - Item gabungan dipisah pada kata 'dan' atau karakter '+'.
# - Setiap jenis item hanya dicatat sekali dalam satu transaksi.
# - Nasi goreng tetap dihitung sebagai menu tersendiri.
# - Air putih biasa berbeda dari es air putih.
# - Tea jus tanpa rasa berarti gula batu.
# - Es kopi good day = good day freeze.
# - Jasjus jeruk/perak = jasjus jeruk peras.
# - Semua nama telah diverifikasi berdasarkan klarifikasi peneliti.
# - Analisis utama memasukkan item nasi dalam transaksi.
# ================================================================

# ---- 0. KONFIGURASI -------------------------------------------------
FILE_INPUT <- 'RISET PEMASARAN(1).xlsx'
FILE_KAMUS <- 'kamus_item_REVISI.csv'
SHEET_INPUT <- 'Sheet2'
FOLDER_OUTPUT <- 'hasil_mba_revisi'

MIN_SUPPORT <- 0.03         # 3% (BUKAN 0.03%)
MIN_CONFIDENCE <- 0.60     # 60%
MIN_RULE_LENGTH <- 2
DOMINANT_ITEM_THRESHOLD <- 0.60
STRONG_MIN_LIFT <- 1.50
STRONG_MIN_COVERAGE <- 0.05

# TRUE mengikuti kating (hanya transaksi >= 2 item).
# Evaluasi terpisah tetap memakai SELURUH transaksi sebagai pembanding.
# PERHATIAN: membuang transaksi satu-item mengubah penyebut support/lift.
HAPUS_TRANSAKSI_SATU_ITEM_UTAMA <- TRUE

# FALSE = hentikan analisis bila ada label baru yang belum dikonfirmasi.
# TRUE hanya untuk eksplorasi sementara saat kamus perlu diperbarui.
IZINKAN_PREVIEW_SEBELUM_KONFIRMASI <- FALSE

# ---- 1. LIBRARY -----------------------------------------------------
paket_wajib <- c('readxl', 'arules')
paket_hilang <- paket_wajib[!vapply(paket_wajib, requireNamespace,
                                  logical(1), quietly = TRUE)]
if (length(paket_hilang) > 0) {
  stop('Paket belum tersedia: ', paste(paket_hilang, collapse = ', '),
       '\nJalankan dahulu: install.packages(c(',
       paste(sprintf('"%s"', paket_hilang), collapse = ', '), '))')
}
suppressPackageStartupMessages(library(arules))
ada_arulesviz <- requireNamespace('arulesViz', quietly = TRUE)
if (!ada_arulesviz) message('Opsional untuk grafik rules: install.packages("arulesViz")')

if (!file.exists(FILE_INPUT)) {
  stop('File data tidak ditemukan: ', FILE_INPUT,
       '\nSimpan file Excel dan script R pada working directory yang sama.')
}
if (!file.exists(FILE_KAMUS)) {
  stop('Kamus nama item tidak ditemukan: ', FILE_KAMUS)
}
dir.create(FOLDER_OUTPUT, recursive = TRUE, showWarnings = FALSE)
file_hasil <- function(nama) file.path(FOLDER_OUTPUT, nama)

# ---- 2. BACA SHEET2 (DATA MENTAH) ----------------------------------
data_mba <- readxl::read_excel(FILE_INPUT, sheet = SHEET_INPUT,
                              col_types = 'text', .name_repair = 'unique')
if (ncol(data_mba) < 4L) stop('Sheet2 minimal harus memiliki kolom A:D.')
if (ncol(data_mba) < 5L) data_mba[[5L]] <- NA_character_
raw <- as.data.frame(data_mba[, 1:5], stringsAsFactors = FALSE)
names(raw) <- c('nomor_asli', 'makanan', 'minuman', 'tanggal_penanda', 'catatan')
raw$baris_excel <- seq_len(nrow(raw)) + 1L
raw$id_internal <- sprintf('TRX-BARIS-%03d', raw$baris_excel)

# Simpan nomor asli apa adanya; kekosongan nomor bukan alasan menghapus data.
cat('Data mentah Sheet2:', nrow(raw), 'baris transaksi\n')
cat('Nomor asli kosong:', sum(is.na(raw$nomor_asli) |
                                 !nzchar(trimws(raw$nomor_asli))), '\n')

# ---- 3. AUDIT & STANDARDISASI KONSERVATIF ----------------------------
normalisasi_teks <- function(x) {
  if (length(x) == 0L || is.na(x)) return('')
  x <- gsub('\u2060', '', as.character(x), fixed = TRUE)
  x <- tolower(trimws(gsub('[[:space:]]+', ' ', x, perl = TRUE)))
  x
}

pecah_item <- function(x) {
  x <- normalisasi_teks(x)
  if (!nzchar(x)) return(character(0))
  hasil <- unlist(strsplit(x, '\\s+dan\\s+|\\s*\\+\\s*',
                           perl = TRUE), use.names = FALSE)
  hasil <- trimws(hasil)
  unique(hasil[nzchar(hasil)])
}

kamus <- read.csv(FILE_KAMUS, stringsAsFactors = FALSE,
                  fileEncoding = 'UTF-8-BOM', check.names = FALSE)
wajib_kamus <- c('jenis', 'raw_normalized', 'usulan_item', 'status')
if (!all(wajib_kamus %in% names(kamus))) {
  stop('Kolom kamus harus mencakup: ', paste(wajib_kamus, collapse = ', '))
}
kamus$jenis <- trimws(tolower(kamus$jenis))
kamus$raw_normalized <- vapply(kamus$raw_normalized,
                              normalisasi_teks, character(1))
kamus$usulan_item <- vapply(kamus$usulan_item,
                            normalisasi_teks, character(1))
kamus$status <- trimws(tolower(kamus$status))
if (anyDuplicated(paste(kamus$jenis, kamus$raw_normalized, sep = '|||'))) {
  stop('Ada kunci (jenis, raw_normalized) duplikat pada kamus.')
}
if (!all(kamus$status %in% c('aman', 'perlu_konfirmasi', 'disetujui'))) {
  stop('Status kamus hanya boleh: aman, perlu_konfirmasi, disetujui.')
}

# Satu nama masukan dapat dipetakan ke beberapa item melalui kamus.
# Simbol | hanya dipakai dalam kamus sebagai delimiter keluaran,
# bukan dianggap item itu sendiri.
petakan_item <- function(x, jenis) {
  token <- pecah_item(x)
  if (!length(token)) return(character(0))
  hasil <- unlist(lapply(token, function(k) {
    idx <- which(kamus$jenis == jenis & kamus$raw_normalized == k)
    if (length(idx) != 1L) {
      stop('Nama item belum ada dalam kamus: [', jenis, '] ', k,
           '\nTambahkan label ke kamus_item_REVISI.csv.')
    }
    if (kamus$status[idx] == 'perlu_konfirmasi') return(k)
    # Penting: split hanya output mapping; input mengandung 'dan' / '+'.
    h <- strsplit(kamus$usulan_item[idx], '\\s*\\|\\s*', perl = TRUE)[[1L]]
    h <- trimws(h)
    h[nzchar(h)]
  }), use.names = FALSE)
  # Pastikan item hasil tercatat tanpa duplikasi dalam satu keranjang.
  if (jenis == 'makanan' && any(!startsWith(token, 'nasi goreng'))) {
    hasil <- c(hasil, 'nasi')
  }
  unique(hasil)
}

# Perhatikan: tanggal hanya penanda awal kelompok baris, tidak memengaruhi Apriori.
parse_tanggal_excel <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || !nzchar(x) || x == 'NA') return(as.Date(NA))
  if (grepl('^[0-9]{5}(\\.0+)?$', x)) {
    return(as.Date(as.numeric(x), origin = '1899-12-30'))
  }
  suppressWarnings(as.Date(x))
}

tanggal_transaksi <- as.Date(rep(NA_character_, nrow(raw)))
tanggal_aktif <- as.Date(NA)
for (i in seq_len(nrow(raw))) {
  tanggal_baru <- parse_tanggal_excel(raw$tanggal_penanda[i])
  if (!is.na(tanggal_baru)) tanggal_aktif <- tanggal_baru
  tanggal_transaksi[i] <- tanggal_aktif
}
raw$tanggal_asumsi <- tanggal_transaksi

basket <- lapply(seq_len(nrow(raw)), function(i) {
  unique(c(petakan_item(raw$makanan[i], 'makanan'),
           petakan_item(raw$minuman[i], 'minuman')))
})
raw$jumlah_item <- lengths(basket)
raw$items_terpetakan <- vapply(basket, paste, collapse = ' | ', character(1))
raw$ada_menu_lauk <- vapply(seq_len(nrow(raw)), function(i) {
  t <- pecah_item(raw$makanan[i])
  any(!startsWith(t, 'nasi goreng'))
}, logical(1))
if (any(raw$jumlah_item == 0L)) {
  warning('Ada transaksi kosong; cek audit_transaksi_per_baris.csv sebelum interpretasi.')
}
cat('Transaksi yang memuat nasi: ', sum(vapply(basket, function(b) 'nasi' %in% b, logical(1))), '\n', sep = '')
write.csv(raw, file_hasil('01_audit_transaksi_per_baris.csv'),
          row.names = FALSE, fileEncoding = 'UTF-8')

pending <- kamus[kamus$status == 'perlu_konfirmasi', , drop = FALSE]
write.csv(pending, file_hasil('02_item_menunggu_konfirmasi.csv'),
          row.names = FALSE, fileEncoding = 'UTF-8')
if (nrow(pending)) {
  warning(nrow(pending), ' nama item belum dikonfirmasi. ',
          'Hasil MBA masih bersifat sementara; item pending diperlakukan terpisah.')
  if (!IZINKAN_PREVIEW_SEBELUM_KONFIRMASI) {
    stop('Konfirmasi item pending dalam kamus dahulu, lalu jalankan ulang.')
  }
}

# ---- 4. KONVERSI KE KELAS TRANSACTIONS -------------------------------
# Berbeda dari kating (yang berangkat dari data 0/1), Sheet2 diubah dari
# daftar item per baris langsung ke transaksi, lalu diekspor juga sebagai 0/1.
transaksi_semua <- as(basket, 'transactions')
mat_biner <- as(transaksi_semua, 'matrix') * 1L
output_biner <- data.frame(id_internal = raw$id_internal,
                           baris_excel = raw$baris_excel,
                           tanggal_asumsi = raw$tanggal_asumsi,
                           mat_biner, check.names = FALSE)
write.csv(output_biner, file_hasil('03_matriks_transaksi_biner.csv'),
          row.names = FALSE, fileEncoding = 'UTF-8')

transaksi_min2 <- transaksi_semua[size(transaksi_semua) >= 2L]
transaksi <- if (HAPUS_TRANSAKSI_SATU_ITEM_UTAMA) transaksi_min2 else transaksi_semua
if (length(transaksi) == 0L) stop('Tidak ada transaksi valid dalam skenario utama.')

# ---- 5. EKSPLORASI DATA & DISTRIBUSI JUMLAH ITEM ---------------------
cat('\nJumlah transaksi SEMUA:', length(transaksi_semua), '\n')
cat('Jumlah transaksi minimal 2 item:', length(transaksi_min2), '\n')
cat('Jumlah transaksi utama:', length(transaksi), '\n')
jumlah_item_aktif <- sum(itemFrequency(transaksi, type = 'absolute') > 0)
cat('Jumlah item unik utama (frekuensi > 0):', jumlah_item_aktif, '\n')
cat('Ringkasan transaksi utama:\n')
print(summary(transaksi))

ukuran <- size(transaksi)
distribusi_item <- as.data.frame(table(jumlah_item = ukuran),
                                stringsAsFactors = FALSE)
names(distribusi_item)[2] <- 'jumlah_transaksi'
write.csv(distribusi_item, file_hasil('04_distribusi_jumlah_item.csv'),
          row.names = FALSE)

kepadatan <- if (jumlah_item_aktif > 0) {
  sum(ukuran) / (length(transaksi) * jumlah_item_aktif)
} else NA_real_
ringkasan_data <- data.frame(
  metrik = c('baris_input', 'transaksi_kosong', 'transaksi_satu_item',
             'transaksi_dua_atau_lebih', 'transaksi_skenario_utama',
             'jenis_item_skenario_utama', 'kepadatan_matriks_utama',
             'rata_rata_item', 'median_item', 'nama_belum_dikonfirmasi'),
  nilai = c(nrow(raw), sum(size(transaksi_semua) == 0L),
            sum(size(transaksi_semua) == 1L), length(transaksi_min2),
            length(transaksi), jumlah_item_aktif, kepadatan,
            mean(ukuran), median(ukuran), nrow(pending))
)
write.csv(ringkasan_data, file_hasil('05_karakteristik_umum.csv'),
          row.names = FALSE)

# ---- 6. FREKUENSI ITEM (SAMA DENGAN KATING) ---------------------------
item_freq <- sort(itemFrequency(transaksi, type = 'relative'),
                  decreasing = TRUE)
frekuensi_item <- data.frame(
  item = names(item_freq),
  frekuensi_relatif = as.numeric(item_freq),
  jumlah_transaksi = as.integer(round(as.numeric(item_freq) * length(transaksi))),
  row.names = NULL
)
write.csv(frekuensi_item, file_hasil('06_frekuensi_item.csv'),
          row.names = FALSE)
print(utils::head(frekuensi_item, 15))

png(file_hasil('07_plot_top10_item.png'), width = 1500, height = 950, res = 160)
tryCatch({
  itemFrequencyPlot(transaksi, topN = min(10L, length(itemLabels(transaksi))),
                    type = 'relative', col = 'aquamarine', horiz = TRUE,
                    main = '10 Item Paling Sering Dibeli')
}, finally = dev.off())

# ---- 7. DETEKSI ITEM DOMINAN ----------------------------------------
item_wajib <- names(item_freq[item_freq > DOMINANT_ITEM_THRESHOLD])
cat('\nItem dominan di atas ', DOMINANT_ITEM_THRESHOLD * 100,
    '%: ', if (length(item_wajib)) paste(item_wajib, collapse = ', ') else '(tidak ada)',
    '\n', sep = '')

# ---- 8. ALGORITMA APRIORI (PARAMETER KATING) -------------------------
jalankan_apriori <- function(x, support = MIN_SUPPORT,
                             confidence = MIN_CONFIDENCE) {
  if (length(x) == 0L || length(itemLabels(x)) < 2L) return(NULL)
  suppressWarnings(arules::apriori(
    x, parameter = list(supp = support, conf = confidence,
                        minlen = MIN_RULE_LENGTH, target = 'rules'),
    control = list(verbose = FALSE)
  ))
}
rules <- jalankan_apriori(transaksi)
if (is.null(rules)) stop('Tidak dapat membangun aturan dari transaksi.')
rules <- sort(rules, by = 'lift', decreasing = TRUE)
cat('Jumlah seluruh aturan Apriori:', length(rules), '\n')
if (length(rules)) inspect(utils::head(rules, 10))

# ---- 9. SELEKSI ATURAN ASOSIASI -----------------------------------
# Pertahankan seluruh aturan pada tabel awal, kemudian lakukan seleksi
# untuk menyoroti aturan yang layak dibahas dalam rekomendasi.

# Apriori menghasilkan satu item RHS per rule. labels(rhs(...)) -> {nama item}.
rhs_nama <- gsub('^\\{|\\}$', '', labels(rhs(rules)))
rules_nobias <- if (length(item_wajib)) {
  rules[!(rhs_nama %in% item_wajib)]
} else rules

# Pemeriksaan keterkaitan yang perlu ditinjau saat interpretasi.
item_menu_lauk <- unique(unlist(lapply(seq_len(nrow(raw)), function(i) {
  if (!isTRUE(raw$ada_menu_lauk[i])) return(character())
  setdiff(petakan_item(raw$makanan[i], 'makanan'), 'nasi')
}), use.names = FALSE))
pecah_label_rules <- function(x) {
  s <- sub('^\\{', '', sub('\\}$', '', x))
  if (!nzchar(s)) return(character(0))
  trimws(strsplit(s, ',', fixed = TRUE)[[1L]])
}
perlu_tinjauan <- function(x) {
  if (!length(x)) return(logical(0))
  mapply(function(a, b) {
    union_item <- unique(c(pecah_label_rules(a), pecah_label_rules(b)))
    ('nasi' %in% union_item) && any(item_menu_lauk %in% union_item)
  }, labels(lhs(x)), labels(rhs(x)), USE.NAMES = FALSE)
}

rules_terseleksi <- if (length(rules_nobias)) {
  rules_nobias[!perlu_tinjauan(rules_nobias)]
} else rules_nobias

rules_strong <- if (length(rules_terseleksi)) {
  subset(rules_terseleksi, subset =
           confidence >= MIN_CONFIDENCE &
           lift >= STRONG_MIN_LIFT &
           coverage >= STRONG_MIN_COVERAGE)
} else rules_terseleksi
rules_strong <- sort(rules_strong, by = 'lift', decreasing = TRUE)

cat('Aturan tanpa RHS dominan: ', length(rules_nobias), '\n', sep = '')
cat('Aturan setelah pemeriksaan keterkaitan: ',
    length(rules_terseleksi), '\n', sep = '')
cat('Aturan kuat yang layak dibahas: ', length(rules_strong), '\n', sep = '')
if (length(rules_strong)) inspect(rules_strong)

# ---- 10. EXPORT RULES & STATISTIK KUALITAS --------------------------
ubah_rules_ke_tabel <- function(x, n_transaksi) {
  if (is.null(x) || length(x) == 0L) {
    return(data.frame(lhs = character(), rhs = character(), support = numeric(),
                      confidence = numeric(), coverage = numeric(), lift = numeric(),
                      count = integer(), perlu_tinjauan = logical(),
                      stringsAsFactors = FALSE))
  }
  q <- quality(x)
  cv <- if ('coverage' %in% names(q)) q$coverage else rep(NA_real_, nrow(q))
  n <- if ('count' %in% names(q)) q$count else round(q$support * n_transaksi)
  data.frame(lhs = labels(lhs(x)), rhs = labels(rhs(x)),
             support = q$support, confidence = q$confidence,
             coverage = cv, lift = q$lift, count = as.integer(n),
             perlu_tinjauan = perlu_tinjauan(x),
             stringsAsFactors = FALSE)
}
write.csv(ubah_rules_ke_tabel(rules, length(transaksi)),
          file_hasil('08_semua_aturan.csv'), row.names = FALSE)
write.csv(utils::head(ubah_rules_ke_tabel(rules, length(transaksi)), 10),
          file_hasil('09_top10_lift_mentah.csv'), row.names = FALSE)
write.csv(ubah_rules_ke_tabel(rules_nobias, length(transaksi)),
          file_hasil('10_aturan_tanpa_RHS_dominan.csv'), row.names = FALSE)
write.csv(ubah_rules_ke_tabel(rules_strong, length(transaksi)),
          file_hasil('11_aturan_kuat.csv'), row.names = FALSE)
write.csv(ubah_rules_ke_tabel(rules_terseleksi, length(transaksi)),
          file_hasil('11a_aturan_terseleksi.csv'), row.names = FALSE)

if (length(rules)) {
  q <- quality(rules)
  metrik <- intersect(c('support', 'confidence', 'coverage', 'lift', 'count'), names(q))
  statistik <- do.call(rbind, lapply(metrik, function(m) {
    data.frame(ukuran = m, minimum = min(q[[m]], na.rm = TRUE),
               rata_rata = mean(q[[m]], na.rm = TRUE),
               maksimum = max(q[[m]], na.rm = TRUE))
  }))
  write.csv(statistik, file_hasil('12_statistik_kualitas_aturan.csv'),
            row.names = FALSE)
}

# ---- 11. VISUALISASI RULES ------------------------------------------
if (ada_arulesviz && length(rules)) {
  rules_pilihan <- if (length(rules_strong)) rules_strong else
                   if (length(rules_terseleksi)) rules_terseleksi else rules
  if (length(rules_pilihan)) {
    tryCatch({
      png(file_hasil('13_grafik_aturan_terbaik.png'),
          width = 1550, height = 1050, res = 160)
      graphics::plot(utils::head(rules_pilihan, 10), method = 'graph')
      dev.off()
    }, error = function(e) {
      message('Grafik graph gagal (grafik lain tetap tersedia): ', conditionMessage(e))
      if (dev.cur() > 1L) dev.off()
    })
  }
  tryCatch({
    png(file_hasil('14_scatter_rules.png'),
        width = 1400, height = 1050, res = 160)
    graphics::plot(rules, method = 'scatterplot',
                    measure = c('support', 'confidence'), shading = 'lift')
    dev.off()
  }, error = function(e) {
    message('Scatterplot gagal: ', conditionMessage(e))
    if (dev.cur() > 1L) dev.off()
  })
}

# ---- 12. MODIFIKASI: SENSITIVITAS & TRANSAKSI SATU ITEM --------------
# Perubahan support, confidence, dan pilihan denominator bisa mengubah aturan.
kombinasi <- expand.grid(
  skenario = c('semua_transaksi', 'minimal_dua_item'),
  support_min = c(0.01, 0.02, 0.03, 0.05),
  confidence_min = c(0.50, 0.60, 0.70),
  stringsAsFactors = FALSE
)
for (i in seq_len(nrow(kombinasi))) {
  tx <- if (kombinasi$skenario[i] == 'semua_transaksi') {
    transaksi_semua
  } else transaksi_min2
  z <- jalankan_apriori(tx, kombinasi$support_min[i],
                        kombinasi$confidence_min[i])
  kombinasi$jumlah_transaksi[i] <- length(tx)
  kombinasi$jumlah_rules[i] <- if (is.null(z)) 0L else length(z)
  kombinasi$rules_lift_diatas_1[i] <- if (is.null(z) || length(z) == 0L) {
    0L
  } else sum(quality(z)$lift > 1)
  kombinasi$rules_terseleksi[i] <- if (is.null(z) || length(z) == 0L) {
    0L
  } else sum(!perlu_tinjauan(z))
  kombinasi$rules_terseleksi_lift_15[i] <- if (is.null(z) || length(z) == 0L) {
    0L
  } else sum(!perlu_tinjauan(z) & quality(z)$lift >= 1.5)
}
write.csv(kombinasi, file_hasil('15_sensitivitas_parameter.csv'),
          row.names = FALSE)

cat('\nSELESAI - output pada folder: ', normalizePath(FOLDER_OUTPUT), '\n',
    'Status: ', if (nrow(pending)) paste0('DRAFT (', nrow(pending), ' label perlu review)') else
                              'Seluruh label sudah terkonfirmasi', '\n', sep = '')

# ---- 13. VALIDASI UNIT ANALISIS -------------------------------------
stopifnot(length(transaksi_semua) == nrow(raw))
stopifnot(!anyDuplicated(raw$id_internal))
stopifnot(all(raw$jumlah_item >= 1L))
cat('Validasi: 1 baris input = 1 ID transaksi; tidak ada baris terhapus karena nomor kosong.\n')
cat('Validasi dan analisis selesai.\n')
