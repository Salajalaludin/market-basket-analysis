# Market Basket Analysis Kantin (Apriori)

Analisis 1-7 memakai data yang sama (`MBA_Data_Bersih.xlsx`, 184 transaksi, 2 sampai 13 November 2026). Analisis 8 memakai data yang diperbarui (`Analisis_8/data/MBA_Riset_Pemasaran.xlsx`, 224 transaksi, 2 sampai 18 November 2026), yaitu 184 transaksi yang sama ditambah 40 transaksi tanggal 16-18 November. Alur kode mengikuti Lampiran 3 laporan kating (K1, 2025): frekuensi item, Apriori (support 3%, confidence 60%), eliminasi RHS item wajib (> 60%), lalu seleksi aturan kuat (lift ≥ 1,5 dan coverage ≥ 0,05). Ada tiga tambahan: uji eksak Fisher, uji sensitivitas parameter, dan pembanding hasil pada seluruh transaksi.

## Isi folder

| Folder | Perlakuan item | Isi |
|---|---|---|
| `Analisis_1_Nasi_Paket/` | Nasi menyatu dengan lauk (paket), minuman per varian | skrip R, `hasil/`, BAB IV-V (.docx & .pdf) |
| `Analisis_2_Nasi_Terpisah/` | Nasi jadi item sendiri ("Nasi putih"), seperti data kating | sama |
| `Analisis_3_Kelompok_Minuman/` | Nasi paket, 18 varian minuman jadi 4 kelompok | sama |
| `Analisis_4_Kelompok_Makanan_Minuman/` | 13 menu jadi 5 kelompok olahan, minuman 4 kelompok | sama |
| `Analisis_5_Minuman_Manis_vs_Air_Putih/` | Nasi paket, minuman jadi 2 kelompok rasa (air putih vs manis); prosedur kating tanpa perubahan | sama |
| `Analisis_6_Seluruh_Transaksi/` | Sama dengan Analisis 5, tetapi transaksi 1 item **tidak** dibuang (184 transaksi) | sama |
| `Analisis_7_Kelompok_Olahan_vs_Rasa_Minuman/` | 5 kelompok olahan makanan × 2 kelompok rasa minuman, ditambah uji Fisher seluruh pasangan item | sama, ditambah `13_uji_fisher_seluruh_pasangan.csv` |
| `Analisis_8/` | Data diperbarui (224 transaksi). Tingkat menu, nasi jadi item sendiri lalu dieliminasi sebagai item wajib di RHS **dan** LHS; transaksi 1 item tidak dibuang (seperti Analisis 6) | sama, ditambah `13_uji_fisher_seluruh_pasangan.csv`, `14_perbandingan_periode.csv`, subfolder `data/` (data mentah & bersih), dan `arsip_versi_lama/` (skrip, BAB III-IV, dan gambar versi sebelumnya, confidence 55%) |

Isi folder `hasil/` sama di setiap analisis: `01-03` eksplorasi data, `04` komposisi keranjang, `05` frekuensi item, `06-09` aturan (semua, statistik, tanpa bias, kuat), `10` uji Fisher, `11` sensitivitas, `12` aturan pada cakupan data pembanding (seluruh 184 transaksi; khusus Analisis 6 dan 8 sebaliknya, yaitu data minimal 2 item), ditambah `Gambar_1` (barplot) dan `Gambar_2` (graf aturan).

## Cara menjalankan

Buka skrip `.R` di RStudio, pilih **Session > Set Working Directory > To Source File Location**, lalu klik **Source**. Paket yang dibutuhkan: `readxl`, `arules`, `arulesViz`.

## Ringkasan hasil (support 3%, confidence 60%)

| Analisis | Transaksi dianalisis | Item | Aturan | Aturan kuat | Temuan utama |
|---|---|---|---|---|---|
| 1. Nasi paket | 129 | 31 | 2 | 0 | Rica rica dan bumbu hitam → es air putih (lift 1,43 dan 1,34, tidak signifikan). 28,3% transaksi berisi makanan tanpa minuman. |
| 2. Nasi terpisah | 175 | 32 | 16 | 0 | Semua aturan berbentuk {lauk} → {nasi} (lift 1,04). Setelah RHS nasi dibuang, tidak ada aturan tersisa. |
| 3. Kelompok minuman | 129 | 17 | 4 | 0 | Tongseng → air putih (lift 1,29). Pada seluruh 184 transaksi aturan ini **kuat dan signifikan** (lift 1,65, p = 0,010). |
| 4. Kelompok makanan & minuman | 128 | 9 | 4 | 0 | Jasjus → lauk goreng sambal (lift 1,45, p = 0,077). Pola tongseng melemah karena digabung dengan soto. |
| 5. Minuman manis vs air putih | 129 | 15 | 6 | **1** | **Kulit ayam → minuman manis** (conf 0,875; lift 2,02; coverage 0,062; p = 0,012, Holm 0,072). |
| 6. Seluruh transaksi | 184 | 15 | 2 | **1** | **Tongseng → air putih** (conf 0,636; lift 1,65; coverage 0,120; p = 0,010, Holm 0,013). Kulit ayam → manis juga signifikan (Holm 0,013), tetapi coverage 0,049. |
| 7. Olahan × rasa minuman | 128 | 7 | 3 | 0 | Aturan Apriori: lauk bumbu/berkuah/bakar → air putih (lemah). Uji seluruh pasangan: **lauk goreng sambal ↔ minuman manis** (33 transaksi, lift 1,28, p = 0,008, Holm 0,075). |
| 8. Data diperbarui (224) | 224 | 32 | 23 (3 tanpa nasi) | **3** | **Tea jus gula batu → ayam penyet** (28 trx, lift 1,77), **ayam bakar → es air putih** (21 trx, lift 2,01), **air putih → tongseng** (11 trx, lift 5,00), semuanya signifikan setelah Holm. Pecel ayam ↔ kulit ayam signifikan (lift 7,75) tetapi conf 0,588. **Catatan:** keempat pola hanya muncul karena 40 transaksi 16-18 Nov (conf 1,000); pada 184 transaksi lama tidak ada aturan (subbab 4.6.4). |

### Semua asosiasi yang signifikan (uji Fisher, p < 0,05)

| Asosiasi | Transaksi | Lift | p | Setelah Holm | Aturan kuat? | Folder |
|---|---|---|---|---|---|---|
| Tongseng ↔ air putih | 14 | 1,65 | 0,010 | 0,013 (signifikan) | Ya | Analisis 6 |
| Kulit ayam ↔ minuman manis | 7 | 2,02–2,35 | 0,007–0,012 | 0,013 (A6) / 0,072 (A5) | Ya di A5, coverage kurang di A6 | Analisis 5 & 6 |
| Lauk goreng sambal ↔ minuman manis | 33 | 1,28 | 0,008 | 0,075 | Tidak (conf 0,56, lift 1,28) | Analisis 7 |

Asosiasi lain yang signifikan hanya didukung 3–4 transaksi, atau bersifat mekanis (lauk ↔ nasi), sehingga tidak dilaporkan sebagai temuan.

Analisis 2 berfungsi sebagai pembanding yang menjelaskan alasan nasi digabung ke dalam paket. Aturan kuat hanya muncul pada Analisis 5 dan 6, dan keduanya memakai kriteria kating tanpa diturunkan. Analisis 6 paling kuat secara statistik karena kedua aturannya tetap signifikan setelah koreksi Holm.

Cara aturan kuat ditemukan: 6 skema pengelompokan × 2 cakupan data disisir dengan support 3%. Hanya dua aturan yang memenuhi kriteria kuat kating dan signifikan pada uji Fisher. Karena hasil ini didapat dari penelusuran beberapa skema, dokumen menyarankan konfirmasi dengan data periode berikutnya. Catatan tambahan: 6 dari 9 transaksi kulit ayam terjadi pada 9 November 2026.

Catatan metodologi: membuang transaksi 1 item (langkah kating) ikut membuang pembeli "makanan tanpa minuman". Akibatnya peluang dasar minuman naik dan lift aturan makanan → minuman turun. Penjelasan lengkapnya ada di subbab 4.6 setiap dokumen.
