bangke_lengkap <- fread("data/K0 Faskes - 9 Januari 2024 - GABUNG.csv")
bangke_lengkap <- bangke_lengkap %>%
  mutate(status_pelatihan = case_when(
    str_detect(Pelatihan, "IUD|Vasektomi|Tubektomi") ~ "Sudah",
    TRUE ~ "Belum"
  ))

bangke_lengkap$`Nama Faskes` <- factor(bangke_lengkap$`Nama Faskes`,
                                       levels = c(unique(bangke_lengkap$`Nama Faskes`), "Total"))

bangke <-
  bangke_lengkap %>%
  group_by(Kabupaten, Kecamatan, `Kelurahan/Desa`, 
           `Nama Faskes`, status_pelatihan, BULAN) %>%
  count() %>%
  arrange(Kabupaten, Kecamatan, `Kelurahan/Desa`) %>%
  spread(status_pelatihan, n) %>%
  ungroup()

tabel_tm_kb <- bangke %>%
  select(Kecamatan, `Kelurahan/Desa`, `Nama Faskes`, Belum, Sudah, BULAN) %>%
  filter(Kecamatan == "TOPOYO", `Kelurahan/Desa` == "TOPOYO",
         BULAN == "DESEMBER")

tabel_tm_kb <- tabel_tm_kb %>%
  mutate(Jumlah_Bidan = rowSums(select(., Sudah, Belum), na.rm = TRUE)) %>%
  select(`Nama Faskes`, Belum, Sudah, Jumlah_Bidan) %>%
  bind_rows(summarise(., across(where(is.numeric), ~sum(., na.rm = TRUE)),
                      across(where(is.factor), ~'Total')))
reactable(tabel_tm_kb)
nama_faskes_subset <- unique(tabel_tm_kb$`Nama Faskes`)[3]

detail_tm_kb <- bangke_lengkap %>%
  select(Kecamatan, `Kelurahan/Desa`, `Nama Faskes`, `Nama Bidan`, status_pelatihan, BULAN) %>%
  filter(Kecamatan == "TOBADAK", `Kelurahan/Desa` == "MAHAHE",
         BULAN == "DESEMBER")

for (i in length(unique(detail_tm_kb$`Nama Faskes`)) ) {
  nama_faskes_subsets <- unique(detail_tm_kb$`Nama Faskes`)[i]
  subset_df <- detail_tm_kb[detail_tm_kb$`Nama Faskes` == nama_faskes_subsets, ]
  print(subset_df)
}

reactable(tabel_tm_kb, details = function(index) {
  nama_faskes_subsets <- unique(detail_tm_kb$`Nama Faskes`)[index]
  nama_bidan_subsets <- detail_tm_kb[detail_tm_kb$`Nama Faskes` == nama_faskes_subsets, ]
  colnames(nama_bidan_subsets)[5] <- c("Status Pelatihan")
  htmltools::div(style = "padding: 1rem",
                 reactable(nama_bidan_subsets[,3:5], outlined = TRUE,
                           columns = list(
                             "Status Pelatihan" = colDef(cell = function(value) {
                               # Render as an X mark or check mark
                               if (value == "Belum") "\u274c Belum" else "\u2714\ufe0f Sudah"
                             })
                           ))
  )
})
