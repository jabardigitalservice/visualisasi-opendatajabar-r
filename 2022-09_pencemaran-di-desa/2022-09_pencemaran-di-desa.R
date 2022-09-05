# load library ----
library(tidyverse)
library(janitor)
library(gt)
library(gtExtras)

# load dataset ----

pencemaran.tanah_raw <- 
  read_csv(
    "2022-09_pencemaran-di-desa/data_raw/dpmdes-idm_sts_kejadian_pencemaran_tanah__des_kel_data.csv", 
    col_types = cols(
      kode_provinsi = col_skip(),
      nama_provinsi = col_skip(), 
      bps_kode_kabupaten_kota = col_skip(),
      bps_nama_kecamatan = col_skip(),
      bps_nama_desa_kelurahan = col_skip(),
      kemendagri_kode_kecamatan = col_skip(),
      kemendagri_kode_desa_kelurahan = col_skip(),
      id = col_skip()
    )
  ) %>% 
  clean_names() %>% 
  mutate(
    across(where(is.character), str_to_title),
    jenis_cemar= "Pencemaran Tanah"
  ) %>% 
  rename(
    kab_kot = bps_nama_kabupaten_kota,
    status_cemar = status_pencemaran_tanah
  )

pencemaran.air_raw <- 
  read_csv(
    "2022-09_pencemaran-di-desa/data_raw/dpmdes-idm_sts_kejadian_pencemaran_air__des_kel_data.csv", 
    col_types = cols(
      kode_provinsi = col_skip(),
      nama_provinsi = col_skip(), 
      bps_kode_kabupaten_kota = col_skip(),
      bps_nama_kecamatan = col_skip(),
      bps_nama_desa_kelurahan = col_skip(),
      kemendagri_kode_kecamatan = col_skip(),
      kemendagri_kode_desa_kelurahan = col_skip(),
      id = col_skip()
    )
  ) %>% 
  clean_names() %>% 
  mutate(
    across(where(is.character), str_to_title),
    jenis_cemar = "Pencemaran Air"
  ) %>% 
  rename(
    kab_kot = bps_nama_kabupaten_kota,
    status_cemar = status_pencemaran_air
  )

pencemaran.sungai_raw <- 
  read_csv(
    "2022-09_pencemaran-di-desa/data_raw/dpmdes-idm_kbrdn_sungai_terkena_pembuangan_limbah__des_kel_data.csv", 
    col_types = cols(
      kode_provinsi = col_skip(),
      nama_provinsi = col_skip(), 
      bps_kode_kabupaten_kota = col_skip(),
      bps_nama_kecamatan = col_skip(),
      bps_nama_desa_kelurahan = col_skip(),
      kemendagri_kode_kecamatan = col_skip(),
      kemendagri_kode_desa_kelurahan = col_skip(),
      id = col_skip()
    )
  ) %>% 
  clean_names() %>% 
  mutate(
    across(where(is.character), str_to_title),
    jenis_cemar = "Pencemaran Sungai"
  ) %>% 
  rename(
    kab_kot = bps_nama_kabupaten_kota,
    status_cemar = status_sungai_tercemar_limbah
  )

# wrangle ----

## jumlah dan persentase desa tercemar tiap kab/kot tahun 2021
jml_cemar <- 
  rbind(
    pencemaran.air_raw,
    pencemaran.sungai_raw,
    pencemaran.tanah_raw
  ) %>% 
  dplyr::filter(
    status_cemar == "Ada",
    tahun == 2021
  ) %>% 
  group_by(jenis_cemar, kab_kot, tahun) %>% 
  count(status_cemar, name = "n_cemar") %>% 
  left_join(
    pencemaran.air_raw %>% 
      group_by(kab_kot,tahun) %>% 
      count(name = "n_desa")
  ) %>%
  # count percentage desa terkena pencemaran  per tahun per kabkot
  mutate(
    pct_cemar = round((n_cemar*100/n_desa),2)
  ) %>% 
  group_by(
    jenis_cemar
  ) %>% 
  arrange(desc(pct_cemar), .by_group = T) %>% 
  group_by(jenis_cemar) %>% 
  slice(1:5)


## perkembangan jumlah desa tercemar dari tahun ke tahun tiap jenis pencemaran

agg_cemar <- 
  rbind(
    pencemaran.air_raw,
    pencemaran.sungai_raw,
    pencemaran.tanah_raw
  ) %>% 
  dplyr::filter(
    status_cemar == "Ada",
    tahun == 2021
  ) %>% 
  group_by(jenis_cemar) %>% 
  count(name = "sum_cemar") %>% 
  left_join(
    rbind(
      pencemaran.air_raw,
      pencemaran.sungai_raw,
      pencemaran.tanah_raw
    ) %>% 
      dplyr::filter(status_cemar == "Ada") %>% 
      group_by(jenis_cemar,tahun) %>% 
      count() %>% 
      group_by(jenis_cemar) %>% 
      summarise(trend_cemar = list(n), .groups = "drop")
  )

# visualization ----
## terbagi menjadi komponen tabel dan tabel

# > bar chart sebagai komponen tabel ----
## function
fun_bar <- 
  function(data){
    rank_bar <- 
      data %>% 
      mutate(label_bar = paste(format(round(pct_cemar,2),nsmall=2),"%")) %>%
      ggplot(aes(x= fct_reorder(kab_kot, pct_cemar, .desc=FALSE), y= pct_cemar))+
      geom_bar(stat = "identity", fill = "#1FB767", width = 0.6)+
      coord_flip()+
      geom_text(aes(y = pct_cemar+1.1, label = label_bar), size = 6)+
      labs(x="",y="")+
      theme_minimal()+
      theme(
        panel.grid = element_blank(),
        axis.text.y = element_text(hjust = 0,vjust = 0, size = 18),
        axis.text.x = element_blank()
      )
    return(rank_bar)
  }

## plot

plot_bar <- 
  jml_cemar %>% 
  group_by(jenis_cemar) %>% 
  nest() %>% 
  mutate(gg=purrr::map(data, fun_bar)) %>%
  select(jenis_cemar = jenis_cemar, gg)

# > tabel ----

tabel_cemar  <- 
  # base table
  agg_cemar %>% 
  mutate(top_five = NA) %>% 
  ungroup() %>% 
  select(1,2,4,3) %>% 
  gt() %>% 
  gt_theme_538() %>% 
  fmt_symbol_first(
    sum_cemar,
    decimals = 0,
    suffix = " desa"
  ) %>% 
  # Add bar plot 
  gt::text_transform(
    locations = cells_body(columns=top_five),
    fn = function(x){
      purrr::map(
        plot_bar$gg, gt::ggplot_image, 
        height = px(180), aspect_ratio = 2.2
      )}
    ) %>%
  # add sparkline
  gtExtras::gt_plt_sparkline(
    trend_cemar,
    palette = c("#1FB767","#FFB900", "#FFB900","#1E88E5","lightgrey"),
    label = FALSE,
    same_limit = TRUE
  ) %>% 
  # rename columns
  cols_label(
    jenis_cemar = md("Jenis Pencemaran"),
    sum_cemar = md("Jumlah Desa Tercemar tahun 2021"),
    top_five = md("5 Kabupaten dengan Persentase Pencemaran Tertinggi"),
    trend_cemar = md("Tren Jumlah Desa Tercemar 2019-2021")
  ) %>% 
  # Align column 
  cols_align(columns = c(1,2),
             align = "left") %>%
  # Header and source note
  tab_header(
    title="Waspadalah! Air, Sungai, dan Tanah di Desa Jawa Barat sudah Tercemar",
    subtitle=md("Pencemaran dapat diartikan sebagai sebagai masuknya atau dimasukkannya makhluk hidup, zat energi, dan atau komponen lain ke dalam lingkungan, sehingga dapat mengganggu ekosistem yang ada di dalamnnya. Hasil survei DPM Desa menunjukkan bahwa desa-desa di Jawa Barat sudah banyak yang tercemar. Tercatat hingga **860 desa** memiliki sungai yang sudah dipenuhi oleh polutan. Di lain sisi, **jumlah desa tercemar menurun** selama 3 tahun terakhir.  **Kabupaten Bekasi, Kabupaten Karawang, dan Kabupaten Bandung** menjadi daerah penyumbang desa tercemar terbanyak untuk ketiga kategori pencemaran.")) %>%
  tab_source_note(source_note = gt::html("Sumber: Open Data Jabar")) %>%
  # Adjust sub-title font
  tab_style(
    style = list(
      cell_text(
        weight="lighter"
      )
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(font=google_font(name = "Roboto Condensed"),
                align = "right")),
    locations = cells_source_notes())

tabel_cemar

gtsave_extra(
  data = tabel_cemar,
  filename = "2022-09_pencemaran-di-desa/2022-09_pencemaran-di-desa.png"
)  
