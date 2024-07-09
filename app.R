library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(waiter)
library(leaflet)
library(leaflegend)
library(shinydashboard)
library(plotly)
library(tidyr)
library(reactablefmtr)
library(bs4Dash)
library(bsicons)
library(data.table)
library(htmltools)
library(stringr)
library(lazyeval)
library(quarto)
library(ggplot2)
library(ggrepel)
library(readr)

link_shiny <- tags$a(
  shiny::icon("instagram"), "Instagram BKKBN Sulbar",
  href = "https://www.instagram.com/bkkbnsulbar/",
  target = "_blank"
)
link_posit <- tags$a(
  shiny::icon("globe"), "Website BKKBN Sulbar",
  href = "https://sulbar-new.bkkbn.go.id/",
  target = "_blank"
)

ui <- page_navbar(
  title = "Profil Desa",
  theme = bs_theme(
    version = 5,
    bootswatch = "spacelab"
  ),
  nav_panel(
    title = "Dashboard", 
    page_fluid(
      autoWaiter(),
      tags$div(
        style = "display: flex; align-items: center; justify-content: center;",
        tags$img(src = "https://bkkbnsulbar.id/wp-content/uploads/2022/12/cropped-logobkkbnsulbar.png", height = "100px"),
        tags$h3("Profil Desa", style = "margin-left: 10px;")
      ),
      br(),
      layout_column_wrap(
        uiOutput("input_rekap_prov"),
        uiOutput("input_rekap_kab"),
        uiOutput("input_rekap_kec"),
        uiOutput("input_rekap_desa"),
        uiOutput("pilih_bulan_rekap"),
        #uiOutput("pilih_bulan"),
      ),
      layout_column_wrap(
        fixed_width = TRUE,
        uiOutput("cari_rekapan", fill = T),
        #uiOutput("buat_pdf", fill = T),
      ),
      h5(textOutput("tes_input_rekap"), style="text-align: center;"),
      navset_card_pill(
        nav_panel(
          "Profil",
          layout_column_wrap(
            card(full_screen = T,
                 leafletOutput("peta_titik_rekap")
            ),
            card(
              layout_column_wrap(
                uiOutput("card_profil_poktan_rekap"),
                uiOutput("card_profil_sd_rekap")
              )
            )
          ),
          layout_column_wrap(
            card(
              plotlyOutput("grafik_piramida_rekap"), 
              full_screen = T
            ),
            card(
              reactableOutput("tabel_piramida_rekap")
            )
          )
        ),
        nav_panel(
          title = "Keluarga Berencana", 
          layout_column_wrap(
            uiOutput("vb_unmet_need_rekap"),
            uiOutput("vb_sdm_kb_rekap"),
            uiOutput("vb_tp_kb_rekap"),
            uiOutput("vb_mkjp_rekap")
          ), # LAYOUT WRAP
          layout_column_wrap(
            plotlyOutput("bar_kontrasepsi_rekap"),
            plotlyOutput("donut_terlatih_tidak_rekap")
          ), #LAYOUT
          fluidRow(
            column(8,
              plotlyOutput("line_mix_kontrasepsi_rekap")
            ),
            column(4,
              fluidRow(
                #uiOutput("vb_pus_rekap")
              ),
              fluidRow(
                #uiOutput("vb_pa_pb_rekap")
              )
            ) #card
          ),
          layout_column_wrap(
            reactableOutput("tabel_tp_kb_rekap")
          )
        ),
        nav_panel(
          title = "Stunting", 
          #textOutput("cek_kec"),
          fluidRow(
            shiny::column(3,
                          uiOutput("profil_stunting_rekap"),
                          card(plotlyOutput("bar_faktor_resiko_rekap")),
                          uiOutput("jumlah_posyandu")
                          
            ),
            shiny::column(9,
                          card(
                            card_header(
                              "Tim Pendamping Keluarga"
                            ),
                            card_body(
                              p("Berdasarkan Data Aplikasi Elsimil", style = "font-size:10px;"),
                              uiOutput("jumlah_tpk_rekap"),
                              layout_column_wrap(
                                card(
                                  fluidRow(
                                    column(12,
                                           plotlyOutput("pie_catin_rekap", height = "250px")
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           uiOutput("legend_pie_catin_rekap")
                                    )
                                  )
                                ),
                                card(
                                  fluidRow(
                                    column(12,
                                           plotlyOutput("pie_bumil_rekap", height = "250px")
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           uiOutput("legend_pie_bumil_rekap")
                                    )
                                  )
                                )
                              ),
                              layout_column_wrap(
                                card(
                                  fluidRow(
                                    column(12,
                                           plotlyOutput("pie_pascasalin_rekap", height = "250px")
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           uiOutput("legend_pie_pascasalin_rekap")
                                    )
                                  )
                                ),
                                card(
                                  fluidRow(
                                    column(
                                      12,
                                      plotlyOutput("pie_baduta_rekap", height = "250px")
                                    )
                                  ),
                                  fluidRow(
                                    column(
                                      12,
                                      uiOutput("legend_pie_baduta_rekap")
                                    )
                                  )
                                )
                              )
                            )
                          )
            )
          ),
          layout_column_wrap(
            navset_card_underline(title = "Penimbangan Posyandu",
                                  nav_panel("Jumlah", plotlyOutput("posyandu_line_agregat")),
                                  nav_panel("Persentase", plotlyOutput("posyandu_line_persentase"))
            ),
            navset_card_underline(title = "Pendampingan TPK",
              nav_panel("Catin", plotlyOutput("catin_line_rekap")),
              nav_panel("Bumil", plotlyOutput("bumil_line_rekap")),
              nav_panel("Pascasalin", plotlyOutput("pascasalin_line_rekap")),
              nav_panel("Baduta", plotlyOutput("baduta_line_rekap")),
            )
          )
        ) #nav panel
      )
    ) #page fluid rekap
  ), #navpanel rekap wilayah
  nav_panel(
    "Indikator RAN PASTI",
    "On proggress"
  ),
  nav_panel(
    "Tabel Data",
    fluidRow(
      column(4,
        uiOutput("pilih_daftar_data"),
      )
    ),
    fluidRow(  
      column(4,
        downloadButton("downloadData", "Download")
      )
    ),
    fluidRow(
      column(12,
        reactableOutput("tabel_data")
      )
    )
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_shiny),
    nav_item(link_posit)
  )
)

server <- function(input, output, session) {
  # # daftar_bulan <- c("JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI", "JULI",
  # #                   "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER")
  # 
  daftar_bulan <- c("JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI")

  observeEvent(input$cari_desa, {
    output$download_btn <- downloadHandler(
      filename = function() {
        paste(input$desa_kel, "-", input$kec, "-", Sys.Date(), ".pdf", sep = "")
      },
      
      content = function(file) {
        withProgress(message = 'Sedang memproses',
                     detail = 'Tunggu ya', value = 0, {
                       incProgress(0.3)
                       quarto::quarto_render(
                         input = "report_profil_desa.qmd", #harus sama
                         output_format = "pdf",
                         execute_params = list(
                           kab_text = input$kab, 
                           kec_text = input$kec, 
                           desa_text = input$desa_kel,
                           bulan = input$bulan
                         )
                       )
                       for (i in 4:7) {
                         incProgress(4/7)
                         sum(runif(10000000,0,1))
                       }
                       # copy the quarto generated file to `file` argument.
                       generated_file_name <- paste("report_profil_desa", "pdf", #harus sama
                                                    sep = ".")
                       incProgress(0.8)
                       file.copy(generated_file_name, file)
                       incProgress(1)
                     }) #progress
      } #contetn
    )
  })
  
  output$buat_pdf <- renderUI({
    if (!is.null(action_nama_desa()) | action_nama_desa() != "") {
      downloadBttn(
        outputId = "download_btn",
        style = "jelly",
        color = "primary", size = "sm"
      )
    } else {
      # Jika checkbox tidak diaktifkan, tidak ada widget yang ditampilkan
      NULL
    }
  })
  
  judul_halaman <- eventReactive(input$cari_desa, {
    if(input$kab == "SEMUA KABUPATEN"){
      judul = "PROVINSI SULAWESI BARAT"
    } else if(input$kec == "SEMUA KECAMATAN"){
      judul = paste("KABUPATEN", input$kab)
    } else if(input$desa_kel == "SEMUA DESA/KEL"){
      judul = paste("KECAMATAN", input$kec)
    } else{
      judul = paste("DESA/KELURAHAN", input$desa_kel)
    }
  })
  
  output$tes_input <- renderText({
    teks <- paste("PROFIL", judul_halaman())
  })
  
  #awal data
  #data_faskes <- fread("data/K0 Faskes - 9 Januari 2024 - GABUNG.csv")
  
  batas_sulbar <- readRDS("data/batas_sulbar.rds")
  
  titik_puskesmas <- fread("data/titik_puskesmas.csv")
  titik_sd <- fread("data/dikdasulbar.csv")
  titik_sd$Latitude <- as.numeric(titik_sd$Latitude)
  
  titik_smp <- fread("data/dikmen.csv")
  titik_smp$Latitude <- as.numeric(titik_smp$Latitude)
  
  data_desa <- fread("data/profil_poktan.csv")
  data_sumber_daya <- fread("data/profil_sumber_daya.csv")

  kelompok_umur_lk <- fread("data/PIRAMIDA PENDUDUK - Laki-laki.csv")
  kelompok_umur_pr <- fread("data/PIRAMIDA PENDUDUK - Perempuan.csv")
  
  data_kb <- fread("data/kb-dummy.csv")
  #bangke <- fread("data/petugas_kb_bangke.csv")
  
  bangke_lengkap <- fread("data/K0 Faskes - 9 Januari 2024 - GABUNG.csv", header = T)
  
  data_stunting <- fread("data/stunting-dummy.csv")
  
  data_posyandu <- fread("data/data_posyandu_dummy.csv")
  
  keberadaan_posyandu <- fread("data/keberadaan_posyandu.csv")
  
  nama_pkb <- fread("data/nama pkb.csv")
  nama_tpk <- fread("data/nama_tpk.csv")
  #akhir bagian data

  ###kb
  
  pilih_bulan_sebelumnya <- function(bulan) {
    daftar_bulan <- c("JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI", "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER")
    
    index <- match(toupper(bulan), daftar_bulan) # Mencari indeks bulan yang diberikan
    if (!is.na(index) && index > 1) { # Jika indeks ditemukan dan bukan bulan pertama
      return(daftar_bulan[index - 1]) # Mengembalikan bulan sebelumnya
    } else {
      return("JANUARI") # Jika bulan tidak ditemukan atau merupakan bulan pertama, kembalikan NA
    }
  }
  
  pilih_sampai_bulan <- function(bulan_yang_dipilih) {
    daftar_bulan <- c("JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI", "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER")
    
    # Temukan indeks bulan yang dipilih
    indeks_bulan <- which(daftar_bulan == toupper(bulan_yang_dipilih))
    
    # Validasi jika bulan tidak ditemukan
    if (length(indeks_bulan) == 0) {
      cat("Bulan tidak ditemukan.")
      return(NULL)
    }
    
    # Buat subset daftar bulan dari Januari hingga bulan yang dipilih
    daftar_bulan_subset <- daftar_bulan[1:indeks_bulan]
    
    return(daftar_bulan_subset)
  }
  

  
  bandingkan_bulan_rekap <- function(bulan_ini, bulan_lalu) {
    if (bulan_ini > bulan_lalu) {
      return("arrow-up-square-fill")
    } else if (bulan_ini < bulan_lalu) {
      return("arrow-down-square-fill")
    } else {
      return("arrow-left-right")
    }
  }
  
  #awal reka
  output$cari_rekapan <- renderUI({
    actionBttn(
      inputId = "cari_rekap",
      label = "Cari",
      style = "jelly", 
      color = "primary", size = "sm"
    )
  })
  
  output$input_rekap_prov <- renderUI({
    selectInput(
      inputId = "prov_rekap", selected = "SULAWESI BARAT", label = "Provinsi", choices = "SULAWESI BARAT"
    )
  })
  
  output$input_rekap_kab <- renderUI({
    selectInput(
      inputId = "kab_rekap", selected = "SEMUA KABUPATEN",
      label = "Pilih Kabupaten", 
      choices = c("SEMUA KABUPATEN", unique(data_desa$KABUPATEN))
    )
  })
  
  inputan_rekap_kab <- reactive({
    if(input$kab_rekap == "SEMUA KABUPATEN"){
      nama_kab = unique(data_desa$KABUPATEN)
    } else{
      nama_kab = input$kab_rekap
    }
  })
  
  output$input_rekap_kec <- renderUI({
    pilihan_kecamatan <- data_desa %>%
      select(KABUPATEN, KECAMATAN) %>%
      filter(KABUPATEN %in% inputan_rekap_kab()) %>%
      select(KECAMATAN)
    if(input$kab_rekap == "SEMUA KABUPATEN"){
      pilihan_kecamatan <- c("SEMUA KECAMATAN") 
    } else{
      pilihan_kecamatan <- c("SEMUA KECAMATAN", unique(pilihan_kecamatan$KECAMATAN))
    }
    selectInput(
      inputId = "kec_rekap",
      label = "Pilih Kecamatan",  selected = "SEMUA KECAMATAN",
      choices = pilihan_kecamatan
    )
  })
  
  action_rekap_kec <- eventReactive(input$cari_rekap, {
    if(input$kec_rekap == "SEMUA KECAMATAN"){
      data_kec_filter <- data_desa %>%
        select(KABUPATEN, KECAMATAN)
      
      data_kec_filter = data_kec_filter %>%
        filter(KABUPATEN %in% inputan_rekap_kab())
      nama_kecamatan = unique(data_kec_filter$KECAMATAN)
    } else{
      nama_kecamatan = input$kec_rekap
    }
  })
  
  inputan_rekap_kec <- reactive({
    if(input$kab_rekap == "SEMUA KECAMATAN"){
      nama_kac = unique(data_desa$KECAMATAN)
    } else{
      nama_kab = input$kec_rekap
    }
  })
  
  output$input_rekap_desa <- renderUI({
    pilihan_desa <- data_desa %>%
      select(KECAMATAN, KELURAHAN) %>%
      filter(KECAMATAN %in% inputan_rekap_kec()) %>%
      select(KELURAHAN)
    if(input$kec_rekap == "SEMUA KECAMATAN"){
      pilihan_desa <- c("SEMUA DESA/KEL") 
    } else{
      pilihan_desa<- c("SEMUA DESA/KEL", unique(pilihan_desa$KELURAHAN))
    }
    selectInput(
      inputId = "desa_rekap",
      label = "Pilih Desa/Kelurahan",  selected = "DESA/KELURAHAN",
      choices = pilihan_desa
    )
  })
  
  action_rekap_desa <- eventReactive(input$cari_rekap, {
    if(input$desa_rekap == "DESA/KELURAHAN"){
      data_desa_filter <- data_desa %>%
        select(KECAMATAN, KELURAHAN)
      
      data_kec_filter = data_kec_filter %>%
        filter(KECAMATAN %in% inputan_rekap_kec())
      nama_desa = unique(data_kec_filter$KELURAHAN)
    } else{
      nama_desa = input$desa_rekap
    }
  })
  
  output$pilih_bulan_rekap <- renderUI({
    selectInput(
      inputId = "bulan_rekap", selected = daftar_bulan[length(daftar_bulan)],
      label = "s.d Bulan", 
      choices = daftar_bulan
    )
  })
  
  action_rekap_bulan <- eventReactive(input$cari_rekap, {
    input$bulan_rekap
  })
  
  bulan_sebelumnya_rekap <- eventReactive(input$cari_rekap, {
    pilih_bulan_sebelumnya(action_rekap_bulan())
  })
  
  desa_filter_rekap <- eventReactive(input$cari_rekap, {
    if(input$kab_rekap == "SEMUA KABUPATEN"){
      desa_filter_rekap <- data_desa %>% 
        select(KELURAHAN)
      desa_filter_rekap = unique(desa_filter_rekap$KELURAHAN)
    } else if(input$kec_rekap == "SEMUA KECAMATAN"){
      desa_filter_rekap <- data_desa %>% 
        filter(KABUPATEN == input$kab_rekap) %>%
        select(KELURAHAN)
      desa_filter_rekap = unique(desa_filter_rekap$KELURAHAN)
    } else if(input$desa_rekap == "SEMUA DESA/KEL"){
      desa_filter_rekap <- data_desa %>% 
        filter(KECAMATAN == input$kec_rekap) %>%
        select(KELURAHAN)
      desa_filter_rekap = unique(desa_filter_rekap$KELURAHAN)
    } else{
      desa_filter_rekap = input$desa_rekap
    }
  })
  
  kec_filter_rekap <- eventReactive(input$cari_rekap, {
    if(input$kab_rekap == "SEMUA KABUPATEN"){
      kec_filter_rekap <- data_desa %>% 
        select(KECAMATAN)
      kec_filter_rekap = unique(kec_filter_rekap$KECAMATAN)
    } else if(input$kec_rekap == "SEMUA KECAMATAN"){
      kec_filter_rekap <- data_desa %>% 
        filter(KABUPATEN == input$kab_rekap) %>%
        select(KECAMATAN)
      kec_filter_rekap = unique(kec_filter_rekap$KECAMATAN)
    } else{
      kec_filter_rekap = input$kec_rekap
    }
  })
  
  values <- reactiveValues(default = 0)
  
  observeEvent(input$cari_rekap,{
    values$default <- input$cari_rekap
  })
  
  teks_judul_rekap <- eventReactive(input$cari_rekap, {
    if(input$kab_rekap == "SEMUA KABUPATEN"){
      nama_daerah = input$prov_rekap
      tingkat_daerah = "PROVINSI"
    } else if(input$kec_rekap == "SEMUA KECAMATAN"){
      nama_daerah = input$kab_rekap
      tingkat_daerah = "KABUPATEN"
    } else if(input$desa_rekap == "SEMUA DESA/KEL"){
      nama_daerah = input$kec_rekap
      tingkat_daerah = "KECAMATAN"
    } else{
      nama_daerah = input$desa_rekap
      tingkat_daerah = "KELURAHAN"
    }
    
    if(tingkat_daerah == "KELURAHAN"){
      teks <- paste0("PROFIL DESA/", tingkat_daerah, " ", nama_daerah, " - ", input$bulan_rekap)
    } else{
      teks <- paste("PROFIL", tingkat_daerah, nama_daerah, "-", input$bulan_rekap)
    }
    

  })
  
  output$tes_input_rekap <- renderText({
    if(values$default == 0){
      teks = "Klik Cari Untuk Menampilkan Halaman"
    }
    else{
      teks = teks_judul_rekap()
    }
      
  })
  
  output$peta_titik_rekap <- renderLeaflet({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    # filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
    #   group_by(PROVINSI) %>%
    # 
    # tingkat_daerah <- tingkat_daerah_filter()
    # nama_daerah <- nama_daerah_filter()
    titik_puskesmas <- titik_puskesmas %>%
      filter(KECAMATAN %in% kecamatan)
    if(nrow(titik_puskesmas) <= 0){
      titik_puskesmas <- data.frame(
        PUSKESMAS = "Puskesmas contoh Contoh",
        LATITUDE = 0,
        LONGITUDE = 0,
        Kelurahan_Desa = "Kelurahan Contoh"
      )
    } else{
      titik_puskesmas = titik_puskesmas
    }
    
    if(titik_sd$Latitude[1] == 0){
      cek_puskesmas = "titik_puskesmas"
    } else{
      cek_puskesmas = ""
    }
    
    titik_sd <- titik_sd %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
      select(KECAMATAN, Nama_Sekolah, Latitude, Longitude, KELURAHAN)
    
    if(nrow(titik_sd) <= 0){
      titik_sd <- data.frame(
        Nama_Sekolah = "Puskesmas Contoh",
        Latitude = 0,
        Longitude = 0,
        Kelurahan_Desa = "Kelurahan Contoh"
      )
    } else{
      titik_sd = titik_sd
    }
    
    if(titik_sd$Latitude[1] == 0){
      cek_sd = "titik_sd"
    } else{
      cek_sd = ""
    }
    
    titik_smp <- titik_smp %>%
      filter(KECAMATAN %in% kecamatan) %>%
      select(KECAMATAN, Nama_Sekolah, Latitude, Longitude, KELURAHAN)
    
    if(nrow(titik_smp) <= 0){
      titik_smp <- data.frame(
        Nama_Sekolah = "Sekolah Contoh",
        Latitude = 0,
        Longitude = 0,
        Kelurahan_Desa = "Kelurahan Contoh"
      )
    } else{
      titik_smp = titik_smp
    }
    
    if(titik_smp$Latitude[1] == 0){
      cek_smp = "titik_smp"
    } else{
      cek_smp = ""
    }
    
    df_peta <- data_desa %>%
      select(PROVINSI, KABUPATEN, KECAMATAN, KELURAHAN, LATITUDE, LONGITUDE) %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel)
    df_peta$LATITUDE <- as.numeric(df_peta$LATITUDE)
    df_peta$LONGITUDE <- as.numeric(df_peta$LONGITUDE)
    
    icon_sekolah <- makeIcon(
      iconUrl = "img/logo_sekolah.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 0, iconAnchorY = 0,
      shadowUrl = "",
      shadowWidth = 50, shadowHeight = 64,
      shadowAnchorX = 4, shadowAnchorY = 62
    )
    
    icon_puskesmas <- makeIcon(
      iconUrl = "img/logo_puskesmas.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 0, iconAnchorY = 0,
      shadowUrl = "",
      shadowWidth = 50, shadowHeight = 64,
      shadowAnchorX = 4, shadowAnchorY = 62
    )
    
    
    leaflet(df_peta) %>% 
      addPolygons(
        data = batas_sulbar,weight = 2,opacity = 1, fillColor = "white",
        color = "darkgreen", dashArray = "3",fillOpacity = 0.7, label = batas_sulbar$KABUPATEN,
        highlightOptions = highlightOptions(
          weight = 5,color = "#666",  dashArray = "",fillOpacity = 0.7,bringToFront = F),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "10px", direction = "auto")) %>%
      addProviderTiles(providers$CyclOSM) %>%
      setView(lng = mean(df_peta$LONGITUDE), lat = mean(df_peta$LATITUDE), zoom = 7) %>%
      addMarkers(~LONGITUDE, ~LATITUDE, label = ~htmlEscape(KELURAHAN), 
                 clusterOptions = markerClusterOptions()) %>%
      addMarkers(lat = titik_smp$Latitude, lng = titik_smp$Longitude, 
                 clusterOptions = markerClusterOptions(), label = ~htmlEscape(titik_smp$Nama_Sekolah), 
                 icon = icon_sekolah, group = "titik_smp") %>%
      addMarkers(lat = titik_sd$Latitude, lng = titik_sd$Longitude, 
                 clusterOptions = markerClusterOptions(), group = "titik_sd",
                 label = ~htmlEscape(titik_sd$Nama_Sekolah), icon = icon_sekolah) %>%
      addMarkers(lat = titik_puskesmas$LATITUDE, lng = titik_puskesmas$LONGITUDE, group = "titik_puskesmas",
                 clusterOptions = markerClusterOptions(),
                 label = ~htmlEscape(titik_puskesmas$PUSKESMAS), icon = icon_puskesmas) %>%
      addLegendImage(images = c("img/logo_desa.png", "img/logo_sekolah.png",
                                "img/logo_puskesmas.png"),
                     labels = c('Desa', 'Sekolah', 'Puskesmas'),width = 20, height = 20,
                     orientation = 'vertical',
                     title = htmltools::tags$div('Keterangan',
                                                 style = 'font-size: 20px; text-align: center;'),
                     position = 'topright') %>%
      hideGroup(cek_sd) %>%
      hideGroup(cek_smp)
    
  })
  
  output$card_profil_poktan_rekap <- renderUI({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    df_profil <- data_desa %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      group_by(PROVINSI) %>%
      dplyr::summarise(
        Desa = n(),
        `Kampung KB` = sum(`Kampung KB` == "Ada"),
        `Rumah DataKU` = sum(`Rumah DataKU` == "Ada"),
        BKB = sum(BKB == "Ada"),
        BKR = sum(BKR == "Ada"),
        BKL = sum(BKL == "Ada"),
        `PIK-R` = sum(`PIK-R` == "Ada"),
        UPPKA = sum(UPPKA == "Ada")) 
    buat_box_profil <- function(df_profil){
      boxProfile(
        title = "",
        subtitle = "Kepemilikan Poktan",
        bordered = TRUE,
        boxProfileItem(
          title = "Desa:",    
          description = as.numeric(df_profil["Desa"])),
        boxProfileItem(
          title = "Kampung KB:",    
          description = as.numeric(df_profil["Kampung KB"])),
        boxProfileItem(
          title = "Rumah Dataku:",
          description = df_profil["Rumah DataKU"]),
        boxProfileItem(
          title = "Bina Keluarga Balita:",
          description = df_profil["BKB"]),  
        boxProfileItem(
          title = "Bina Keluarga Remaja:",
          description = df_profil["BKR"]),  
        boxProfileItem(
          title = "Bina Keluarga Lansia:",
          description = df_profil["BKL"]),
        boxProfileItem(
          title = "UPPKA:",
          description = df_profil["UPPKA"]),
        boxProfileItem(
          title = "PIK-R:",
          description = df_profil["PIK-R"])
      )
    } #batas fungsi
    
    buat_box_profil(df_profil)
  })
  
  output$card_profil_sd_rekap <- renderUI({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    df_sd_rekap <- data_sumber_daya %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
      group_by(PROVINSI) %>%
      dplyr::summarise(
        LUAS_WILAYAH = sum(LUAS_WILAYAH),
        JUMLAH_PENDUDUK = sum(JUMLAH_PENDUDUK),
        KEPADATAN_PENDUDUK = round(JUMLAH_PENDUDUK/LUAS_WILAYAH,2),
        KRS = sum(KRS))
    if(length(desa_kel) > 1){
      idm = ""
    } else{
      idm = data_sumber_daya %>%
        filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
        select(IDM)
      idm = idm$IDM
    }
    boxProfile(
      title = "",
      subtitle = "Profil Wilayah",
      bordered = TRUE,
      boxProfileItem(
        title = "Luas Wilayah:",    
        description = paste(df_sd_rekap[1, "LUAS_WILAYAH"], "km²")
      ),
      boxProfileItem(
        title = "Jumlah Penduduk:",
        description = df_sd_rekap[1,"JUMLAH_PENDUDUK"]
      ),
      boxProfileItem(
        title = "Kepadatan Penduduk:",
        description = df_sd_rekap[1,"KEPADATAN_PENDUDUK"]
      ),
      boxProfileItem(
        title = "IDM:",
        description = idm
      )
    )
  })
  
  output$grafik_piramida_rekap <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    kelompok_umur_lk <- kelompok_umur_lk %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)  %>%
      gather("Kelompok_Umur", "Jumlah", 4:20) %>%
      mutate(Jenis_Kelamin = rep("Laki-laki", 17))
    
    kelompok_umur_pr <- kelompok_umur_pr %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)  %>%
      gather("Kelompok_Umur", "Jumlah", 4:20) %>%
      mutate(Jenis_Kelamin = rep("Peremupuan", 17))
    
    #data_piramida <- rbind(kelompok_umur_lk, kelompok_umur_pr)
    
    ku <- c("0 - 1",	"2 - 4",	"5 - 9",	"10 - 14",	"15 - 19",
            "20 - 24",	"25 - 29",	"30 - 34",	"35 - 39",	"40 - 44",
            "45 - 49",	"50 - 54",	"55 - 59	", "60 - 64",
            "65 - 69",	"70 - 74",	"75+")
    
    # Membuat data untuk grafik piramida
    piramida_data <- data.frame(
      Kelompok_Umur = factor(rep(ku, times = 2), levels = ku),
      Jumlah = c(kelompok_umur_lk$Jumlah, -kelompok_umur_pr$Jumlah),
      Jenis_Kelamin = rep(c("Laki-Laki", "Perempuan"), each = length(kelompok_umur_lk$Kelompok_Umur))
    )
    
    
    #grafik piramida penduduk interaktif menggunakan plotly
    piramida_interaktif <- plot_ly(
      piramida_data,
      x = ~Jumlah,
      y = ~Kelompok_Umur,
      type = "bar",
      orientation = "h",
      color = ~Jenis_Kelamin,
      colors = c("#0d6efd", "#ffc107"),
      hoverinfo = "text",
      text = ~paste(Jenis_Kelamin, abs(Jumlah)),
      textposition = 'none',
      showlegend = FALSE  # Menghapus legend
    ) 
    
    # Fungsi untuk mengambil kelipatan 20 ke atas
    ambil_kelipatan_20_ke_atas <- function(angka) {
      kelipatan_20_terdekat <- ceiling(angka/50) * 50
      return(kelipatan_20_terdekat)
    }
    
    batas_angka <- ambil_kelipatan_20_ke_atas(max(piramida_data$Jumlah))
    
    
    
    # Menambahkan label dan judul
    piramida_interaktif <- piramida_interaktif %>%
      layout(title = "Grafik Piramida Penduduk",
             xaxis = list(title = "Jumlah Penduduk", tickangle=0,
                          tickvals = seq(-batas_angka, batas_angka+1, by = round(batas_angka/3)), 
                          ticktext = abs(seq(-batas_angka, batas_angka+1, by = round(batas_angka/3)))),
             yaxis = list(title = "Kelompok Usia", standoff = 100),
             barmode = "relative", 
             legend = list(orientation = 'h'),
             font = list(family = "Arial"),  # Mengatur jenis font global
             margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
             paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
             plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
             hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
             legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
             hovermode = "closest",  # Mengatur mode hover
             hoverdistance = 30,  # Mengatur jarak hover
             hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
             updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
      )
    
    # Menampilkan grafik interaktif
    piramida_interaktif %>%
      add_annotations(
        text = "Perempuan", x = -batas_angka, y = 15,
        showarrow = FALSE, font = list(color = "#000", size = 14), xref = "x", yref = "y"
      ) %>%
      add_annotations(
        text = "Laki-Laki", x = batas_angka, y = 15,
        showarrow = FALSE, font = list(color = "#000", size = 14), xref = "x", yref = "y"
      )
    
  })
  
  output$tabel_piramida_rekap <- renderReactable({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    kelompok_umur_lk <- kelompok_umur_lk %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)  %>%
      gather("Kelompok_Umur", "Jumlah", 4:20) %>%
      mutate(Jenis_Kelamin = rep("Laki-laki", 17))
    
    kelompok_umur_pr <- kelompok_umur_pr %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)  %>%
      gather("Kelompok_Umur", "Jumlah", 4:20) %>%
      mutate(Jenis_Kelamin = rep("Peremupuan", 17))
    
    tabel_piramida <- left_join(kelompok_umur_lk, kelompok_umur_pr,
                                by = "Kelompok_Umur") %>%
      rename(Laki_Laki = Jumlah.x, Perempuan = Jumlah.y) %>%
      group_by(Kelompok_Umur) %>%
      mutate(Total = sum(Laki_Laki + Perempuan)) 
    
    tabel_piramida <- as_tibble(tabel_piramida)
    
    ku <- c("0 - 1",	"2 - 4",	"5 - 9",	"10 - 14",	"15 - 19",
            "20 - 24",	"25 - 29",	"30 - 34",	"35 - 39",	"40 - 44",
            "45 - 49",	"50 - 54",	"55 - 59	", "60 - 64",
            "65 - 69",	"70 - 74",	"75+")
    
    tabel_piramida$Kelompok_Umur <- factor(ku, levels = ku)
    
    tabel_piramida <- tabel_piramida %>%  
      select(4,5,10,12) %>%
      bind_rows(summarise(., across(where(is.numeric), sum),
                          across(where(is.factor), ~'Total')))
    colnames(tabel_piramida) <- c("Umur", "Laki-laki", "Perempuan", "Total")
    reactable(tabel_piramida,
              columns = list(
                `Laki-laki` = colDef(
                  format = colFormat(separators = TRUE, locales = "en-US")
                ),
                Perempuan = colDef(
                  format = colFormat(separators = TRUE, locales = "en-US")
                ),
                Total = colDef(
                  format = colFormat(separators = TRUE, locales = "en-US")
                )
              ),
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE,
              theme = reactableTheme(
                borderColor = "#dfe2e5",
                stripedColor = "#f6f8fa",
                highlightColor = "#f0f5f9",
                cellPadding = "8px 12px",
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                searchInputStyle = list(width = "100%"))
    )
  })
  
  ## value_box unmet need
  unmet_need_vb_1_rekap <- eventReactive(input$cari_rekap, {
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    unmet_need <- data_kb %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      group_by(PROVINSI) %>%
      summarise(UNMET_NEED = sum(PUS_TIDAK_KB)/sum(PUS)*100)
    
    unmet_need <- round(unmet_need$UNMET_NEED, 2)
  })
  
  unmet_need_1_vb_rekap <- eventReactive(input$cari_rekap, {
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- bulan_sebelumnya_rekap()
    unmet_need <- data_kb %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      group_by(PROVINSI) %>%
      summarise(UNMET_NEED = sum(PUS_TIDAK_KB)/sum(PUS)*100)
    
    unmet_need <- round(unmet_need$UNMET_NEED, 2)
  })
  
  output$vb_unmet_need_rekap <- renderUI({
    if(unmet_need_vb_1_rekap() < unmet_need_1_vb_rekap()) {
      cek = "Turun"
    } else if(unmet_need_vb_1_rekap() == unmet_need_1_vb_rekap()){
      cek = "Sama"
    } else {
      cek = "Naik"
    }
    
    text_un = paste0(cek, " dari capaian ", str_to_title(bulan_sebelumnya_rekap()), " (",unmet_need_1_vb_rekap(), ")")
    
    #arrow
    if(unmet_need_vb_1_rekap() < unmet_need_1_vb_rekap()) {
      cek1 = "arrow-down"
    } else if(unmet_need_vb_1_rekap() == unmet_need_1_vb_rekap()){
      cek1 = "arrow-left-right"
    } else {
      cek1 = "arrow-up"
    }
    
    value_box(
      h1(span(
        strong("Unmet Need"), 
        bsicons::bs_icon(cek1),
        style = "font-size:20px;")),
      unmet_need_vb_1_rekap(),
      span(
        text_un,
        style = "font-size:12px;"
      ),
      showcase = bs_icon("clipboard"),
      theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E")
    )
  })
  
  ## value mkjp
  mkjp_vb_1_rekap <- eventReactive(input$cari_rekap, {
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    mkjp <- data_kb %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      group_by(PROVINSI) %>%
      summarise(MKJP = sum(MKJP))
    mkjp <- mkjp$MKJP
  })
  
  mkjp_1_vb_rekap <- eventReactive(input$cari_rekap, {
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- bulan_sebelumnya_rekap()
    mkjp <- data_kb %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      group_by(PROVINSI) %>%
      summarise(MKJP = sum(MKJP))
    mkjp <- mkjp$MKJP
  })
  
  output$vb_mkjp_rekap <- renderUI({
    
    if(mkjp_vb_1_rekap() < mkjp_1_vb_rekap()) {
      cek = "Turun"
    } else if(mkjp_vb_1_rekap() == mkjp_1_vb_rekap()){
      cek = "Sama"
    } else {
      cek = "Naik"
    }
    
    text_un = paste0(cek, " dari capaian ", 
                     str_to_title(bulan_sebelumnya_rekap()), 
                     " (",format(mkjp_1_vb_rekap(), big.mark = ".", decimal.mark = ","), ")")
    
    #arrow
    if(mkjp_vb_1_rekap() < mkjp_1_vb_rekap()) {
      cek1 = "arrow-down"
    } else if(mkjp_vb_1_rekap() == mkjp_1_vb_rekap()){
      cek1 = "arrow-left-right"
    } else {
      cek1 = "arrow-up"
    }
    
    value_box(
      h1(span(
        strong("MKJP"), 
        bsicons::bs_icon(cek1),
        style = "font-size:20px;")),
      format(mkjp_vb_1_rekap(), big.mark = ".", decimal.mark = ","),
      span(
        text_un,
        style = "font-size:12px;"
      ),
      showcase = bs_icon("capsule-pill"),
      theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
    )
  })
  ## akhir value box mkjp
  
  tp_kb_vb_1_rekap <- eventReactive(input$cari_rekap, {
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()

    tp_kb_cek <- bangke_lengkap %>%
      filter(BULAN %in% bulan) %>%
      filter(KECAMATAN %in% kecamatan) %>%
      filter(KELURAHAN %in% desa_kel)
    
    if(nrow(tp_kb_cek) <= 0){
      tp_kb_cek = 0
    } else{
      tp_kb_cek <- length(unique(tp_kb_cek$`Nama Faskes`))
    }
    tp_kb_cek = tp_kb_cek
  })
  
  output$vb_tp_kb_rekap <- renderUI({
    value_box(
      h1(span(
        strong("Tempat Pelayanan KB"),
        style = "font-size:20px;")),
      tp_kb_vb_1_rekap(),
      showcase = bs_icon("hospital"),
      theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
    )
    
  })
  
  sdm_vb_1_rekap <- eventReactive(input$cari_rekap,{
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    sdm_cek <- bangke_lengkap %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel, BULAN %in% bulan)
    
    if(nrow(sdm_cek) <= 0){
      sdm_cek = 0
    } else{
      sdm_cek <- nrow(sdm_cek)
    }
    sdm_cek 
    
  })
  
  output$vb_sdm_kb_rekap <- renderUI({
    value_box(
      h1(span(
        strong("Tenaga Kesehatan"),
        style = "font-size:20px;")),
      sdm_vb_1_rekap(),
      showcase = bs_icon("person-square"),
      theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
    )
  })
  
  output$bar_kontrasepsi_rekap <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    # Data contoh
    data_kontrasepsi <- data_kb %>%
      filter(KECAMATAN %in%  kecamatan, 
             KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      select(-c(PUS, PB, PA, MKJP, KBPP, PUS_KB, PUS_TIDAK_KB, UNMET_NEED)) %>%
      gather("Metode_Kontrasepsi", "Penggunaan", 5:12) %>%
      group_by(PROVINSI, Metode_Kontrasepsi) %>%
      summarise_if(is.numeric, sum)
    
    # Membuat grafik bar Plotly
    grafik_kontrasepsi <- plot_ly(
      data_kontrasepsi,
      x = ~Metode_Kontrasepsi,
      y = ~Penggunaan,
      text = ~Penggunaan,
      type = "bar",
      marker = list(color = "#0d6efd")  # Warna bar
    ) 
    
    # Menambahkan label dan judul
    grafik_kontrasepsi <- grafik_kontrasepsi %>%
      layout(
        title = "Penggunaan Metode Kontrasepsi",
        xaxis = list(title = "Metode Kontrasepsi",
                     categoryorder = "total ascending"),
        yaxis = list(title = "Jumlah Penggunaan"),
        font = list(family = "Arial"),  # Mengatur jenis font global
        margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
        paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
        plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
        hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
        legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
        hovermode = "closest",  # Mengatur mode hover
        hoverdistance = 30,  # Mengatur jarak hover
        hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
        updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
      )
    
    # Menampilkan grafik
    grafik_kontrasepsi
    
  })
  
  output$donut_terlatih_tidak_rekap <- renderPlotly({
    #filter(KECAMATAN == "TOBADAK", KELURAHAN == "MAHAHE")
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    # Data contoh
    terlatih_tidak <- bangke_lengkap %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel, BULAN == bulan) %>%
      select(Pelatihan) %>%
      mutate(status = if_else(grepl("IUD|Tubektomi|Vasektomi", Pelatihan), "Sudah", "Belum"))
    
    terlatih_tidak <- table(terlatih_tidak$status)
    
    terlatih_tidak <- as.data.frame(terlatih_tidak)
    
    # Memberi nama kolom
    names(terlatih_tidak) <- c("Kategori", "Jumlah")
    
    terlatih_tidak %>% 
      plot_ly(labels = ~Kategori, 
              values = ~Jumlah, 
              textinfo='label+percent+value',
              marker = list(colors = c("#ffc107", "#0d6efd"),
                            line = list(color = '#000', width = 1))
      )%>% 
      add_pie(hole = 0.4)%>% 
      layout(title = "Perbandingan Pelatihan/Kompetensi Tenaga KB",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                          showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                          showticklabels = FALSE),
             font = list(family = "Arial"),  # Mengatur jenis font global
             margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
             paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
             plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
             hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
             legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
             hovermode = "closest",  # Mengatur mode hover
             hoverdistance = 30,  # Mengatur jarak hover
             hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
             updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
      )
  })
  
  output$tabel_tp_kb_rekap <- renderReactable({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    
    bangke_lengkap$NAMA_FASKES <- factor(bangke_lengkap$`Nama Faskes`,
                                 levels = c(unique(bangke_lengkap$`Nama Faskes`), "Total"))
    
    bangke_lengkap <- bangke_lengkap %>%
      mutate(status_pelatihan = case_when(
        str_detect(Pelatihan, "IUD|Vasektomi|Tubektomi") ~ "Sudah",
        TRUE ~ "Belum"
      ))
    
    bangke_lengkap$`Nama Faskes` <- factor(bangke_lengkap$`Nama Faskes`,
                                           levels = c(unique(bangke_lengkap$`Nama Faskes`), "Total"))
    
    if(input$kab_rekap == "SEMUA KABUPATEN"){
      detail_tm_kb <- bangke_lengkap %>%
        filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
               BULAN %in% bulan) %>%
        select(KABUPATEN, KECAMATAN, KELURAHAN, `Nama Faskes`, `Nama Bidan`, status_pelatihan)
      colnames(detail_tm_kb) <- c("KABUPATEN", "KECAMATAN", "KELURAHAN", "NAMA FASKES", "NAMA BIDAN",
                                  "STATUS PELATIHAN")
      
      reactable(detail_tm_kb,
                searchable = TRUE,
                bordered = TRUE,
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(
                  borderColor = "#dfe2e5",
                  stripedColor = "#f6f8fa",
                  highlightColor = "#f0f5f9",
                  cellPadding = "8px 12px",
                  style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                  searchInputStyle = list(width = "100%")),
                groupBy = c("KABUPATEN", "KECAMATAN", "KELURAHAN"),
                columns = list(
                  `NAMA BIDAN` = colDef(aggregate = "count"),
                  `STATUS PELATIHAN` = colDef(aggregate = "frequency",
                                              cell = function(value) {
                                                # Render as an X mark or check mark
                                                if (value == "Belum") "\u274c Belum" else "\u2714\ufe0f Sudah"
                                              })
                ))
    } else if(input$kab_rekap != "SEMUA KABUPATEN" & input$kec_rekap == "SEMUA KECAMATAN"){
      detail_tm_kb <- bangke_lengkap %>%
        filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
               BULAN %in% bulan) %>%
        select(KECAMATAN, KELURAHAN, `Nama Faskes`, `Nama Bidan`, status_pelatihan)
      colnames(detail_tm_kb) <- c("KECAMATAN", "KELURAHAN", "NAMA FASKES", "NAMA BIDAN",
                                  "STATUS PELATIHAN")
      
      reactable(detail_tm_kb,
                searchable = TRUE,
                bordered = TRUE,
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(
                  borderColor = "#dfe2e5",
                  stripedColor = "#f6f8fa",
                  highlightColor = "#f0f5f9",
                  cellPadding = "8px 12px",
                  style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                  searchInputStyle = list(width = "100%")),
                groupBy = c("KECAMATAN", "KELURAHAN"),
                columns = list(
                  `NAMA BIDAN` = colDef(aggregate = "count"),
                  `STATUS PELATIHAN` = colDef(aggregate = "frequency",
                                              cell = function(value) {
                                                # Render as an X mark or check mark
                                                if (value == "Belum") "\u274c Belum" else "\u2714\ufe0f Sudah"
                                              })
                ))
    } else{
      tabel_tm_kb <- bangke_lengkap %>%
        filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel, BULAN == bulan) %>%
        select(KELURAHAN, `Nama Faskes`, `Nama Bidan`, Pelatihan) %>%
        mutate(status = if_else(grepl("IUD|Tubektomi|Vasektomi", Pelatihan), "Sudah", "Belum")) %>%
      group_by(KELURAHAN, `Nama Faskes`) %>%
        summarize(Sudah = sum(status == "Sudah", na.rm = TRUE),
                  Belum = sum(status == "Belum", na.rm = TRUE))
      # tabel_tm_kb <- bangke %>%
      #   select(KABUPATEN, KECAMATAN, KELURAHAN, NAMA_FASKES, Belum, Sudah, BULAN) %>%
      #   filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
      #          BULAN %in% bulan)
      
      tabel_tm_kb <- tabel_tm_kb %>%
        mutate(Jumlah_Bidan = Sudah + Belum) %>%
        select(KELURAHAN, `Nama Faskes`, Belum, Sudah, Jumlah_Bidan) %>%
        bind_rows(summarise(., across(where(is.numeric), ~sum(., na.rm = TRUE)),
                            across(where(is.factor), ~'Total')))
      
      
      detail_tm_kb <- bangke_lengkap %>%
        select(KECAMATAN, KELURAHAN, `Nama Faskes`, `Nama Bidan`, status_pelatihan, BULAN) %>%
        filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
               BULAN %in% bulan)

      reactable(tabel_tm_kb, 
                bordered = TRUE,
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(
                  borderColor = "#dfe2e5",
                  stripedColor = "#f6f8fa",
                  highlightColor = "#f0f5f9",
                  cellPadding = "8px 12px",
                  style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                  searchInputStyle = list(width = "100%")),
                details = function(index) {
                  nama_faskes_subsets <- unique(detail_tm_kb$`Nama Faskes`)[index]
                  if(!nama_faskes_subsets %in% unique(detail_tm_kb$`Nama Faskes`)){
                    nama_bidan_subsets = detail_tm_kb
                  } else{
                    nama_bidan_subsets <- detail_tm_kb[detail_tm_kb$`Nama Faskes` == nama_faskes_subsets, ] 
                  }
                  colnames(nama_bidan_subsets)[5] <- c("Status Pelatihan")
                  htmltools::div(style = "padding: 1rem",
                                 reactable(nama_bidan_subsets[,3:5], outlined = TRUE,
                                           theme = reactableTheme(
                                             borderColor = "#dfe2e5",
                                             stripedColor = "#f6f8fa",
                                             highlightColor = "#f0f5f9",
                                             cellPadding = "8px 12px",
                                             style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                                             searchInputStyle = list(width = "100%")),
                                           columns = list(
                                             "Status Pelatihan" = colDef(cell = function(value) {
                                               # Render as an X mark or check mark
                                               if (value == "Belum") "\u274c Belum" else "\u2714\ufe0f Sudah"
                                             })
                                           ))
                  )
      })
    }
    
    
  })
  
  pilih_sampai_bulan_rekap <- function(bulan_yang_dipilih) {
    daftar_bulan <- c("JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI", "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER")
    
    # Temukan indeks bulan yang dipilih
    indeks_bulan <- which(daftar_bulan == toupper(bulan_yang_dipilih))
    
    # Validasi jika bulan tidak ditemukan
    if (length(indeks_bulan) == 0) {
      cat("Bulan tidak ditemukan.")
      return(NULL)
    }
    
    # Buat subset daftar bulan dari Januari hingga bulan yang dipilih
    daftar_bulan_subset <- daftar_bulan[1:indeks_bulan]
    
    return(daftar_bulan_subset)
  }
  
  sampai_bulan_rekap <- eventReactive(input$cari_rekap, {
    pilih_sampai_bulan_rekap(action_rekap_bulan())
  })
  
  output$line_mix_kontrasepsi_rekap <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- sampai_bulan_rekap()
    
    data_sd_kontrasepsi <- data_kb %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
             BULAN %in% bulan) %>%
      group_by(PROVINSI, BULAN) %>%
      summarise(TOTAL = sum(SUNTIK, PIL, KONDOM, IMPLAN, IUD, VASEKTOMI, TUBEKTOMI, MAL))
    # Konversi bulan menjadi faktor dengan level yang sesuai
    data_sd_kontrasepsi$BULAN <- factor(data_sd_kontrasepsi$BULAN, levels = c("JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI", "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER"), ordered = TRUE)
    # 
    # # Urutkan dataframe berdasarkan bulan
    data_sd_kontrasepsi <- data_sd_kontrasepsi[order(data_sd_kontrasepsi$BULAN), ]
    
    plot_ly(data_sd_kontrasepsi, x=~BULAN, y=~TOTAL,
            name='FIRST',  hoverinfo = 'text',
            showlegend = FALSE,
            text = ~paste('Total: ', data_sd_kontrasepsi$TOTAL),
            type = 'scatter', mode = 'plines') %>%
      layout(title = 'Jumlah Penggunaan Kontrasepsi',
             transition = list(duration = 2000, easing = "cubic-in-out"),
             xaxis = list(title = "BULAN", tickmode = 'linear', dtick = 1),
             yaxis = list(title = 'TOTAL'),
             font = list(family = "Arial"),  # Mengatur jenis font global
             margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
             paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
             plot_bgcolor = "#f6f8fa"  # Mengatur jenis font untuk menu pembaruan
      )
  })
  
  ##value box pus
  pus_vb_1_rekap <- eventReactive(input$cari_rekap, {
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    
    pus <- data_kb %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
             BULAN %in% bulan) %>%
      group_by(PROVINSI) %>%
      summarise(PUS = sum(PUS))
    
    pus <- pus$PUS
  })
  
  pus_1_vb_rekap <- eventReactive(input$cari_rekap, {
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- bulan_sebelumnya_rekap()
    pus <- data_kb %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
             BULAN == bulan) %>%
      group_by(PROVINSI) %>%
      summarise(PUS = sum(PUS))
    
    pus <- pus$PUS
  })
  
  output$vb_pus_rekap <- renderUI({
    if(pus_vb_1_rekap() < pus_1_vb_rekap()) {
      cek = "Turun"
    } else if(pus_vb_1_rekap() == pus_1_vb_rekap()){
      cek = "Sama"
    } else {
      cek = "Naik"
    }
    
    text_un = paste0(cek, " dari angka ", 
                     str_to_title(bulan_sebelumnya_rekap()), 
                     " (",format(pus_1_vb_rekap(), big.mark = ".", decimal.mark = ","), ")")
    
    #arrow
    if(pus_vb_1_rekap() < pus_1_vb_rekap()) {
      cek1 = "arrow-down"
    } else if(pus_vb_1_rekap() == pus_1_vb_rekap()){
      cek1 = "arrow-left-right"
    } else {
      cek1 = "arrow-up"
    }
    
    value_box(
      h1(span(
        strong("Pasangan Usia Subur"), 
        bsicons::bs_icon(cek1),
        style = "font-size:20px;")),
      format(pus_vb_1_rekap(), big.mark = ".", decimal.mark = ","),
      span(
        text_un,
        style = "font-size:12px;"
      ),
      showcase = bs_icon("person-heart"),
      theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
    )
    
  })
  ##akhir value box pus
  # output$pa_vb_rekap <- renderText({
  #   kecamatan <- kec_filter_rekap()
  #   desa_kel <- desa_filter_rekap()
  #   bulan <- sampai_bulan_rekap()
  #   
  #   pa <- data_kb %>%
  #     filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
  #            BULAN == bulan) %>%
  #     group_by(PROVINSI) %>%
  #     summarise(PA = sum(PA))
  #   
  #   pa <- pa$PA
  # })
  # 
  # output$pb_vb_rekap <- renderText({
  #   kecamatan <- kec_filter_rekap()
  #   desa_kel <- desa_filter_rekap()
  #   bulan <- sampai_bulan_rekap()
  #   
  #   pb <- data_kb %>%
  #     filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
  #            BULAN == bulan) %>%
  #     group_by(PROVINSI) %>%
  #     summarise(PB = sum(PB))
  #   
  #   pb <- pb$PB
  # })
  
  pa_vb_1_rekap <- eventReactive(input$cari_rekap, {
      kecamatan <- kec_filter_rekap()
      desa_kel <- desa_filter_rekap()
      bulan <- action_rekap_bulan()

    pa <- data_kb %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
             BULAN == bulan) %>%
      group_by(PROVINSI) %>%
      summarise(PA = sum(PA))
    
    pa <- pa$PA
  })
  
  pa_1_vb_rekap <- eventReactive(input$cari_rekap, {
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- bulan_sebelumnya_rekap()
    
    pa <- data_kb %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
             BULAN == bulan) %>%
      group_by(PROVINSI) %>%
      summarise(PA = sum(PA))
    
    pa <- pa$PA
  })
  
  # pb_vb_1_rekap <- eventReactive(input$cari_rekap, {
  #   kecamatan <- kec_filter_rekap()
  #   desa_kel <- desa_filter_rekap()
  #   bulan <- action_rekap_bulan()
  #   
  #   pb <- data_kb %>%
  #     filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
  #            BULAN == bulan) %>%
  #     group_by(PROVINSI) %>%
  #     summarise(PB = sum(PB))
  #   
  #   pb <- pb$PB
  # })
  
  # pb_1_vb_rekap <- eventReactive(input$cari_rekap, {
  #   kecamatan <- kec_filter_rekap()
  #   desa_kel <- desa_filter_rekap()
  #   bulan <- bulan_sebelumnya_rekap()
  #   
  #   pb <- data_kb %>%
  #     filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel,
  #            BULAN == bulan) %>%
  #     group_by(PROVINSI) %>%
  #     summarise(PB = sum(PB))
  #   
  #   pb <- pb$PB
  # })
  
  output$vb_pa_pb_rekap <- renderUI({
    if(pa_vb_1_rekap() < pa_1_vb_rekap()) {
      cek_text_pa = "Turun"
    } else if(pa_vb_1_rekap() == pa_1_vb_rekap()){
      cek_text_pa = "Sama"
    } else {
      cek_text_pa = "Naik"
    }
    
    text_pa = paste0(" dari capaian ", 
                     str_to_title(bulan_sebelumnya_rekap()), 
                     " (",format(pa_1_vb_rekap(), big.mark = ".", decimal.mark = ","), ")")
    
    #arrow
    if(pa_vb_1_rekap() < pa_1_vb_rekap()) {
      cek_pa = "arrow-down"
    } else if(pa_vb_1_rekap() == pa_1_vb_rekap()){
      cek_pa = "arrow-left-right"
    } else {
      cek_pa = "arrow-up"
    }
    
    
    # if(pb_vb_1_rekap() < pb_1_vb_rekap()) {
    #   cek_text_pb = "Turun"
    # } else if(pb_vb_1_rekap() == pb_1_vb_rekap()){
    #   cek_text_pb = "Sama"
    # } else {
    #   cek_text_pb = "Naik"
    # }
    # 
    # text_pb = paste0(" dari capaian ", str_to_title(bulan_sebelumnya_rekap()), " (",pb_1_vb_rekap(), ")")
    # 
    # #arrow
    # if(pb_vb_1_rekap() < pb_1_vb_rekap()) {
    #   cek_pb = "arrow-down"
    # } else if(pb_vb_1_rekap() == pb_1_vb_rekap()){
    #   cek_pb = "arrow-left-right"
    # } else {
    #   cek_pb = "arrow-up"
    # }
    # 
    
    value_box(
      span(
        "Peserta KB Aktif:", 
        h5(format(pa_vb_1_rekap(), big.mark = ".", decimal.mark = ",")),
        style = "font-size:12px;"
      ),
      span(
        strong(cek_text_pa), text_pa,
        style = "font-size:12px;"
      ),
      br(),
      # span(
      #   "Peserta KB Baru:", h5(pb_vb_1_rekap()),
      #   style = "font-size:12px;"
      # ),
      # span(
      #   strong(cek_text_pb), text_pb,
      #   style = "font-size:12px;"
      # ),
      showcase = bs_icon("person-hearts"),
      theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
    )
    
  })
  ##akhir value kb

  #stunting
  bandingkan_bulan_rekap <- function(bulan_ini, bulan_lalu) {
    if (bulan_ini > bulan_lalu) {
      return("arrow-up-square-fill")
    } else if (bulan_ini < bulan_lalu) {
      return("arrow-down-square-fill")
    } else {
      return("arrow-left-right")
    }
  }
  
  output$profil_stunting_rekap <- renderUI({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    
    data_profil_stunting <- data_stunting %>%
      select(1:10, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel, 
             BULAN == bulan) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)
    
    boxProfile(
      title = "",
      subtitle = "",
      bordered = F,
      boxProfileItem(
        title = "Jumlah Keluarga:",    
        description =format(data_profil_stunting$JUMLAH_KELUARGA, big.mark = ".", decimal.mark = ",")
      ),
      boxProfileItem(
        title = "Jumlah Sasaran:",
        description = format(data_profil_stunting$JUMLAH_SASARAN, big.mark = ".", decimal.mark = ",")
      ),
      boxProfileItem(
        title = "Jumlah KRS:",
        description = format(data_profil_stunting$JUMLAH_KRS, big.mark = ".", decimal.mark = ",")
      ),      
      boxProfileItem(
        title = "Penerima BAAS:",
        description = format(data_profil_stunting$PENERIMA_BAAS, big.mark = ".", decimal.mark = ",")
      ),      
      boxProfileItem(
        title = "Bantuan Pangan:",
        description = format(data_profil_stunting$PENERIMA_BANTUAN_PANGAN, big.mark = ".", decimal.mark = ",")
      ),      
      boxProfileItem(
        title = "Jumlah Stunting:",
        description = format(data_profil_stunting$JUMLAH_BALITA_STUNTING, big.mark = ".", decimal.mark = ",")
      )
    )
  })
  
  output$bar_faktor_resiko_rekap <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    
    data_faktor_resiko <- data_stunting %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel, 
             BULAN == bulan) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)
    
    
    data_faktor_resiko <- data_faktor_resiko %>%
      mutate(PERSEN_TUA = round(TERLALU_TUA/JUMLAH_KRS, 2),
             PERSEN_MUDA = round(TERLALU_MUDA/JUMLAH_KRS, 2),
             PERSEN_BANYAK = round(TERLALU_BANYAK/JUMLAH_KRS, 2),
             PERSEN_DEKAT = round(TERLALU_DEKAT/JUMLAH_KRS, 2),
             PERSEN_SANITASI = round(SANITASI/JUMLAH_KRS, 2),
             PERSEN_AIR_MINUM = round(SUMBER_AIR_MINUM/JUMLAH_KRS, 2),
             PERSEN_KB_MODERN = round(KB_MODERN/JUMLAH_KRS, 2)
      ) %>% select(PERSEN_TUA, PERSEN_MUDA, PERSEN_BANYAK, PERSEN_DEKAT, 
                   PERSEN_SANITASI, PERSEN_AIR_MINUM, PERSEN_KB_MODERN)
    
    
    
    colnames(data_faktor_resiko) <- c("TERLALU TUA",
                                      "TERLALU MUDA", "TERLALU BANYAK", "TERLALU DEKAT",
                                      "JAMBAN", "SUMBER AIR", "KB MODERN")
    
    data_faktor_resiko <- 
      data_faktor_resiko %>%
      gather(
        "FAKTOR", "PERSENTASE", 1:7
      )
    
    # Mengatur urutan kolom
    data_faktor_resiko$FAKTOR <- factor(data_faktor_resiko$FAKTOR, 
                                        levels = c("JAMBAN", "SUMBER AIR", "KB MODERN", "TERLALU TUA", 
                                                   "TERLALU MUDA", "TERLALU BANYAK", "TERLALU DEKAT"))
    
    # Membuat grafik bar Plotly
    baru_faktor_resiko <- plot_ly(
      data_faktor_resiko,
      x = ~PERSENTASE,
      y = ~FAKTOR,
      text = ~paste(PERSENTASE, "%"),
      insidetextfont = list(color = '#FFFFFF'),
      type = "bar",
      orientation = 'h',
      marker = list(color = "#0d6efd")  # Warna bar
    ) 
    
    # Menambahkan label dan judul
    baru_faktor_resiko <- baru_faktor_resiko %>%
      layout(
        title = "Faktor Resiko KRS",
        xaxis = list(title = "PERSENTASE"),
        yaxis = list(title = ""),
        font = list(family = "Arial"),  # Mengatur jenis font global
        margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
        paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
        plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
        hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
        legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
        hovermode = "closest",  # Mengatur mode hover
        hoverdistance = 30,  # Mengatur jarak hover
        hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
        updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
      )
    baru_faktor_resiko %>%
      layout(
        bargap = 0.4
      ) %>%
      config(displayModeBar = FALSE) %>%
      config(displaylogo = FALSE) %>%
      config(scrollZoom = FALSE) %>%
      config(edits = list(editType = "plotly"))
    
  })
  
  output$jumlah_posyandu <- renderUI({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    
    total_posyandu <- keberadaan_posyandu %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
      group_by(PROVINSI) %>%
      count()
    
    d_s <- data_posyandu %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      select(PROVINSI, Persen_Ukur_Timbang) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, mean)
    
    value_box(
      title = "Balita diukur & ditimbang per Sasaran",
      value = paste0(round(d_s$Persen_Ukur_Timbang, 2), "%"),
      showcase = bs_icon("file-medical-fill"),
      p("Total Posyandu ", total_posyandu$n),
      theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
    )
  })
  
  output$jumlah_tpk_rekap <- renderUI({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    nama_pkb <- nama_pkb %>%
      filter(Kecamatan %in% kecamatan, Kelurahan %in% desa_kel)
    
    nama_tpk <- nama_tpk %>%
      filter(Kecamatan %in% kecamatan, Kelurahan %in% desa_kel)
    
    if(length(desa_kel) > 1){
      nama_pkb <- nama_pkb %>%
        summarise(n=n_distinct(paste(Kecamatan, `Nama PKB`)))
      
      nama_tpk <- nama_tpk %>%
        summarise(n=n_distinct(Register))
      
      vbs <- list(
        value_box(
          title = "Jumlah PKB",
          value = nama_pkb$n,
          showcase = bs_icon("person-x"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("PKB (Penyuluh Keluarga Berencana)")
        ),
        value_box(
          title = "Jumlah TPK",
          value = nama_tpk$n,
          showcase = bs_icon("person"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("TPK (Tim Pendamping Keluarga)")
        )
      )
      
      layout_column_wrap(
        width = "250px",
        !!!vbs
      )
    } else {
      jumlah_tpk <- nama_tpk %>%
        summarise(n=n_distinct(Register))
      
      nama_kader_pkk <- nama_tpk %>%
        filter(status == "PKK")
      nama_kader_pkk <- paste(nama_kader_pkk$`Nama Anggota`[1:nrow(nama_kader_pkk)], collapse = " & ")
      
      nama_bidan <- nama_tpk %>%
        filter(status == "Bidan")
      nama_bidan <- paste(nama_bidan$`Nama Anggota`[1:nrow(nama_bidan)], collapse = " & ") 
      
      nama_kader_kb <- nama_tpk %>%
        filter(status == "Kader KB")
      nama_kader_kb <- paste(nama_kader_kb$`Nama Anggota`[1:nrow(nama_kader_kb)], collapse = " & ") 
      
      vbs <- list(
        value_box(
          title = "Nama PKB",
          value =   h1(span(
            strong(nama_pkb$`Nama PKB`),
            style = "font-size:20px;")),
          showcase = bs_icon("person-x"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("Penyuluh Kelurga Berencana")
        ),
        value_box(
          title = "Jumlah TPK",
          value = h1(span(
            strong(jumlah_tpk$n),
            style = "font-size:20px;")),
          showcase = bs_icon("person"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("Tim Pendamping Keluarga")
        ),
        value_box(
          title = "Nama PKK",
          value = h1(span(
            strong(nama_kader_pkk),
            style = "font-size:20px;")),
          showcase = bs_icon("person"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("Pemberdayaan Kes. Keluarga")
        ),
        value_box(
          title = "Nama Bidan",
          value = h1(span(
            strong(nama_bidan),
            style = "font-size:20px;")),
          showcase = bs_icon("person"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("Tenaga Kesehatan")
        ),
        value_box(
          title = "Nama Kader KB",
          value = h1(span(
            strong(nama_kader_kb),
            style = "font-size:20px;")),
          showcase = bs_icon("person"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("Kader Keluarga Berencana")
        )
      )
      card(
        layout_columns(fillable = T, 
          vbs[[1]], vbs[[2]]
        ),
        layout_columns(fillable = T, 
         vbs[[3]], vbs[[4]], vbs[[5]]
        )
      )
    }
  })
  
  ubah_angka <- function(angka) {
    if (angka < 0 | is.infinite(angka) | is.nan(angka) | is.na(angka)) {
      return(0)
    } else {
      return(angka)
    }
  }
  
  output$pie_catin_rekap <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    
    data_pie_catin <- data_stunting %>%
      select(KECAMATAN, KELURAHAN, CATIN_SASARAN, CATIN_TERDAMPINGI, 
             CATIN_TIDAK_TERDAMPINGI,CATIN_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, BULAN == bulan)
    
    jumlah_sasaran <- sum(data_pie_catin$CATIN_SASARAN)
    terdamping_data_frame = sum(data_pie_catin$CATIN_TERDAMPINGI)
    tidak_terdamping_data_frame = sum(data_pie_catin$CATIN_TIDAK_TERDAMPINGI)
    
    terdamping_data_frame <- ubah_angka(terdamping_data_frame)
    tidak_terdamping_data_frame = ubah_angka(tidak_terdamping_data_frame)
    
    data_pie_catin <-data.frame(
      Kategori = c("Terdampingi", "Tidak Terdampingi"),
      Nilai = c(terdamping_data_frame, 
                tidak_terdamping_data_frame)
    )
    
    # Tentukan teks untuk setiap sektor
    #teks <- paste(data_pie$Kategori, data_pie$Nilai, sep = ": ")
    
    persentase_terdampingi <- paste0(ubah_angka(round(data_pie_catin$Nilai[1] / sum(data_pie_catin$Nilai) * 100, 1)), "%")
    
    # Tentukan warna untuk setiap kategori
    warna <- c("purple", "#404040")
    
    # Buat plot donat menggunakan plot_ly dengan argumen text untuk menampilkan teks
    plot_ly(data_pie_catin, labels = ~Kategori, values = ~Nilai, 
            type = "pie", hole = 0.6, sort = FALSE,
            textinfo = "none",
            texttemplate = "",
            hoverinfo = "none",
            hovertext = paste0(data_pie_catin$Kategori, ": ", data_pie_catin$Nilai),
            marker = list(colors = warna)) %>%
      add_annotations(text = persentase_terdampingi,
                      x = 0.5, y = 0.5, 
                      font = list(color = "#404040", size = 30),
                      showarrow = FALSE) %>%
      layout(title = "Persentase Pendampingan Catin", 
             xaxis = list(categoryorder = "array", categoryarray = c("Terdampingi", "Tidak Terdampingi")),
             showlegend = F,
             annotations = list(
               text = paste('Jumlah Sasaran:', jumlah_sasaran),
               x = 0.5,
               y = -0.2,
               xref = 'paper',
               yref = 'paper',
               showarrow = FALSE,
               font = list(size = 14)
             ))
    
  })
  
  output$legend_pie_catin_rekap <- renderUI({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    bulan_lalu <- bulan_sebelumnya_rekap()
    
    data_bulan_ini_catin <- data_stunting %>%
      select(KECAMATAN, KELURAHAN, CATIN_SASARAN, CATIN_TERDAMPINGI, 
             CATIN_TIDAK_TERDAMPINGI,CATIN_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, 
             BULAN %in% bulan)
    
    data_bulan_lalu_catin <- data_stunting %>%
      select(KECAMATAN, KELURAHAN, CATIN_SASARAN, CATIN_TERDAMPINGI, 
             CATIN_TIDAK_TERDAMPINGI,CATIN_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, 
             BULAN %in% bulan_lalu)
    
    terdampingi <- sum(data_bulan_ini_catin$CATIN_TERDAMPINGI)
    tidak_terdampingi <- sum(data_bulan_ini_catin$CATIN_TIDAK_TERDAMPINGI)
    beresiko <- sum(data_bulan_ini_catin$CATIN_BERESIKO)
    
    terdampingi_icon <- bandingkan_bulan_rekap(sum(data_bulan_ini_catin$CATIN_TERDAMPINGI),
                                               sum(data_bulan_lalu_catin$CATIN_TERDAMPINGI))
    
    tidak_terdampingi_icon <- bandingkan_bulan_rekap(sum(data_bulan_ini_catin$CATIN_TIDAK_TERDAMPINGI),
                                                     sum(data_bulan_lalu_catin$CATIN_TIDAK_TERDAMPINGI))
    
    beresiko_icon <- bandingkan_bulan_rekap(sum(data_bulan_ini_catin$CATIN_BERESIKO),
                                            sum(data_bulan_ini_catin$CATIN_BERESIKO))
    
    span(
      h6(
        span(
          bsicons::bs_icon(terdampingi_icon),
          style = "font-size:20px;color:purple"),
        span(strong("Terdampingi:", terdampingi), 
             style = "font-size:12px;color:purple"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        span(
          bsicons::bs_icon(tidak_terdampingi_icon),
          style = "font-size:20px;color:#404040"),
        span(strong("Tidak Terdampingi:", tidak_terdampingi), 
             style = "font-size:12px;color:#404040"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        style="text-align: center;"
      ),
      h6(
        span(
          bsicons::bs_icon(beresiko_icon),
          style = "font-size:20px;color:#800000"),
        span(strong("Beresiko:", beresiko), 
             style = "font-size:12px;color:#800000"),
        style="text-align: center;"
      )
    )
  })
  
  output$pie_bumil_rekap <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    
    data_pie_bumil <- data_stunting %>%
      select(KECAMATAN, KELURAHAN, BUMIL_SASARAN, BUMIL_TERDAMPINGI, 
             BUMIL_TIDAK_TERDAMPINGI,BUMIL_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, BULAN == bulan)
    
    jumlah_sasaran <- sum(data_pie_bumil$BUMIL_SASARAN)
    
    terdamping_data_frame = sum(data_pie_bumil$BUMIL_TERDAMPINGI)
    tidak_terdamping_data_frame = sum(data_pie_bumil$BUMIL_TIDAK_TERDAMPINGI)
    
    terdamping_data_frame <- ubah_angka(terdamping_data_frame)
    tidak_terdamping_data_frame = ubah_angka(tidak_terdamping_data_frame)
    
    data_pie_bumil <-data.frame(
      Kategori = c("Terdampingi", "Tidak Terdampingi"),
      Nilai = c(terdamping_data_frame, 
                tidak_terdamping_data_frame)
    )
    
    # Tentukan teks untuk setiap sektor
    #teks <- paste(data_pie$Kategori, data_pie$Nilai, sep = ": ")
    
    persentase_terdampingi <- paste0(ubah_angka(round(data_pie_bumil$Nilai[1] / sum(data_pie_bumil$Nilai) * 100, 1)), "%")
    
    # Tentukan warna untuk setiap kategori
    warna <- c("#0d6efd", "#404040")
    
    # Buat plot donat menggunakan plot_ly dengan argumen text untuk menampilkan teks
    plot_ly(data_pie_bumil, labels = ~Kategori, values = ~Nilai, 
            type = "pie", hole = 0.6, sort = FALSE,
            textinfo = "none",
            texttemplate = "",
            hoverinfo = "none",
            hovertext = paste0(data_pie_bumil$Kategori, ": ", data_pie_bumil$Nilai),
            marker = list(colors = warna)) %>%
      add_annotations(text = persentase_terdampingi,
                      x = 0.5, y = 0.5, 
                      font = list(color = "#404040", size = 30),
                      showarrow = FALSE) %>%
      layout(title = "Persentase Pendampingan Bumil", 
             xaxis = list(categoryorder = "array", categoryarray = c("Terdampingi", "Tidak Terdampingi")),
             showlegend = F,
             annotations = list(
               text = paste('Jumlah Sasaran:', jumlah_sasaran),
               x = 0.5,
               y = -0.2,
               xref = 'paper',
               yref = 'paper',
               showarrow = FALSE,
               font = list(size = 14)
             ))
    
  })
  
  output$legend_pie_bumil_rekap <- renderUI({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    bulan_lalu <- bulan_sebelumnya_rekap()
    
    data_bulan_ini_bumil <- data_stunting %>%
      select(KECAMATAN, KELURAHAN, BUMIL_SASARAN, BUMIL_TERDAMPINGI, 
             BUMIL_TIDAK_TERDAMPINGI,BUMIL_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, 
             BULAN %in% bulan)
    
    data_bulan_lalu_bumil <- data_stunting %>%
      select(KECAMATAN, KELURAHAN, BUMIL_SASARAN, BUMIL_TERDAMPINGI, 
             BUMIL_TIDAK_TERDAMPINGI,BUMIL_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, 
             BULAN %in% bulan_lalu)
    
    terdampingi <- sum(data_bulan_ini_bumil$BUMIL_TERDAMPINGI)
    tidak_terdampingi <- sum(data_bulan_ini_bumil$BUMIL_TIDAK_TERDAMPINGI)
    beresiko <- sum(data_bulan_ini_bumil$BUMIL_BERESIKO)
    
    terdampingi_icon <- bandingkan_bulan_rekap(sum(data_bulan_ini_bumil$BUMIL_TERDAMPINGI),
                                               sum(data_bulan_lalu_bumil$BUMIL_TERDAMPINGI))
    
    tidak_terdampingi_icon <- bandingkan_bulan_rekap(sum(data_bulan_ini_bumil$BUMIL_TIDAK_TERDAMPINGI),
                                                     sum(data_bulan_lalu_bumil$BUMIL_TIDAK_TERDAMPINGI))
    
    beresiko_icon <- bandingkan_bulan_rekap(sum(data_bulan_ini_bumil$BUMIL_BERESIKO),
                                            sum(data_bulan_ini_bumil$BUMIL_BERESIKO))
    
    span(
      h6(
        span(
          bsicons::bs_icon(terdampingi_icon),
          style = "font-size:20px;color:#0d6efd"),
        span(strong("Terdampingi:", terdampingi), 
             style = "font-size:12px;color:#0d6efd"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        span(
          bsicons::bs_icon(tidak_terdampingi_icon),
          style = "font-size:20px;color:#404040"),
        span(strong("Tidak Terdampingi:", tidak_terdampingi), 
             style = "font-size:12px;color:#404040"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        style="text-align: center;"
      ),
      h6(
        span(
          bsicons::bs_icon(beresiko_icon),
          style = "font-size:20px;color:#800000"),
        span(strong("Beresiko:", beresiko), 
             style = "font-size:12px;color:#800000"),
        style="text-align: center;"
      )
    )
  })
  
  output$pie_pascasalin_rekap <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    
    data_pie_pascasalin <- data_stunting %>%
      select(KECAMATAN, KELURAHAN, PASCASALIN_SASARAN, PASCASALIN_TERDAMPINGI, 
             PASCASALIN_TIDAK_TERDAMPINGI,PASCASALIN_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, BULAN == bulan)
    
    jumlah_sasaran <- sum(data_pie_pascasalin$PASCASALIN_SASARAN)
    terdamping_data_frame = sum(data_pie_pascasalin$PASCASALIN_TERDAMPINGI)
    tidak_terdamping_data_frame = sum(data_pie_pascasalin$PASCASALIN_TIDAK_TERDAMPINGI)
    
    terdamping_data_frame <- ubah_angka(terdamping_data_frame)
    tidak_terdamping_data_frame = ubah_angka(tidak_terdamping_data_frame)
    
    data_pie_pascasalin <-data.frame(
      Kategori = c("Terdampingi", "Tidak Terdampingi"),
      Nilai = c(terdamping_data_frame, 
                tidak_terdamping_data_frame)
    )
    
    # Tentukan teks untuk setiap sektor
    #teks <- paste(data_pie$Kategori, data_pie$Nilai, sep = ": ")
    
    persentase_terdampingi <- paste0(ubah_angka(round(data_pie_pascasalin$Nilai[1] / sum(data_pie_pascasalin$Nilai) * 100, 1)), "%")
    # Tentukan warna untuk setiap kategori
    warna <- c("#2eb857", "#404040")
    
    # Buat plot donat menggunakan plot_ly dengan argumen text untuk menampilkan teks
    plot_ly(data_pie_pascasalin, labels = ~Kategori, values = ~Nilai, 
            type = "pie", hole = 0.6, sort = FALSE,
            textinfo = "none",
            texttemplate = "",
            hoverinfo = "none",
            hovertext = paste0(data_pie_pascasalin$Kategori, ": ", data_pie_pascasalin$Nilai),
            marker = list(colors = warna)) %>%
      add_annotations(text = persentase_terdampingi,
                      x = 0.5, y = 0.5, 
                      font = list(color = "#404040", size = 30),
                      showarrow = FALSE) %>%
      layout(title = "Persentase Pendampingan Pascasalin", 
             xaxis = list(categoryorder = "array", categoryarray = c("Terdampingi", "Tidak Terdampingi")),
             showlegend = F,
             annotations = list(
               text = paste('Jumlah Sasaran:', jumlah_sasaran),
               x = 0.5,
               y = -0.2,
               xref = 'paper',
               yref = 'paper',
               showarrow = FALSE,
               font = list(size = 14)
             ))
    
  })
  
  output$legend_pie_pascasalin_rekap <- renderUI({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    bulan_lalu <- bulan_sebelumnya_rekap()
    
    data_bulan_ini_pascasalin <- data_stunting %>%
      select(KECAMATAN, KELURAHAN, PASCASALIN_SASARAN, PASCASALIN_TERDAMPINGI, 
             PASCASALIN_TIDAK_TERDAMPINGI,PASCASALIN_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, 
             BULAN %in% bulan)
    
    data_bulan_lalu_pascasalin <- data_stunting %>%
      select(KECAMATAN, KELURAHAN, PASCASALIN_SASARAN, PASCASALIN_TERDAMPINGI, 
             PASCASALIN_TIDAK_TERDAMPINGI,PASCASALIN_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, 
             BULAN %in% bulan_lalu)
    
    terdampingi <- sum(data_bulan_ini_pascasalin$PASCASALIN_TERDAMPINGI)
    tidak_terdampingi <- sum(data_bulan_ini_pascasalin$PASCASALIN_TIDAK_TERDAMPINGI)
    
    terdampingi <- ubah_angka(terdampingi)
    tidak_terdampingi = ubah_angka(tidak_terdampingi)
    
    beresiko <- sum(data_bulan_ini_pascasalin$PASCASALIN_BERESIKO)
    
    terdampingi_icon <- bandingkan_bulan_rekap(sum(data_bulan_ini_pascasalin$PASCASALIN_TERDAMPINGI),
                                               sum(data_bulan_lalu_pascasalin$PASCASALIN_TERDAMPINGI))
    
    tidak_terdampingi_icon <- bandingkan_bulan_rekap(sum(data_bulan_ini_pascasalin$PASCASALIN_TIDAK_TERDAMPINGI),
                                                     sum(data_bulan_lalu_pascasalin$PASCASALIN_TIDAK_TERDAMPINGI))
    
    beresiko_icon <- bandingkan_bulan_rekap(sum(data_bulan_ini_pascasalin$PASCASALIN_BERESIKO),
                                            sum(data_bulan_ini_pascasalin$PASCASALIN_BERESIKO))
    
    span(
      h6(
        span(
          bsicons::bs_icon(terdampingi_icon),
          style = "font-size:20px;color:#2eb857"),
        span(strong("Terdampingi:", terdampingi), 
             style = "font-size:12px;color:#2eb857"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        span(
          bsicons::bs_icon(tidak_terdampingi_icon),
          style = "font-size:20px;color:#404040"),
        span(strong("Tidak Terdampingi:", tidak_terdampingi), 
             style = "font-size:12px;color:#404040"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        style="text-align: center;"
      ),
      h6(
        span(
          bsicons::bs_icon(beresiko_icon),
          style = "font-size:20px;color:#800000"),
        span(strong("Beresiko:", beresiko), 
             style = "font-size:12px;color:#800000"),
        style="text-align: center;"
      )
    )
  })
  
  output$pie_baduta_rekap <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    
    data_pie_baduta <- data_stunting %>%
      select(KECAMATAN, KELURAHAN, BADUTA_SASARAN, BADUTA_TERDAMPINGI, 
             BADUTA_TIDAK_TERDAMPINGI,BADUTA_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, BULAN == bulan)
    
    jumlah_sasaran <- sum(data_pie_baduta$BADUTA_SASARAN)
    terdamping_data_frame = sum(data_pie_baduta$BADUTA_TERDAMPINGI)
    tidak_terdamping_data_frame = sum(data_pie_baduta$BADUTA_TIDAK_TERDAMPINGI)
    terdamping_data_frame <- ubah_angka(terdamping_data_frame)
    tidak_terdamping_data_frame = ubah_angka(tidak_terdamping_data_frame)
    
    data_pie_baduta <-data.frame(
      Kategori = c("Terdampingi", "Tidak Terdampingi"),
      Nilai = c(terdamping_data_frame, 
                tidak_terdamping_data_frame)
    )
    
    # Tentukan teks untuk setiap sektor
    #teks <- paste(data_pie$Kategori, data_pie$Nilai, sep = ": ")
    
    persentase_terdampingi <- paste0(ubah_angka(round(data_pie_baduta$Nilai[1] / sum(data_pie_baduta$Nilai) * 100, 1)), "%")
    
    # Tentukan warna untuk setiap kategori
    warna <- c("#f05e16", "#404040")
    
    # Buat plot donat menggunakan plot_ly dengan argumen text untuk menampilkan teks
    plot_ly(data_pie_baduta, labels = ~Kategori, values = ~Nilai, 
            type = "pie", hole = 0.6,  sort = FALSE,
            textinfo = "none",
            texttemplate = "",
            hoverinfo = "none",
            hovertext = paste0(data_pie_baduta$Kategori, ": ", data_pie_baduta$Nilai),
            marker = list(colors = warna)) %>%
      add_annotations(text = persentase_terdampingi,
                      x = 0.5, y = 0.5, 
                      font = list(color = "#404040", size = 30),
                      showarrow = FALSE) %>%
      layout(title = "Persentase Pendampingan Baduta", 
             xaxis = list(categoryorder = "array", categoryarray = c("Terdampingi", "Tidak Terdampingi")),
             showlegend = F,
             annotations = list(
               text = paste('Jumlah Sasaran:', jumlah_sasaran),
               x = 0.5,
               y = -0.2,
               xref = 'paper',
               yref = 'paper',
               showarrow = FALSE,
               font = list(size = 14)
             ))
    
  })
  
  output$legend_pie_baduta_rekap <- renderUI({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- action_rekap_bulan()
    bulan_lalu <- bulan_sebelumnya_rekap()
    
    data_bulan_ini_baduta <- data_stunting %>%
      select(KECAMATAN, KELURAHAN, BADUTA_SASARAN, BADUTA_TERDAMPINGI, 
             BADUTA_TIDAK_TERDAMPINGI,BADUTA_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, 
             BULAN %in% bulan)
    
    data_bulan_lalu_baduta <- data_stunting %>%
      select(KECAMATAN, KELURAHAN, BADUTA_SASARAN, BADUTA_TERDAMPINGI, 
             BADUTA_TIDAK_TERDAMPINGI,BADUTA_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, 
             BULAN %in% bulan_lalu)
    
    terdampingi <- sum(data_bulan_ini_baduta$BADUTA_TERDAMPINGI)
    tidak_terdampingi <- sum(data_bulan_ini_baduta$BADUTA_TIDAK_TERDAMPINGI)
    beresiko <- sum(data_bulan_ini_baduta$BADUTA_BERESIKO)
    
    terdampingi_icon <- bandingkan_bulan_rekap(sum(data_bulan_ini_baduta$BADUTA_TERDAMPINGI),
                                               sum(data_bulan_lalu_baduta$BADUTA_TERDAMPINGI))
    
    tidak_terdampingi_icon <- bandingkan_bulan_rekap(sum(data_bulan_ini_baduta$BADUTA_TIDAK_TERDAMPINGI),
                                                     sum(data_bulan_lalu_baduta$BADUTA_TIDAK_TERDAMPINGI))
    
    beresiko_icon <- bandingkan_bulan_rekap(sum(data_bulan_ini_baduta$BADUTA_BERESIKO),
                                            sum(data_bulan_ini_baduta$BADUTA_BERESIKO))
    
    span(
      h6(
        span(
          bsicons::bs_icon(terdampingi_icon),
          style = "font-size:20px;color:#f05e16"),
        span(strong("Terdampingi:", terdampingi), 
             style = "font-size:12px;color:#f05e16"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        span(
          bsicons::bs_icon(tidak_terdampingi_icon),
          style = "font-size:20px;color:#404040"),
        span(strong("Tidak Terdampingi:", tidak_terdampingi), 
             style = "font-size:12px;color:#404040"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        style="text-align: center;"
      ),
      h6(
        span(
          bsicons::bs_icon(beresiko_icon),
          style = "font-size:20px;color:#800000"),
        span(strong("Beresiko:", beresiko), 
             style = "font-size:12px;color:#800000"),
        style="text-align: center;"
      )
    )
  })
  
  buat_grafik_line_rekap <- function(data, judul="Masukkan Judul") {
    plot <- # Customized Multi Line plot using R
      plot_ly(data = data,x = ~BULAN,
              y = ~Sasaran, name = "Sasaran",
              type = "scatter", mode = "lines+markers",
              line=list(width=6, color="#0047ab"),
              marker=list(width=8, color="black")) %>%
      add_trace(y = ~Beresiko, name = "Beresiko",mode = "lines+markers",
                line = list(width = 5, color="#bd1628"),
                marker=list(width=8, color="black")) %>%
      add_trace(y = ~`Tidak Terdampingi`, name = "Tidak Terdampingi",mode = "lines+markers",
                line = list(width = 4, color="#0b6e4f"),
                marker=list(width=8, color="black")) %>%
      layout(title=judul,
             hovermode = "x unified", 
             legend = list(orientation = 'h', y = -0.2),
             yaxis = list(title = "Jumlah"))
    return(plot)
  }
  
  ## akhir fungsi membuat grafik line
  output$catin_line_rekap <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- sampai_bulan_rekap()
    
    data_line_catin <- data_stunting %>%
      select(PROVINSI, KABUPATEN, KECAMATAN, KELURAHAN, CATIN_SASARAN, CATIN_TERDAMPINGI, 
             CATIN_TIDAK_TERDAMPINGI,CATIN_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      group_by(PROVINSI, BULAN) %>%
      summarise_if(is.numeric, sum)
    
    data_line_catin$BULAN <- substr(data_line_catin$BULAN, 1, 3)
    data_line_catin$BULAN <- factor(data_line_catin$BULAN, 
                                    levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"), ordered = TRUE)
    
    # 
    # # Urutkan dataframe berdasarkan bulan
    data_line_catin <- data_line_catin[order(data_line_catin$BULAN), ]
    
    colnames(data_line_catin) <- c("PROSINSI", "BULAN", "Sasaran", "Terdampingi",  
                                   "Tidak Terdampingi", "Beresiko")
    buat_grafik_line_rekap(data_line_catin, "Progress Pendampingan Catin")
    
  })
  
  output$bumil_line_rekap <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- sampai_bulan_rekap()
    
    data_line_bumil <- data_stunting %>%
      select(PROVINSI, KABUPATEN, KECAMATAN, KELURAHAN, BUMIL_SASARAN, BUMIL_TERDAMPINGI, 
             BUMIL_TIDAK_TERDAMPINGI, BUMIL_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      group_by(PROVINSI, BULAN) %>%
      summarise_if(is.numeric, sum)
    data_line_bumil$BULAN <- substr(data_line_bumil$BULAN, 1, 3)
    data_line_bumil$BULAN <- factor(data_line_bumil$BULAN, 
                                    levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"), ordered = TRUE)
    
    # 
    # # Urutkan dataframe berdasarkan bulan
    data_line_bumil <- data_line_bumil[order(data_line_bumil$BULAN), ]
    
    colnames(data_line_bumil) <- c("PROSINSI", "BULAN", "Sasaran", "Terdampingi",  
                                   "Tidak Terdampingi", "Beresiko")
    
    buat_grafik_line_rekap(data_line_bumil, "Progress Pendampingan Bumil")
    
  })
  
  output$pascasalin_line_rekap <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- sampai_bulan_rekap()
    
    data_line_pascasalin <- data_stunting %>%
      select(PROVINSI, KABUPATEN, KECAMATAN, KELURAHAN, PASCASALIN_SASARAN, PASCASALIN_TERDAMPINGI, 
             PASCASALIN_TIDAK_TERDAMPINGI, PASCASALIN_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      group_by(PROVINSI, BULAN) %>%
      summarise_if(is.numeric, sum)
    
    data_line_pascasalin$BULAN <- substr(data_line_pascasalin$BULAN, 1, 3)
    data_line_pascasalin$BULAN <- factor(data_line_pascasalin$BULAN, 
                                    levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"), ordered = TRUE)
    
    # 
    # # Urutkan dataframe berdasarkan bulan
    data_line_pascasalin <- data_line_pascasalin[order(data_line_pascasalin$BULAN), ]
    
    colnames(data_line_pascasalin) <- c("PROSINSI", "BULAN", "Sasaran", "Terdampingi",  
                                   "Tidak Terdampingi", "Beresiko")
    buat_grafik_line_rekap(data_line_pascasalin, "Progress Pendampingan Pascasalin")
    
  })
  
  output$baduta_line_rekap <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- sampai_bulan_rekap()
    
    data_line_baduta <- data_stunting %>%
      select(PROVINSI, KABUPATEN, KECAMATAN, KELURAHAN, BADUTA_SASARAN, BADUTA_TERDAMPINGI, 
             BADUTA_TIDAK_TERDAMPINGI, BADUTA_BERESIKO, BULAN) %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      group_by(PROVINSI, BULAN) %>%
      summarise_if(is.numeric, sum)
    
    data_line_baduta$BULAN <- substr(data_line_baduta$BULAN, 1, 3)
    data_line_baduta$BULAN <- factor(data_line_baduta$BULAN, 
                                    levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"), ordered = TRUE)
    
    # 
    # # Urutkan dataframe berdasarkan bulan
    data_line_baduta <- data_line_baduta[order(data_line_baduta$BULAN), ]
    
    colnames(data_line_baduta) <- c("PROSINSI", "BULAN", "Sasaran", "Terdampingi",  
                                   "Tidak Terdampingi", "Beresiko")
    buat_grafik_line_rekap(data_line_baduta, "Progress Pendampingan Baduta")
  })
  
  output$posyandu_line_agregat <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- sampai_bulan_rekap()
    
    data_agregat_posyandu <- data_posyandu %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      group_by(PROVINSI, BULAN) %>%
      summarise_if(is.numeric, sum)
    
    data_agregat_posyandu$BULAN <- substr(data_agregat_posyandu$BULAN, 1, 3)
    data_agregat_posyandu$BULAN <- factor(data_agregat_posyandu$BULAN, 
                                          levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOVR", "DES"), ordered = TRUE)
    
    # 
    # # Urutkan dataframe berdasarkan bulan
    data_agregat_posyandu <- data_agregat_posyandu[order(data_agregat_posyandu$BULAN), ]
    
    # Customized Multi Line plot using R
    plot_ly(data = data_agregat_posyandu,x = ~BULAN,
            y = ~Stunting, name = "Stunting",
            type = "scatter", mode = "lines+markers",
            line=list(width=6, color="#bd1628"),
            marker=list(width=8, color="black")) %>%
      add_trace(y = ~Wasting, name = "Wasting",mode = "lines+markers",
                line = list(width = 5, color="#0047ab"),
                marker=list(width=8, color="black")) %>%
      add_trace(y = ~Underweight, name = "Underweight",mode = "lines+markers",
                line = list(width = 4, color="#0b6e4f"),
                marker=list(width=8, color="black")) %>%
      layout(title="Persentase Status Gizi Balita Per Bulan",
             hovermode = "x unified",
             legend = list(orientation = 'h', y = -0.2),
             yaxis = list(title = "Persentase(%)"))
    
  })
  
  output$posyandu_line_persentase <- renderPlotly({
    kecamatan <- kec_filter_rekap()
    desa_kel <- desa_filter_rekap()
    bulan <- sampai_bulan_rekap()
    
    data_agregat_posyandu <- data_posyandu %>%
      filter(KECAMATAN %in% kecamatan, 
             KELURAHAN %in% desa_kel, BULAN %in% bulan) %>%
      group_by(PROVINSI, BULAN) %>%
      summarise_if(is.numeric, sum) %>%
      mutate(
        Stunting = round(Stunting/Sasaran, 2),
        Wasting = round(Wasting/Sasaran, 2),
        Underweight =  round(Underweight/Sasaran, 2)
      )
    
    data_agregat_posyandu$BULAN <- substr(data_agregat_posyandu$BULAN, 1, 3)
    data_agregat_posyandu$BULAN <- factor(data_agregat_posyandu$BULAN, 
                                          levels = c("JAN", "FEB", "MAR", "APR", "MEI", "JUN", "JUL", "AGU", "SEP", "OKT", "NOV", "DES"), ordered = TRUE)
    
    # 
    # # Urutkan dataframe berdasarkan bulan
    data_agregat_posyandu <- data_agregat_posyandu[order(data_agregat_posyandu$BULAN), ]
    
    # Customized Multi Line plot using R
    plot_ly(data = data_agregat_posyandu,x = ~BULAN,
            y = ~Stunting, name = "Stunting",
            type = "scatter", mode = "lines+markers",
            line=list(width=6, color="#bd1628"),
            marker=list(width=8, color="black")) %>%
      add_trace(y = ~Wasting, name = "Wasting",mode = "lines+markers",
                line = list(width = 5, color="#0047ab"),
                marker=list(width=8, color="black")) %>%
      add_trace(y = ~Underweight, name = "Underweight",mode = "lines+markers",
                line = list(width = 4, color="#0b6e4f"),
                marker=list(width=8, color="black")) %>%
      layout(title="Persentase Status Gizi Balita Per Bulan",
             hovermode = "x unified", 
             legend = list(orientation = 'h', y = -0.2),
             yaxis = list(title = "Persentase(%)"))
  })
  
  
  
  output$cek_kec <- renderText({
      kecamatan <- kec_filter_rekap()
      desa_kel <- desa_filter_rekap()
      
      paste(kecamatan, desa_kel)
  })
  #akhir rekap
  
  #awal tabel data
  
  output$pilih_daftar_data <- renderUI({
    selectInput("dataset", "Pilih DataFrame:",
                choices = c("Pendidikan Dasar" = "titik_sd",
                            "Pendidikan Menengah" = "titik_smp",
                            "Kelompok Kegiatan" = "data_desa",
                            "Sumber Daya" = "data_sumber_daya",
                            "Kelompok Umur Laki-laki" = "kelompok_umur_lk",
                            "Kelompok Umur Perempuan" = "kelompok_umur_pr",
                            #"Petugas Kesehatan" = "bangke_lengkap",
                            #"Tim Pendamping Keluarga" = "nama_tpk",
                            #"Penyuluh KB" = "nama_pkb",
                            "Data Stunting" = "data_stunting",
                            "EPPGBM" = "data_posyandu"))
  })
  
  # output$pilih_tingkat_wil_data <- renderUI({
  #   selectInput(
  #     "pilih_data", "Tingkat Wilayah", 
  #     selectInput("level", "Pilih Tingkat Wilayah:",
  #                 choices = c("Provinsi", "Kabupaten", "Kecamatan", "Desa"))
  #   )
  # })
  # 
  # output$tampilkan_data <- renderUI({
  #   actionBttn(
  #     inputId = "tampilkan_data",
  #     label = "Tampilkan",
  #     style = "jelly", 
  #     color = "primary", size = "sm"
  #   )
  # })
  
  # Fungsi untuk memilih dataframe
  df <- reactive({
    switch(input$dataset,
           "titik_sd" = titik_sd,
           "titik_smp" = titik_smp,
           "data_desa" = data_desa,
           "data_sumber_daya" = data_sumber_daya,
           "kelompok_umur_lk" = kelompok_umur_lk,
           "kelompok_umur_pr" = kelompok_umur_pr,
           "bangke_lengkap" = bangke_lengkap,
           "data_stunting" = data_stunting,
           "data_posyandu" = data_posyandu,
           "nama_tpk" = nama_tpk,
           "nama_pkb" = nama_pkb)
  })
  
  output$tabel_data <- renderReactable({
    df = df()
    df <- df %>%
      mutate(No = row_number()) %>%
      relocate(No, .before = 1)
    
    change_column_names <- function(df) {
      # Mendapatkan nama kolom
      col_names <- colnames(df)
      
      # Mengonversi nama kolom menjadi capital case dan mengganti _ dengan spasi
      capital_col_names <- gsub("_", " ", toupper(col_names))
      
      # Mengatur nama kolom baru
      colnames(df) <- capital_col_names
      
      return(df)
    }
    
    df <- change_column_names(df)
    
    reactable(
      df,
      defaultColDef = colDef(
        align = "center",
        minWidth = 120,
        headerStyle = list(background = "#7393B3", color = "black")
      ),
      filterable = TRUE, minRows = 10,
      bordered = TRUE, striped = TRUE, highlight = TRUE,
      resizable = TRUE,
      theme = reactableTheme(
        borderColor = "#808080",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
      ))
  })
  
  # Fungsi untuk menghasilkan file yang akan diunduh
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df(), file)
    }
  )
  
  #akhir tabel data
} #akhir server

shinyApp(ui, server)