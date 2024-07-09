library(shiny)
library(DT) # Untuk menampilkan dataframe

# Membuat dataframe contoh
df2 <- data.frame(
  Prov = c("A", "A", "B", "B", "B", "C"),
  Kab = c("Kab1", "Kab1", "Kab2", "Kab2", "Kab3", "Kab3"),
  Kec = c("Kec1", "Kec2", "Kec3", "Kec4", "Kec5", "Kec6"),
  Desa = c("Desa1", "Desa2", "Desa3", "Desa4", "Desa5", "Desa6"),
  Value = c(10, 20, 30, 40, 50, 60)
)

df1 <- data.frame(
  Prov = c("D", "D", "D", "E", "E", "F"),
  Kab = c("Kab1", "Kab1", "Kab2", "Kab2", "Kab3", "Kab3"),
  Kec = c("Kec1", "Kec2", "Kec3", "Kec4", "Kec5", "Kec6"),
  Desa = c("Desa1", "Desa2", "Desa3", "Desa4", "Desa5", "Desa6"),
  Value = c(10, 20, 30, 40, 50, 60)
)

# UI
ui <- fluidPage(
  titlePanel("Filter dan Agregasi Dataframe dengan Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Pilih DataFrame:",
                  choices = c("DataFrame 1" = "df1",
                              "DataFrame 2" = "df2")),
      selectInput("level", "Pilih Tingkat Wilayah:",
                  choices = c("Provinsi", "Kabupaten", "Kecamatan", "Desa"))
    ),
    
    mainPanel(
      DTOutput("table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Fungsi untuk membuat filter dinamis berdasarkan tingkat wilayah yang dipilih
  # output$filter_ui <- renderUI({
  #   level <- input$level
  #   
  #   if (level == "Provinsi") {
  #     selectInput("filter", "Pilih Provinsi:", choices = unique(df$Prov))
  #   } else if (level == "Kabupaten") {
  #     selectInput("filter", "Pilih Kabupaten:", choices = unique(df$Kab))
  #   } else if (level == "Kecamatan") {
  #     selectInput("filter", "Pilih Kecamatan:", choices = unique(df$Kec))
  #   } else if (level == "Desa") {
  #     NULL
  #   }
  # })
  
  # Fungsi untuk memilih dataframe
  df <- reactive({
    switch(input$dataset,
           "df1" = df1,
           "df2" = df2)
  })
  
  # Fungsi untuk melakukan agregasi berdasarkan tingkat wilayah yang dipilih
  aggregated_df <- reactive({
    df = df()
    level <- input$level
    
    if (level == "Provinsi") {
      aggregate(Value ~ Prov, data = df, FUN = sum)
    } else if (level == "Kabupaten") {
      aggregate(Value ~ Prov + Kab, data = df, FUN = sum)
    } else if (level == "Kecamatan") {
      aggregate(Value ~ Prov + Kab + Kec, data = df, FUN = sum)
    } else {
      aggregate(Value ~ Prov + Kab + Kec + Desa, data = df, FUN = sum)
    }
  })
  
  # Fungsi untuk melakukan filter berdasarkan pilihan pengguna
  # filtered_df <- reactive({
  #   level <- input$level
  #   filter_value <- input$filter
  #   
  #   if (level == "Provinsi") {
  #     filter(df, Prov == filter_value)
  #   } else if (level == "Kabupaten") {
  #     filter(df, Kab == filter_value)
  #   } else if (level == "Kecamatan") {
  #     filter(df, Kec == filter_value)
  #   } else if (level == "Desa") {
  #     filter(df, Desa == filter_value)
  #   }
  # })
  
  # Menampilkan hasil dataframe sesuai dengan pilihan pengguna
  output$table <- renderDT({
    if (input$level == "Desa") {
      datatable(aggregated_df())
    } else {
      datatable(aggregated_df())
    }
  })
}

# Menjalankan aplikasi Shiny
shinyApp(ui = ui, server = server)
