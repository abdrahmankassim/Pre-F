library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Pre-F Inventory",
                  titleWidth = 200),
  
  dashboardSidebar(width = 200,
                   sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Carian..."),
                   fileInput("datamasuk", "Sila Masukkan Data Anda"),
                   hr(),
                   sidebarMenu(menuItem("Maklumat Data", tabName = "datainfo"),
                               menuItem("Data Utama", tabName = "maindata"),
                               menuItem("Rumusan Data", tabName = "summarydata"),
                               menuItem("Jadual", tabName = "Jadual",
                                         menuSubItem("Jadual 1", tabName = "J1"),
                                         menuSubItem("Jadual 2", tabName = "J2"),
                                         menuSubItem("Jadual 3", tabName = "J3"),
                                         menuSubItem("Jadual 4", tabName = "J4")),
                               menuItem("Opsyen Tebangan", tabName = "PT")),
                   hr(),
                   img(src="LogoKorporatFRIM.png", height=200, width=200)),
  
  dashboardBody(fluidPage(titlePanel("Pre-Felling Forest Inventory Data Analysis"),
                align = "center",
                tabItems( tabItem(tabName = "datainfo", "MAKLUMAT FAIL", tableOutput("DI")),
                          tabItem(tabName = "maindata", "DATA PRE-F", tableOutput("MD")),
                          tabItem(tabName = "summarydata", "RUMUSAN DATA PRE-F", tableOutput("SD")),
                          tabName = "J1", "Jadual 1 : Anggaran bukan kumulatif bilangan pokok dan isipadu (kasar & bersih) serta keluasan pangkal (basal area) sehektar mengikut kelas diameter dan kumpulan spesies", tableOutput("J1")),
                          tabItem(tabName = "J2", "JADUAL 2 : ANGGARAN KUMULATIF BILANGAN POKOK DAN ISIPADU (KASAR & BERSIH)SERTA KELUASAN PANGKAL (BASAL AREA) SEHEKTAR MENGIKUT KELAS DIAMETER DAN KUMPULAN SPESIES.", tableOutput("J2")),
                          tabItem(tabName = "J3", "JADUAL 3 : OPSYEN HAD TEBANGAN TANPA MENGAMBILKIRA POKOK-POKOK DARI KELAS DIAMETER 15 - 30 CM", tableOutput("J3")),
                          tabItem(tabName = "J4", "JADUAL 4 : OPSYEN HAD TEBANGAN DENGAN MENGAMBILKIRA POKOK-POKOK DARI KELAS DIAMETER 15 - 30 CM", tableOutput("J4")),
                                          TABITEM(TABNAME = "PT", "OPSYEN HAD TEBANGAN : Pemilihan mengikut syarat  SMS ", tableOutput("PT")))))
)
tabItem(tabName = "PT", "OPSYEN HAD TEBANGAN : Pemilihan mengikut syarat  SMS ", tableOutput("PT"))
