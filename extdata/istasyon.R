#ŞEHİRLERİN İSTASYONLARINI BİRLEŞTİRİP TEK BİR EXCEL DOSYASI ALTINDA TOPLAMA
library(readxl)
library(openxlsx)

klasor_yolu <- "C:/Adana/"

dosya_listesi <- list.files(klasor_yolu, pattern = "\\.xlsx$", full.names = TRUE)

veri_listesi <- lapply(dosya_listesi, function(dosya) {
  read_excel(dosya)
})

# Sadece verileri birleştirin
birlesik_veri <- do.call(cbind, veri_listesi)

# Excel dosyasını oluştur
wb <- createWorkbook()

# Birleştirilmiş veriyi bir sayfaya ekleyin
addWorksheet(wb, "Birlesik_Veri")
writeData(wb, sheet = "Birlesik_Veri", x = birlesik_veri, startCol = 1, startRow = 1)

# Sütun genişliklerini belirli bir değere ayarlayın (örneğin, 20)
setColWidths(wb, sheet = "Birlesik_Veri", cols = 1:ncol(birlesik_veri), widths = 20)

#head(birlesik_veri)


birlesik_dosya_yolu <- paste0(klasor_yolu, "Adana.xlsx")
saveWorkbook(wb, file = birlesik_dosya_yolu)
