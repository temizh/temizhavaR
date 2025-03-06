# temizhavaR

# Ham Veriler
Hava kalitesi ham verileri https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew 
adresinden indirilmektedir.

Yeni DB yaratmak için : 
.Rprofile dosyasına ham verilerin bulunduğu dizin ismini belirtin

`options(temizhavaR.base_dir = "..../HamVeriler_2022")`

Bu dizinin içine `init.temizhavaR_data.R` adlı dosya yaratın. Örnek dosya :
`extdata/init.temizhavaR_data.R.sample`

Veritabanı şemalarını yaratmak için `SQLite.R` adlı script'i çalıştırın.


```
library(temizhavaR)
create_SQL_schema()
```


## Eksik dosya analizi

```
extdata/eksik_dosyalar_analizi.R
```

## SQLite veritabani şeması

### 2022
`make_location_table_2022.R` dosyasinda bulunan `create_location_table_2022()` fonksiyonu
2023 verilerini baz alip location_2022 tablosuna kaydedin.

## Gunluk verileri iceren Excel dosyalarını SQLite veritabanına kaydedilmesi :
Excel verilerini elle SQL veritabanına yazmak için save_excel_to_database.R 
script dosyasını kullanın. 

Mesela sadece "Erzurum - Palandöken" istasyonunu yazmak isterseniz location tablosunu
aşağıdakini benzer bir komutla filtreleyebilirsiniz :

```  
locationT <- locationT[grep("Erzurum - Palandöken", locationT$Istasyonlar),]
```


## 2022 verileri
2022 ham verileri elle indirildiği zaman halihazırda mevcut olmayan bir formatta 
indirmek mümkün idi. Bu formatta bakanlığın web sitesinden bir şehre ait bütün 
istasyonları tek seferde tek bir Excel dosyasında indirmek mümkün idi. 

temizhavaR paketindeki veri ithal fonksiyonları bu veri tipini desteklememektedir. 
O yüzden istisnai olarak 2022 verileri için `extdata/convert 2022 to 2023.R` 
dosyasındaki script ile 2022 formatı (bir şehre ait bütün istasyonlar tek Excel dosyasında) 2023 formatına 
yeni formata (her istasyon için ayrı Excel dosyası) çevrilmiştir. Bu çevrimden sonra
`detail_savesqlite.R` dosyasinda bulunan `detail_save_to_database()` 
fonksiyonu ile günlük ve saatlik verileri SQLite veritabanına yazılmıştır.

## Analizler

Analizleri yapan scriptler aşağıdaki dizinlerde bulunmaktadır : 
- extdata/daily_detail
- extdata/hourly_detail

