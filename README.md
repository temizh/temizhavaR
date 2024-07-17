# temizhavaR

Yeni DB yaratmak için : 
.Rprofile dosyasına ham verilerin bulunduğu dizin ismini belirtin

`options(temizhavaR.raw_dir = "..../HamVeriler_2022")`

Bu dizinin içine `init.temizhavaR_data.R` adlı dosya yaratın. Örnek dosya :
`extdata/init.temizhavaR_data.R.sample`

Database tablo şemalarını yaratın. Script ? `SQLite.R`


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
2023 verilerini baz alip location_2022 tablosuna kaydedilmiştir..

## Gunluk verileri iceren Excel dosyalarını SQLite veritabanına kaydedilmesi :

`extdata/convert 2022 to 2023.R` dosyasındaki script ile 2022 formatı (bir şehre ait bütün istasyonlar tek Excel dosyasında) 2023 formatına (her istasyon için ayrı Excel dosyası) çevrilmiştir.

`save_excel_to_database.R` dosyasinda bulunan `detail_save_to_database()` fonksiyonu ile 2022 günlük ve saatlik verileri SQLite veritabanına yazılmıştır.

## Analizler

Analizleri yapan scriptler aşağıdaki dizinlerde bulunmaktadır : 
- extdata/daily_detail
- extdata/hourly_detail

## Güncelleme

- write_to_excel() fonksiyonunu çağırdıktan sonra 'Error in output[[I]]$data[, 1] <- "" : matriste hatalı sayıda altindis hatası' hata mesajı geri dönüyordu.Düzeltildi.

- exdata/daily_detail dizini altında olan NO2_daily_detail.R,PM2.5_daily_detail.R ve Pm10_daily_detail.R Script dosyaları istenilen verileri geri döndürmüyordu.Düzeltildi.

- exdata/hourly_detail dizini altındaki bütün R script dosyaları düzgün çalışmıyordu ve istenilen verileri excel tablosuna aktarmıyordu.Düzeltildi.



