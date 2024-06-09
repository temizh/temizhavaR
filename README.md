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

Eksik dosya analizi
```
extdata/eksik_dosyalar_analizi.R
```


`make_location_table_2022.R` dosyasinda bulunan `create_location_table_2022()` fonksiyonu
2023 verilerini baz alip location_2022 tablosuna kaydeder.

Gunluk verileri iceren Excel dosyalarını SQLite veritabanına kaydedilmesi

```
daily_detail_save_to_database()
```
