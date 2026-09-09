# temizhavaR

Türkiye hava kalitesi verilerini Bakanlığın SIM sistemi üzerinden indirir,
PostgreSQL'de saklar ve yıllık analiz tabloları üretir.

Bu depo iki ayrı iş akışı içerir:

- **Güncel ham veri akışı:** indirme, dosya doğrulama, sürümlü snapshot'a
  aktarma ve eski-yeni karşılaştırması. 2024 ve 2025 için doğrulanmıştır.
- **Yıllık analiz akışı:** `public` şemasındaki temizlenmiş tablolardan günlük,
  saatlik ve AQI tablolarını üretir. Bu akış snapshot'ı doğrudan okumaz.

İlk kez çalıştıracaksanız veya süreci devralıyorsanız önce
[Operasyon Rehberi](docs/OPERATIONS.md) dosyasını okuyun. Kurulumdan son
kontrole kadar kullanılacak komutlar, yaklaşık süreler, çıktıların anlamı ve
eski scriptlerin durumu orada yer alıyor.

## Kısa yol: 2024 ve 2025 ham veri akışı

R oturumunu proje kökünde açın:

```r
pkgload::load_all(".")
options(temizhavaR.base_dir = "/mutlak/yol/TemizHava_base_dir")
```

Ardından sırasıyla:

```r
source("extdata/5_Download_Raw_Data.R")
download_temizhava_data(mode = "yearly", year = 2025)
download_temizhava_data(mode = "yearly", year = 2024)

source("extdata/validate_raw_downloads.R")
stopifnot(validate_raw_downloads(2025, check_content = TRUE)$ok)
stopifnot(validate_raw_downloads(2024, check_content = TRUE)$ok)

source("extdata/import_raw_snapshot.R")
import_raw_snapshot(
  years = c(2025L, 2024L),
  schema = "raw_YYYYMMDD"
)

source("extdata/validate_raw_snapshot.R")
stopifnot(validate_raw_snapshot(schema = "raw_YYYYMMDD")$ok)
```

`YYYYMMDD`, indirme kataloğunun alındığı tarihtir. Her yeni indirmede yeni bir
şema adı kullanın. Böylece önceki ham veri korunur.

## Güvenlik notu

`extdata/6_Excel_Files_Merge_Create_DB.R` legacy yükleyicidir. Fonksiyonları
korunmuştur ancak aktif arşiv üzerinde kullanılmamalıdır; `public` ham
tablolarını topluca değiştirebilir. Güncel yol `import_raw_snapshot()`
fonksiyonudur.

Gerçek `.env` dosyaları repoya eklenmez. Örnek değişkenler için
[.env.example](.env.example) kullanılmalıdır.

## Veri kaynağı

<https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew>
