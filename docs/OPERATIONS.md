# Temiz Hava veri operasyonu

Bu belge, projeyi daha önce görmemiş birinin yıllık veriyi indirip kontrol
edebilmesi için yazıldı. Normal yıllık çalışma için aşağıdaki sırayı izlemek
yeterli. Sayfanın sonundaki script kataloğu, depodaki eski dosyaların neden hâlâ
durduğunu açıklar.

## Önce şu ayrımı bilin

Projede bugün iki veri hattı var:

1. **Ham veri ve karşılaştırma hattı** Bakanlık verisini Excel olarak indirir,
   doğrular ve tarihli bir PostgreSQL şemasına yazar. Eski veriyi koruyan,
   2024–2025 çalışmasında kullandığımız güncel hat budur.
2. **Raporlama hattı** `public` şemasındaki temizlenmiş tablolardan günlük,
   saatlik ve AQI analizleri üretir. Tarihli ham snapshot'ı henüz doğrudan
   okumaz. Bu nedenle snapshot oluşturmak, veriyi otomatik olarak yıllık analiz
   tablolarına taşımaz.

Bu ayrım önemli: `raw_YYYYMMDD` şeması arşiv ve karşılaştırma içindir;
`analysis_YYYY` şeması raporlama çıktısıdır. Aradaki `public` veri yayınlama
adımı için hâlâ legacy kod bulunduğundan, mevcut arşiv üzerinde otomatik
çalıştırılmamalıdır.

## 1. Çalışma ortamı

### Gerekli bileşenler

- R 4.5 ve paket bağımlılıkları
- Google Chrome
- RSelenium'ın başlattığı Selenium sunucusu
- PostgreSQL erişimi
- Ham dosyalar için repodan ayrı bir veri dizini

Bu iş istasyonunda R, ana kabuğa değil `temizhava-r` toolbox'ına kuruludur.
`R: command not found` görülürse:

```sh
podman start temizhava-r
podman exec -it \
  --workdir /home/byte/Work/temizhavaR \
  --env R_LIBS_USER=/home/byte/R/library/4.5 \
  --env LANG=C.UTF-8 \
  --env LC_ALL=C.UTF-8 \
  temizhava-r R
```

Başka bir makinede R doğrudan kuruluysa proje kökünde `R` komutu yeterlidir.
Türkçe istasyon adlarının bozulmaması için oturum UTF-8 locale ile açılmalıdır.

### Veritabanı ayarları

Ham veri dizininde bir `.env` dosyası bulunur. Şablon:

```sh
cp .env.example /mutlak/yol/TemizHava_base_dir/.env
```

Dosyada şu beş değişken doldurulur:

```text
POSTGRES_HOST
POSTGRES_PORT
TEMIZHAVA_DB
POSTGRES_TUSER
POSTGRES_TUSER_PASSWORD
```

Gerçek parola hiçbir zaman Git'e eklenmez veya mesajla paylaşılmaz.

### Her yeni R oturumunda

```r
pkgload::load_all(".")
options(temizhavaR.base_dir = "/home/byte/Work/TemizHava_base_dir")

con <- create_postgres_conn()
stopifnot(!is.null(con), DBI::dbIsValid(con))
disconnect_postgres(con)
```

Önce bir scripti `source()` edip sonra `load_all()` çalıştırmak aynı isimli
fonksiyonların çakışmasına yol açar. Böyle bir durumda en temiz çözüm R
oturumunu kapatıp yukarıdaki sırayla yeniden açmaktır.

Hazırlık ve bağlantı kontrolü normalde 2–5 dakika sürer.

## 2. Yıllık veriyi indirme

İndirme, canlı Bakanlık kataloğunu başlangıçta okur. Veritabanındaki katalogla
karşılaştırır; yeni, artık listelenmeyen ve etiketi değişen istasyonları ayrıca
raporlar.

Önce tek istasyonla deneyin:

```r
source("extdata/5_Download_Raw_Data.R")

download_temizhava_data(
  mode = "yearly",
  year = 2025,
  one_station = "İstanbul-Alibeyköy"
)
```

Tek istasyon günlük ve saatlik dosyalarıyla birlikte genellikle 2–5 dakika
sürer. Dosyalar ilgili şehir klasörüne geldiyse tam indirmeyi başlatın:

```r
download_temizhava_data(mode = "yearly", year = 2025)
download_temizhava_data(mode = "yearly", year = 2024)
```

2025 öncelikli olduğu için önce çalıştırılır. Bir yıllık tam tarama Bakanlık
sitesinin hızına göre uzun sürer; bu iş istasyonunda 2024 taraması yaklaşık 26
saat sürdü. Güvenli planlama payı **yıl başına 1–2 gün** olmalıdır.

İndirme yarıda kalırsa aynı komutu tekrar çalıştırın. Script:

- okunabilir günlük/saatlik Excel çiftlerini yeniden indirmez;
- doğrulanmış `no_data` işaretlerini varsayılan olarak tekrar sorgulamaz;
- eksik veya bozuk dosyadan devam eder.

Bakanlığın “veri yok” dediği kayıtları daha sonraki bir tarihte tekrar kontrol
etmek için:

```r
download_temizhava_data(
  mode = "yearly",
  year = 2025,
  recheck_no_data = TRUE
)
```

Bir istasyon grubunun veritabanında `Other` olması hata değildir. Güncel kod bu
istasyonlarda bölge filtresini boş bırakıp şehir ve Bakanlık istasyon kimliğiyle
seçim yapar. Eski `Other -> None` davranışı kullanılmaz.

### İndirme sonunda oluşanlar

Her istasyon için mümkünse dört Excel dosyası oluşur:

```text
ISTASYON_gunluk_detay_2025-2026.xlsx
ISTASYON_gunluk_ozet_2025-2026.xlsx
ISTASYON_saatlik_detay_2025-2026.xlsx
ISTASYON_saatlik_ozet_2025-2026.xlsx
```

Bakanlık veri döndürmezse Excel yerine sorgulanan istasyon kimliğini, dönemi ve
parametre sayısını içeren bir `*_no_data_*.txt` kanıt dosyası yazılır.

Kök veri dizininde ayrıca şunlar oluşur:

- `station_catalogue_YYYY.csv`: o çalışmada kullanılan tam katalog;
- `station_catalogue_changes_YYYY.md`: yeni, eksilen ve adı değişen istasyonlar.

Katalog raporundaki “yeni/eksilen” ifadesi iki katalog arasındaki farktır.
Bakanlık eklenme veya kaldırılma tarihi vermediği için değişimin tam olarak
hangi yıl gerçekleştiğini tek başına kanıtlamaz.

## 3. Dosyaları doğrulama

Tam indirme bittikten sonra DB'ye geçmeden önce içerik kontrolü zorunludur:

```r
source("extdata/validate_raw_downloads.R")

check_2025 <- validate_raw_downloads(2025, check_content = TRUE)
check_2024 <- validate_raw_downloads(2024, check_content = TRUE)
stopifnot(check_2025$ok, check_2024$ok)
```

Bu kontrol dosyanın yalnızca varlığına bakmaz. XLSX yapısını açar, tarihlerin
sıralı olduğunu ve istenen yıl sınırında kaldığını kontrol eder. `no_data`
dosyasını da istasyon kimliği, dönem ve parametre seçimiyle doğrular.

Başarılı sonuçta `incomplete_or_invalid` sayısı sıfır olmalıdır. Raporlar:

```text
raw_download_validation_2025.csv
raw_download_validation_2024.csv
```

Son kontrolde iki yılın ayrıntılı dosyalarını okumak birkaç dakika sürmüştür.
Disk yavaşsa daha uzun sürebilir.

2024 artık yıldır. Beklenen takvim 366 gün ve 8.784 saattir. 2025 için 365 gün
ve 8.760 saat kullanılır. Bitiş tarihi bir sonraki yılın 1 Ocak günüdür fakat
analizlerde bu sınır dahil edilmez.

## 4. PostgreSQL'e güvenli snapshot aktarımı

Her scrape için yeni bir şema adı seçin. Örneğin katalog 8 Eylül 2026'da
alındıysa `raw_20260908`:

```r
source("extdata/import_raw_snapshot.R")

import_raw_snapshot(
  years = c(2025L, 2024L),
  schema = "raw_20260908",
  scrape_date = as.Date("2026-09-08")
)
```

Aktarım başlamadan dosya doğrulaması tekrar yapılır. Her dosya transaction
içinde yazılır ve MD5 değeri `import_manifest` tablosuna kaydedilir. İşlem
kesilirse aynı komut tamamlanmış dosyaları atlayarak devam eder.

Bu işlem yalnız verilen `raw_...` şemasına yazar; `public.daily_detail`,
`public.hourly_detail` ve `public.location` tablolarını değiştirmez.

Bu çalışmada ölçülen süreler:

| Yıl | Dosya | DB satırı | Süre |
| --- | ---: | ---: | ---: |
| 2025 | 704 | 3.214.748 | 27 dakika |
| 2024 | 676 | 3.079.477 | 18 dakika |

Sunucu ve ağ yüküne göre süre değişebilir.

Snapshot içindeki temel tablolar:

| Tablo | İçerik |
| --- | --- |
| `catalogue_snapshot` | O gün görülen istasyon kataloğu |
| `daily_detail` | Günlük ayrıntı verisi |
| `hourly_detail` | Saatlik ayrıntı verisi |
| `no_data_status` | Veri-yok ve katalogda-yok kayıtları ile kanıtları |
| `import_manifest` | Dosya MD5'i, satır sayısı ve aktarım zamanı |

## 5. Snapshot'ı doğrulama

```r
source("extdata/validate_raw_snapshot.R")

snapshot_check <- validate_raw_snapshot(
  years = c(2025L, 2024L),
  schema = "raw_20260908"
)
stopifnot(snapshot_check$ok)
```

Kontrol; katalog, manifest ve gerçek tablo sayılarını karşılaştırır. Mükerrer
istasyon-zaman, saat başına oturmayan zaman, yıl dışı kayıt ve manifest satır
farkı arar. Milyonlarca satırlık mevcut snapshot'ta yaklaşık 45 saniye sürdü.

Başarılı çalışma sonunda bütün `passed` değerleri `TRUE` olmalıdır. Rapor:

```text
raw_snapshot_validation_raw_20260908.csv
```

## 6. Eski-yeni scrape ve doluluk eşiği

Aşağıdaki analiz bu çalışma için özeldir: eski `public` 2024 verisini yeni
snapshot'taki 2024 verisiyle karşılaştırır; yeni 2024/2025 ve eski 2024 için
doluluk oranlarını hesaplar.

```r
source("extdata/analyze_snapshot_comparison.R")

analyze_snapshot_comparison(
  schema = "raw_20260908",
  thresholds = c(75, 80, 85, 89.5, 90, 95)
)
```

Yaklaşık 20–30 saniye sürer. Payda, tabloda görülen satır sayısı değil yılın
beklenen gün/saat sayısıdır. Bu sayede eksik günler doluluğu yapay biçimde
yükseltmez.

Başlıca çıktılar:

- `scrape_and_threshold_summary.md`: kısa yönetici özeti;
- `scrape_comparison_2024_station.csv`: istasyon bazında eski-yeni farkı;
- `scrape_comparison_2024_parameter_summary.csv`: kirletici bazında fark;
- `data_availability_threshold_summary.csv`: tüm eşiklerin özeti;
- `data_availability_89_5_vs_90.csv`: %89,5 ve %90 karşılaştırması;
- `data_availability_89_5_band_stations.csv`: iki eşik arasında kalan kayıtlar.

Bu çıktılar hem veri dizinine yazılır hem de karşılık gelen rapor tabloları
snapshot şemasında tutulur.

`52 istasyon etkileniyor` ifadesi tek bir kirleticiyi anlatmaz. 2025'te tüm
kirleticiler ve günlük/saatlik veri birlikte ele alındığında %89,5–%90 bandında
70 istasyon-parametre-veri türü kaydı, tekilleştirildiğinde 52 istasyon vardır.

## 7. Yıllık analiz tabloları

Bu bölümdeki kod günceldir ancak girdisini `public` şemasından alır. Yeni
snapshot doğrulandı diye doğrudan çalıştırmayın. Önce kullanılacak ham verinin
`public` temizleme hattına nasıl yayınlanacağı ve eski verinin nasıl korunacağı
kararlaştırılmalıdır.

Gerekli yıl `public` içinde hazırlanmışsa sıralama şöyledir:

```r
options(temizhavaR.analysis_years = c(2025L, 2024L))
source("create_zcleaned_seasonal_table.R")

source("run_annual_analysis.R")
run_annual_analysis(2025, schema_name = "analysis_2025")
run_annual_analysis(2024, schema_name = "analysis_2024_v2")
```

İlk script kaynak yılın bütün takvim günlerini içerdiğini kontrol eder, sonra
yıla özel saatlik aykırı değer tablolarını oluşturur. İkinci script günlük,
saatlik ve AQI analizlerini ayrı bir şemaya yazar. Var olan analiz şemasının
üzerine yazmaz; tekrar çalıştırırken yeni bir şema adı gerekir.

Bu aşama mevcut snapshot aktarımında zamanlanmadı. Veri miktarına göre onlarca
dakika sürebilir; ilk çalıştırmada ayrı bir bakım penceresi ayırın ve çıkan
şemadaki tablo/satır sayılarını ayrıca kaydedin.

## 8. Güncel durum: 2024–2025 çalışması

8–9 Eylül 2026 tarihli çalışmada:

- `raw_20260908` şeması oluşturuldu;
- 1.380 dosya ve toplam 6.294.225 ayrıntı satırı aktarıldı;
- 32 DB kontrolünün tamamı geçti;
- eski `public` tabloları korunarak 2024 karşılaştırması yapıldı;
- canlı katalogda 17 yeni, 7 artık listelenmeyen ve 1 etiketi değişen kayıt
  raporlandı;
- Batman ve Van'ın eski 2024 verisi bulunduğu hâlde Bakanlık bugün aynı sorguya
  “veri yok” dönüyor. Bu, geçmişte veri olmadığı anlamına gelmez; arşiv erişimi
  sonradan değişmiş olabilir;
- `analysis_2024` mevcut, `analysis_2025` henüz oluşturulmuş değildir.

Ekiple paylaşılacak kopyalar veri dizinindeki
`TemizHava_paylasim_2024_2025` klasöründedir. Ham Excel ve `.env` dosyaları bu
klasöre konmaz.

## 9. Sorun olduğunda

### `download_check masks temizhavaR::download_check()`

Aynı oturumda script kaynaklandıktan sonra `load_all()` çağrılmıştır. R'yi
yeniden başlatın; önce `pkgload::load_all(".")`, sonra scriptleri kaynaklayın.

### Selenium bir istasyonda takıldı veya kapandı

Aynı yıl komutunu yeniden çalıştırın. Tam dosyalar atlanır. Sorun yalnız tek
istasyondaysa `one_station` ile deneyin. Üç tekrar sonunda hâlâ hata varsa
`raw_download_validation_YYYY.csv` içindeki `incomplete_or_invalid` kayıtlarını
inceleyin.

### Bakanlık “seçilen parametrelere göre veri bulunamadı” diyor

Güncel kod formdaki istasyon kimliğini, parametreleri ve tarihleri sorgudan önce
doğrular. Bunlar doğruysa `no_data` kanıtı üretir. Yine de eski snapshot'larda
veri olup olmadığını kontrol edin; Batman ve Van örneğinde Bakanlığın güncel
yanıtı ile eski arşiv farklıdır.

### Analiz şeması zaten var

Bu koruma bilinçlidir. Şemayı silmek yerine `analysis_2025_v2` gibi yeni bir ad
verin.

## 10. Script kataloğu

| Script | Durum | Ne zaman kullanılır? |
| --- | --- | --- |
| `extdata/5_Download_Raw_Data.R` | **Güncel** | Canlı katalog ve yıllık Excel indirmesi |
| `extdata/validate_raw_downloads.R` | **Güncel** | İndirmeden hemen sonra, DB aktarımından önce |
| `extdata/import_raw_snapshot.R` | **Güncel** | Doğrulanmış Excel'leri tarihli `raw_...` şemasına yazmak için |
| `extdata/validate_raw_snapshot.R` | **Güncel** | Snapshot aktarımından sonra zorunlu kontrol |
| `extdata/analyze_snapshot_comparison.R` | **Güncel / 2024–2025 özel** | Eski 2024 karşılaştırması ve eşik çalışması |
| `create_zcleaned_seasonal_table.R` | **Güncel / public girdili** | Yayınlanmış ham veriden yıllık saatlik temiz tablolar |
| `create_daily_zcleaned_seasonal_year.R` | **Güncel yardımcı** | Saatlik temiz tablodan yıla özel günlük tablo |
| `run_annual_analysis.R` | **Güncel giriş noktası** | Hazır `public` girdiden yıllık analiz şeması |
| `extdata/analysis/*.R` | **Güncel yardımcılar** | `run_annual_analysis.R` tarafından çağrılır; tek tek başlatılmaz |
| `extdata/audit_db_2024_2025.R` | **Salt okunur tanı** | Mevcut 2024/2025 DB durumunu incelemek için |
| `extdata/0_Create_DB_Schema.R` | **Legacy / yalnız boş DB** | Sıfırdan eski tip `public` şeması kurulumunda |
| `extdata/1_Retrieve_Station_Info.R` | **Legacy** | Eski katalog toplama ve doğrudan `location` güncellemesi |
| `extdata/2_Alter_Tables.R`–`4_Convert_Timestamp_Format.R` | **Legacy migration** | Eski DB biçimlerini dönüştürür; güncel DB'de çalıştırılmaz |
| `extdata/6_Excel_Files_Merge_Create_DB.R` | **Legacy / riskli** | Eski toplu yükleyici; aktif arşivde kullanılmaz |
| `extdata/6,5_Fix_Cities.R` | **Tek seferlik migration** | Eski Kırıkkale şehir düzeltmesi |
| `extdata/7_Match_Station_Types.R` | **Legacy / doğrudan yazar** | Eski Excel'den `location` metadata eşleme |
| `extdata/8_Clean_DB_Data.R` | **Legacy public temizleme** | Eski public hattı; snapshot üzerinde çalışmaz |
| `create_daily_zcleaned_table.R` | **Legacy / yıkıcı** | İsimsiz günlük temiz tabloyu düşürüp yeniden kurar |
| `create_daily_zcleaned_seasonal_table.R` | **Legacy / 2024 sabit** | Eski 2024 günlük üretimi; yeni yıllar için kullanılmaz |
| `extdata/Count_Stations/*`, `extdata/List_Stations/*` | **Legacy raporlar** | Sabit %90 yaklaşımı; yeni eşik karşılaştırmasının yerine kullanılmaz |
| `extdata/temp*.R`, `extdata/test.R`, `run.R` | **Geliştirici/artık** | Operasyon akışının parçası değildir |

Kural basit: yıllık rutin işte **Güncel** olarak işaretlenen giriş noktaları
dışındaki bir dosyayı doğrudan `source()` etmeden önce içindeki son satırları ve
DB yazma komutlarını kontrol edin. Eski scriptlerin çoğu kaynaklandığı anda
çalışmaya başlar.

### Bilinen paket bakım borcu

Paket build edilir ve operasyon dosyaları temiz oturumda yüklenir. Buna karşılık
tam `R CMD check`, yıllar içinde birikmiş eski örnekler ve Rd belgeleri nedeniyle
uyarı verir; bazı örnekler de canlı veritabanı bağlantısı bekler. Bu uyarılar
güncel indirme/snapshot hattının kontrollerinden ayrıdır. Paket dokümantasyonu
elden geçirilirken legacy örneklerin ayrıca temizlenmesi gerekir.
