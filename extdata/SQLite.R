library(DBI)
library(readxl)
library(uuid)
#library(RSQLite)

# create a connection to database
mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")



 dbDisconnect(mydb)

 hourly_detail <- dbReadTable(mydb, "hourly_detail")
 location_manuel <- dbReadTable(mydb, "location_manuel")
 daily_detail <- dbReadTable(mydb, "daily_detail")
 location_2023 <- dbReadTable(mydb, "location_2023")
 dbExecute(mydb, "ALTER TABLE location_deneme RENAME TO location_2023;")

 location_deneme_first_20 <- dbGetQuery(mydb, "SELECT * FROM location_deneme LIMIT 20")
# İlk 20 satırı görmek için yazdır
 print(location_deneme_first_20)


 #LOCATION 2023
mydb_location <- "
CREATE TABLE location (
    Bolge TEXT,
    Sehir TEXT,
    Plaka TEXT,
    Istasyonlar TEXT,
    Sehir_Istasyon TEXT,
    Id TEXT PRIMARY KEY
);
"
mydb_hourly_detail <- "
CREATE TABLE hourly_detail (
    Istasyon TEXT,
    location_id TEXT,
    Tarih DATETIME,
    PM10 DOUBLE,
    \"PM2.5\" DOUBLE,
    SO2 DOUBLE,
    CO DOUBLE,
    NO2 DOUBLE,
    NOX DOUBLE,
    NO DOUBLE,
    O3 DOUBLE

);
"

mydb_daily_detail <- "
CREATE TABLE daily_detail(
   Istasyon TEXT,
    location_id TEXT,
    Tarih DATETIME,
    PM10 DOUBLE,
    \"PM2.5\" DOUBLE,
    SO2 DOUBLE,
    CO DOUBLE,
    NO2 DOUBLE,
    NOX DOUBLE,
    NO DOUBLE,
    O3 DOUBLE
);
"

dbExecute(mydb, "
CREATE TABLE IF NOT EXISTS location_deneme (
    Bolge TEXT,
    Sehir TEXT,
    Plaka TEXT,
    Istasyonlar TEXT,
    Id TEXT PRIMARY KEY
);
")

# CREATE TABLE
 dbExecute(mydb, mydb_location)
 dbExecute(mydb, mydb_hourly_detail)

 dbExecute(mydb, mydb_daily_detail)

#

# delete table
dbExecute(mydb, "DROP TABLE IF EXISTS hourly_detail")


location_data <- read_excel("C:/Users/Hp/Desktop/location_veri_ayri.xlsx")
location_data$Id <- sapply(1:nrow(location_data), function(i) as.character(UUIDgenerate()))


dbWriteTable(mydb, "location", location_data, append = TRUE, row.names = FALSE)

query_result1 <- dbGetQuery(mydb, "SELECT * FROM hourly_detail LIMIT 10")


print(query_result1)

# Tablo içeriğini sorgulayın
# query <- "SELECT * FROM hourly_detail"
 result <- dbGetQuery(mydb, query)

query_step1 <- "ALTER TABLE hourly_detail ADD COLUMN location_id INTEGER;"
dbExecute(mydb, query_step1)

# sonrasında burdan itibaren bu adımı öbür tablolar için de yap.

query_step2 <- "
UPDATE hourly_detail
SET location_id = (
    SELECT Id FROM location WHERE location.Istasyonlar = hourly_detail.Istasyon
);"
dbExecute(mydb, query_step2)

# Adım 3: location tablosundan istasyonun diğer bilgilerini çekmek için sorgu
query_step3 <- "
SELECT hourly_detail.Istasyon, hourly_detail.Tarih, hourly_detail.PM10, hourly_detail.\"PM2.5\", hourly_detail.SO2, hourly_detail.NO2, hourly_detail.NOX, hourly_detail.NO, hourly_detail.O3, location.Sehir, location.Plaka
FROM hourly_detail
JOIN location ON hourly_detail.location_id = location.Id;"
dbGetQuery(mydb, query_step3)










#  ('AKDENIZ THM', 'ADANA', '01', 'Catalan'),
      #  ('AKDENIZ THM', 'ADANA', '01', 'Cukurova'),
      #  ('AKDENIZ THM', 'ADANA', '01', 'Dogankent'),
      #  ('AKDENIZ THM', 'ADANA', '01', 'Meteoroloji'),
      #  ('AKDENIZ THM', 'ADANA', '01', 'Valilik'),
      #  ('AKDENIZ THM', 'ADANA', '01', 'Seyhan'),
      #  ('AKDENIZ THM', 'ADANA', '01', 'Turhan Cemal Beriker Bulvari'),
      #  ('AKDENIZ THM', 'ADANA', '01', 'Yakapinar'),
      #  ('AKDENIZ THM', 'GAZIANTEP', '27', 'Gaziantep'),
      #  ('AKDENIZ THM', 'GAZIANTEP', '27', 'Beydilli'),
      #  ('AKDENIZ THM', 'GAZIANTEP', '27', 'Fevzi Cakmak'),
      #  ('AKDENIZ THM', 'GAZIANTEP', '27', 'Gaski D6'),
      #  ('AKDENIZ THM', 'GAZIANTEP', '27', 'Nizip'),
      #  ('AKDENIZ THM', 'GAZIANTEP', '27', 'Atapark'),
      #  ('AKDENIZ THM', 'HATAY', '31', 'Antakya'),
      #  ('AKDENIZ THM', 'HATAY', '31', 'Iskenderun'),
      #  ('AKDENIZ THM', 'HATAY', '31', 'Iskenderun Merkez'),
      #  ('AKDENIZ THM', 'HATAY', '31', 'Vali Kavsagi'),
      #  ('AKDENIZ THM', 'KAHRAMANMARAS', '46', 'Dulkadiroglu'),
      #  ('AKDENIZ THM', 'KAHRAMANMARAS', '46', 'Elbistan'),
      #  ('AKDENIZ THM', 'KAHRAMANMARAS', '46', 'Kent Meydani'),
      #  ('AKDENIZ THM', 'KAHRAMANMARAS', '46', 'Onikisubat'),
      #  ('AKDENIZ THM', 'KILIS', '78', 'Kilis'),
      #  ('AKDENIZ THM', 'MERSIN', '33', 'Akdeniz'),
      #  ('AKDENIZ THM', 'MERSIN', '33', 'Huzurkent'),
      #  ('AKDENIZ THM', 'MERSIN', '33', 'Istiklal Cad'),
      #  ('AKDENIZ THM', 'MERSIN', '33', 'Tarsus'),
      #  ('AKDENIZ THM', 'MERSIN', '33', 'Tasucu'),
      #  ('AKDENIZ THM', 'MERSIN', '33', 'Toroslar'),
      #  ('AKDENIZ THM', 'OSMANIYE', '80', 'Osmaniye'),
      #  ('AKDENIZ THM', 'OSMANIYE', '80', 'Kadirli'),
      #  ('DOGU ANADOLU THM', 'AGRI', '04', 'Agri'),
      #  ('DOGU ANADOLU THM', 'AGRI', '04', 'Dogu Beyazit'),
      #  ('DOGU ANADOLU THM', 'AGRI', '04', 'Patnos'),
      #  ('DOGU ANADOLU THM', 'ARDAHAN', '75', 'Ardahan'),
      #  ('DOGU ANADOLU THM', 'ARTVIN', '08', 'Artvin'),
      #  ('DOGU ANADOLU THM', 'BAYBURT', '69', 'Bayburt'),
      #  ('DOGU ANADOLU THM', 'ERZINCAN', '24', 'Erzincan'),
      #  ('DOGU ANADOLU THM', 'ERZINCAN', '24', 'Trafik'),
      #  ('DOGU ANADOLU THM', 'ERZURUM', '25', 'Erzurum'),
      #  ('DOGU ANADOLU THM', 'ERZURUM', '25', 'Aziziye'),
      #  ('DOGU ANADOLU THM', 'ERZURUM', '25', 'Palandoken'),
      #  ('DOGU ANADOLU THM', 'ERZURUM', '25', 'Pasinler'),
      #  ('DOGU ANADOLU THM', 'ERZURUM', '25', 'Tashan'),
      #  ('DOGU ANADOLU THM', 'GUMUSHANE', '29', 'Gumushane'),
      #  ('DOGU ANADOLU THM', 'IGDIR', '76', 'Igdir'),
      #  ('DOGU ANADOLU THM', 'IGDIR', '76', 'Aralik'),
      #  ('DOGU ANADOLU THM', 'KARS', '36', 'Istasyon Mah'),
      #  ('DOGU ANADOLU THM', 'KARS', '36', 'Trafik'),
      #  ('DOGU ANADOLU THM', 'RIZE', '53', 'Rize'),
      #  ('DOGU ANADOLU THM', 'RIZE', '53', 'Ardesen'),
      #  ('DOGU ANADOLU THM', 'TRABZON', '61', 'Akcaabat'),
      #  ('DOGU ANADOLU THM', 'TRABZON', '61', 'Besirli'),
      #  ('DOGU ANADOLU THM', 'TRABZON', '61', 'Fatih'),
      #  ('DOGU ANADOLU THM', 'TRABZON', '61', 'Meydan'),
      #  ('DOGU ANADOLU THM', 'TRABZON', '61', 'Uzungol'),
      #  ('DOGU ANADOLU THM', 'TRABZON', '61', 'Valilik'),
      #  ('EGE THM', 'AYDIN', '07', 'Aydin'),
      #  ('EGE THM', 'AYDIN', '07', 'Didim'),
      #  ('EGE THM', 'AYDIN', '07', 'Efeler'),
      #  ('EGE THM', 'AYDIN', '07', 'Germencik'),
      #  ('EGE THM', 'AYDIN', '07', 'Nazilli'),
      #  ('EGE THM', 'AYDIN', '07', 'Soke'),
      #  ('EGE THM', 'AYDIN', '07', 'Trafik'),
      #  ('EGE THM', 'DENIZLI', '20', 'Bayramyeri'),
      #  ('EGE THM', 'DENIZLI', '20', 'Civril'),
      #  ('EGE THM', 'DENIZLI', '20', 'Honaz'),
      #  ('EGE THM', 'DENIZLI', '20', 'Merkezefendi'),
      #  ('EGE THM', 'DENIZLI', '20', 'Sumer'),
      #  ('EGE THM', 'DENIZLI', '20', 'Trafik'),
      #  ('EGE THM', 'DENIZLI', '20', 'Saraykoy'),
      #  ('EGE THM', 'IZMIR', '35', 'Seferihisar'),
      #  ('EGE THM', 'IZMIR', '35', 'Aliaga'),
      #  ('EGE THM', 'IZMIR', '35', 'Aliaga Bozkoy'),
      #  ('EGE THM', 'IZMIR', '35', 'Alsancak IBB'),
      #  ('EGE THM', 'IZMIR', '35', 'Bayrakli IBB'),
      #  ('EGE THM', 'IZMIR', '35', 'Bergama IBB'),
      # ('EGE THM', 'IZMIR', '35', 'Bornova'),
      # ('EGE THM', 'IZMIR', '35', 'Bornova IBB'),
      # ('EGE THM', 'IZMIR', '35', 'Cesme'),
      # ('EGE THM', 'IZMIR', '35', 'Cigli IBB'),
      # ('EGE THM', 'IZMIR', '35', 'Gaziemir'),
      # ('EGE THM', 'IZMIR', '35', 'Guzelyali IBB'),
      # ('EGE THM', 'IZMIR', '35', 'Karabaglar'),
      # ('EGE THM', 'IZMIR', '35', 'Karaburun'),
      # ('EGE THM', 'IZMIR', '35', 'Karsiyaka'),
      # ('EGE THM', 'IZMIR', '35', 'Karsiyaka IBB'),
      # ('EGE THM', 'IZMIR', '35', 'Konak'),
      # ('EGE THM', 'IZMIR', '35', 'Menemen'),
      # ('EGE THM', 'IZMIR', '35', 'Odemis'),
      #  ('EGE THM', 'IZMIR', '35', 'Sirinyer IBB'),
      #  ('EGE THM', 'IZMIR', '35', 'Torbali'),
      #  ('EGE THM', 'IZMIR', '35', 'Yenifoca'),
      #  ('EGE THM', 'IZMIR', '35', 'Guzelbahce IBB'),
      #  ('EGE THM', 'IZMIR', '35', 'Kemalpasa'),
      #  ('EGE THM', 'MANISA', '45', 'Manisa'),
      #  ('EGE THM', 'MANISA', '45', 'Akhisar'),
      #  ('EGE THM', 'MANISA', '45', 'Alasehir'),
      #  ('EGE THM', 'MANISA', '45', 'Kirkagac'),
      #  ('EGE THM', 'MANISA', '45', 'Salihli'),
      #  ('EGE THM', 'MANISA', '45', 'Soma'),
      #  ('EGE THM', 'MANISA', '45', 'Turgutlu'),
      #  ('EGE THM', 'MANISA', '45', 'Ulupark'),
      #  ('EGE THM', 'MANISA', '45', 'Yunusemre'),
      #  ('EGE THM', 'MUGLA', '48', 'Milas'),
      #  ('EGE THM', 'MUGLA', '48', 'Milas Oren'),
      #  ('EGE THM', 'MUGLA', '48', 'Musluhittin'),
      #  ('EGE THM', 'MUGLA', '48', 'Trafik'),
      #  ('EGE THM', 'MUGLA', '48', 'Yatagan'),
      #  ('EGE THM', 'USAK', '64', 'Usak'),
      # ('EGE THM', 'USAK', '64', 'Trafik'),
      # ('GUNEY DOGU ANADOLU THM', 'ADIYAMAN', '02', 'Adıyaman'),
      # ('GUNEY DOGU ANADOLU THM', 'BATMAN', '72', 'Batman'),
      # ('GUNEY DOGU ANADOLU THM', 'BINGOL', '12', 'Bingol'),
      # ('GUNEY DOGU ANADOLU THM', 'BITLIS', '13', 'Bitlis'),
      # ('GUNEY DOGU ANADOLU THM', 'DIYARBAKIR', '21', 'Diyarbakir'),
      # ('GUNEY DOGU ANADOLU THM', 'ELAZIG', '23', 'Elazig'),
      # ('GUNEY DOGU ANADOLU THM', 'HAKKARI', '30', 'Hakkari'),
      # ('GUNEY DOGU ANADOLU THM', 'MALATYA', '44', 'Malatya'),
      # ('GUNEY DOGU ANADOLU THM', 'MARDIN', '47', 'Mardin'),
      # ('GUNEY DOGU ANADOLU THM', 'MUS', '49', 'Mus'),
      # ('GUNEY DOGU ANADOLU THM', 'SIIRT', '56', 'Siirt'),
      # ('GUNEY DOGU ANADOLU THM', 'SANLIURFA', '63', 'Sanliurfa'),
      # ('GUNEY DOGU ANADOLU THM', 'SIRNAK', '73', 'Sirnak'),
      # ('GUNEY DOGU ANADOLU THM', 'SIRNAK', '73', 'Silopi'),
      # ('GUNEY DOGU ANADOLU THM', 'TUNCELI', '62', 'Tunceli'),
      # ('GUNEY DOGU ANADOLU THM', 'VAN', '65', 'Van'),
      # ('GUNEY IC ANADOLU THM', 'AFYONKARAHISAR', '03', 'Merkez Karayollari'),
      # ('GUNEY IC ANADOLU THM', 'AFYONKARAHISAR', '03', 'Sandikli'),
      # ('GUNEY IC ANADOLU THM', 'AFYONKARAHISAR', '03', 'Selcuk Cami'),
      # ('GUNEY IC ANADOLU THM', 'AKSARAY', '68', 'Aksaray'),
      # ('GUNEY IC ANADOLU THM', 'ANTALYA', '07', 'Alanya'),
      # ('GUNEY IC ANADOLU THM', 'ANTALYA', '07', 'Gazipasa'),
      # ('GUNEY IC ANADOLU THM', 'ANTALYA', '07', 'Kepez'),
      # ('GUNEY IC ANADOLU THM', 'ANTALYA', '07', 'Kumluca'),
      # ('GUNEY IC ANADOLU THM', 'ANTALYA', '07', 'Manavgat'),
      # ('GUNEY IC ANADOLU THM', 'ANTALYA', '07', 'Muratpasa'),
      # ('GUNEY IC ANADOLU THM', 'ANTALYA', '07', 'Serik'),
      # ('GUNEY IC ANADOLU THM', 'ANTALYA', '07', 'Trafik'),
      # ('GUNEY IC ANADOLU THM', 'BURDUR', '15', 'Burdur'),
      # ('GUNEY IC ANADOLU THM', 'BURDUR', '15', 'Bucak'),
      # ('GUNEY IC ANADOLU THM', 'ISPARTA', '32', 'Isparta'),
      # ('GUNEY IC ANADOLU THM', 'KARAMAN', '70', 'Karaman'),
      # ('GUNEY IC ANADOLU THM', 'KARAMAN', '70', 'Ermenek'),
      # ('GUNEY IC ANADOLU THM', 'KAYSERI', '38', 'Kocasinan'),
      # ('GUNEY IC ANADOLU THM', 'KAYSERI', '38', 'Melikgazi'),
      # ('GUNEY IC ANADOLU THM', 'KAYSERI', '38', 'OSB'),
      # ('GUNEY IC ANADOLU THM', 'KAYSERI', '38', 'Talas'),
      # ('GUNEY IC ANADOLU THM', 'KAYSERI', '38', 'Trafik'),
      # ('GUNEY IC ANADOLU THM', 'KONYA', '42', 'Aksehir'),
      # ('GUNEY IC ANADOLU THM', 'KONYA', '42', 'Bosna'),
      # ('GUNEY IC ANADOLU THM', 'KONYA', '42', 'Karatay'),
      # ('GUNEY IC ANADOLU THM', 'KONYA', '42', 'Karkent'),
      # ('GUNEY IC ANADOLU THM', 'KONYA', '42', 'Laboratuvar'),
      # ('GUNEY IC ANADOLU THM', 'KONYA', '42', 'Meram'),
      # ('GUNEY IC ANADOLU THM', 'KONYA', '42', 'Sarayonu'),
      # ('GUNEY IC ANADOLU THM', 'KONYA', '42', 'Trafik'),
      # ('GUNEY IC ANADOLU THM', 'KONYA', '42', 'Eregli'),
      # ('GUNEY IC ANADOLU THM', 'KONYA', '42', 'Erenkoy Belediye'),
      # ('GUNEY IC ANADOLU THM', 'KONYA', '42', 'Karatay'),
      # ('GUNEY IC ANADOLU THM', 'KONYA', '42', 'Selcuklu Belediye'),
      # ('GUNEY IC ANADOLU THM', 'NEVSEHIR', '50', 'Nevsehir'),
      # ('GUNEY IC ANADOLU THM', 'NEVSEHIR', '50', 'Avanos'),
      # ('GUNEY IC ANADOLU THM', 'NIGDE', '51', 'Nigde'),
      # ('GUNEY IC ANADOLU THM', 'NIGDE', '51', 'Bor'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Bahcelievler'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Batikent'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Cankaya'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Demetevler'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Etimesgut'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Etlik'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Kayas'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Keciören '),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Sanatoryum'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Mamak'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Ostim'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Polatli'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Sihhiye'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Sincan'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Siteler'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Törekent'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Ulus'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Yasamkent'),
      # ('KUZEY IC ANADOLU THM', 'ANKARA', '06', 'Cubuk'),
      # ('KUZEY IC ANADOLU THM', 'BARTIN', '74', 'Bartin'),
      # ('KUZEY IC ANADOLU THM', 'BOLU', '14', 'Abant'),
      # ('KUZEY IC ANADOLU THM', 'BOLU', '14', 'Karacayir Parki'),
      # ('KUZEY IC ANADOLU THM', 'BOLU', '14', 'Kizilay Parki'),
      # ('KUZEY IC ANADOLU THM', 'CANKIRI', '18', 'Cankiri'),
      # ('KUZEY IC ANADOLU THM', 'DUZCE', '81', 'Duzce'),
      # ('KUZEY IC ANADOLU THM', 'DUZCE', '81', 'Bahcesehir'),
      # ('KUZEY IC ANADOLU THM', 'DUZCE', '81', 'Belediye'),
      # ('KUZEY IC ANADOLU THM', 'ESKISEHİR', '26', 'Cumhuriyet Bulvari'),
      # ('KUZEY IC ANADOLU THM', 'ESKISEHİR', '26', 'Metin Sonmez'),
      # ('KUZEY IC ANADOLU THM', 'ESKISEHİR', '26', 'Odunpazari'),
      # ('KUZEY IC ANADOLU THM', 'ESKISEHİR', '26', 'Tepebasi'),
      # ('KUZEY IC ANADOLU THM', 'ESKISEHİR', '26', 'Visnepark'),
      # ('KUZEY IC ANADOLU THM', 'KARABUK', '78', '75. Yil'),
      # ('KUZEY IC ANADOLU THM', 'KARABUK', '78', 'Safranbolu'),
      # ('KUZEY IC ANADOLU THM', 'KARABUK', '78', 'Toren Alani'),
      # ('KUZEY IC ANADOLU THM', 'KASTAMONU', '37', 'Kastamonu'),
      # ('KUZEY IC ANADOLU THM', 'KASTAMONU', '37', 'Azdavay'),
      # ('KUZEY IC ANADOLU THM', 'KIRIKKALE', '71', 'Kirikkale'),
      # ('KUZEY IC ANADOLU THM', 'KIRIKKALE', '71', 'Bulvar Park'),
      # ('KUZEY IC ANADOLU THM', 'KIRSEHIR', '40', 'Kirsehir'),
      # ('KUZEY IC ANADOLU THM', 'KUTAHYA', '43', 'Ataturk Bulvari'),
      # ('KUZEY IC ANADOLU THM', 'KUTAHYA', '43', 'Heymeana Cad'),
      # ('KUZEY IC ANADOLU THM', 'KUTAHYA', '43', 'Kentpark'),
      # ('KUZEY IC ANADOLU THM', 'KUTAHYA', '43', 'Tavsanli'),
      # ('KUZEY IC ANADOLU THM', 'YOZGAT', '66', 'Yozgat'),
      # ('KUZEY IC ANADOLU THM', 'YOZGAT', '66', 'Sorgun'),
      # ('KUZEY IC ANADOLU THM', 'ZONGULDAK', '67', 'Catalagzi Kuzyaka'),
      # ('KUZEY IC ANADOLU THM', 'ZONGULDAK', '67', 'Caycuma'),
      # ('KUZEY IC ANADOLU THM', 'ZONGULDAK', '67', 'Karadeniz Eregli'),
      # ('KUZEY IC ANADOLU THM', 'ZONGULDAK', '67', 'Kilimli'),
      # ('KUZEY IC ANADOLU THM', 'ZONGULDAK', '67', 'Kozlu'),
      # ('KUZEY IC ANADOLU THM', 'ZONGULDAK', '67', 'Muslu Tepekoy'),
      # ('KUZEY IC ANADOLU THM', 'ZONGULDAK', '67', 'Trafik'),
      # ('MARMARA THM', 'BALIKESIR', '10', 'Balikesir'),
      # ('MARMARA THM', 'BILECIK', '11', 'Bilecik'),
      # ('MARMARA THM', 'BILECIK', '11', 'Bozuyuk MTHM'),
      # ('MARMARA THM', 'BURSA', '16', 'Bursa'),
      # ('MARMARA THM', 'BURSA', '16', 'Beyazit Cad MTHM'),
      # ('MARMARA THM', 'BURSA', '16', 'Inegol MTHM'),
      # ('MARMARA THM', 'BURSA', '16', 'Kestel MTHM'),
      # ('MARMARA THM', 'BURSA', '16', 'Kultur Park MTHM'),
      # ('MARMARA THM', 'BURSA', '16', 'Uludag Univ MTHM'),
      # ('MARMARA THM', 'BURSA', '16', 'Gursu'),
      # ('MARMARA THM', 'BURSA', '16', 'Kestel Hilal Parki'),
      # ('MARMARA THM', 'BURSA', '16', 'Nilufer'),
      # ('MARMARA THM', 'CANAKKALE', '17', 'Canakkale'),
      # ('MARMARA THM', 'CANAKKALE', '17', 'Biga MTHM'),
      # ('MARMARA THM', 'CANAKKALE', '17', 'Biga Icdas'),
      # ('MARMARA THM', 'CANAKKALE', '17', 'Can MTHM'),
      # ('MARMARA THM', 'CANAKKALE', '17', 'Lapseki MTHM'),
      # ('MARMARA THM', 'EDIRNE', '22', 'Edirne'),
      # ('MARMARA THM', 'EDIRNE', '22', 'Karaagac MTHM'),
      # ('MARMARA THM', 'EDIRNE', '22', 'Kesan MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Arnavutkoy'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Bagcilar'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Basaksehir MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Buyukada'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Catladikapi'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Esenyurt MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Kagithane MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Kandilli MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Kartal'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Kumkoy'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Maslak'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Mecidiyekoy MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Portatif'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Sancaktepe'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Sariyer'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Selimiye'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Silivri MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Sultanbeyli MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Sultangazi MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Sile MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Sirinevler MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Sisli Egitim MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Tuzla'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Umraniye'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Umraniye MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Uskudar'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Uskudar MTHM'),
      # ('MARMARA THM', 'ISTANBUL', '34', 'Yenibosna'),
      # ('MARMARA THM', 'KIRKLARELI', '39', 'Vize'),
      # ('MARMARA THM', 'KIRKLARELI', '39', 'Kırklareli'),
      # ('MARMARA THM', 'KIRKLARELI', '39', 'Limankoy MTHM'),
      # ('MARMARA THM', 'KIRKLARELI', '39', 'Luleburgaz MTHM'),
      # ('MARMARA THM', 'KOCAELI', '41', 'Kocaeli'),
      # ('MARMARA THM', 'KOCAELI', '41', 'Alikahya MTHM'),
      # ('MARMARA THM', 'KOCAELI', '41', 'Dilovasi'),
      # ('MARMARA THM', 'KOCAELI', '41', 'Dilovasi IMES OSB 1 MTHM'),
      # ('MARMARA THM', 'KOCAELI', '41', 'Dilovasi IMES OSB 2 MTHM'),
      # ('MARMARA THM', 'KOCAELI', '41', 'Gebze MTHM'),
      # ('MARMARA THM', 'KOCAELI', '41', 'Gebze OSB MTHM'),
      # ('MARMARA THM', 'KOCAELI', '41', 'Golcuk MTHM'),
      # ('MARMARA THM', 'KOCAELI', '41', 'Izmit MTHM'),
      # ('MARMARA THM', 'KOCAELI', '41', 'Kandira MTHM'),
      # ('MARMARA THM', 'KOCAELI', '41', 'Korfez MTHM'),
      # ('MARMARA THM', 'KOCAELI', '41', 'Yenikoy MTHM'),
      # ('MARMARA THM', 'SAKARYA', '54', 'Sakarya'),
      # ('MARMARA THM', 'SAKARYA', '54', 'Hendek OSB MTHM'),
      # ('MARMARA THM', 'SAKARYA', '54', 'Merkez MTHM'),
      # ('MARMARA THM', 'SAKARYA', '54', 'Ozanlar MTHM'),
      # ('MARMARA THM', 'TEKIRDAG', '59', 'Tekirdag'),
      # ('MARMARA THM', 'TEKIRDAG', '59', 'Cerezkoy MTHM'),
      # ('MARMARA THM', 'TEKIRDAG', '59', 'Corlu MTHM'),
      # ('MARMARA THM', 'TEKIRDAG', '59', 'Corlu OSB MTHM'),
      # ('MARMARA THM', 'TEKIRDAG', '59', 'Merkez MTHM'),
      # ('MARMARA THM', 'YALOVA', '77', 'Yalova'),
      # ('MARMARA THM', 'YALOVA', '77', 'Altinova MTHM'),
      # ('MARMARA THM', 'YALOVA', '77', 'Armutlu MTHM'),
      # ('ORTA KARADENIZ THM', 'AMASYA', '05', 'Amasya'),
      # ('ORTA KARADENIZ THM', 'AMASYA', '05', 'Merzifon'),
      # ('ORTA KARADENIZ THM', 'AMASYA', '05', 'Suluova'),
      # ('ORTA KARADENIZ THM', 'AMASYA', '05', 'Sehzade'),
      # ('ORTA KARADENIZ THM', 'CORUM', '19', 'Corum'),
      # ('ORTA KARADENIZ THM', 'CORUM', '19', 'Bahabey'),
      # ('ORTA KARADENIZ THM', 'CORUM', '19', 'Mimar Sinan'),
      # ('ORTA KARADENIZ THM', 'GIRESUN', '28', 'Giresun'),
      # ('ORTA KARADENIZ THM', 'GIRESUN', '28', 'Gemilercekegi'),
      # ('ORTA KARADENIZ THM', 'ORDU', '52', 'Fatsa'),
      # ('ORTA KARADENIZ THM', 'ORDU', '52', 'Karsiyaka'),
      # ('ORTA KARADENIZ THM', 'ORDU', '52', 'Stadyum'),
      # ('ORTA KARADENIZ THM', 'ORDU', '52', 'Unye'),
      # ('ORTA KARADENIZ THM', 'SAMSUN', '55', 'Atakum'),
      # ('ORTA KARADENIZ THM', 'SAMSUN', '55', 'Bafra'),
      # ('ORTA KARADENIZ THM', 'SAMSUN', '55', 'Canik'),
      # ('ORTA KARADENIZ THM', 'SAMSUN', '55', 'Ilkadim Hastane'),
      # ('ORTA KARADENIZ THM', 'SAMSUN', '55', 'Tekkekoy'),
      # ('ORTA KARADENIZ THM', 'SAMSUN', '55', 'Yuzuncuyil'),
      # ('ORTA KARADENIZ THM', 'SINOP', '57', 'Sinop'),
      # ('ORTA KARADENIZ THM', 'SINOP', '57', 'Boyabat'),
      # ('ORTA KARADENIZ THM', 'SINOP', '57', 'Erfelek'),
      # ('ORTA KARADENIZ THM', 'SIVAS', '58', 'Basogretmen'),
      # ('ORTA KARADENIZ THM', 'SIVAS', '58', 'Istasyonkavsagi'),
      # ('ORTA KARADENIZ THM', 'SIVAS', '58', 'Meteoroloji'),
      # ('ORTA KARADENIZ THM', 'TOKAT', '60', 'Tokat'),
      # ('ORTA KARADENIZ THM', 'TOKAT', '60', 'Erbaa'),
      # ('ORTA KARADENIZ THM', 'TOKAT', '60', 'Meydan'),
      # ('ORTA KARADENIZ THM', 'TOKAT', '60', 'Turhal')



















 dbDisconnect(mydb)





# #deleting table
# tablo_adi <- "adana-catalan-saatlik-detay"
# # Tabloyu silmek için dbRemoveTable kullanın
# dbRemoveTable(mydb, tablo_adi)





#dbDisconnect(mydb)



