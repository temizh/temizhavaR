library(DBI)
library(readxl)
#library(RSQLite)
# reading excel
# data_dir <- "C:/Fonksiyon_sehirler/"
# istasyon <- read_excel(paste0(data_dir, "adana-catalan-saatlik-detay.xlsx"))
# create a connection to database
mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")
# Write the istasyon dataset into a table
# dbWriteTable(mydb, "adana-catalan-saatlik-detay", istasyon)
 #dbListTables(mydb)

mydb_location <- "
CREATE TABLE location (
    bolge TEXT,
    sehir TEXT,
    plaka TEXT,
    istasyon TEXT,
    PRIMARY KEY (istasyon)
);
"
mydb_hourly_detail <- "
CREATE TABLE hourly_detail (
    istasyon TEXT
    PM10

);
"
mydb_hourly_sum <- "
CREATE TABLE hourly_sum(
   istasyon TEXT
);
"

mydb_daily_detail <- "
CREATE TABLE daily_detail(
   istasyon TEXT
);
"

mydb_daily_sum <- "
CREATE TABLE daily_sum(
   istasyon TEXT
);
"

# SQL sorgularını veritabanına gönderin
dbExecute(mydb, mydb_location)
dbExecute(mydb, mydb_hourly_detail)
dbExecute(mydb, mydb_hourly_sum)
dbExecute(mydb, mydb_daily_detail)
dbExecute(mydb, mydb_daily_sum)

query <- "
INSERT INTO location (bolge, sehir, plaka, istasyon)
VALUES ('AKDENIZ THM', 'ADANA', '01', 'Catalan'),
       ('AKDENIZ THM', 'ADANA', '01', 'Cukurova'),
       ('AKDENIZ THM', 'ADANA', '01', 'Dogankent'),
       ('AKDENIZ THM', 'ADANA', '01', 'Meteoroloji'),
       ('AKDENIZ THM', 'ADANA', '01', 'Valilik'),
       ('AKDENIZ THM', 'ADANA', '01', 'Seyhan'),
       ('AKDENIZ THM', 'ADANA', '01', 'Turhan Cemal Beriker Bulvari'),
       ('AKDENIZ THM', 'ADANA', '01', 'Yakapinar'),
       ('AKDENIZ THM', 'GAZIANTEP', '27', 'Gaziantep'),
       ('AKDENIZ THM', 'GAZIANTEP', '27', 'Beydilli'),
       ('AKDENIZ THM', 'GAZIANTEP', '27', 'Fevzi Cakmak'),
       ('AKDENIZ THM', 'GAZIANTEP', '27', 'Gaski D6'),
       ('AKDENIZ THM', 'GAZIANTEP', '27', 'Nizip'),
       ('AKDENIZ THM', 'GAZIANTEP', '27', 'Atapark'),
       ('AKDENIZ THM', 'HATAY', '31', 'Antakya'),
       ('AKDENIZ THM', 'HATAY', '31', 'Iskenderun'),
       ('AKDENIZ THM', 'HATAY', '31', 'Iskenderun Merkez'),
       ('AKDENIZ THM', 'HATAY', '31', 'Vali Kavsagi'),
       ('AKDENIZ THM', 'KAHRAMANMARAS', '46', 'Dulkadiroglu'),
       ('AKDENIZ THM', 'KAHRAMANMARAS', '46', 'Elbistan'),
       ('AKDENIZ THM', 'KAHRAMANMARAS', '46', 'Kent Meydani'),
       ('AKDENIZ THM', 'KAHRAMANMARAS', '46', 'Onikisubat'),
       ('AKDENIZ THM', 'KILIS', '78', 'Kilis'),
       ('AKDENIZ THM', 'MERSIN', '33', 'Akdeniz'),
       ('AKDENIZ THM', 'MERSIN', '33', 'Huzurkent'),
       ('AKDENIZ THM', 'MERSIN', '33', 'Istiklal Cad'),
       ('AKDENIZ THM', 'MERSIN', '33', 'Tarsus'),
       ('AKDENIZ THM', 'MERSIN', '33', 'Tasucu'),
       ('AKDENIZ THM', 'MERSIN', '33', 'Toroslar'),
       ('AKDENIZ THM', 'OSMANIYE', '80', 'Osmaniye'),
       ('AKDENIZ THM', 'OSMANIYE', '80', 'Kadirli'),
       ('DOGU ANADOLU THM', 'AGRI', '04', 'Agri'),
       ('DOGU ANADOLU THM', 'AGRI', '04', 'Dogu Beyazit'),
       ('DOGU ANADOLU THM', 'AGRI', '04', 'Patnos'),
       ('DOGU ANADOLU THM', 'ARDAHAN', '75', 'Ardahan'),
       ('DOGU ANADOLU THM', 'ARTVIN', '08', 'Artvin'),
       ('DOGU ANADOLU THM', 'BAYBURT', '69', 'Bayburt'),
       ('DOGU ANADOLU THM', 'ERZINCAN', '24', 'Erzincan'),
       ('DOGU ANADOLU THM', 'ERZINCAN', '24', 'Trafik'),
       ('DOGU ANADOLU THM', 'ERZURUM', '25', 'Erzurum'),
       ('DOGU ANADOLU THM', 'ERZURUM', '25', 'Aziziye'),
       ('DOGU ANADOLU THM', 'ERZURUM', '25', 'Palandoken'),
       ('DOGU ANADOLU THM', 'ERZURUM', '25', 'Pasinler'),
       ('DOGU ANADOLU THM', 'ERZURUM', '25', 'Tashan'),
       ('DOGU ANADOLU THM', 'GUMUSHANE', '29', 'Gumushane'),
       ('DOGU ANADOLU THM', 'IGDIR', '76', 'Igdir'),
       ('DOGU ANADOLU THM', 'IGDIR', '76', 'Aralik'),
       ('DOGU ANADOLU THM', 'KARS', '36', 'Istasyon Mah'),
       ('DOGU ANADOLU THM', 'KARS', '36', 'Trafik'),
       ('DOGU ANADOLU THM', 'RIZE', '53', 'Rize'),
       ('DOGU ANADOLU THM', 'RIZE', '53', 'Ardesen'),
       ('DOGU ANADOLU THM', 'TRABZON', '61', 'Akcaabat'),
       ('DOGU ANADOLU THM', 'TRABZON', '61', 'Besirli'),
       ('DOGU ANADOLU THM', 'TRABZON', '61', 'Fatih'),
       ('DOGU ANADOLU THM', 'TRABZON', '61', 'Meydan'),
       ('DOGU ANADOLU THM', 'TRABZON', '61', 'Uzungol'),
       ('DOGU ANADOLU THM', 'TRABZON', '61', 'Valilik'),
       ('EGE THM', 'AYDIN', '07', 'Aydin'),
       ('EGE THM', 'AYDIN', '07', 'Didim'),
       ('EGE THM', 'AYDIN', '07', 'Efeler'),
       ('EGE THM', 'AYDIN', '07', 'Germencik'),
       ('EGE THM', 'AYDIN', '07', 'Nazilli'),
       ('EGE THM', 'AYDIN', '07', 'Soke'),
       ('EGE THM', 'AYDIN', '07', 'Trafik'),
       ('EGE THM', 'DENIZLI', '20', 'Bayramyeri'),
       ('EGE THM', 'DENIZLI', '20', 'Civril'),
       ('EGE THM', 'DENIZLI', '20', 'Honaz'),
       ('EGE THM', 'DENIZLI', '20', 'Merkezefendi'),
       ('EGE THM', 'DENIZLI', '20', 'Sumer'),
       ('EGE THM', 'DENIZLI', '20', 'Trafik'),
       ('EGE THM', 'DENIZLI', '20', 'Saraykoy'),
       ('EGE THM', 'IZMIR', '35', 'Seferihisar'),
       ('EGE THM', 'IZMIR', '35', 'Aliaga'),
       ('EGE THM', 'IZMIR', '35', 'Aliaga Bozkoy'),
       ('EGE THM', 'IZMIR', '35', 'Alsancak IBB'),
       ('EGE THM', 'IZMIR', '35', 'Bayrakli IBB'),
       ('EGE THM', 'IZMIR', '35', 'Bergama IBB'),
      ('EGE THM', 'IZMIR', '35', 'Bornova'),
      ('EGE THM', 'IZMIR', '35', 'Bornova IBB'),
('EGE THM', 'IZMIR', '35', 'Cesme'),
('EGE THM', 'IZMIR', '35', 'Cigli IBB'),
('EGE THM', 'IZMIR', '35', 'Gaziemir'),
('EGE THM', 'IZMIR', '35', 'Guzelyali IBB'),
('EGE THM', 'IZMIR', '35', 'Karabaglar'),
('EGE THM', 'IZMIR', '35', 'Karaburun'),
('EGE THM', 'IZMIR', '35', 'Karsiyaka'),
('EGE THM', 'IZMIR', '35', 'Karsiyaka IBB'),
('EGE THM', 'IZMIR', '35', 'Konak'),
('EGE THM', 'IZMIR', '35', 'Menemen'),
('EGE THM', 'IZMIR', '35', 'Odemis'),
       ('EGE THM', 'IZMIR', '35', 'Sirinyer IBB'),
       ('EGE THM', 'IZMIR', '35', 'Torbali'),
       ('EGE THM', 'IZMIR', '35', 'Yenifoca'),
       ('EGE THM', 'IZMIR', '35', 'Guzelbahce IBB'),
       ('EGE THM', 'IZMIR', '35', 'Kemalpasa'),
       ('EGE THM', 'IZMIR', '35', 'Sirinyer IBB'),
       ('EGE THM', 'IZMIR', '35', 'Torbali'),
       ('EGE THM', 'IZMIR', '35', 'Yenifoca'),
       ('EGE THM', 'IZMIR', '35', 'Guzelbahce IBB'),
       ('EGE THM', 'IZMIR', '35', 'Kemalpasa'),
       ('EGE THM', 'MANISA', '45', 'Manisa'),
       ('EGE THM', 'MANISA', '45', 'Akhisar'),
       ('EGE THM', 'MANISA', '45', 'Alasehir'),
       ('EGE THM', 'MANISA', '45', 'Kirkagac'),
       ('EGE THM', 'MANISA', '45', 'Salihli'),
       ('EGE THM', 'MANISA', '45', 'Soma'),
       ('EGE THM', 'MANISA', '45', 'Turgutlu'),
       ('EGE THM', 'MANISA', '45', 'Ulupark'),
       ('EGE THM', 'MANISA', '45', 'Yunusemre'),
       ('EGE THM', 'MUGLA', '48', 'Milas'),
       ('EGE THM', 'MUGLA', '48', 'Milas Oren'),
       ('EGE THM', 'MUGLA', '48', 'Musluhittin'),
       ('EGE THM', 'MUGLA', '48', 'Trafik'),
       ('EGE THM', 'MUGLA', '48', 'Yatagan'),
       ('EGE THM', 'USAK', '64', 'Usak'),
      ('EGE THM', 'USAK', '64', 'Trafik'),








"


















# reading excel
 # data_dir <- "C:/Fonksiyon_sehirler/"
 # stations <- read_excel(paste0(data_dir, "adana-catalan-saatlik-detay.xlsx"))
 #
 # dbWriteTable(mydb, "hourly_detail", stations, append = TRUE, row.names = FALSE)
 #
 # hourly_detail <- dbReadTable(mydb, "hourly_detail")
 # head(hourly_detail, 10)

 dbDisconnect(mydb)





# #deleting table
# tablo_adi <- "adana-catalan-saatlik-detay"
# # Tabloyu silmek için dbRemoveTable kullanın
# dbRemoveTable(mydb, tablo_adi)





#dbDisconnect(mydb)



