<p align="center">
  <img width="350" height="350" src="image/logo StatHub.png">
</p>

<div align="center">

#  Natural Disaster Occur in 2024

[Tentang](#notebook-tentang)
•
[Dashboard Database](#desktop_computer-dashboard-database)
•
[Demo](#octocat-demo)

</div>

## :bookmark_tabs: Menu

- [Tentang](#notebook-tentang)
- [INFO](#bookmark_tabs-INFO)
- [Dashboard Database](#desktop_computer-dashboard-database)
- [Demo](#octocat-demo)
- [Requirements](#exclamation-requirements)
- [Skema Database](#film_strip-skema-database)
- [ERD](#chart_with_downwards_trend-erd)
- [Deskripsi Data](#postbox-deskripsi-data)
- [Struktur Folder](#open_file_folder-struktur-folder)
- [Tim Pengembang](#ninja-tim-pengembang)


## :notebook: Tentang
<div align="justify">
Dalam menghadapi tantangan terkait bencana alam, informasi yang akurat dan cepat adalah kunci. Disaster Dashboard hadir untuk memberikan alat yang mempermudah analisis dan pengambilan keputusan berdasarkan data. Dashboard ini dirancang untuk menyajikan data terkait bencana alam seperti gempa bumi, banjir, tanah longsor, dan lainnya secara interaktif.

**Apa Itu Natural Disaster Dashboard?**

Natural Disaster Dashboard adalah platform visualisasi data berbasis web yang menyediakan informasi penting mengenai bencana alam. Dengan mengintegrasikan data dari berbagai sumber, dashboard ini membantu pengguna memahami pola, tren, dan dampak bencana secara komprehensif.

**Fitur Utama**

**Statistik Deskriptif**: Merangkum jumlah korban, kerugian, dan wilayah terdampak dalam format yang mudah dimengerti.

**Tren Temporal**: Grafik interaktif untuk melihat perkembangan kejadian bencana dari waktu ke waktu.

**Filter Data**: Kemampuan untuk menyaring data berdasarkan jenis bencana, wilayah, dan periode waktu tertentu.

**Laporan**: Menghasilkan laporan dalam format PDF atau CSV untuk kebutuhan dokumentasi dan analisis lanjutan.

**Mengapa Penting?**

Di era perubahan iklim dan meningkatnya frekuensi bencana, alat seperti Natural Disaster Dashboard sangat penting untuk membantu masyarakat, pemerintah, dan organisasi kemanusiaan dalam merespons dan memitigasi dampak bencana. Dengan menggunakan data yang tersedia, pengguna dapat:

- Memahami wilayah yang paling rentan terhadap bencana.
- Mengidentifikasi kebutuhan sumber daya berdasarkan dampak bencana.
- Merencanakan strategi mitigasi yang lebih efektif.
- Mengambil keputusan berbasis data yang tepat waktu dan akurat.


### :bookmark_tabs: INFO
**Stathub** adalah portal info jurusan statistika perguruan tinggi negeri terlengkap di Indonesia. Seperti yang kita ketahui bersama, perguruan tinggi memiliki banyak jenis, mulai dari Universitas, Institut, Sekolah Tinggi. Banyaknya jenis perguruan tinggi ini memberikan banyak pilihan bagi siswa untuk melanjutkan pendidikan ke jenjang Sarjana(S1). Selain itu terdapat info jenjang bagi freshgraduate untuk melanjutkan ke jenjang Master(S2) dan Doktor(S3).

**StatHub** hadir untuk membantu kamu untuk menemukan pilihan kampus yang terbaik. Tersedia 30 info jurusan statistika di beberapa perguruan tinggi yang tersebar di 34 provinsi dari seluruh Indonesia mulai dari Aceh hingga Papua. Kamu bisa lakukan dengan mudah dengan fitur lokasi kampus.


## :desktop_computer: Dashboard Database

Berikut merupakan tampilan awal dari Portal StatHub Database
<p align="center">
  <img width="900" height="450" src="image/dashboard.png">
</p>

## :octocat: Demo

Berikut merupakan link untuk shinnyapps atau dashboard dari project kami:
[https://dvprmta.shinyapps.io/Disaster_Dashboard/](https://dvprmta.shinyapps.io/Disaster_Dashboard/)

## :film_strip: Skema Database

Menggambarkan struktur primary key Wilayah, Universitas, Program Studi dan jalur dengan masing-masing foreign key dalam membangun relasi antara tabel atau entitas.

Berikut merupakan Skema Database dari Portal StatHub Database

<p align="center">
  <img width="600" height="400" src="image/skema_MDS.jpg">
</p>

## :exclamation: Requirements

- RDBMS yang digunakan adalah PostgreSQL dan ElephantSQL
- Dashboard menggunakan `shinny`, `shinnythemes`, `bs4Dash`, `DT`, dan `dplyr` dari package R

## :chart_with_downwards_trend: ERD

ERD (Entity Relationship Diagram) menampilkan hubungan antara entitas dengan atribut. Pada project ini, entitas wilayah terdapat atribut yang berhubungan dengan atribut universitas yaitu id_univ.

Selanjutnya, entitas universitas berhubungan dengan dua atribut pada entitas lain yaitu id_prodi berhubungan dengan entitas prodi, dan jalur.

Selain itu, entitas prodi saling berhubungan dengan jalur.

<p align="center">
  <img width="350" height="650" src="image/ERD.jpeg">
</p>

## :postbox: Deskripsi Data
StatHub adalah sebuah portal database universitas-universitas negeri di Indonesia yang memiliki jurusan statistika. Dengan hal ini, data yang diambil yakni berdasarkan pddikti dan sumber web resmi masing-masing universitas. Data yang kami ambil meliputi Universitas, Wilayah Universitas, Data Prodi (Dosen, Mahasiswa), Jalur Masuk yang ada di Universitas tersebut untuk jurusan Statistika.

Berisi tentang tabel-tabel yang digunakan berikut dengan sintaks SQL DDL (CREATE).

### Create Database
Database STATHub menyimpan informasi yang mewakili atribut data yang saling berhubungan untuk kemudian dianalisis.
```sql
CREATE DATABASE portal_StatHub 
    WITH
    OWNER = postgres
    ENCODING = 'UTF8'
    CONNECTION LIMIT = -1
    IS_TEMPLATE = False;
```
### Create Table Wilayah
Table wilayah memberikan informasi kepada user mengenai posisi wilayah universitas, sehingga user dapat mengetahui id wilayah, nama kab/kota, nama provinsi universitas tersebut berada. Berikut deskripsi untuk setiap tabel instansi.
| Attribute         | Type                  | Description                    |
|:------------------|:----------------------|:-------------------------------|
| id_wilayah        | character varying(10) | Id Wilayah                     |
| nama_kabkota      | character varying(50) | Nama Kab/Kota                  |
| nama_prov         | character varying(50) | Nama Provinsi                  |

dengan script SQL sebagai berikut:
```sql
CREATE TABLE IF NOT EXISTS wilayah (
    id_wilayah CHAR(10) PRIMARY KEY,
    nama_kabkota VARCHAR(50) NOT NULL,
    nama_prov VARCHAR(50) NOT NULL
);

select * from wilayah
```
### Create Table Universitas
Table Universitas memberikan informasi yang memudahkan user mengetahui Universitas yang memiliki Jurusan Statistika di dalamnya melalui id Universitas, id wilayah, nama universitas, dan akreditasi universitas terkait. Berikut deskripsi untuk setiap tabel Universitas.
| Attribute          | Type                  | Description                     |
|:-------------------|:----------------------|:--------------------------------|
| id_univ            | integer               | Id Universitas                  |
| id_wilayah         | character varying(10) | Id Wilayah                      |
| nama_univ          | character varying(50) | Nama Universitas                |
| akred_univ         | character varying(10) | Akreditasi Universitas          |

dengan script SQL sebagai berikut:
```sql
CREATE TABLE IF NOT EXISTS universitas (
    id_univ int PRIMARY KEY,
    id_wilayah CHAR(10) NOT NULL,
    nama_univ VARCHAR(50) NOT NULL,
    akred_univ VARCHAR(10) NOT NULL,
    FOREIGN KEY (id_wilayah) REFERENCES wilayah(id_wilayah)
);
select * from universitas
```
### Create Table Prodi
Table prodi memberikan informasi kepada user mengenai beberapa informasi mengenai program studi di universitas tersebut. User dapat mengetahui id prodi dari universitas, id univ, nama program studi,jumlah dosen, jumlah mahasiswa, akreditasi program studi tersebut dan jenjang. Berikut deskripsi untuk setiap tabel penulis.

| Attribute                  | Type                  | Description                     		 |
|:---------------------------|:----------------------|:------------------------------------------|
| id_prodi                   | integer		     | Id prodi                       		 |
| id_univ                    | integer		     | Id universitas                   	 |
| nama_prodi                 | character varying(50) | Nama program studi                     	 |	
| jumlah_dosen               | integer		     | Jumlah dosen                 	         |
| jumlah_mahasiswa           | integer	             | Jumlah mahasiswa                 	 |
| akred_prodi    	     | character varying(30) | Akreditasi prodi                          |
| jenjang		     | character varying(10) | Jenjang pendidikan                        |

dengan script SQL sebagai berikut:
```sql
CREATE TABLE IF NOT EXISTS prodi (
    id_prodi int PRIMARY KEY,
    id_univ int NOT NULL,
    nama_prodi VARCHAR(50) NOT NULL,
    jumlah_dosen int NOT NULL,
    jumlah_mahasiswa int NOT NULL,
    akred_prodi VARCHAR(30) NOT NULL,
    jenjang VARCHAR(10) NOT NULL,
    FOREIGN KEY (id_univ) REFERENCES universitas (id_univ)
);
```

Penilaian Peringkat Akreditasi Program Studi berdasarkan:

a. Unggul;

Predikat Unggul diberikan BAN-PT kepada perguruan tinggi yang mendapat nilai akreditasi A dan memenuhi syarat masuk predikat Unggul atau strata tertinggi dalam akreditasi.

b. Baik Sekali;

Predikat Baik Sekali diberikan oleh BAN-PT kepada perguruan tinggi yang mendapat nilai akreditasi A namun belum memenuhi seluruh syarat predikat Unggul.

c. Baik;

Predikat Baik diberikan kepadapa perguruan tinggi yang mencapai nilai akreditasi B dengan nilai akreditasi di atas 200 poin.

d. A;

Akreditasi A menunjukkan nilai akreditasi antara 361-400 poin.

e. B; dan

Akreditasi B menunjukkan nilai akreditasi antara 301-360 poin.

f. C;

Akreditasi C dengan nilai akreditasi antara 200-300 poin.


### Create Table Jalur
Table Jalur menyajikan informasi lengkap mengenai jalur masuk calon mahasiswa yang akan mendaftar ke universitas yang diinginkan. Selain dapat mengetahui jalur, user juga akan mendapatkan informasi daya tampung dan website tempat mendaftar universitas. Informasi spesifik mengenai id prodi, id universitas, jalur masuk, daya tampung, dan website dapat diketahui melalui table ini.  Berikut deskripsi untuk setiap tabel Jalur.
| Attribute                  | Type                   | Description                     	  |
|:---------------------------|:-----------------------|:------------------------------------------|
| id_prodi                   | integer                | Id Prodi                       		  |
| id_univ                    | integer                | Id Universitas                            |
| jalur_masuk                | character varying(100) | Jalur Masuk                    		  |	
| daya_tampung               | integer                | Daya Tampung                      	  |
| website                    | character varying(1000)| Website                                   |

dengan script SQL sebagai berikut:              
```sql
CREATE TABLE IF NOT EXISTS jalur (
    id_prodi int NULL,
    id_univ int NOT NULL,
    jalur_masuk VARCHAR(100) NOT NULL,
    daya_tampung int NOT NULL,
    website VARCHAR(1000) NOT NULL,
	FOREIGN KEY (id_prodi) REFERENCES prodi (id_prodi),
	FOREIGN KEY (id_univ) REFERENCES universitas (id_univ),
);
```

## :open_file_folder: Struktur Folder

```
.
├── app           # ShinyApps
│   ├── server.R
│   └── ui.R
│
├── data 
│   ├── Prodi.csv
│   ├── jalur_masuk.csv
│   ├──	universitas_StatHub.csv
|   └── wilayah_StatHub.csv
│
├── image
│   ├── ERD.jpeg
│   ├── dashboard.png
│   ├──	logo StatHub.png
|   └──	skema2.png
│   
├── src		# Project source code
│   ├──	conn_elephantsql.R
|   └──	input data SQL StatHub.R
| 
└── README.md
```


## :ninja: Tim Pengembang
+ 💃Database Manager : [Adhiyatma Nugraha](https://github.com/adhiyatmanugraha) (G1501231085)
+ 👩‍🍳UI/UX Developer : [Devi Permata Sari](https://github.com/dvprmta) (G1501231026)
+ 🏄‍♀️Technical Writer : [Tukhfatur Rizmah A.](https://github.com/tukhfaturr) (G1501231023)
