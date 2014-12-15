# This script prepares a dataset of places that will be used to construct networks of relations between people and places.
source("Places/PlacesHelper.R")

P2 <- read.csv("rawDaat/Places244.csv")
P.back = P # full backup dataset
load("MainData/MainData1.RData") # get the main dataset
D.back = D # full backup dataset

# get ids of discarded respondents
ids = which(!P$id %in% D$id)
ids3 = P$id[ids]

P = P[-ids, ]
P = P[, seq(2, 38, 2)] # dataset with only names of places
N = getNames(P) # get names of places
N = data.frame(N=N)
P = toLower(P)
# save the names and recode it partially by hand
write.csv(N, file="Places/NameList.csv", row.names=FALSE)

N.uniq = N # list of unique place names
# Get distribution of names
N = getNames(P)
N.back = getNames(P) # backup of the place list
table(N)[table(N) > 1]
table(N)[1:10]

########################################################################
### IMPORTANT NOTE: from this place on to the next heading like this ###
### this script should be used with caution, because there is a      ###
### (slight) chance that it can somehow distort tha data             ###
########################################################################
# Begin recoding and organizing place names (this will be long and messy :/)
# 1500m2
N[grep("1500", N)] = "1500m2"
# 55
N[grep("55",N)] = "klub_55"
# Agrykola
N[grep("agry", N)]
N[grep("agy", N)] = "agrykola"
# Aioli
N[grep("aio", N)] = "aioli"
# Antykwariat Cafe
N[grep("anty|anyt", N)] = "antykwariat_cafe"
# Arkadia
N[grep("arkad", N)] = "arkadia" # I recode all places in Arkadia gallery to arkadia
# Au Lac
N[grep("au lac", N)] = "au_lac"
# AWF
N[grep("awf", N)] = "awf"
# SGGW
N[grep("sggw", N)] = "kampus_sggw" # this also contains also SGGW amenities
# Barka
N[grep("barka", N)] = "barka_club"
# Bar Studio
N[grep("bar[aA-zZ ]*studio", N)] = "bar_studio"
# Klub Basen
N[grep("basen$", N)] = "klub_basen"
N[grep("centra.*base", N)] = "klub_basen"
# Basen Inflancka
N[grep("inflan", N)] = "basen_inflancka"
# Basen Muszelka
N[grep("muszel", N)] = "basen_muszelka"
# Basen Włochy
N[grep("basen[aA-zZ ]*włoch", N)] = "basen_włochy"
# Basen Polna
N[grep("basen[aA-zZ ]*poln", N)] = "basen_polna"
# Beirut Bar
N[grep("beir", N)] = "beirut_bar"
N[grep("humm|beir", N)]  = "beirut_bar"
# Berlin-Warszawa Pub
N[grep("berli", N)] = "berlin_warszawa_pub"
# BUW
N[grep("^buw", N)] = "buw/ogrody"
N[grep("fenome", N)] = "buw/ogrody"
N[grep("ogro[aA-zZ ]*buw|dach[aA-zZ ]*buw", N)] ="buw/ogrody"
N[grep("biblioteka uw|biblloteka uw", N)] = "buw/ogrody"
N[grep("bibliot[aA-zZ ]*uniwers", N)] = "buw/ogrody"
N[grep("nowy bu", N)] = "buw/ogrody"
# Recode unidentified library ("bibliotek") into NA
N[N=="biblioteka"] = NA
# Stary BUW
N[grep("star[aA-zZ ]*buw", N)] = "stary_buw"
# Biblioteka Narodowa
N[grep("biblio[aA-zZ ]*naro", N)] = "biblioteka_narodowa"
# WUM
N[grep("bib[aA-zZ ]*wum", N)] = "biblioteka_wum"
# Bobby Burger
N[grep("bob", N)] = "bobby_burger"
# Chenge some unidentified football pitches into NA
N[grep("boisk", N)] = NA
# Pub Bolek
N[grep("bol", N)] = "pub_bolek"
# Crux
N[grep("crux|bould", N)] = "crux"
N[grep("[sś]cian.*ho[zż]", N)] = "crux"
# British Buldog Pub
N[grep("bull|brit", N)] = "british_buldog_pub"
# Nad Wisłą - in general on the riverbanks - not a specific place and not the boulvards
N[grep("mos.*poniatow", N)] = "nad_wisłą"
N[grep("brzeg", N)] = "nad_wisłą"
N[grep("^nad wis[lł]", N)] = "nad_wisłą"
N[grep("^wis[lł]", N)] = "nad_wisłą"
N[grep("wis[lł]y", N)] = "nad_wisłą"
N[grep("ścieżka na|plener nad", N)] = "nad_wisłą"
N[grep("nadwi[sś]l.*miejsc", N)] = "nad_wisłą"
# Schodki i plaża nad Wisłą
N[grep("plaża nad w|schod[aA-zZ ]*wi", N)] = "schody/plaża_nad_wisłą"
N[grep("pla[zż]a", N)] = "schody/plaża_nad_wisłą"
N[grep("płaza", N)] = "schody/plaża_nad_wisłą"
# Bulwary nad wisłą
N[grep("bulwa|bary nad|lokale nad", N)] = "bulwary_wiślane"
N[grep("bulwe", N)] = "bulwary_wiślane"
# Cud Nad Wisłą
N[grep("cud.*wis", N)] = "cud_nad_wisłą"

# Backup save of N
N.back1 = N

# Być Może
N[grep("być mo", N)] = "być_może"
# Cafe Fajka
N[grep("fajk", N)] = "kafefajka"
# Kawiarnia Kafka
N[grep("kafk", N)] = "kafka"
N[grep("kaff", N)] = "kafka"
N[grep("kawka", N)] = "kafka"
# Kino / Cafe Iluzjon
N[grep("ilu", N)] = "kino/cafe_iluzjon"
# Cafe Kulturalna
N[grep("cafe kul|kulturalna", N)] = "cafe_kulturalna"
# Cafe Nero przy UW
N[grep("nero.*uw", N)] = "cafe_nero_uw"
# Cafe Nero na Placu Unii Lubelskiej
N[grep("nero na", N)] = "cafe_nero_pl_unii_lubelskiej"
N[grep("fe ne.*na", N)] = "cafe_nero_pl_unii_lubelskiej"
# Cafe Nero Telimena
N[grep("telime", N)] = "telimena"
# Recode unidentified Green Cafe Nero to NA
N[grep("en c[ao].*ne", N)] = NA
N[grep("fe ne", N)] = NA
# Próżna Cafe
N[grep("pr[oó][zż]", N)] = "próżna_cafe"
# Cafe Rock & Roll
N[grep("rock|roll", N)] = "rock_n_roll"
# Cafe Szpulka
N[grep("szpul", N)] = "cafe_szpulka"
# Cafe Zagadka
N[grep("ca.* zaga",N)] = "cafe_zagadka"
# Calypso Fitness Europlex
N[grep("cal[iy]ps.*17|plex", N)] = "calypso_europlex"
# Calypso Ursynów
N[grep("caly.*urs", N)] = "calypso_ursynów"
# Calypso Żoliborz
N[grep("caly.*[zż]oli", N)] = "calypso_żoliborz"
# Calypso Targówek
N[grep("caly.*tar", N)] = "calypso_targówek"
# Unidentified fitness clubs Calypso - recode to NA
N[grep("^caly.*pso$", N)] = NA
N[grep("^fit.*pso$", N)] = NA
N[grep("^si[lł].*pso$", N)] = NA

# Save backup dataset
N.back2 = N

# Capitol
N[grep("capi", N)] = "capitol"
# Centrum
N[grep("^cent.*um$", N)] = "centrum"
# Recode unidentified shopping malls to NAs
N[grep("^cent.*handlow[ey]$", N)] = NA
N[grep("^galeri.*owe$", N)] = NA
# Blue City
N[grep("blue", N)] = "blue_city"
# Promenada
N[grep("promen", N)] = "promenada"
# Złote Tarasy
N[grep("z[lł]ot.*ta", N)] = "złote_tarasy"
# Centrum Zarządzania Światem (sic!)
N[grep("cent.*zarz.*[sś]w", N)] = "centrum_zarządzania_światem"
# CSW
N[grep("cent.*sztu|csw", N)] = "csw"
# Centrum Nauki Kopernik
N[grep("koper", N)] = "kopernik"
N[grep("cnk", N)] = "kopernik"
# Charlotte
N[grep("charlo|szarlo", N)] = "charlotte"
# Chmielarnia
N[grep("chmielar", N)] = "chmielarnia"
# Chmielna
N[grep("chmieln[ae]", N)] = "chmielna"
# Chmury
N[grep("chmu", N)] = "chumry"
# Chwila Da Klub
N[grep("chwi", N)] = "chwila_da_klub"

# Backup save
N.back3 = N

# Cinema City Bemowo
N[grep("cine.*bemo", N)] = "cinema_city_bemowo"
# Recode unidentified Cinema Cities to NAs
N[grep("cine.*city$", N)] = NA
# City24
N[grep("ci.*24", N)] = "city24"
# Klub Park
N[grep("[ck]lu.*park", N)] = "klub_park"
N[grep("^pa.*k$", N)] = "klub_park"
# Coffeina
N[grep("coffe.*na$", N)] = "coffeina"
N[grep("cofei", N)] = "coffeina"
# Recode unidentified Coffe Heavens to NAs
N[grep("cof.*hea", N)] = NA
# Coffeetura
N[grep("cof.*e.*ra$", N)] = "coffeetura"
# Costa Coffe Krakowskie Przedmieście
N[grep("cost.*kra", N)] = "costa_krk_przedmieście"
# Recode unidentified Costa Coffes into NA
N[grep("costa ", N)] = NA
N[grep("co.*ta$",N)] = NA

# Backup save
N.back4 = N

# Cuda Na Kiju
N[grep("cuda", N)] = "cuda_na_kiju"
# Cytadela
N[grep("cyta", N)] = "cytadela"
# Czuły Barbarzyńca
N[grep("barba|czu[lł]", N)] = "czuły_barbarzyńca"
# CH Targówek
N[grep("c.*h.*targów|multi.*targó", N)] = "ch_targówek"
# Delite Club
N[grep("deli", N)] = "delite_club"
# Recode a joke to NA
N[grep("długa.*ta", N)] = NA
# Centrum Sportowe Karowa
N[grep("doj|karo", N)] = "cs_karowa"
N[grep("jud.*uw", N)] = "cs_karowa"
# Recode all answers 'dom' to NAs
N[grep("do.*m$|dom*a|dom*h|dom.*chł", N)] = NA
N[grep("dom.*zna|dom.*rodz", N)] = NA
# Recode 'road to job' to NA
N[grep("droga do", N)] = NA

# Backup save
N.back5 = N

# Dwóch Takich
N[grep("dw[oó]", N)] = "dwóch_takich"

# Dworzec Wileński
N[grep("wile[nń]", N)] = "dw_Wileński"

# Klub Dziekanat 161
N[grep("dziekana", N)] = "klub_dziekanat_161"

# Emerald's Pub
N[grep("emer", N)] = "emeralds_pub"

# Recode unidentified Empik to NA
N[grep("empik", N)] = NA

# Recode unidentified Energy Fitness Clubs to NAs
N[grep("energ", N)] = NA

# Enklawa
N[grep("enkla", N)] = "enklawa"

# Eufemia
N[grep("eufe", N)] = "eufemia"

# Save backup - last table - [145:160]
N.back6 = N

# Explosion Club
N[grep("explo", N)] = "explosion_club"
# Francuska 30
N[grep("f.*30", N)] = "francuska_30"
# Lodowisko Figlowisko
N[grep("figlo", N)] = "lodowisko_figlowisko"
N[grep("lodo.*imie", N)] = "lodowisko_figlowisko"
# Filtry i Pole Mokotowskie
N[grep("filt|pol[ae]", N)] = "filtry/pole_mokotowskie"
# Kawiarnia Fawory
N[grep("fa[wv]", N)] = "kawiarnia_fawory"
# Recode unidentified fitness clubs to NAs
N[grep("^fit.*ss$", N)] = NA
N[grep("fitness klub$|klub fitness$", N)] = NA
N[grep("mc.*fit", N)] = NA
# Park Fontann
N[grep("^fontan|multi.*fon|^par.*fon|^star.*fon", N)] = "park_fontann"
# Fort Bema
N[grep("for.*bem", N)] = "fort_bema"
N[grep("fot.*be", N)] = "fort_bema"
# Fort Włochy
N[grep("for.*wło", N)] = "fort_włochy"

# Backup save
N.back7 = N

# Francuska
N[grep("franc.*ska$|ępie$|saskiej$", N)] = "francuska"
# Galeria Mokotów
N[grep("ga.*mokotó|tok.*moko", N)] = "galeria_mokotów"
# Galeria Wypieków
N[grep("wypie", N)] = "galeria_wypieków"
# Zachęta
N[grep("zach[eę]", N)] = "zachęta"
# Club Garage
N[grep("gara", N)] ="club_garaż"
# Gimnazjum nr 16 na ul. Skarżyńskiego
N[grep("skarż", N)] = "gimnazjum16_na_skarżyńskiego"
# Glam
N[grep("gla", N)] = "glam"
# Głębokie Gardło
N[grep("głębo", N)] = "głębokie_gardło"
# Górki Ursynowskie
N[grep("gór[ka]", N)] = "górki_ursynowskie"
# Grawitacja / Jaś i Małgosia
N[grep("grawi|ja[sś].*ma[łl]", N)] = "grawitacja/jaś_i_małgosia"
# Green Pub
N[grep("gre.*pub", N)] = "green_pub"
# Recode unidentified green coffees to NAs
N[grep("gree.*coffee$", N)] = NA
# Green Coffee na Placu Konstytucji
N[grep("green.*konsty", N)] = "green_coffee_pl_konstytucji"
# Recod unidentified Grycan sweatshops to NAs
N[grep("gry.*an$", N)] = NA
N[grep("gry.*oboj", N)] = NA
N[grep("gryc", N)]
# Hala sportowa na Banacha
N[grep("bana[hc]", N)] = "hala_sportowa_banacha"
# Harenda
N[grep("^ha.*ren|^pi.*haren|^pu.*haren", N)] = "harenda"
# Recode unidentified Holmes Place to NA
N[grep("holm.*ce$", N)] = NA
# Hula Kula
N[grep("hula", N)] = "hula_kula"
# Huśtawka
N[grep("hu[sś]", N)] = "huśtawka"
# Stadion Hutnika
N[grep("hutn", N)] = "stadion_hutnika"
# Hybrydy
N[grep("hybr", N)] = "hybrydy"

# Backup save
N.back8 = N

# Hydrozagadka
N[grep("hydro|11.*isto", N)] = "hydrozagadka"
# IKS Mokotów
N[grep("iks.*kot", N)] = "iks_mokotów"
# Unidentified football pitches on Mokotów to NA
N[grep("orli.*kot", N)] = NA
# Indeks
N[grep("ind.*[ckx]", N)] = "indeks"
# Pizzeria Nowolipki
N[grep("izz.*nowo", N)] = "pizzeria_nowolipki"
# Unidentified place or expression to NA
N[grep("i tak", N)] = NA
# Jeff's
N[grep("jef", N)] = "jeffs"
# Kampus Centralny UW
N[grep("uniw.*szawski$", N)] = "kampus_główny_uw"
N[grep("kamp.*ce.*uw|kamp.*g[lł].*uw", N)] = "kampus_główny_uw"
N[grep("uw$", N)[11]] = "kampus_główny_uw"
# Karma
N[grep("karma|karm.*zbawi", N)] = "karma"
# Kawiarnia Hoża51
N[grep("hoż.*51", N)] = "kawiarnia_hoża51"
# Kawiarnia Marcinek
N[grep("marcin", N)] = "kawiarnia_marcinek"
# Kawiarnia Relaks
N[grep("rel", N)] = "cafe_relaks"
# Cafe Szpilka
N[grep("szpil", N)] = "cafe_szpilka"
# Tarabuk
N[grep("tarab", N)] = "tarabuk"
# Unidentified Wedel cafes and chocolateries to NAs
N[grep("wedl", N)] = NA

# Backup save
N.back9 = N

# Recode unidentified kebabs to NAs
N[grep("kebab $", N)] = NA
N[grep("keb.*emil", N)] = NA
# Unidentified Kebab Kings to NAs
N[grep("kerb|keb.*king", N)] = NA
# Unidentified Amrti Kebabs to NA
N[grep("amri", N)] = NA
# Unidentified Kebab Adana to NA
N[grep("adana", N)] = NA
# Kępa Potocka
N[grep("k[eę].*poto", N)] = "kępa_potocka"
# Unidentified KFCs to NAs
N[grep("kfc", N)] = NA
# Unidentified cinems (kino) to NA
N[grep("^k.*no$", N)] = NA
# Kinoteka
N[grep("kinot", N)] = "kinoteka"
# Kino Atlantic
N[grep("atlan", N)] = "kino_atlantic"
# Kino Luna
N[grep("kin.*lun", N)] = "kino_luna"
N[grep("luna", N)] = "kino_luna"
# Kino Muranów
N[grep("kin.*mur", N)] = "kino_muranów"
N[grep("muran", N)] = "kino_muranów"
# Kino Praha
N[grep("kin.*pra", N)] = "kino_praha"
# Kino Wisła
N[grep("kin.*wis", N)] = "kino_wisła"
# Unidentified club (klub) to NA
N[grep("^kl.*b$", N)] = NA
# Klubogaleria przy pl. Wilsona
N[grep("klu.*wils", N)] = "klubogaleria_wilsona"
# Klubokawiarnia
N[grep("^klubo.*wiarnia$", N)] = "klubokawiarnia"
# Latawiec
N[grep("lataw", N)] = "latawiec"

# Backup save
N.back10 = N

# Klub 22
N[grep("klu.*22", N)] = "klub_22"
# Klub Komediowy
N[grep("klu.*komed", N)] = "klub_komediowy"
# Proxima
N[grep("prox", N)] = "proxima"
# Klub Rama
N[grep("rama", N)] = "klub_rama"
# Klub Stodoła
N[grep("stodo", N)] = "klub_stodoła"
# Ząbkowska
N[grep("z[aą]bk", N)] = "ząbkowska"
# Kopa Cywila
N[grep("kopa", N)] = "kopa_cywila"
# Krakowskie Przedmieśce/Nowy Świat
N[grep("^nowy|^krak", N)] = "krk_przedmieście/nowy_świat"
N[grep("^ul. no|loc.*now.*wia|oko.*krak|bar we.*krak|bary.*now", N)] = "krk_przedmieście/nowy_świat"
N[grep("loc.*nowy", N)] = "krk_przedmieście/nowy_świat"
# Pawilony
N[grep("pawilo", N)] = "pawilony"
# Królikarnia
N[grep("króli", N)] = "królikarnia"
# Unidentified bookstore to NA
N[grep("ksi[ęe]garni", N)] = NA
# Kufle I Kapsle
N[grep("kufle", N)] = "kufle_i_kapsle"
# Land Club
N[grep("land c", N)] = "land_club"
# Zapiecek na Nowym Świecie
N[grep("zapiec.*now", N)] = "zapiecek_nowy_świat"
# Unidentified forests to NAs
N[grep("^l.*s$|^la.*y$", N)] = NA
# Lasek Bielański
N[grep("las.*biela[nń]", N)] = "lasek_bielański"
# Lasek Bródnowski
N[grep("las.*br[oó]d", N)] = "lasek_bródnowski"
# Lasek na Kole
N[grep("las.*kol", N)] = "lasek_na_kole"
# Las Bemowski
N[grep("las.*bemo", N)] = "las_bemowski"
# Las Kabacki
N[grep("las.*kaba|las.*kabc", N)] = "las_kabacki"
# Las Młociński
N[grep("las.*m[lł]o", N)] = "las_młociński"
N[grep("par.*m[lł]oci", N)] = "las_młociński"
# Latający Holender
N[grep("lat.*hole", N)] = "latający_holender"

# Backup save
N.back11 = N

# Park Łazienkowski/Ujazdowski
N[grep("[lł]azien", N)] = "park_łazienkowski"
N[grep("łazin", N)] = "park_łazienkowski"
N[grep("par.*ujaz|łazien", N)] = "park_łazienkowski/ujazdowski"
# Leniviec
N[grep("leni", N)] = "leniviec"
# Pub Local
N[grep("loca", N)] = "pub_local"
# Lotnisko Chopina
N[grep("lotni.*cho", N)] = "lotnisko_chopina"
# Loving Hut
N[grep("lovin", N)] = "loving_hut"
# Lustra
N[grep("lust", N)] = "po_drugiej_stronie_lustra"
# Unidentified ice skating place to NA
N[grep("łyż.*żon", N)] = NA
# Mały Wojtek
N[grep("ma[lł].*woj", N)] = "mały_wojtek"
# Mam Ochotę
N[grep("mam.*chot", N)] = "mam_ochotę"
# Manekin
N[grep("manek", N)] = "manekin"
# Cmentarz Mauzoleum Żołnierzy Radzieckich
N[grep("radzie", N)] = "mauzoleum_żołnierzy_radzieckich"
# Mazowiecka
N[grep("mazowiecka", N)] = "mazowiecka"
# Mazowiecki Park Krajobrazowy to NA
N[grep("mazow.*park", N)] = NA
# Unidntified McDonalds to NAs
N[grep("mcdon.*[ds ]$", N)] = NA
N[grep("mc d", N)] = NA
# McDonalds Górczewska
N[grep("donal.*g[oó]r", N)] = "mcdonalds_górczewska"
# Warsaw Subway to NA
N[grep("metro w", N)] = NA
# Private flats to NAs
N[grep("mieszk", N)] = NA
# Miasto Cypel
N[grep("cype", N)] = "miasto_cypel"
# Między Nami
N[grep("mi[eę]dz.*ami", N)] = "między_nami"
# Ministerstwo Kawy
N[grep("mini.*kaw|cof.*mini", N)] = "ministerstwo_kawy"
# Unidentified footbal pitche in Mokotów to NA
N[grep("moko.*orli", N)] = NA
# Mono Bar
N[grep("mono", N)] = "mono_bar"
# Muzemu Sztuki Nowoczesnej
N[grep("msn|muze.*nowo|sztu", N)] = "msn"
# Unidntified Multikinos to NA
N[grep("^mul.*ino$", N)] = NA
# Multikino Ursynów
N[grep("multiki.*ursy", N)] = "multikino_ursynów"
# Multikino Wola Park
N[grep("multiki.*wol", N)] = "multikino_wola_park"
# Multipub
N[grep("multipu", N)] = "multipub"
# Piw Paw
N[grep("piw.*paw|pif.*paf", N)] = "piw_paw"

# Backup save
N.back12 = N

# Muzeum Narodowe
N[grep("muze.*naro", N)] = "muzeum_narodowe"
# Muzeum Powstania Warszawskiego
N[grep("muze.*powst", N)] = "muzeum_powstania_warszawskiego"
# Muzeum Historii Żydów Polskich
N[grep("muze.*[zż]y|muz.*his", N)] = "muzeum_historii_żydów_polskich"
# Cafe Bistro Myjnia
N[grep("myjni", N)] = "cafe_bistro_myjnia"
# My o My
N[grep("my.*my", N)] = "my_o_my"
# Na Lato
N[grep("na.*lato$", N)] = "na_lato"
# Negative responses to NAs
N[grep("nie.*byw|nie.*mam|nie.*ma.*tak", N)] = NA
# Nie Zawsze Musi Być Chaos
N[grep("nie.*zaw.*chao", N)] = "nie_zawsze_musi_byc_chaos"
# Unidntified nightskating to NA
N[grep("night.*atin", N)] = NA
# Pub Nora
N[grep("nora$", N)] = "pub_nora"
# Bar Norka
N[grep("norka$", N)] = "bar_norka"
# Jerozolima
N[grep("^no.*jeroz", N)] = "jerozolima"
# Stare i Nowe Miasto
N[grep("star[oó]w", N)] = "stare/nowe_miasto"
N[grep("star.*mias|now.*mias", N)] = "stare/nowe_miasto"
N[grep("tra.*kr", N)] = "stare/nowe_miasto"
N[grep("pa[lł]a.*kr", N)] = "stare/nowe_miasto"
N[grep("pla.*zamk", N)] = "stare/nowe_miasto"
N[grep("podzam", N)] = "stare/nowe_miasto"
# Ogród Saski
N[grep("ogr.*sas", N)] = "ogród_saski"
# Ogródek Wydziału Psychologii
N[grep("ogr.*psyc", N)] = "ogródek_psychologii"
N[grep("ogr.*staw", N)] = "ogródek_psychologii"
# Ogród Krasińskich
N[grep("ogr.*kras", N)] = "ogród_krasińskich"
N[grep("par.*krasi", N)] = "ogród_krasińskich"

# Backup save
N.back13 = N

# Plac Bankowy
N[grep("pla.*bankowego$", N)] = "plac_bankowy"
N[grep("^pl. bank", N)] = "plac_bankowy"
# Plac Zbawiciela
N[grep("zbawi", N)] = "plac_zbawiciela"
# Stary Mokotów
N[grep("sta.*moko", N)] = "stary_mokotów"
# Śródmieście
N[grep("[sś]r[oó]dmie", N)] = "śródmieście"
# Opera Club
N[grep("opera", N)] = "opera_club"
# Organza
N[grep("organ", N)] = "organza"
# Unidentified neighbourhood to NA
N[grep("osiedle$", N)] = NA
# Las na Wawrze
N[grep("las.*wawr", N)] = "las_na_wawrze"
# OSIR Żoliborz
N[grep("osi.*[zż]ol", N)] = "osir_żoliborz"
# O Obrotach Ciał Niebieskich
N[grep("cia.*niebie", N)] = "o_obrotach_ciał_niebieskich"
# Pałac Wilanowski i okolice
N[grep("pa[lł]a.*wila", N)] = "pałac_wilanowski/okolice"
# Państwomiasto
N[grep("pa[nń].*mia", N)] = "państwomiasto"
# Ogórdki działkowe Radiowo
N[grep("radiow", N)] = "ogródki_działkowe_radiowo"
# Paradox Cafe
N[grep("parado", N)] = "paradox_cafe"
# Pardon To Tu
N[grep("pardo", N)] = "pardon_to_tu"
# Unidentified park to NAs
N[grep("parki $|parki$", N)] = NA
# Parking Bar
N[grep("parking$", N)] = "parking_bar"
# Parkingowa Zagłębie Klubowe
N[grep("parkingowa$", N)] = "parkingowa_klub"
N[grep("parkingowa|parkingowej", N)] = "parkingowa_zagłębie_klubowe"
# PKP Powiśla
N[grep("^pkp.*pow|^wars.*pow", N)] = "pkp_powiśle"
# Powiśle
N[grep("powi[sś]le |powi[sś]lu|powisle|^powi[sś]le", N)] = "powiśle"
# Żoliborz/parki żoliborskie
N[grep("pa.*[zż]oli", N)] = "żoliborz_parki"
N[grep("żoliborz_|żoliborskie.*ice", N)] = "żoliborz/parki"
# Park Bródnowski
N[grep("park.*br[oó]d", N)] = "park_bródnowski"
# Park Dolinka Służewiecka
N[grep("par.*doli.*s[lł]u", N)] = "park_dolina_służewiecka"
# Park Balaton
N[grep("^par.*balato", N)] = "park_nad_balatonem"
# Pub nad Balatonem
N[grep("pub.*bala", N)] = "pub_nad_balatonem"
# Park Moczydło
N[grep("par.*moczy", N)] = "park_moczydło"
N[grep("par.*elek", N)] = "park_moczydło"
# Park Morskie Oko
N[grep("par.*morsk", N)] = "park_morskie_oko"
# Unidentified park in Sadyba
N[grep("par.*sadyb", N)] = NA
# Park Praski
N[grep("par.*pras", N)] = "park_praski"
# Park Polińskiego
N[grep("par.*garwo", N)] = "park_polińskiego"
# Plac Defilad / Pałac Kultury
N[grep("par.*pa[lł]a.*kultu", N)] = "plac_defilad"
N[grep("pla.*defi", N)] = "plac_defilad"
N[grep("pl.*kultu|pla.*defi", N)] = "plac_defilad/pałac_kultury"
# Unidentified park in Ursynów
N[grep("par.*poto.*s[lł]u", N)] = NA
# Park Skaryszewski
N[grep("skarysz", N)] = "park_skaryszewski"

# Backup save
N.back14 = N

# Park Szczęśliwicki
N[grep("par.*szcz[ęe][sś]", N)] = "park_szczęśliwicki"
# Park Szymańskiego
N[grep("par.*szyma[nń]", N)] = "park_szymańskiego"
# Park Sowińskiego
N[grep("par.*sowi[ńn]|par.*s[lł]o", N)] = "park_sowińskiego"
# Park outside of Warsaw to NA
N[grep("jabło", N)] = NA
# Park Żołnierzy Żywiciela
N[grep("par.*żywi", N)] = "park_żołnierzy_żywiciela"
# Unidentified train platform to NA
N[grep("peron", N)] = NA
# Pijalnia Wódki i Piwa
N[grep("pijal.*w[oó].*pi", N)] = "pijalnia_wódki_piwa"
N[grep("pijal.*pi.*w[oó]", N)] = "pijalnia_wódki_piwa"
N[grep("pijal.*w[oó]", N)] = "pijalnia_wódki_piwa"
# Unidentified place to NA
N[grep("ping pong", N)] = NA
# Pink Flamingo
N[grep("pink", N)] = "pink_flamingo"
# Unidentified Pizza Huts to NAs
N[grep("piz.*hut$", N)] = NA
# Unidentified pizzeria to NA
N[grep("piz.*przy.*poli", N)] = NA
# Pizzeria na barskiej
N[grep("piz.*barsk", N)] = "pizzeria_barska"
# Pizzeria Potocka 21
N[grep("potocka.*21", N)] = "pizzeria_potocka21"

# Backup save
N.back15 = N

# Plac Konstytucji
N[grep("plac.*konsty", N)]
# Plac Zabaw
N[grep("pla.*zaba", N)] = "plac_zabaw"
# Plan B
N[grep("pla.*b$", N)] = "plan_b"
# Platinium
N[grep("platin", N)] = "platinium"
# Kawiarnia Plażowa
N[grep("pla[zż]ow", N)] = "kawiarnia_plażowa"
# Unidentified outdoor place to NA
N[grep("plene", N)] = NA
# Plac Teatralny
N[grep("teatral", N)] = "plac_teatralny"
# Unidentified swimming pool to NA
N[grep("walnie$", N)] = NA
# Unidentified park to NA
N[grep("^pobli.*ark$", N)] = NA
# Pochwała Niekonsekwencji
N[grep("pochwała", N)] = "pochwała_niekonsekwencji"
# Restauracja Polka
N[grep("polka", N)] = "restauracja_polka"
# Klubokawiarnia Polonez
N[grep("polonez", N)] = "klubokawiarnia_polonez"
# Kawiarnia Południk Zero
N[grep("południ.*ero", N)] = "kawiarnia_południk_zero"
# Pomnik Kościuszkowski
N[grep("kosciuszko", N)] = "pomnik_kościuszkowski"
# Pomost 511
N[grep("pomo.*511", N)] = "pomost_511"
# Powiększenie
N[grep("powi[eę]ksz", N)] = "powiększenie(kiedyś)"

# Backup save
N.back16 = N

# Powsin
N[grep("powsi[nń]", N)] = "powsin"
# Unidentified workplac to NA
N[grep("praca$", N)] = NA
# Unidentified part of Praga to NA
N[grep("praga$", N)] = NA
# Targowa
N[grep("praga p.*ta", N)] = "targowa"
# Inżynierska
N[grep("inżynie", N)] = "inżynierska"
# Pracovnia
N[grep("pracovn", N)] = "pracovnia"
# Bar Prasowy
N[grep("prasow", N)] = "bar_prasowy"
# Kawiarnia Prochownia
N[grep("procho", N)] = "kawiarnia_prochownia"
# Progresja
N[grep("progres", N)] = "progresja"
# Klub Przestrzeń Prywatna
N[grep("przes.*pryw", N)] = "klub_przestrzeń_prywatna"
# Unidentified pubs to NAs
N[grep("^pu.*b$", N)] = NA
N[grep("^pu.*by$", N)] = NA
# Białołęka
N[grep("białołę", N)] = "białołęka"
# Inside
N[grep("insid|insajd", N)] = "inside"
# Pub Kaktus
N[grep("pub.*kak", N)] = "pub_kaktus"
# Unidentified pub in Natolin
N[grep("pub.*nato", N)] = NA
# Resort
N[grep("resort", N)] = "resort"
# Retrospekcja
N[grep("rstros|retros", N)] = "retrospekcja"
# Zakątek
N[grep("pu.*zak[aą]t", N)] = "zakątek"
# Puszcza Kampinoska (sic!) to NA
N[grep("kampinos", N)] = NA
# Regeneracja
N[grep("regen", N)] = "regeneracja"

# Backup save
N.back17 = N

# Unidentified restaurant to NA
N[grep("^rest.*racja$", N)] = NA
# Restauracja Hoża 25
N[grep("hoż.*25", N)] = "restauracja_hoża25"
# Łosiowe Błota
N[grep("łosio.*bło", N)] = "łosiowe_błota"
# Unidentified parents' place to NA
N[grep("^rodz.*ce$", N)] = NA
# Równonoc
N[grep("równo n|równon", N)] = "równonoc"
# Unidentified S4 Fitness Clubs to NAs
N[grep("s4$", N)] = NA
# Unidentified recording studio to NA
N[grep("sala.*pr", N)] = NA
# Unidentified dance studio to NA
N[grep("sal.*ta[nń]", N)] = NA
# SAM na Lipowej
N[grep("^sam", N)] = "sam_lipowa"
# Saska Kępa
N[grep("sask.*k[ęe]p", N)] = "saska_kępa"
# Ścianka na Obozowej 60
N[grep("obozo.*60", N)] = "ścianka_obozowa60"
# Secret Life Cafe
N[grep("secr.*li.*ca", N)] = "secret_life_cafe"
# Sen Pszczoły
N[grep("sen.*szczo", N)] = "sen_pszczoły(kiedyś)"
# Unidentified Sphinx restaurants to NAs
N[grep("sfinx$|sphinx$", N)] = NA
# Sphinx w centrum
N[grep("sphinx.*cen", N)] = "sphinx_centrum"
# SGH
N[grep("sgh", N)] = "sgh"
# Unidentified shot bar to NA
N[grep("^shot.*bar$", N)] = NA
# Siedziba ARMA PL
N[grep("siedzib.*arm", N)] = "siedziba_armapl"
# Unidentified fitness clubs to NAs
N[grep("si[lł]o.*nia$", N)] = NA
# Siłownia OverJah
N[grep("sił.*overja", N)] = "siłownia_overjah"

# Backup save
N.back18 = N

# Sketch NITE
N[grep("sketch|nite", N)] = "sketch_nite"
# Unidentified square to NA
N[grep("^sk.*er$", N)] = NA
# unidentified school
N[grep("skzol|szko[lł]a$", N)] = NA
# Sioux
N[grep("siux|sioux", N)] = "sioux"
# Solec 44
N[grep("solec|sol.*44", N)] = "solec44"
# Spiskowcy Rozkoszy
N[grep("spisk.*koszy|spsi", N)] = "spiskowcy_rozkoszy"
# Stadion Narodowy
N[grep("stad.*narod", N)] = "stadion_narodowy"
# Unidentified Starbucks
N[grep("starbucks cafe$", N)] = NA
N[grep("starbucks$|starbucks $|starbucks  $", N)] = NA
# Starlight
N[grep("sta.*ligh", N)] = "starlight"

# Backup save
N.back18 = N

# Stary Wawrzyszew
N[grep("wawrzy", N)] = "stary_wawrzyszew"
# Unidentified joga studio to NA
N[grep("stu.*jogi", N)] = NA
# Olga Kinga Tatoo
N[grep("olg.*king.*tat", N)] = "olga_kinga_tatoo"
# Unidentified Subway Sandwich Bars to NAs
N[grep("subway$", N)] = NA
# Sultan Club
N[grep("sulta", N)] = "sultan_club"
# Sushi Akashia al. Jana Pawła
N[grep("sush.*akash", N)] = "sushi_akashi_jana_pawła"
# SWPS
N[grep("swps", N)] = "swps"
# Ścianka na Nowowiejskiej
N[grep("[sś]cia.*nowow", N)] = "ścianka_nowowiejska"
# Ścianka na Warszawiance
N[grep("[sś]cian.*warsz", N)] = "ścianka_warszawianka"
# Unidentified boulder clubs to NAs
N[grep("[sś]cian.*wspin.*we$|[sś]cian.*wspin.*we $", N)] = NA
# Unidentified place in Ursynów to NA
N[grep("ście.*ursyn", N)] = NA

# Backup save
N.back19 = N

# Teatr Baza
N[grep("teat.*aza", N)] = "teatr_baza"
# Tel-Aviv Cafe
N[grep("te.*viv", N)] = "tel_aviv_cafe"
# Temat Rzeka
N[grep("tema.*zeka", N)] = "temat_rzeka"
# The Eve Club
N[grep("the.*ve", N)] = "the_eve_club"
# Tor kartingowy na Modularnej
N[grep("modular", N)] = "tor_kartingowy_modularna"
# Unidentified total fitness club to NA
N[grep("tot.*fitne", N)] = NA
# Teatr Rozmaitości
N[grep("tr$", N)[4]] = "teatr_rozmaitości"
# Biking route is nor really a place, or is it? But anyways- to NA
N[grep("tras.*gass", N)] = NA
N[grep("tras.*czersk", N)] = NA
N[grep("tras.*row", N)] = NA
# Unidentified school/uni to NA
N[grep("uczelnia$", N)] = NA
# Marszałkowska
N[grep("marszałkowska$", N)] = "marszałkowska"
# Unspecified street to NA
N[grep("ul.*feli", N)] = NA
# Poznańska
N[grep("poznańsk", N)] = "poznańska"

# Backup save
N.back20 = N

# Kulinarny Bzik
N[grep("kulinarny bzik", N)] = "kulinarny_bzik"
# Ursynów
N[grep("ursynów -", N)] = "ursynów"
# Uniwer Pub
N[grep("uni[vw]er.*pub", N)] = "uniwer_pub"
# Unidentified places to NAs
N[grep("u koleż", N)] = NA
N[grep("u znaj", N)] = NA
# Ulubiona Cafe
N[grep("ulubion", N)] = "ulubiona_cafe"
# U Pana Michała
N[grep("u pana", N)] = "u_pana_michała"
# Vapiano
N[grep("vapian", N)] = "vapiano"
# Warsaw Tortilla Factory
N[grep("tortil.*faco", N)] = "warsaw_tortilla_factory"

# Backup save
N.back21 = N
# Wars i Sawa
N[grep("wars.*sawa", N)] = "wars_i_sawa"
# ZOO
N[grep("zoo", N)] = "zoo"
# Unidentified Wiking restaurant to NA
N[grep("wiking", N)] = NA
# Wilanów
N[grep("wilan[oó]w$", N)] = "wilanów"
# Warszawianka
N[grep("^wa.*szawianka$|wodny pa", N)] = "warszawianka"
# Wola/Mirów
N[grep("wola$|mir[oó]w", N)[c(1,3)]] = "wola/mirów"
# Wola Park
N[grep("wola.*park", N)] = "wola_park"
# Przekąski Zakąski
N[grep("zak[aą]ski", N)] = "przekąski_zakąski"
# Zapiecek
N[grep("zapiecek", N)] = "zapiecek"
# Zielonka Gęś
N[grep("zielona g[eę][sś]", N)] = "zielona_gęś"
# Znajomi Znajomych
N[grep("znajomi$|znajomi zna", N)] = "znajomi_znajomych"
# Cocktailbar Zamieszanie
N[grep("zamieszani", N)] = "cocktailbar_zamieszanie"
# Foksal Joga
N[grep("foksa.*joga", N)] = "foskal_joga"
# Strange signs to NAs
N[grep("-$|/.$", N)] = NA
# Unidentified students halls to NAs
N[grep("akad[ae]mik", N)] = NA
# Akademia Gorilla
N[grep("gorila", N)] = "akademia_gorilla"
# Arena Ursynów
N[grep("aren.*ursyn", N)] = "arena_ursynów"
# Wilcza/Poznańska
N[grep("wilcz[ae]|pozna[nń]sk", N)] = "wilcza/poznańska"
# Bar Słoik
N[grep("s[lł]oik", N)] = "bar_słoik"
# Bar Aloha
N[grep("aloha", N)] = "bar_aloha"
# Correct Grawitacja
N[grep("grawitac", N)]

# Backup save
N.back22 = N


N = sort(N)
N.uniq = unique(N)

# Some by hand corrections
N.uniq = gsub(" ", "_", N.uniq)
N.uniq = gsub("-", "_", N.uniq)
save(N, file="Places/PlaceNames.RData")
save(N.uniq, file="Places/PlaceNamesUniq.RData")
save.image("Places/PlaceNamesWork.RData")



#################################
### CHANGING PLACE NAMES IN P ###
#################################
P.back = P # Full data backup
NP = matrix(0, nrow=565, ncol=1) # checking matrix for assigning names
rownames(NP) = N.uniq
p1 = P[,1] # Firts columns of P data frame
p1 = gsub("\"", "", p1)
df1 = approxMatch(p1, N.uniq)

# Ok this is done in the console partially by hand - no documentation for that - sorry