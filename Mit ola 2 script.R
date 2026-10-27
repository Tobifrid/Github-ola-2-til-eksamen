# =====================================================================
# OLA 2 – Boliger, forbrugertillid, forbrug og Eurostat (opgave 1–5)
# Skrevet i samme stil som OLA 3: pipe |>, dplyr og danske navne uden forkortelser.
# Kør altid scriptet fra toppen. Opgave 4 bruger data, der hentes i opgave 2.
# =====================================================================
library(readr)
newhomes_til_ola_2 <- read_csv("newhomes_til_ola_2.csv")   # boligdata fra Newhomes (bruges i opgave 1.3)
View(newhomes_til_ola_2)

library(dkstat)
library(tidyverse)   # indeholder dplyr (filter, mutate, group_by ...), tidyr (pivot_wider) og ggplot2
library(scales)      # label_number() til pæne tal på akserne

# ---- Datafiler: hentes fra GitHub ned i projektmappen, hvis de ikke allerede ligger der ----
github_mappe <- "https://raw.githubusercontent.com/Tobifrid/Github-ola-2-til-eksamen/main/"   # mit eget repo
datafiler    <- c("DST_BY1.csv", "Landekoder.csv", "00_vaelg_lande.R")
for (fil in datafiler) {                                                               # loop: fil for fil
  if (!file.exists(fil)) download.file(paste0(github_mappe, fil), destfile = fil, mode = "wb")   # hent kun, hvis filen mangler
}
file.exists(datafiler)
# TRUE TRUE TRUE: alle 3 filer ligger nu i mappen

# ---- Hjælpefunktioner, som bruges flere steder ----

# Laver et bynavn om til samme format i både boligdata og DST-data, fx "Sønderborg" -> "soenderborg"
til_slug <- function(tekst) {
  tekst <- tolower(tekst)                  # små bogstaver
  tekst <- gsub("æ", "ae", tekst)
  tekst <- gsub("ø", "oe", tekst)
  tekst <- gsub("å", "aa", tekst)
  tekst <- gsub("[^a-z0-9]+", "-", tekst)  # alt der ikke er bogstav eller tal bliver til "-"
  gsub("^-+|-+$", "", tekst)               # fjern "-" i starten og slutningen
}

# Laver et tal om til kroner med punktum som tusindtalsskilletegn, fx 44352 -> "44.352 kr."
kroner <- function(tal) paste0(format(round(tal), big.mark = ".", decimal.mark = ",", trim = TRUE), " kr.")

# Laver en dato om til et kvartal som tekst, fx 2020-04-01 -> "2020 K2"
kvartal_tekst <- function(dato) paste0(format(dato, "%Y"), " K", (as.numeric(format(dato, "%m")) - 1) %/% 3 + 1)


# =====================================================================
# Opgave 1.1: Find en tabel med byer og indbyggertal
# =====================================================================
tabel_soegning <- dst_search(string = "befolkning", field = "text")   # søg i DST's tabeller efter ordet "befolkning"
nrow(tabel_soegning)
# 133 tabeller

# Søgningen kigger kun i titlen, så vi samler hver tabels variabelnavne til én tekst og søger i den
tabel_soegning$variabel_tekst <- sapply(tabel_soegning$variables, paste, collapse = ", ")

tabeller_med_by <- tabel_soegning |>
  filter(grepl("by", variabel_tekst, ignore.case = TRUE)) |>   # kun tabeller, hvor en variabel indeholder "by"
  select(id, text, variabel_tekst)
tabeller_med_by
# BY1, BY2 og BY3 – vi vælger BY1, fordi den giver de rå indbyggertal pr. byområde

by1_meta <- dst_meta(table = "BY1", lang = "da")   # hvilke variable og værdier har BY1?
by1_meta$variables
# BYER, ALDER, KØN og Tid


# =====================================================================
# Opgave 1.2: Kategorivariabel (bycat)
# =====================================================================
# BY1 er hentet som CSV for 1. januar 2026 (alle byer, alle aldre og begge køn samlet)
byer_raadata <- read.csv("DST_BY1.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

# BYER ser fx sådan her ud: "15101100 151-01100 Ballerup (del af Hovedstadsområdet)"
# De første 8 tegn er koden: 3 tegn til kommunen og 5 tegn til bynummeret
byer_alle <- byer_raadata |>
  mutate(kode     = substr(BYER, 1, 8),
         kommune  = substr(kode, 1, 3),
         bynummer = substr(kode, 4, 8),
         navn     = sub("^[0-9]{8} [0-9]{3}-[0-9]{5} ", "", BYER),   # fjern koderne foran navnet
         navn     = sub(" \\(del af.*\\)$", "", navn))               # fjern "(del af Hovedstadsområdet)"

# Fjern de rækker, der ikke er rigtige byer
byer <- byer_alle |>
  filter(kode != "00001100",                    # hele Hovedstadsområdet (vi bruger kommunedelene i stedet)
         bynummer != "99999",                   # landdistrikter
         bynummer != "99997",                   # uden fast bopæl
         bynummer != paste0("00", kommune))     # kommunetotaler
nrow(byer)
# ca. 1.618 byområder

# Byer med samme navn lægges sammen, og hver by får en kategori ud fra indbyggertallet
byer <- byer |>
  mutate(by_slug = til_slug(navn)) |>
  group_by(by_slug) |>
  summarise(indbyggere = sum(value)) |>
  mutate(bycat = cut(indbyggere,
                     breaks = c(-Inf, 1000, 5000, 20000, 100000, Inf),   # grænserne mellem kategorierne
                     labels = c("landsby", "lille by", "almindelig by", "større by", "storby"),
                     right  = FALSE))                                    # right = FALSE: 1.000 hører til "lille by"

byer |> count(bycat)
# landsby 983, lille by 378, almindelig by 95, større by 46, storby 5

# Tabellen til rapporten: vores kategorier bygger på DST's egne størrelsesklasser
kategori_tabel <- data.frame(
  Kategori    = c("landsby", "lille by", "almindelig by", "større by", "storby"),
  Indbyggere  = c("under 1.000", "1.000 - 4.999", "5.000 - 19.999", "20.000 - 99.999", "100.000 eller flere"),
  DST_klasser = c("landdistrikt, 200-249, 250-499, 500-999", "1.000-1.999, 2.000-4.999",
                  "5.000-9.999, 10.000-19.999", "20.000-49.999, 50.000-99.999", "100.000-999.999"),
  Antal_byer  = as.numeric(table(byer$bycat))
)
kategori_tabel


# =====================================================================
# Opgave 1.3: Merge boligdata (OLA 1) med bycat
# =====================================================================
# newhomes_til_ola_2 er indlæst øverst i scriptet. Den er rå tekst, fx pris = "3.200.000 kr.", kvm = "126 m²" og zip = "Vivild, 8961 Allingåbro"
boliger <- newhomes_til_ola_2 |>
  filter(!is.na(bolig_id)) |>                                            # fjern tomme rækker
  distinct(bolig_id, .keep_all = TRUE) |>                                # samme bolig må kun være med én gang
  mutate(pris    = as.numeric(gsub("[^0-9]", "", pris)),                 # fjern alt der ikke er tal: "3.200.000 kr." -> 3200000
         kvm     = ifelse(grepl("^[0-9.]+ m", kvm), as.numeric(gsub("[^0-9]", "", kvm)), NA),   # kun rigtige m², fx "126 m²" -> 126
         postnr  = as.numeric(sub(".*([0-9]{4}) .*", "\\1", zip)),      # de 4 cifre i zip: "Vivild, 8961 Allingåbro" -> 8961
         by      = sub(".*[0-9]{4} ", "", zip),                          # teksten efter postnummeret: "Allingåbro"
         kvmpris = pris / kvm,                                           # pris pr. kvadratmeter
         by_slug = til_slug(by))                                         # samme format som DST-byerne, så de kan sættes sammen
nrow(boliger)
# 11.339 boliger, når tomme rækker og dubletter er fjernet

mean(boliger$by_slug %in% byer$by_slug)
# kun ca. 74 % af boligerne matcher direkte på bynavn

boliger |>
  filter(!by_slug %in% byer$by_slug) |>   # de boliger, der ikke matcher
  count(by_slug, sort = TRUE) |>
  head(15)
# mange har et retningssuffiks (koebenhavn-s, aarhus-c) eller er bydele i Hovedstadsområdet (kastrup, valby, hellerup)

# Bydele i Hovedstadsområdet kobles via postnummer til DST's kommunedel (fx Hellerup -> Gentofte)
# Det er en tilnærmelse, fordi postnumre ikke følger kommunegrænserne helt præcist
postnummer_til_kommunedel <- c(
  "2100" = "koebenhavn",      "2150" = "koebenhavn",      "2200" = "koebenhavn",
  "2300" = "koebenhavn",      "2400" = "koebenhavn",      "2450" = "koebenhavn",
  "2500" = "koebenhavn",      "2700" = "koebenhavn",      "2720" = "koebenhavn",
  "2600" = "glostrup",        "2605" = "broendby",        "2660" = "broendby",
  "2610" = "roedovre",        "2620" = "albertslund",     "2625" = "vallensbaek",
  "2665" = "vallensbaek",     "2635" = "ishoej",          "2650" = "hvidovre",
  "2670" = "greve-strand",    "2730" = "herlev",          "2740" = "ballerup",
  "2750" = "ballerup",        "2760" = "ballerup",        "2770" = "taarnby",
  "2800" = "lyngby-taarbaek", "2830" = "lyngby-taarbaek",
  "2840" = "rudersdal",       "2850" = "rudersdal",       "2950" = "rudersdal",
  "2860" = "gladsaxe",        "2880" = "gladsaxe",
  "2820" = "gentofte",        "2870" = "gentofte",        "2900" = "gentofte",
  "2920" = "gentofte",        "2930" = "gentofte"
)

# Håndlavede aliasser, hvor Newhomes' bynavn afviger fra DST's
alias <- c("nykoebing-sj" = "nykoebing-s",
           "risskov"      = "aarhus",
           "hoejbjerg"    = "aarhus")

# Vi matcher i fire trin. case_when() tager det første trin, der er TRUE
boliger <- boliger |>
  mutate(postnummer_by = case_when(
    postnr >= 1000 & postnr <= 1799 ~ "koebenhavn",
    postnr >= 1800 & postnr <= 2000 ~ "frederiksberg",
    TRUE ~ unname(postnummer_til_kommunedel[as.character(postnr)])),   # resten slås op i listen ovenfor
    uden_suffiks = sub("-[a-z]{1,3}$", "", by_slug),                       # "aarhus-c" og "randers-soe" bliver til "aarhus" og "randers"
    match_slug = case_when(
      by_slug %in% names(alias)      ~ unname(alias[by_slug]),   # 1) alias
      !is.na(postnummer_by)          ~ postnummer_by,            # 2) postnummer i Hovedstadsområdet
      by_slug %in% byer$by_slug      ~ by_slug,                  # 3) direkte match på bynavn
      uden_suffiks %in% byer$by_slug ~ uden_suffiks,             # 4) uden retningssuffiks
      TRUE                           ~ NA_character_))           # ellers intet match

# left_join() = merge(all.x = TRUE): alle boliger beholdes, også dem uden match
boliger_med_bycat <- boliger |>
  left_join(byer |> select(by_slug, indbyggere, bycat), by = c("match_slug" = "by_slug"))

sum(!is.na(boliger_med_bycat$bycat))
nrow(boliger_med_bycat)
# 10.527 af 11.339 boliger (92,8 %) har fået en bycat

boliger_med_bycat |>
  filter(is.na(bycat)) |>   # de boliger, der stadig ikke har en bycat
  count(by_slug, sort = TRUE) |>
  head(15)
# lokale stednavne, som DST ikke opgør som byområder, fx alleroed, smoerum og egaa


# =====================================================================
# Opgave 1.4: Dataframe og plot
# =====================================================================
# De første 10 boliger med by, pris, kvadratmeterpris og bycat, sorteret efter by
eksempel_boliger <- boliger_med_bycat |>
  filter(!is.na(by), !is.na(bycat), !grepl("^[0-9]", by)) |>     # spring bynavne over, der starter med et tal
  select(by, pris, kvmpris, bycat) |>
  arrange(by) |>
  head(10) |>
  mutate(pris    = kroner(pris),
         kvmpris = format(round(kvmpris), big.mark = ".", decimal.mark = ",", trim = TRUE))
eksempel_boliger

# Kun boliger med både bycat og kvadratmeterpris
boliger_til_plot <- boliger_med_bycat |>
  filter(!is.na(bycat), !is.na(kvmpris))

# Gennemsnit og median pr. bytype
pris_pr_bytype <- boliger_til_plot |>
  group_by(bycat) |>
  summarise(gennemsnit_kvmpris = mean(kvmpris),
            median_kvmpris     = median(kvmpris),
            antal              = n())
pris_pr_bytype
# Gennemsnittet er højere for landsby end for lille by, fordi enkelte meget dyre boliger trækker det op.
# Vi bruger derfor medianen, som er mere robust over for outliers.

# Hvor mange gange dyrere er en storby end en landsby?
faktor <- pris_pr_bytype$median_kvmpris[pris_pr_bytype$bycat == "storby"] /
  pris_pr_bytype$median_kvmpris[pris_pr_bytype$bycat == "landsby"]
faktor
# ca. 3,8

pris_pr_bytype |>
  ggplot(aes(x = bycat, y = median_kvmpris, fill = bycat)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = kroner(median_kvmpris)), vjust = -0.6, size = 4, fontface = "bold") +   # prisen over hver søjle
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title    = paste0("Boliger i storbyer koster ca. ", format(round(faktor, 1), decimal.mark = ","),
                         " gange så meget pr. m² som i landsbyer"),
       subtitle = "Median kvadratmeterpris efter bytype. Landsby og lille by ligger på samme niveau.",
       x = NULL, y = "Median pris pr. m² (kr.)",
       caption  = paste0("Kilde: Newhomes og Danmarks Statistik (BY1). ",
                         format(nrow(boliger_til_plot), big.mark = ".", decimal.mark = ","), " boliger.")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

# De 3 dyreste byer i hver bytype (kun byer med mindst 10 boliger)
bynavne <- byer_alle |>
  mutate(match_slug = til_slug(navn)) |>
  distinct(match_slug, .keep_all = TRUE) |>   # ét navn pr. by
  select(match_slug, By = navn)

top3_pr_bytype <- boliger_til_plot |>
  group_by(match_slug, bycat) |>
  summarise(median_kvmpris = median(kvmpris), antal = n(), .groups = "drop") |>
  filter(antal >= 10) |>
  left_join(bynavne, by = "match_slug") |>
  group_by(bycat) |>
  slice_max(median_kvmpris, n = 3, with_ties = FALSE) |>   # de 3 højeste i hver bytype
  mutate(Rang = row_number()) |>
  ungroup() |>
  transmute(Bytype = bycat, Rang, By,
            `Median pris pr. m²` = kroner(median_kvmpris),
            `Antal boliger`      = antal)
top3_pr_bytype
# Storby: Frederiksberg (ca. 66.600 kr.), København (ca. 58.200 kr.) og Aarhus (ca. 41.900 kr.)


# =====================================================================
# Opgave 2.1: Opdatering af DI's forbrugertillidsindikator
# =====================================================================
# Y = den årlige realvækst i husholdningernes forbrug (NKN1, P.31)
# X = DI's forbrugertillidsindikator (F2, F4, F9, F10) og DST's forbrugertillidsindikator (F1)

# ---- FORV1: hent alle spørgsmål én gang (bruges også i opgave 4) ----
forv1_raadata <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = "*", Tid = "*"),   # "*" = alle spørgsmål og alle måneder
  lang = "da",
  parse_dst_tid = FALSE                       # behold tiden som tekst, fx "2026M07"
) |>
  mutate(aar     = as.numeric(substr(TID, 1, 4)),
         maaned  = as.numeric(substr(TID, 6, 7)),
         kvartal = paste0(aar, "K", ceiling(maaned / 3)))   # fx "2026K3"

# DI's indikator: gennemsnittet af F2, F4, F9 og F10 i alle kvartalets måneder
di_fti_kvartal <- forv1_raadata |>
  filter(grepl("^F2 |^F4 |^F9 |^F10 ", INDIKATOR)) |>   # ^ = starter med, mellemrummet gør at F10 ikke tæller som F1
  group_by(kvartal) |>
  summarise(DI_FTI = mean(value)) |>
  rename(TID = kvartal)

forv1_raadata |> filter(grepl("^F2 |^F4 |^F9 |^F10 ", INDIKATOR)) |> distinct(INDIKATOR)
# tjek: skal give præcis 4 spørgsmål

# DST's indikator: den er allerede beregnet af DST, så vi tager bare gennemsnittet pr. kvartal
dst_fti_kvartal <- forv1_raadata |>
  filter(grepl("Forbrugertillidsindikatoren", INDIKATOR)) |>
  group_by(kvartal) |>
  summarise(DST_FTI = mean(value)) |>
  rename(TID = kvartal)

# ---- NKN1: husholdningernes forbrug og den årlige realvækst ----
NKN1 <- dst_get_data(
  table = "NKN1",
  query = list(
    TRANSAKT  = "P.31 Husholdningernes forbrugsudgifter",
    PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
    "SÆSON"   = "Sæsonkorrigeret",
    Tid       = "*"
  ),
  lang = "da",
  parse_dst_tid = FALSE
) |>
  arrange(TID) |>                                                  # sorter efter tid, så lag() rammer samme kvartal året før
  mutate(aarlig_realvaekst = (value / lag(value, 4) - 1) * 100)    # vækst ift. samme kvartal året før i procent

# ---- Saml væksten og begge indikatorer i én tabel, 2000K1 til 2026K2 ----
model_data <- NKN1 |>
  select(TID, aarlig_realvaekst) |>
  inner_join(di_fti_kvartal,  by = "TID") |>   # inner_join beholder kun kvartaler, der findes i begge
  inner_join(dst_fti_kvartal, by = "TID") |>
  filter(TID >= "2000K1", TID <= "2026K2")     # tekst kan sammenlignes, fordi formatet er det samme
range(model_data$TID)
# "2000K1" "2026K2"

# ---- Korrelation og forklaringsgrad for begge indikatorer ----
model_di  <- lm(aarlig_realvaekst ~ DI_FTI,  data = model_data)
model_dst <- lm(aarlig_realvaekst ~ DST_FTI, data = model_data)

sammenligning <- data.frame(
  Indikator       = c("DI-FTI", "DST-FTI"),
  Korrelation     = c(cor(model_data$DI_FTI,  model_data$aarlig_realvaekst),
                      cor(model_data$DST_FTI, model_data$aarlig_realvaekst)),
  Forklaringsgrad = c(summary(model_di)$r.squared, summary(model_dst)$r.squared)
)
sammenligning
# DI-FTI: korrelation 0,61 og R2 37,4 %. DST-FTI: korrelation 0,51 og R2 26,0 %.
# DI's indikator har stadig den stærkeste sammenhæng med forbruget, men begge er faldet
# ift. DI's tal fra 2016 (DI-FTI: r = 0,73 og R2 = 0,54. FTI: r = 0,65 og R2 = 0,42).


# =====================================================================
# Opgave 2.2: Forudsig den årlige realvækst i 3. kvartal 2026
# =====================================================================
di_2026K3  <- di_fti_kvartal  |> filter(TID == "2026K3")   # indikatoren i 3. kvartal 2026
dst_2026K3 <- dst_fti_kvartal |> filter(TID == "2026K3")

predict(model_di,  newdata = di_2026K3,  interval = "prediction")
predict(model_dst, newdata = dst_2026K3, interval = "prediction")
# fit = forudsigelsen, lwr og upr = det 95 %-forudsigelsesinterval
# DI: -0,24 % (-4,62 til 4,14). DST: -0,74 % (-5,53 til 4,06).
# OBS: Tallene er fra 1. oktober 2026, hvor kun juli og august var udgivet.
# Kører du scriptet nu, er september med, og så ændrer tallene sig lidt.


# =====================================================================
# Opgave 2.3 og 2.4: Salg resten af året og prognoser fra DI og Nationalbanken
# =====================================================================
# 2.3: Begge modeller peger på en svagt negativ realvækst i 3. kvartal 2026, men intervallerne
#      er brede og dækker både fald og vækst. Forbruget har vokset siden 2. kvartal 2024,
#      og modellen bruger kun forbrugertilliden som forklarende variabel. Så vi ville ikke
#      være bekymrede ud fra modellen alene.
# 2.4: DI (maj 2026) forventer 1,8 % og Nationalbanken (marts 2026) 1,9 % for hele 2026.
#      Begge er højere end vores kvartalsvise estimater, men de gælder hele året og bygger
#      på flere oplysninger end forbrugertilliden.


# =====================================================================
# Opgave 3.1: Modellernes forudsigelser (beregnet i hånden)
# =====================================================================
coef(model_di)
coef(model_dst)
# DI: skæring 2,24 og hældning 0,19. DST: skæring 1,42 og hældning 0,16.

# Estimeret vækst = skæring + hældning * indikator. Residual = faktisk vækst - estimeret vækst
model_data <- model_data |>
  mutate(estimeret_di  = coef(model_di)[1]  + coef(model_di)[2]  * DI_FTI,
         estimeret_dst = coef(model_dst)[1] + coef(model_dst)[2] * DST_FTI,
         residual_di   = aarlig_realvaekst - estimeret_di,
         residual_dst  = aarlig_realvaekst - estimeret_dst)
# det samme kan fås med fitted(model_di) og residuals(model_di)


# =====================================================================
# Opgave 3.2: Residualer
# =====================================================================
# Residualer mod de estimerede værdier for hver model
model_data |>
  ggplot(aes(x = estimeret_di, y = residual_di)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residualer vs. estimerede værdier - DI-FTI",
       x = "Estimeret årlig realvækst", y = "Residual") +
  theme_minimal()

model_data |>
  ggplot(aes(x = estimeret_dst, y = residual_dst)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residualer vs. estimerede værdier - DST-FTI",
       x = "Estimeret årlig realvækst", y = "Residual") +
  theme_minimal()

# Residualer over tid for begge modeller i samme graf
model_data |>
  ggplot(aes(x = TID)) +
  geom_point(aes(y = residual_di,  colour = "DI-FTI")) +
  geom_point(aes(y = residual_dst, colour = "DST-FTI")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete(breaks = model_data$TID[seq(1, nrow(model_data), by = 4)]) +   # vis kun hvert 4. kvartal på aksen
  labs(title    = "Modellens præcision varierer over tid",
       subtitle = "I nogle perioder ligger estimaterne tæt på den faktiske realvækst, mens de\ni andre perioder afviger markant mere.",
       x = "Kvartal", y = "Residualer", colour = "Model",
       caption = "Kilde: Egen tilvirkning efter data fra DST") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Over 0: modellen ramte for lavt. Under 0: modellen ramte for højt.
# De største fejl ses omkring 2008-2009 og 2020-2023.


# =====================================================================
# Opgave 3.3: RSS og TSS
# =====================================================================
RSS_di  <- sum(model_data$residual_di^2)    # den variation, DI-modellen ikke kan forklare
RSS_dst <- sum(model_data$residual_dst^2)   # den variation, DST-modellen ikke kan forklare
TSS     <- sum((model_data$aarlig_realvaekst - mean(model_data$aarlig_realvaekst))^2)   # den samlede variation i væksten

RSS_di
RSS_dst
TSS
# RSS_di 499,20, RSS_dst 590,25 og TSS 797,22.
# TSS er den samme for begge, fordi det er den samme vækst, der skal forklares.


# =====================================================================
# Opgave 3.4: Forklaringsgraden
# =====================================================================
# R2 = 1 - RSS / TSS: den andel af variationen, som modellen forklarer
R2_di  <- 1 - RSS_di  / TSS
R2_dst <- 1 - RSS_dst / TSS
R2_di
R2_dst
# DI: 0,37 og DST: 0,26 – det samme som summary(model)$r.squared i opgave 2.1


# =====================================================================
# Opgave 4.1: Illustration af forbrugertillid fra 1996
# =====================================================================
# Månederne laves om til kvartaler: gennemsnittet af de tre måneder for hvert spørgsmål.
# kvartal_dato er første dag i kvartalet, så ggplot kan bruge den som en rigtig dato på x-aksen.
forv1_kvartal <- forv1_raadata |>
  filter(TID >= "1996M01") |>
  mutate(kvartal_dato = as.Date(sprintf("%d-%02d-01", aar, (ceiling(maaned / 3) - 1) * 3 + 1))) |>   # fx 1996-04-01
  group_by(INDIKATOR, kvartal, kvartal_dato) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = INDIKATOR, values_from = value) |>   # ét spørgsmål pr. kolonne
  arrange(kvartal_dato)

names(forv1_kvartal) <- sub(" .*", "", names(forv1_kvartal))   # kun F-nummeret som kolonnenavn, fx "F9"
names(forv1_kvartal)
# "kvartal" "kvartal_dato" "F1" "F10" ... "F9"

# Perioder med kriser (grå) og med gode tider (grønne) til grafen
kriser <- data.frame(
  start  = as.Date(c("2001-03-01", "2007-06-01", "2020-02-01", "2021-10-01", "2024-09-01")),
  slut   = as.Date(c("2003-03-01", "2009-06-01", "2020-07-01", "2023-06-01", "2025-06-01")),
  tekst  = c("IT-boblen", "Finanskrisen", "Covid-19", "Energi- og\ninflationskrise", "Ny amerikansk\npræsident"),
  hoejde = c(22, 22, 22, 16, 22)   # forskellig højde, så teksterne ikke overlapper
)
gode_perioder <- data.frame(
  start  = as.Date(c("1997-01-01", "2005-06-01", "2014-02-01")),
  slut   = as.Date(c("2000-01-01", "2007-06-01", "2016-07-01")),
  tekst  = c("Faldende ledighed\nog god vækst", "Boligpriser der\nsteg voldsomt", "Lav rente og\nstigende boligpriser"),
  hoejde = c(-22, -22, -22)
)

forv1_kvartal |>
  ggplot(aes(x = kvartal_dato, y = F1)) +
  geom_rect(data = kriser, aes(xmin = start, xmax = slut, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey75", alpha = 0.4) +
  geom_rect(data = gode_perioder, aes(xmin = start, xmax = slut, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "lightgreen", alpha = 0.4) +
  geom_text(data = kriser, aes(x = start + (slut - start) / 2, y = hoejde, label = tekst),
            inherit.aes = FALSE, size = 3) +
  geom_text(data = gode_perioder, aes(x = start + (slut - start) / 2, y = hoejde, label = tekst),
            inherit.aes = FALSE, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1.08) +
  scale_x_date(date_breaks = "2 years", labels = kvartal_tekst) +
  labs(title = "DST's forbrugertillidsindikator, 1996-i dag", subtitle = "Kvartalsvise værdier",
       x = "Kvartal", y = "DST-FTI", caption = "Kilde: Danmarks Statistik, tabel FORV1.") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Mest optimistiske midt i 2000'erne og i 2014-2016. Mindst optimistiske under Covid-19 og energikrisen.


# =====================================================================
# Opgave 4.2: Gennemsnit af F9 (fordelagtigt at anskaffe større forbrugsgoder?)
# =====================================================================
forv1_fra_2000 <- forv1_kvartal |> filter(kvartal_dato >= as.Date("2000-01-01"))

mean(forv1_fra_2000$F9)
summary(forv1_fra_2000$F9)[c(1, 3, 4, 6)]   # minimum, median, gennemsnit og maksimum
# min -44,43, median -8,05, gennemsnit -10,84 og maks -0,03: aldrig positiv, selv i de bedste kvartaler

forv1_fra_2000 |>
  ggplot(aes(x = kvartal_dato, y = F9)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = mean(forv1_fra_2000$F9), colour = "red", linetype = "dashed") +   # gennemsnittet
  annotate("text", x = as.Date("2023-06-01"), y = mean(forv1_fra_2000$F9) + 2,
           label = "Gennemsnit", colour = "red", hjust = 0, size = 3.5) +
  geom_line(linewidth = 1.08) +
  scale_x_date(date_breaks = "2 years", labels = kvartal_tekst) +
  labs(title = "Fordelagtighed ved at anskaffe større forbrugsgoder, 2000-i dag",
       subtitle = "Kvartalsvise værdier med streg for gennemsnittet",
       x = "Kvartal", y = "Nettotal", caption = "Kilde: Danmarks Statistik, tabel FORV1.") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# F9 sammenlignet med DST's forbrugertillidsindikator (F1)
forv1_fra_2000 |>
  ggplot(aes(x = kvartal_dato)) +
  geom_line(aes(y = F1, colour = "Forbrugertillidsindikatoren"), linewidth = 1.08) +
  geom_line(aes(y = F9, colour = "Anskaffelse af forbrugsgoder (fordelagtigt)"), linewidth = 1.08) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(date_breaks = "2 years", labels = kvartal_tekst) +
  scale_colour_manual(values = c("Forbrugertillidsindikatoren" = "black",
                                 "Anskaffelse af forbrugsgoder (fordelagtigt)" = "steelblue")) +
  labs(title = "Forbrugertillid vs. holdning til forbrugsgoder", subtitle = "Kvartalsvise værdier, 2000-i dag",
       x = "Kvartal", y = "Nettotal", colour = NULL, caption = "Kilde: Danmarks Statistik, tabel FORV1.") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
# F9 stiger kun lidt i gode tider, men falder meget i kriser: den virker mest som en pessimisme-indikator


# =====================================================================
# Opgave 4.3: De 15 grupper af forbrug
# =====================================================================
# NAHC021 (år) bruges til "hvad brugte danskerne flest penge på", fordi kædede værdier ikke kan lægges sammen
# NKHC021 (kvartal) bruges til ændringen fra ét kvartal til et andet
forbrug_grupper_aar <- dst_get_data(
  table = "NAHC021",
  query = list(FORMAAAL = "*", PRISENHED = "2020-priser, kædede værdier", Tid = "*"),
  lang = "da",
  parse_dst_tid = FALSE
) |>
  mutate(FORMAAAL = substring(FORMAAAL, 5)) |>   # fjern koden foran navnet på gruppen
  select(FORMAAAL, TID, mio_kr = value)

forbrug_grupper_kvartal <- dst_get_data(
  table = "NKHC021",
  query = list(FORMAAAL = "*", PRISENHED = "2020-priser, kædede værdier", "SÆSON" = "Sæsonkorrigeret", Tid = "*"),
  lang = "da",
  parse_dst_tid = FALSE
) |>
  mutate(FORMAAAL = substring(FORMAAAL, 5)) |>
  select(FORMAAAL, TID, mio_kr = value)

# Hvad brugte danskerne flest penge på i 2025?
forbrug_grupper_aar |>
  filter(TID == "2025", FORMAAAL != "I alt") |>
  ggplot(aes(x = reorder(FORMAAAL, mio_kr), y = mio_kr / 1000)) +   # reorder sorterer søjlerne efter størrelse
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Danskerne brugte flest penge på boligbenyttelse i 2025",
       subtitle = "Husholdningernes forbrug fordelt på formål - Mia. kr., kædede 2020-priser",
       x = NULL, y = "Mia. kr.", caption = "Kilde: Danmarks Statistik, tabel NAHC021.") +
  theme_minimal()

# Hvilken gruppe steg mest fra 2023K2 til 2026K2?
aendring_i_forbrug <- forbrug_grupper_kvartal |>
  filter(TID %in% c("2023K2", "2026K2"), FORMAAAL != "I alt") |>
  pivot_wider(names_from = TID, values_from = mio_kr) |>   # én kolonne pr. kvartal
  mutate(aendring_procent = (`2026K2` / `2023K2` - 1) * 100)   # backticks, fordi kolonnenavnet starter med et tal

aendring_i_forbrug |>
  ggplot(aes(x = reorder(FORMAAAL, aendring_procent), y = aendring_procent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = 'Gruppen af forbruget der steg mest fra 2023K2\ntil 2026K2 er "Køb af køretøjer"',
       subtitle = "Husholdningernes forbrug fordelt på formål: ændring 2023K2-2026K2.\nProcentvis ændring i forbrug, kædede 2020-priser.",
       x = NULL, y = "Ændring i forbrug (%)", caption = "Kilde: Danmarks Statistik, tabel NKHC021.") +
  theme_minimal()
# Køb af køretøjer steg ca. 49 %. Fødevarer faldt mest.


# =====================================================================
# Opgave 4.4: 30 simple lineære regressioner (15 grupper x 2 indikatorer)
# =====================================================================
# DI's indikator for hvert kvartal: gennemsnittet af F2, F4, F9 og F10
forv1_kvartal <- forv1_kvartal |>
  mutate(DI_FTI = (F2 + F4 + F9 + F10) / 4)

# Én kolonne pr. forbrugsgruppe og én række pr. kvartal fra 2000K1, sat sammen med de to indikatorer
regressions_data <- forbrug_grupper_kvartal |>
  filter(TID >= "2000K1") |>
  pivot_wider(names_from = FORMAAAL, values_from = mio_kr) |>
  inner_join(forv1_kvartal |> select(kvartal, F1, DI_FTI), by = c("TID" = "kvartal"))

forbrugsgrupper <- setdiff(names(regressions_data), c("TID", "I alt", "F1", "DI_FTI"))   # alle kolonner undtagen disse
length(forbrugsgrupper)
# 15 grupper

# Loopet laver 2 regressioner for hver gruppe og gemmer begge summaries i en liste med gruppens navn
resultater <- list()
for (gruppe in forbrugsgrupper) {                   # loop: gruppe for gruppe
  forbrug <- regressions_data[[gruppe]]              # forbruget i gruppen er y
  resultater[[gruppe]] <- list(
    DST_lm = summary(lm(forbrug ~ regressions_data$F1)),       # x = DST's indikator
    DI_lm  = summary(lm(forbrug ~ regressions_data$DI_FTI))    # x = DI's indikator
  )
}
length(resultater)
# 15 lister med 2 summaries hver = 30 regressioner

# Træk R2 ud af alle summaries
forklaringsgrader <- data.frame(
  gruppe = names(resultater),
  DST_R2 = sapply(resultater, function(resultat) resultat$DST_lm$r.squared),
  DI_R2  = sapply(resultater, function(resultat) resultat$DI_lm$r.squared),
  row.names = NULL
) |>
  arrange(desc(DST_R2))
forklaringsgrader
# Højest: Medicin (0,28), Beklædning (0,27) og Boligbenyttelse (0,23). DST slår DI i næsten alle grupper.

forklaringsgrader |>
  pivot_longer(c(DST_R2, DI_R2), names_to = "Indikator", values_to = "R2") |>   # én række pr. gruppe og indikator
  mutate(Indikator = ifelse(Indikator == "DST_R2", "DST-FTI", "DI-FTI"),
         gruppe    = factor(gruppe, levels = rev(forklaringsgrader$gruppe))) |>   # samme rækkefølge som tabellen
  ggplot(aes(x = gruppe, y = R2, fill = Indikator)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("DST-FTI" = "steelblue", "DI-FTI" = "grey")) +
  labs(title = "Forklaringsgrad (R²) for DST-FTI vs. DI-FTI",
       subtitle = "Simple lineære regressioner mod de 15 forbrugsgrupper",
       x = NULL, y = "Forklaringsgrad (R²)", fill = NULL,
       caption = "Kilde: Danmarks Statistik, tabel NKHC021 og FORV1, 2000K1 - 2026K2.") +
  theme_bw()


# =====================================================================
# Opgave 5.1: Kvartalsvis årlig realvækst for en række Eurolande
# =====================================================================
library(eurostat)

source("00_vaelg_lande.R")   # laver vektoren "lande" med landekoderne, fx c("DK", "BE", "NL" ...)

# Sådan fandt vi tabellen og koderne i Eurostat
eurostat_soegning <- search_eurostat("Household final consumption expenditure", type = "dataset")
eurostat_soegning[, c("code", "title")]
# tabellen hedder namq_10_fcs
get_eurostat_dic("na_item") |> filter(code_name == "P31_S14")                           # husholdningernes forbrugsudgift
get_eurostat_dic("unit")    |> filter(code_name %in% c("CLV20_MEUR", "CLV_PCH_SM"))     # kædede 2020-priser

# Data hentes via Eurostats API: husholdningernes forbrug, kædede 2020-priser, sæsonkorrigeret
eurostat_data <- get_eurostat("namq_10_fcs",
                              filters = list(geo = lande, na_item = "P31_S14", unit = "CLV20_MEUR", s_adj = "SCA"),
                              time_format = "date")   # time bliver en rigtig dato, fx 2020-04-01
table(eurostat_data$geo)

# Landenavne til visning (koden er nøglen, navnet bruges kun i tabeller og grafer)
landekoder <- read.csv2("Landekoder.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
landekoder$TITEL[landekoder$KODE == "NL"] <- "Holland"
iso_kode   <- function(kode) ifelse(kode == "EL", "GR", ifelse(kode == "UK", "GB", kode))   # Eurostat bruger EL og UK
landenavn  <- setNames(landekoder$TITEL[match(iso_kode(lande), landekoder$KODE)], lande)
land_med_kode <- function(kode) paste0(kode, " - ", landenavn[kode])   # fx "DK - Danmark"

# Årlig realvækst (kolonnen vaekst): ændringen i forhold til samme kvartal året før, for hvert land for sig
eurostat_vaekst <- eurostat_data |>
  group_by(geo) |>
  arrange(time, .by_group = TRUE) |>                               # sorter efter tid inden for hvert land
  mutate(vaekst = (values / lag(values, 4) - 1) * 100) |>          # lag(…, 4) = samme kvartal året før
  ungroup() |>
  filter(time >= as.Date("2000-01-01"))

# Seneste kvartal, hvor alle landene har tal
sidste_kvartal_pr_land <- eurostat_vaekst |>
  filter(!is.na(vaekst)) |>
  group_by(geo) |>
  summarise(sidste = max(time))
seneste_kvartal <- min(sidste_kvartal_pr_land$sidste)

eurostat_vaekst <- eurostat_vaekst |> filter(time <= seneste_kvartal)
kvartal_tekst(range(eurostat_vaekst$time))
# fra 2000 K1 til seneste fælles kvartal

# Én lille graf pr. land
eurostat_vaekst |>
  mutate(land = land_med_kode(geo)) |>
  ggplot(aes(x = time, y = vaekst, colour = geo)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_line() +
  geom_point(size = 0.8) +
  facet_wrap(~ land, ncol = 3, scales = "free_y") +   # ét panel pr. land med sin egen y-akse
  labs(title = paste0("Årlig realvækst i husholdningernes forbrug i ", length(lande), " lande, 2000K1 til dags dato"),
       x = NULL, y = "Realvækst i % (samme kvartal året før)",
       caption = "Kilde: Eurostat, tabel namq_10_fcs. Plot: genereret i RStudio") +
  theme_minimal() +
  theme(legend.position = "none")


# =====================================================================
# Opgave 5.2: Højeste gennemsnitlige kvartalsvise årlige realvækst
# =====================================================================
gennemsnit_pr_land <- eurostat_vaekst |>
  group_by(geo) |>
  summarise(gennemsnit = mean(vaekst)) |>
  arrange(desc(gennemsnit))
gennemsnit_pr_land
# Sverige 2,22 %, Spanien 1,60 % ... Italien laveste med 0,41 %

gennemsnit_pr_land |>
  mutate(land = factor(land_med_kode(geo), levels = rev(land_med_kode(geo)))) |>   # højeste øverst
  ggplot(aes(x = land, y = gennemsnit, fill = geo == gennemsnit_pr_land$geo[1])) + # det højeste land får mørk farve
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#08519C", "FALSE" = "#9ECAE1")) +
  labs(title = paste0(landenavn[gennemsnit_pr_land$geo[1]], " har haft den højeste gennemsnitlige forbrugsvækst siden 2000"),
       subtitle = "Gennemsnitlig årlig realvækst i husholdningernes forbrug pr. kvartal, 2000K1 til dags dato",
       x = NULL, y = "Gennemsnitlig årlig realvækst (%)",
       caption = "Kilde: Eurostat, tabel namq_10_fcs. Plot: genereret i RStudio") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))


# =====================================================================
# Opgave 5.3: Coronakrisen som outlier
# =====================================================================
# Coronakrisen = 2020K1 til 2021K4 for alle lande: faldet i 2020 og genopretningen i 2021.
# Begge år skal fjernes, fordi væksten i 2021 er kunstigt høj, når den sammenlignes med det lave 2020.
effekt_af_corona <- eurostat_vaekst |>
  mutate(corona = time >= as.Date("2020-01-01") & time <= as.Date("2021-10-01")) |>   # TRUE i Coronakvartalerne
  group_by(geo) |>
  summarise(med_corona  = mean(vaekst),                 # gennemsnit med alle kvartaler
            uden_corona = mean(vaekst[!corona])) |>     # gennemsnit uden Coronakvartalerne
  mutate(forskel = uden_corona - med_corona,            # hvor meget Corona trak gennemsnittet ned
         land    = landenavn[geo]) |>
  arrange(desc(forskel))
effekt_af_corona
# Spanien: størst effekt, +0,38 procentpoint (fra 1,60 til 1,98 %). Danmark: næsten ingen effekt (-0,02)

effekt_af_corona |>
  pivot_longer(c(med_corona, uden_corona), names_to = "type", values_to = "gennemsnit") |>
  mutate(type = ifelse(type == "med_corona", "Med Corona", "Uden Corona"),
         geo  = factor(geo, levels = effekt_af_corona$geo)) |>   # sorteret efter Coronaens effekt
  ggplot(aes(x = geo, y = gennemsnit, fill = type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = c("Med Corona" = "#9ECAE1", "Uden Corona" = "#08519C")) +
  labs(title = paste0("Uden Corona stiger gennemsnittet mest i ", effekt_af_corona$land[1], " (", effekt_af_corona$geo[1], ")"),
       subtitle = "Gennemsnitlig årlig realvækst pr. kvartal med og uden Coronakrisen (2020K1-2021K4), 2000K1 til dags dato",
       x = "Landekode", y = "Årlig realvækst (%)", fill = NULL,
       caption = "Kilde: Eurostat, tabel namq_10_fcs. Plot: genereret i RStudio") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))


# =====================================================================
# Opgave 5.4: Laveste gennemsnitlige vækst fra 2020K1 til i dag
# =====================================================================
gennemsnit_fra_2020 <- eurostat_vaekst |>
  filter(time >= as.Date("2020-01-01")) |>
  group_by(geo) |>
  summarise(gennemsnit = mean(vaekst)) |>
  arrange(gennemsnit)
gennemsnit_fra_2020
# Østrig laveste med 0,69 %, Spanien højeste med 2,00 %

gennemsnit_fra_2020 |>
  mutate(land = factor(land_med_kode(geo), levels = rev(land_med_kode(geo)))) |>
  ggplot(aes(x = land, y = gennemsnit, fill = geo == gennemsnit_fra_2020$geo[1])) +   # det laveste land får mørk farve
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#08519C", "FALSE" = "#9ECAE1")) +
  labs(title = paste0(landenavn[gennemsnit_fra_2020$geo[1]], " har haft den laveste gennemsnitlige vækst fra 2020 til i dag"),
       subtitle = "Gennemsnitlig årlig realvækst pr. kvartal, 1. kvartal 2020 til dags dato",
       x = NULL, y = "Gennemsnitlig årlig realvækst (%)",
       caption = "Kilde: Eurostat, tabel namq_10_fcs. Plot: genereret i RStudio") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))