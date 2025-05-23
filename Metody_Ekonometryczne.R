#############################################
#                                           #
#          METODY EKONOMETRYCZNE            #
#                                           #
#             ~ Marek Polit                 #
#############################################
# Trzeba ustawić directory !!!!!!!!!!!
getwd()
setwd("C:/Users/marco/OneDrive/Documents/School/Metody ekonometryczne")
rm(list = ls())
#####################################
#                                   #
#  Wczytanie niezbędnych paczek     #
#                                   #
#####################################
packages <- c(
  "tidyverse",    
  "lmtest",
  "MASS",
  "tseries",
  "car",
  "corrplot",
  "eurostat",
  "quantmod",
  "xml2",
  "zoo",
  "ggbreak",
  "patchwork",
  "RColorBrewer",
  "ARDL",
  "forecast",
  "seasonal",
  "seastests",
  "urca",
  "vars",
  "dynlm",
  "strucchange"
)

to_install <- setdiff(packages, rownames(installed.packages()))
if (length(to_install)) {
  install.packages(to_install)
}

invisible(lapply(packages, library, character.only = TRUE))

###################################
#                                 #
#  Proces pozyskiwania danych     #
#                                 #
###################################
# -------------------------------
# 1. CPI Indeks
# -------------------------------
cpi_pl <- get_eurostat("prc_hicp_midx", time_format="date") %>%
  filter(geo=="PL", coicop=="CP00", unit=="I15") %>%
  mutate(
    Month     = as.yearmon(TIME_PERIOD),
    CPI_index = values
  ) %>%
  dplyr::select(Month, CPI_index) %>%
  arrange(Month)

# -------------------------------
# 2. Stopa bezrobocia
# -------------------------------
unemp_pl <- get_eurostat("une_rt_m", time_format="date") %>%
  filter(
    geo   == "PL",
    unit  == "PC_ACT",
    age   == "TOTAL",
    sex   == "T",
    s_adj == "NSA"
  ) %>%
  distinct(TIME_PERIOD, .keep_all=TRUE) %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    Month = as.yearmon(TIME_PERIOD),
    Unemp = values
  ) %>%
  dplyr::select(Month, Unemp)

# -------------------------------
# 3. Produkcja przemysłowa
# -------------------------------
df_ip <- get_eurostat("sts_inpr_m", time_format = "date") %>%
  filter(
    geo      == "PL",     
    indic_bt == "PRD",    
    unit     == "I15",    
    s_adj    == "NSA",    
    nace_r2  == "C"       
  ) %>%
  distinct(TIME_PERIOD, .keep_all = TRUE) %>%  
  arrange(TIME_PERIOD) %>%
  transmute(
    Month    = as.yearmon(TIME_PERIOD),  
    IP_manuf = values                    
  )
# -------------------------------
# 4. Cena zamknięcia akcji Orlenu 
# -------------------------------

getSymbols("PKN.WA", src="yahoo", from="2000-01-01", auto.assign=TRUE)
orl_close <- Cl(PKN.WA)
monthly_close <- apply.monthly(orl_close, last)
df_orlen <- data.frame(
  Month       = as.yearmon(index(monthly_close)),
  Close_Orlen = coredata(monthly_close)
)
rm(PKN.WA, orl_close, monthly_close)

# -------------------------------
# 5. Stopy procentowe NBP
# -------------------------------

url <- "https://static.nbp.pl/dane/stopy/stopy_procentowe_archiwum.xml"
doc <- read_xml(url)
positions <- xml_find_all(doc, ".//pozycje")
df_list <- lapply(positions, function(pos) {
  date_str <- xml_attr(pos, "obowiazuje_od")
  entries  <- xml_find_all(pos, "./pozycja")
  data.frame(
    Date = as.Date(date_str),
    id   = xml_attr(entries, "id"),
    rate = as.numeric(gsub(",", ".", xml_attr(entries, "oprocentowanie"))),
    stringsAsFactors = FALSE
  )
})
df_rates <- bind_rows(df_list) %>%
  pivot_wider(names_from = id, values_from = rate) %>%
  arrange(Date) %>%
  mutate(Month = as.yearmon(Date)) %>%
  dplyr::select(Month, everything(), -Date)
rm(df_list, doc, positions, url)

# -------------------------------
# 6. Indeks WIG ze Stooq 
# -------------------------------
url_wig <- "https://stooq.com/q/d/l/?s=wig&d1=19970416&d2=20250612&i=m"
df_wig <- read.csv(url_wig, stringsAsFactors=FALSE) %>%
  mutate(
    Date  = as.Date(Date),
    Month = as.yearmon(Date)
  ) %>%
  dplyr::select(Month, WIG_Close=Close) %>%
  arrange(Month)
rm(url_wig)

# -------------------------------
# 7. Cena ropy Brent z FRED
# -------------------------------


url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=POILBREUSDM&scale=left&cosd=1990-01-01&coed=2025-03-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-05-12&revision_date=2025-05-12&nd=1990-01-01"

df_brent <- read_csv(url, col_types = cols(
  DATE = col_date(format = ""),
  POILBREUSDM = col_double()
))
df_brent <- df_brent %>%
  mutate(Month = as.yearmon(observation_date)) %>%
  rename(Brent_USD = POILBREUSDM) %>%
  dplyr::select(Month, Brent_USD)

rm(url)
# -------------------------------
# 8. Cena gazu Henry Hub z FRED
# -------------------------------
url_gas <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?",
  "id=MHHNGSP&fq=Monthly&fam=avg&cosd=1990-01-01&coed=2025-05-01"
)
df_gas <- read_csv(url_gas, col_types=cols(
  observation_date = col_date(),
  MHHNGSP          = col_double()
)) %>%
  mutate(
    Month   = as.yearmon(observation_date),
    Gas_USD = MHHNGSP
  ) %>%
  dplyr::select(Month, Gas_USD)
rm(url_gas)
# -------------------------------
# 9. Kurs EUR/PLN ze Stooq
# -------------------------------
url_eur <- "https://stooq.com/q/d/l/?s=eurpln&d1=19970102&d2=20250613&i=m"
df_eurpln <- read.csv(url_eur, stringsAsFactors=FALSE) %>%
  mutate(
    Date  = as.Date(Date),
    Month = as.yearmon(Date)
  ) %>%
  dplyr::select(Month, EURPLN=Close) %>%
  arrange(Month)
rm(url_eur)
# -------------------------------
# 10. Kurs USD/PLN ze Stooq
# -------------------------------
url_usd <- "https://stooq.com/q/d/l/?s=usdpln&d1=19970102&d2=20250613&i=m"
df_usdpln <- read.csv(url_usd, stringsAsFactors=FALSE) %>%
  mutate(
    Date  = as.Date(Date),
    Month = as.yearmon(Date)
  ) %>%
  dplyr::select(Month, USDPLN=Close) %>%
  arrange(Month)
rm(url_usd)
###########################################
#                                         # 
#  Proces łączenia i zapisania danych     #
#                                         #
###########################################

df <- df_orlen %>%
  left_join(df_brent,    by = "Month") %>%
  left_join(df_eurpln,   by = "Month") %>%
  left_join(df_gas,      by = "Month") %>%
  left_join(df_rates,    by = "Month") %>%
  left_join(df_usdpln,   by = "Month") %>%
  left_join(df_wig,      by = "Month") %>%
  left_join(df_ip,       by = "Month") %>%
  left_join(unemp_pl,    by = "Month") %>%
  left_join(cpi_pl,      by = "Month")
missing_counts <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols       = everything(),
    names_to   = "Variable",
    values_to  = "MissingCount"
  )
df <- df %>%
  fill(ref, lom, red, dep, dys, .direction = "down")
df <- df[!is.na(df$IP_manuf), ]
df <- df[!is.na(df$ref), ]
df <- df %>% dplyr::select(-dep,-dys)
missing_counts <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols       = everything(),
    names_to   = "Variable",
    values_to  = "MissingCount"
  )
rm(cpi_pl,df_brent, df_eurpln, df_gas, df_orlen, df_rates, df_usdpln, df_wig, df_ip, unemp_pl, missing_counts)

#################################################################################################################
#                                                                                                               #
  write.csv(df, file = file.path(getwd(), "Metody_Ekonometryczne.csv"), row.names = FALSE)    
#                                                                                                               #
#################################################################################################################
#     #             #             #             #             #
#    ###           ###           ###           ###           ###
#   #####         #####         #####         #####         #####
#  #######       #######       #######       #######       #######
#     |             |             |             |             |                       

#############################################################################
#                                                                           #
#  Przegląd danych oraz przygotowanie ich do modelu ekonometrycznego (EDA)  #
#                                                                           #
#############################################################################
rm(list=ls())
df <- read.csv("Metody_Ekonometryczne.csv")
########################Wykres 1#############################################
pol_months <- c(sty=1,lut=2,mar=3,kwi=4,maj=5,cze=6,
                lip=7,sie=8,wrz=9,paź=10,lis=11,gru=12)

df_plot <- df %>%
  mutate(
    mon = substr(Month, 1, 3),
    yr  = sub(".* ", "", Month),
    Date = as.Date(paste0(yr, "-", pol_months[mon], "-01"))
  ) %>%
  dplyr::select(Date, Gas_USD, EURPLN, Brent_USD, USDPLN, PKN.WA.Close) %>%
  pivot_longer(
    cols      = c(Gas_USD, EURPLN, Brent_USD, USDPLN, PKN.WA.Close),
    names_to  = "Series",
    values_to = "Value"
  )


gora <- df_plot %>%
  filter(Series %in% c("Brent_USD", "PKN.WA.Close")) %>%
  ggplot(aes(Date, Value, color = Series)) +
  geom_line(size = 1) +
  scale_color_brewer(
    name    = "Instrument",
    palette = "Set1"      
  ) +
  labs(title = "Cena za beczkę ropy naftowej oraz kurs zamknięcia PKN Orlen", y = "Wartość") +
  theme_minimal() +
  theme(
    axis.text.x     = element_blank(),
    axis.title.x    = element_blank(),
    legend.position = "top"
  )

dol <- df_plot %>%
  filter(Series %in% c("EURPLN", "USDPLN", "Gas_USD")) %>%
  ggplot(aes(Date, Value, color = Series)) +
  geom_line(size = 1) +
  scale_color_brewer(
    name    = "Instrument",
    palette = "Set1"
  ) +
  labs(title = "Kursy walut oraz cena za beczkę gazu", x = "Data", y = "Wartość") +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

gora / dol
rm(dol,gora, df_plot, pol_months)
########################Wykres 2##########################################
df_long <- df %>%
  dplyr::select(PKN.WA.Close, IP_manuf, Unemp, CPI_index) %>%
  pivot_longer(
    cols      = c(IP_manuf, Unemp, CPI_index),
    names_to  = "zmienna",
    values_to = "wartosc"
  )

ggplot(df_long, aes(
  x     = PKN.WA.Close,
  y     = wartosc,
  color = zmienna
)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_x_continuous(
    breaks = seq( min(df_long$PKN.WA.Close), max(df_long$PKN.WA.Close), length.out = 5 ),
    labels = scales::comma_format(accuracy = 0.1)
  ) +
  scale_y_continuous(
    breaks = seq( floor(min(df_long$wartosc)), ceiling(max(df_long$wartosc)), by = 10 )
  ) +
  scale_color_manual(
    breaks = c("IP_manuf", "Unemp", "CPI_index"),
    labels = c("Produkcja przemysłowa", "Stopa bezrobocia", "Indeks cen CPI"),
    values = c("IP_manuf"  = "#1b9e77",
               "Unemp"     = "#d95f02",
               "CPI_index" = "#7570b3")
  ) +
  labs(
    x     = "Cena zamknięcia PKN ORLEN",
    y     = "Wartość zmiennej",
    color = "Zmienna"
  ) +
  theme_minimal()
rm(df_long)
# usuwamy zmienną bezrobocie, gdyż nie różnicuje poziomu zamknięcia PKN Orlen
df <- df %>%dplyr::select(-Unemp)
########################Wykres 3##########################################
df2 <- df %>%
  separate(Month, into = c("m","Y"), sep=" ", convert=TRUE) %>%
  mutate(
    m = dplyr::recode(m,
               sty="01", lut="02", mar="03", kwi="04", maj="05", cze="06",
               lip="07", sie="08", wrz="09", paź="10", paz="10", lis="11", gru="12"
    ),
    Date = as.Date(paste(Y, m, "01", sep = "-"))
  ) %>%
  dplyr::select(-m, -Y)


wig_plot <- ggplot(df2, aes(Date, WIG_Close)) +
  geom_line(color = "#1b9e77", size = 1) +
  labs(y = "Cena zamknięcia WIG") +
  scale_x_date(
    breaks = "5 years",        
    date_labels = "%Y"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

df_rates <- df2 %>%
  dplyr::select(Date, ref, lom, red) %>%
  pivot_longer(c(ref, lom, red), names_to="stopa", values_to="wartosc")

rates_plot <- ggplot(df_rates, aes(Date, wartosc, color = stopa)) +
  geom_line(size = 1) +
  scale_color_manual(
    breaks = c("ref","lom","red"),
    labels = c("Referencyjna","Lombardowa","Depozytowa"),
    values = c(ref="#d95f02", lom="#7570b3", red="#1b9e77")
  ) +
  labs(x = "Data", y = "Poziom stopy (%)", color = "Rodzaj stopy") +
  scale_x_date(
    breaks = "5 years",        # co 5 lat
    date_labels = "%Y"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_line()
  )

wig_plot / rates_plot +
  plot_layout(ncol = 1, heights = c(2,1))
rm(df_rates, df2, rates_plot, wig_plot)
# Ponieważ stopy procentowe mają prawie że identyczną zmienność,
# decytujemy o porzuceniu zmiennych i zostawieniu tylko stopy referencyjnej

df <- df %>% dplyr::select(-lom, -red)
##########################Sprawdzanie sezonowości##########################
df$Date <- as.yearmon(df$Month, format="%b %Y", locale="pl_PL")  

# tworzenie szeregów czasowych
ts_brent       <- ts(df$Brent_USD,     start = c(2002,1), frequency = 12)
ts_eurpln      <- ts(df$EURPLN,        start = c(2002,1), frequency = 12)
ts_gas         <- ts(df$Gas_USD,       start = c(2002,1), frequency = 12)
ts_pkn_close   <- ts(df$PKN.WA.Close,  start = c(2002,1), frequency = 12)
ts_ref         <- ts(df$ref,           start = c(2002,1), frequency = 12)
ts_usdpln      <- ts(df$USDPLN,        start = c(2002,1), frequency = 12)
ts_wig_close   <- ts(df$WIG_Close,     start = c(2002,1), frequency = 12)
ts_ip_manuf    <- ts(df$IP_manuf,      start = c(2002,1), frequency = 12)
ts_cpi_index   <- ts(df$CPI_index,     start = c(2002,1), frequency = 12)
# testowanie szeregów czasowych

isSeasonal(ts_brent, test = "combined")
isSeasonal(ts_eurpln, test = "combined")
isSeasonal(ts_gas, test = "combined")
isSeasonal(ts_pkn_close, test = "combined")
isSeasonal(ts_ref, test = "combined")
isSeasonal(ts_usdpln, test = "combined")
isSeasonal(ts_wig_close, test = "combined")
isSeasonal(ts_ip_manuf, test = "combined")
isSeasonal(ts_cpi_index, test = "combined")

# z testów wyszło, że dwie zmienne są sezonowe, dlatego zbadamy to 
# za pomocą subsidiary plot:

cols2 <- c(
  IP_manuf  = "#17A589",  
  CPI_index = "#8E44AD"   
)
point_size <- 0.1


p_ip <- ggsubseriesplot(ts_ip_manuf) +
  geom_line(color = cols2["IP_manuf"], size = 1) +
  geom_point(color = cols2["IP_manuf"], size = point_size) +
  labs(
    title = "Produkcja przemysłowa (IP manuf.)",
    x     = "Miesiąc",
    y     = "Wartość"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

p_cpi <- ggsubseriesplot(ts_cpi_index) +
  geom_line(color = cols2["CPI_index"], size = 1) +
  geom_point(color = cols2["CPI_index"], size = point_size) +
  labs(
    title = "Indeks cen konsumpcyjnych (CPI)",
    x     = "Miesiąc",
    y     = "Wartość indeksu"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

p_ip / p_cpi

# Ponieważ obrazek wskazuje na niesezonowość CPI, sprawdzimy to używając innej biblioteki:
nsdiffs(ts_cpi_index)
nsdiffs(ts_ip_manuf)
nsdiffs(ts_wig_close)
# Jak widać tylko w przypadku produkcji przemysłowej teza o sezonowości potwierdziła się
rm(cols2, point_size, p_cpi, p_ip)
##############################
#                            #
#      Macierz korelacji     #
#                            #
##############################
vars <- c("PKN.WA.Close", "Brent_USD", "EURPLN", "Gas_USD",
          "ref", "USDPLN", "WIG_Close", "IP_manuf", "CPI_index")


df_lagged <- df %>%
  dplyr::select(all_of(vars)) %>%
  mutate(across(everything(), ~ lag(.x, 1), .names = "{.col}_lag"))

corr_map <- sapply(vars, function(x) {
  sapply(paste0(vars, "_lag"), function(y) {
    cor(df[[x]], df_lagged[[y]], use = "pairwise.complete.obs")
  })
})

corr_df <- as.data.frame(as.table(corr_map))

ggplot(corr_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Freq)), size = 2.8) +
  scale_fill_gradient2(
    midpoint = 0, low = "blue", mid = "white", high = "red", limits = c(-1, 1)
  ) +
  labs(
    x = "Zmienna (t)",
    y = "Zmienna (t-1)",
    fill = "ρ"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title  = element_text(size = 11)
  )

##############################
#                            #
#  Transformacje zmiennych   #
#                            #
##############################
num_cols <- c("Brent_USD","Gas_USD","PKN.WA.Close",
              "WIG_Close","EURPLN","USDPLN",
              "IP_manuf","CPI_index", "ref")

for(col in num_cols){
  x <- ts(df[[col]], start = c(2002,1), frequency = 12)
  lambda <- BoxCox.lambda(x, method="guerrero")  
  cat(sprintf("%-15s  optimal lambda = %.2f\n", col, lambda))
}

df <- df %>%
  mutate(
    Brent_USD_log    = log(Brent_USD),
    Gas_USD_log      = log(Gas_USD),
    PKN_WA_Close_log = log(`PKN.WA.Close`),
    IP_manuf_log     = log(IP_manuf)
  )




df <-  df %>% dplyr::select(-Brent_USD, -Gas_USD, -PKN.WA.Close, -IP_manuf)

rm(col, lambda, num_cols)
##########################
#                        #
#  Testy stacjonarności  #
#                        #
##########################
vars_stat <- df %>% dplyr::select(-Date, -Month) %>% names()

stationarity_tests <- map_df(vars_stat, function(var) {
  x <- na.omit(df[[var]])  
  adf_res  <- adf.test(x)
  kpss_res <- kpss.test(x)
  tibble(
    variable = var,
    adf_p    = adf_res$p.value,
    kpss_p   = kpss_res$p.value,
    stationary = (adf_res$p.value < 0.05) & (kpss_res$p.value > 0.05)
  )
})

print(stationarity_tests)
##### Wniosek: wszystkie zmienne są niestacjonarne

diffs_required <- purrr::map_dfr(vars_stat, function(var) {
  x <- ts(df[[var]], start = c(2002,1), frequency = 12)
  tibble::tibble(
    variable = var,
    d        = forecast::ndiffs(x),
    D        =forecast::nsdiffs(x)
  )
})

print(diffs_required)
# model sugeruje dwukrotne różnicowanie stopy referencyjnej i indeksu CPI
# dlatego w dalszej czesci analizy OZNACZYMY te zmienne jako awaryjne 
rm(ts_brent, ts_cpi_index,ts_eurpln,ts_gas,ts_ip_manuf,ts_pkn_close,ts_ref,ts_usdpln,ts_wig_close)
#######################################
#                                     #
#  Tworzenie stacjonarnych zmiennych  #
#                                     #
#######################################

df_diff <- df %>%
  
  mutate(
    IP_seas = IP_manuf_log - lag(IP_manuf_log, 12)
  ) %>%
  
  mutate(
    d_EURPLN           = EURPLN             - lag(EURPLN),
    d_USDPLN           = USDPLN             - lag(USDPLN),
    d_WIG_Close   = WIG_Close     - lag(WIG_Close),
    d_Brent_USD_log    = Brent_USD_log      - lag(Brent_USD_log),
    d_Gas_USD_log      = Gas_USD_log        - lag(Gas_USD_log),
    d_PKN_WA_Close_log = PKN_WA_Close_log   - lag(PKN_WA_Close_log),
    

    d_ref = ref - 2*lag(ref) + lag(ref, 2),
    d_CPI_index = CPI_index - 2*lag(CPI_index) + lag(CPI_index, 2),
    
    d_IP_manuf_log = IP_seas - lag(IP_seas)
  ) %>%
  
  
  filter(
    !is.na(d_IP_manuf_log)
  )

df_diff <-  df_diff %>% dplyr::select(-EURPLN,-ref, -USDPLN,-WIG_Close, -Brent_USD_log,-Gas_USD_log,-PKN_WA_Close_log, -IP_manuf_log, -CPI_index, -IP_seas)
df_diff <- df_diff %>%
  mutate(
    D_Crisis2008    = ifelse(Date >= as.yearmon("2008-01") & Date <= as.yearmon("2009-12"), 1, 0),
    D_Shock2014_16  = ifelse(Date >= as.yearmon("2014-07") & Date <= as.yearmon("2016-06"), 1, 0),
    D_COVID2020     = ifelse(Date >= as.yearmon("2020-03") & Date <= as.yearmon("2020-12"), 1, 0),
    D_War2022       = ifelse(Date >= as.yearmon("2022-02") & Date <= as.yearmon("2022-12"), 1, 0)
  )


##########################################
#                                        #
#            PEŁNY MODEL LM              #
#                                        #  
##########################################
model_lm_full <- lm(
  d_PKN_WA_Close_log ~ d_EURPLN + d_USDPLN + d_WIG_Close + d_Brent_USD_log +
    d_Gas_USD_log + d_ref + d_IP_manuf_log + d_CPI_index +
    D_Crisis2008 + D_Shock2014_16 + D_COVID2020 + D_War2022,
  data = df_diff
)

# Wyświetl podsumowanie modelu
summary(model_lm_full)
AIC(model_lm_full)
##########################################
#                                        #
#        TESTY NA PEŁNYM MODELU          #
#                                        #  
##########################################
jarque.bera.test(residuals(model_lm_full))      


par(mfrow=c(1,2))
hist(residuals(model_lm_full), main = "Histogram reszt", col = "lightblue", breaks = 20)
qqnorm(residuals(model_lm_full)); qqline(residuals(model_lm_full), col = "red")



dwtest(model_lm_full)                           
bgtest(model_lm_full, order = 4)               


bptest(model_lm_full)                       

ncvTest(model_lm_full)                         

par(mfrow=c(1,1))
plot(model_lm_full$fitted.values, residuals(model_lm_full),
     main = "Reszty vs wartości dopasowane",
     xlab = "Wartości dopasowane", ylab = "Reszty")
abline(h = 0, col = "red")


vif(model_lm_full)  


acf(residuals(model_lm_full), main = "Autokorelacja reszt (ACF)")
pacf(residuals(model_lm_full), main = "Częściowa autokorelacja (PACF)")

##########################################
#                                        #
#            ZREDUKOWANY MODEL           #
#                                        #  
##########################################
model_lm_reduced <- lm(
  d_PKN_WA_Close_log ~ d_USDPLN + d_EURPLN + d_WIG_Close + d_ref +
    D_Shock2014_16 + d_Gas_USD_log,
  data = df_diff
)
summary(model_lm_reduced)
AIC(model_lm_reduced)
##########################################
#                                        #
#      TESTY NA ZREDUKOWANYM MODELU      #
#                                        #  
##########################################

jarque.bera.test(residuals(model_lm_reduced))


par(mfrow=c(1,2))
hist(residuals(model_lm_reduced), main = "Histogram reszt", col = "lightblue", breaks = 20)
qqnorm(residuals(model_lm_reduced))
qqline(residuals(model_lm_reduced), col = "red")



dwtest(model_lm_reduced)                        
bgtest(model_lm_reduced, order = 4)             


bptest(model_lm_reduced)        
ncvTest(model_lm_reduced)       
par(mfrow=c(1,1))
plot(model_lm_reduced$fitted.values, residuals(model_lm_reduced),
     main = "Reszty vs wartości dopasowane",
     xlab = "Wartości dopasowane", ylab = "Reszty")
abline(h = 0, col = "red")


vif(model_lm_reduced)  # Dopuszczalne < 5


acf(residuals(model_lm_reduced), main = "Autokorelacja reszt (ACF)")
pacf(residuals(model_lm_reduced), main = "Częściowa autokorelacja (PACF)")
##########################################
#                                        #
#          ZOPTYMALIZOWANY MODEL         #
#                                        #  
##########################################
model_step <- stepAIC(model_lm_full, direction = "both", trace = TRUE)
summary(model_step)
AIC(model_step)
#####################################################
#                                                   #
#          TESTY NA ZOPTYMALIZOWANYM MODELU         #
#                                                   #  
#####################################################
jarque.bera.test(residuals(model_step))

par(mfrow = c(1, 2))

hist(residuals(model_step),
     main = "Histogram reszt z rozkładem normalnym",
     col = "lightblue",
     breaks = 20,
     freq = FALSE,  
     xlab = "Reszty",
     ylim = c(0, 6)) 


x_norm <- seq(min(residuals(model_step)), max(residuals(model_step)), length = 100)
y_norm <- dnorm(x_norm, mean = mean(residuals(model_step)), sd = sd(residuals(model_step)))
lines(x_norm, y_norm, col = "red", lwd = 2)

qqnorm(residuals(model_step))
qqline(residuals(model_step), col = "red")


dwtest(model_step)                             
bgtest(model_step, order = 4)                  

                          
ncvTest(model_step)                            

par(mfrow = c(1,1))
plot(model_step$fitted.values,
     residuals(model_step),
     main = "Reszty vs wartości dopasowane",
     xlab = "Wartości dopasowane",
     ylab = "Reszty")
abline(h = 0, col = "red")

vif(model_step)

acf(residuals(model_step), main = "Autokorelacja reszt (ACF)")
pacf(residuals(model_step), main = "Częściowa autokorelacja reszt (PACF)")

bptest(model_step)

gqtest(model_step,
       order.by = fitted.values(model_step),
       fraction = 0.2)

bptest(model_step,
       ~ fitted.values(model_step) + I(fitted.values(model_step)^2))

ncvTest(model_step)

