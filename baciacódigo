#--------------------------------------------------------------------------------------------------------------------------------------------------------------
# Projeto de Pesquisa: PI05919-2025 - DIAGNÓSTICO DO USO E OCUPAÇÃO DO SOLO 
# NA ÁREA DO ALTO CURSO DA MICROBACIA HIDROGRÁFICA DO RIBEIRÃO VAI-E-VEM NO MUNICÍPIO DE IPAMERI (GO)
# Orientador: RAFAEL DE AVILA RODRIGUES
# Co-Orientador: ANTOVER PANAZZOLO SARMENTO
# Centro: UNIVERSIDADE FEDERAL DE CATALÃO
# Discente: JOAO VITTOR DE FARIA PEREIRA
# Objetivo: Criar um fluxo automatizado para a extração, processamento e análise de dados de uso e cobertura do solo na microbacia do Ribeirão Vai e Vem (Ipameri-GO), utilizando apenas a linguagem R e ferramentas de código aberto.
#--------------------------------------------------------------------------------------------------------------------------------------------------------------

# DELIMITAÇÃO DA ÁREA DE ESTUDO (ANA)

desktop <- file.path(Sys.getenv("USERPROFILE"), "Desktop")

if (!dir.exists(desktop)) {
  desktop <- file.path(Sys.getenv("HOME"), "Desktop")
}

pasta_projeto <- file.path(desktop, "projetomicrobacia")

if (!dir.exists(pasta_projeto)) {
  dir.create(pasta_projeto, recursive = TRUE)
}

setwd(pasta_projeto)

cat("Diretório do projeto:", pasta_projeto, "\n")


# Instalação de pacotes essenciais
pacotes <- c("sf", "terra", "mapview", "dplyr", "tidyr", "purrr", "readr", "ggplot2", "dygraphs", "xts", "viridis", "svDialogs")

instalar <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]

if (length(instalar) > 0) {
  install.packages(instalar, dependencies = TRUE)
}

invisible(lapply(pacotes, library, character.only = TRUE))

sf_use_s2(FALSE)


# Estrutura de pastas
pastas <- c(
  "dados",
  "dados/bacias",
  "dados/drenagem",
  "dados/mapbiomas",
  "resultados",
  "resultados/shapefiles",
  "resultados/mapas",
  "resultados/tabelas",
  "resultados/rasters",
  "resultados/graficos"
)

for (p in pastas) {
  if (!dir.exists(p)) {
    dir.create(p, recursive = TRUE)
  }
}


# Download dos dados da ANA
url_bacias <- "https://metadados.snirh.gov.br/files/09436656-f4cf-4793-b169-33700b2d40ee/GEOFT_BHO_AREACONTRIBUICAO.zip"
url_drenagem <- "https://metadados.snirh.gov.br/files/2091576b-6821-4320-ad87-2deb86753984/GEOFT_BHO_PAN_TDR.zip"

dest_bacias <- "dados/bacias/bacias.zip"
dest_drenagem <- "dados/drenagem/drenagem.zip"

if (!file.exists(dest_bacias)) {
  download.file(url_bacias, dest_bacias, mode = "wb")
}

if (!file.exists(dest_drenagem)) {
  download.file(url_drenagem, dest_drenagem, mode = "wb")
}

unzip(dest_bacias, exdir = "dados/bacias")
unzip(dest_drenagem, exdir = "dados/drenagem")


arquivo_bacias <- list.files(path = "dados/bacias", pattern = "GEOFT_BHO_AREACONTRIBUICAO.*\\.shp$", full.names = TRUE, recursive = TRUE)[1]
arquivo_drenagem <- list.files(path = "dados/drenagem", pattern = "GEOFT_BHO_PAN_TDR.*\\.shp$", full.names = TRUE, recursive = TRUE)[1]

if (is.na(arquivo_drenagem)) {
  arquivo_drenagem <- list.files(path = "dados/drenagem", pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
}

bacias <- st_read(arquivo_bacias, quiet = TRUE)
drenagem <- st_read(arquivo_drenagem, quiet = TRUE)


geom_bacias <- attr(bacias, "sf_column")
geom_drenagem <- attr(drenagem, "sf_column")

names(bacias)[names(bacias) != geom_bacias] <- toupper(names(bacias)[names(bacias) != geom_bacias])
names(drenagem)[names(drenagem) != geom_drenagem] <- toupper(names(drenagem)[names(drenagem) != geom_drenagem])

st_geometry(bacias) <- geom_bacias
st_geometry(drenagem) <- geom_drenagem

drenagem <- st_transform(drenagem, st_crs(bacias))
bacias <- st_make_valid(bacias)
drenagem <- st_make_valid(drenagem)

bacias$COBACIA <- as.character(bacias$COBACIA)
drenagem$COBACIA <- as.character(drenagem$COBACIA)


# Ponto de exutório (Selecione o Ponto de exútorio da bacia em estudo)
lon <- -48.17193060976696
lat <- -17.70561924093522

ponto_exutorio <- st_as_sf(data.frame(id = 1, lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4674)
ponto_exutorio <- st_transform(ponto_exutorio, st_crs(drenagem))


dist <- st_distance(ponto_exutorio, drenagem)
trecho_id <- which.min(dist)
trecho_principal <- drenagem[trecho_id, ]

linha_snap <- st_nearest_points(ponto_exutorio, trecho_principal)
pontos_snap <- st_cast(linha_snap, "POINT")

ponto_exutorio_snap <- st_as_sf(data.frame(id = 1), geometry = st_sfc(pontos_snap[2], crs = st_crs(drenagem)))


drenagem$COTRECHO <- as.character(drenagem$COTRECHO)
drenagem$NUTRJUS <- as.character(drenagem$NUTRJUS)
trecho_inicial <- as.character(trecho_principal$COTRECHO)


buscar_montante <- function(trecho_id, drenagem_sf) {
  visitados <- character(0)
  fila <- trecho_id
  
  while(length(fila) > 0) {
    atual <- fila[1]
    fila <- fila[-1]
    
    if(atual %in% visitados) next
    visitados <- c(visitados, atual)
    
    upstream <- drenagem_sf$COTRECHO[drenagem_sf$NUTRJUS == atual]
    upstream <- as.character(upstream[!is.na(upstream)])
    
    novos <- setdiff(upstream, visitados)
    fila <- unique(c(fila, novos))
  }
  
  return(unique(visitados))
}


trechos_montante <- buscar_montante(trecho_inicial, drenagem)

drenagem_micro <- drenagem %>% 
  dplyr::filter(COTRECHO %in% trechos_montante)


cobacias_montante <- unique(drenagem_micro$COBACIA)
cobacias_montante <- cobacias_montante[!is.na(cobacias_montante)]


bacia_micro <- bacias %>% 
  dplyr::filter(COBACIA %in% cobacias_montante)

bacia_micro <- st_make_valid(bacia_micro)
bacia_micro <- st_union(bacia_micro)

bacia_micro <- st_as_sf(data.frame(id = 1), geometry = st_sfc(bacia_micro, crs = st_crs(bacias)))

drenagem_micro <- suppressWarnings(st_intersection(drenagem_micro, bacia_micro))


bacia_utm <- st_transform(bacia_micro, 31983)
area_km2 <- as.numeric(st_area(bacia_utm)) / 1000000

cat("Área total da bacia delimitada (km²):", round(area_km2, 4), "\n")


st_write(bacia_micro, "resultados/shapefiles/microbacia_vai_e_vem_alto_curso.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(drenagem_micro, "resultados/shapefiles/drenagem_microbacia_vai_e_vem.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(ponto_exutorio_snap, "resultados/shapefiles/exutorio_vai_e_vem.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(bacia_micro, "resultados/shapefiles/microbacia_vai_e_vem_alto_curso.shp", delete_layer = TRUE, quiet = TRUE)


#Mapview
mapa_interativo <- mapview(
  bacia_micro,
  col.regions = "lightgreen",
  alpha.regions = 0.35,
  layer.name = "Microbacia"
) +
  mapview(
    drenagem_micro,
    color = "blue",
    lwd = 2,
    layer.name = "Drenagem"
  ) +
  mapview(
    ponto_exutorio_snap,
    col.regions = "red",
    cex = 6,
    layer.name = "Exutório"
  )

print(mapa_interativo)

escolha_colecao <- svDialogs::dlg_list(
  choices = c("MapBiomas Coleção 9 (1985-2023)", "MapBiomas Coleção 10 (1985-2024)"),
  title = "Escolha a Coleção do MapBiomas:"
)$res


if (length(escolha_colecao) == 0 || escolha_colecao == "") {
  stop("Nenhuma coleção foi selecionada. Execução cancelada.")
}


if (grepl("Coleção 9", escolha_colecao)) {
  anos <- 1985:2023
  caminho_nuvem <- "initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_"
} else {
  anos <- 1985:2024
  caminho_nuvem <- "initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_"
}

dir_mapbiomas_recortes <- "resultados/mapbiomas_recortes"
dir.create(dir_mapbiomas_recortes, recursive = TRUE, showWarnings = FALSE)

for (ano in anos) {
  
  cat("\nProcessando MapBiomas", ano, "...\n")
  
  url <- paste0("/vsicurl/https://storage.googleapis.com/mapbiomas-public/", caminho_nuvem, ano, ".tif")
  
  r <- rast(url)
  
  bacia_v <- vect(st_transform(bacia_micro, crs(r)))
  
  recorte <- crop(r, bacia_v)
  recorte <- mask(recorte, bacia_v)
  
  writeRaster(recorte, file.path(dir_mapbiomas_recortes, paste0("mapbiomas_bacia_", ano, ".tif")), overwrite = TRUE)
}

arquivos_mapbiomas <- file.path(dir_mapbiomas_recortes, paste0("mapbiomas_bacia_", anos, ".tif"))

mapbiomas <- rast(arquivos_mapbiomas)

names(mapbiomas) <- paste0("classification_", anos)

caminho_raster_final <- paste0("resultados/rasters/mapbiomas_microbacia_", min(anos), "_", max(anos), "_recortado.tif")

writeRaster(mapbiomas, caminho_raster_final, overwrite = TRUE)

mapbiomas_mask <- mapbiomas


legenda_mapbiomas <- data.frame(
  classe = c(
    3, 4, 5, 6, 9, 11, 12, 13, 15, 18, 19, 20, 21, 23, 
    24, 25, 29, 30, 31, 32, 33, 35, 36, 39, 40, 41, 46, 
    47, 48, 49, 50, 62, 75, 27
  ),
  nome = c(
    "Formação Florestal", "Formação Savânica", "Mangue", 
    "Floresta Alagável", "Silvicultura", "Campo Alagado e Área Pantanosa", 
    "Formação Campestre", "Outra Formação Natural Não Florestal", 
    "Pastagem", "Agricultura", "Lavoura Temporária", "Cana", 
    "Mosaico de Usos", "Praia, Duna e Areal", "Área Urbanizada", 
    "Outras Áreas Não Vegetadas", "Afloramento Rochoso", "Mineração", 
    "Aquicultura", "Apicum", "Rio, Lago e Oceano", "Dendê", 
    "Lavoura Perene", "Soja", "Arroz", "Soja",
    "Café", "Citrus", "Outras Lavouras Perenes", "Restinga Arbórea", 
    "Restinga Herbácea", "Algodão (beta)", "Usina Fotovoltaica (beta)", 
    "Não Observado"
  ),
  stringsAsFactors = FALSE
)


calc_area_ano <- function(raster_ano, ano){
  
  raster_ano[raster_ano == 0] <- NA
  
  freq_tab <- terra::freq(raster_ano)
  
  if(is.null(freq_tab) || nrow(freq_tab) == 0) return(NULL)
  
  df <- as.data.frame(freq_tab)
  
  df <- df[, c("value", "count")]
  
  names(df) <- c("classe", "pixels")
  
  df$area_m2 <- df$pixels * 900
  df$area_ha <- df$area_m2 / 10000
  df$area_km2 <- df$area_m2 / 1000000
  df$ano <- ano
  
  return(df)
}

resultado_bruto <- purrr::map_dfr(seq_along(anos), ~ calc_area_ano(mapbiomas_mask[[.x]], anos[.x]))

resultado_uso_solo <- resultado_bruto %>%
  left_join(legenda_mapbiomas, by = "classe") %>%
  mutate(nome = ifelse(is.na(nome), paste("Classe", classe), nome)) %>%
  # Agrupa por ano e nome para somar e fundir a "Soja" verdadeira e o erro do satélite
  group_by(ano, nome) %>%
  summarise(
    pixels = sum(pixels, na.rm = TRUE),
    area_ha = sum(area_ha, na.rm = TRUE),
    area_km2 = sum(area_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ano) %>%
  mutate(
    area_total_ha = sum(area_ha, na.rm = TRUE),
    area_total_km2 = sum(area_km2, na.rm = TRUE),
    proporcao_percent = (area_ha / area_total_ha) * 100
  ) %>%
  ungroup() %>%
  select(ano, nome, pixels, area_ha, area_km2, proporcao_percent) %>%
  arrange(ano, desc(area_ha))

tabela_resumida <- resultado_uso_solo %>%
  select(ano, nome, proporcao_percent) %>%
  tidyr::pivot_wider(names_from = nome, values_from = proporcao_percent, values_fill = 0) %>%
  arrange(ano)

write.csv2(resultado_uso_solo, "resultados/tabelas/uso_solo_area_proporcao_por_ano.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(tabela_resumida, "resultados/tabelas/uso_solo_proporcao_resumida_por_ano.csv", row.names = FALSE, fileEncoding = "UTF-8")

cores_mapbiomas <- c(
  "Formação Florestal" = "#1f8d49", "Formação Savânica" = "#7dc975", "Mangue" = "#04381d",
  "Floresta Alagável" = "#026975", "Silvicultura" = "#7a5900", "Campo Alagado e Área Pantanosa" = "#519799",
  "Formação Campestre" = "#d6bc74", "Pastagem" = "#edde8e", "Agricultura" = "#f5b3c8",
  "Lavoura Temporária" = "#ffefc3", "Cana" = "#db7093", "Mosaico de Usos" = "#ffb3ff",
  "Praia, Duna e Areal" = "#ffd966", "Área Urbanizada" = "#d4271e", "Outras Áreas Não Vegetadas" = "#db4d4f",
  "Afloramento Rochoso" = "#b39ddb", "Mineração" = "#9c0027", "Aquicultura" = "#091077",
  "Apicum" = "#fc8114", "Rio, Lago e Oceano" = "#2532e4", "Dendê" = "#7a0177",
  "Lavoura Perene" = "#ff8c00", "Soja" = "#c71585", "Arroz" = "#f54ca9",
  "Outras Lavouras Temporárias" = "#ff69b4", "Café" = "#8b4513", "Citrus" = "#ffb347",
  "Outras Lavouras Perenes" = "#daa520", "Restinga Arbórea" = "#4caf50", "Restinga Herbácea" = "#c2b280",
  "Algodão (beta)" = "#e6b800", "Usina Fotovoltaica (beta)" = "#ffe119", "Não Observado" = "#ffffff"
)


resultado_uso_solo <- resultado_uso_solo %>%
  mutate(nome = as.character(nome)) %>%
  filter(!is.na(nome))

for(a in anos){
  
  r <- mapbiomas_mask[[paste0("classification_", a)]]
  r[r == 0] <- NA
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(df) <- c("x", "y", "classe")
  
  df <- df %>%
    left_join(legenda_mapbiomas, by = "classe") %>%
    mutate(nome = ifelse(is.na(nome), paste("Classe", classe), nome))
  
  prop_classes <- df %>%
    count(nome, name = "n") %>%
    mutate(prop = (n / sum(n)) * 100, legenda = paste0(nome, " (", round(prop, 1), "%)"))
  
  labels_legenda <- prop_classes$legenda
  names(labels_legenda) <- prop_classes$nome
  
  cores_presentes <- cores_mapbiomas[names(cores_mapbiomas) %in% unique(df$nome)]
  
  mapa <- ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = nome)) +
    geom_sf(data = st_transform(bacia_micro, crs(r)), fill = NA, color = "black", linewidth = 0.8) +
    scale_fill_manual(values = cores_presentes, breaks = names(labels_legenda), labels = labels_legenda, name = "Uso do Solo", na.translate = FALSE) +
    coord_sf(expand = FALSE) +
    labs(title = paste("Uso e Cobertura do Solo -", a), subtitle = "Microbacia Ribeirão Vai-e-Vem", x = "Longitude", y = "Latitude") +
    theme_classic() +
    theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 11, hjust = 0.5), legend.position = "right")
  
  
  print(mapa)
  
  ggsave(filename = paste0("resultados/mapas/mapa_uso_solo_", a, ".png"), plot = mapa, width = 9, height = 6, dpi = 300)
}


grafico_temporal <- ggplot(resultado_uso_solo, aes(x = ano, y = proporcao_percent, color = nome)) +
  geom_line(linewidth = 1) +
  labs(title = "Mudança do Uso e Cobertura do Solo", subtitle = paste0("Microbacia Ribeirão Vai-e-Vem (", min(anos), "–", max(anos), ")"), x = "Ano", y = "Proporção (%)", color = "Classe") +
  theme_minimal() +
  scale_color_manual(values = cores_mapbiomas) # <- LINHA ADICIONADA AQUI

print(grafico_temporal)
ggsave("resultados/graficos/mudanca_uso_solo.png", grafico_temporal, width = 10, height = 6, dpi = 300)

dados_dy <- tabela_resumida
dados_dy[,-1] <- lapply(dados_dy[,-1], as.numeric)

dados_xts <- xts::xts(dados_dy[,-1], order.by = as.Date(paste0(dados_dy$ano, "-01-01")))

grafico_interativo <- dygraph(dados_xts) %>% 
  dyRangeSelector() %>% 
  dyOptions(stackedGraph = TRUE)

print(grafico_interativo)
htmlwidgets::saveWidget(widget = grafico_interativo, file = "resultados/graficos/mudanca_uso_solo_interativo.html", selfcontained = TRUE)

write.csv2(resultado_uso_solo, "resultados/tabelas/uso_solo_area_proporcao_por_ano.csv", row.names = FALSE, fileEncoding = "Latin1")
write.csv2(tabela_resumida, "resultados/tabelas/uso_solo_proporcao_resumida_por_ano.csv", row.names = FALSE, fileEncoding = "Latin1")

ano_inicial <- min(anos)
ano_final <- max(anos)

comparacao <- resultado_uso_solo %>% 
  filter(ano %in% c(ano_inicial, ano_final))

grafico_barra <- ggplot(comparacao, aes(x = nome, y = area_ha, fill = factor(ano))) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Comparação do Uso do Solo", subtitle = paste0(ano_inicial, " vs ", ano_final), x = "Classe", y = "Área (ha)", fill = "Ano") +
  theme_minimal()

print(grafico_barra)
ggsave(paste0("resultados/graficos/comparacao_", ano_inicial, "_", ano_final, ".png"), grafico_barra, width = 10, height = 7, dpi = 300)


mudanca_anual <- resultado_uso_solo %>%
  arrange(nome, ano) %>%
  group_by(nome) %>%
  mutate(
    area_ano_anterior = lag(area_ha),
    mudanca_ha = area_ha - area_ano_anterior,
    mudanca_percent = ifelse(!is.na(area_ano_anterior) & area_ano_anterior > 0, (mudanca_ha / area_ano_anterior) * 100, NA)
  ) %>%
  ungroup()

write.csv2(mudanca_anual, "resultados/tabelas/mudanca_anual_uso_solo.csv", row.names = FALSE, fileEncoding = "Latin1")

ordem_classes <- c(
  "Soja", "Pastagem", "Mosaico de Usos", 
  "Silvicultura", "Formação Florestal", "Formação Savânica", 
  "Formação Campestre", "Campo Alagado e Área Pantanosa", 
  "Outras Áreas Não Vegetadas", "Área Urbanizada", "Rio, Lago e Oceano", "Algodão (beta)"
)

resultado_uso_solo$nome <- factor(resultado_uso_solo$nome, levels = ordem_classes)

mudancas <- mapbiomas_mask[[1]]
mudancas[] <- 0

for(i in 2:nlyr(mapbiomas_mask)){
  mudou <- mapbiomas_mask[[i]] != mapbiomas_mask[[i - 1]]
  mudou[is.na(mudou)] <- 0
  mudancas <- mudancas + mudou
}

bacia_vect <- vect(bacia_micro)
mudancas_bacia <- mask(crop(mudancas, bacia_vect), bacia_vect)

writeRaster(
  mudancas_bacia,
  "resultados/rasters/hotspots_mudanca.tif",
  overwrite = TRUE
)

df_hotspot <- as.data.frame(mudancas_bacia, xy = TRUE, na.rm = TRUE)
colnames(df_hotspot) <- c("x", "y", "mudancas")

mapa_hotspot <- ggplot() +
  geom_raster(
    data = df_hotspot,
    aes(x = x, y = y, fill = mudancas)
  ) +
  geom_sf(
    data = st_transform(bacia_micro, crs(mudancas_bacia)),
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) +
  scale_fill_viridis_c(
    name = "Nº de mudanças",
    option = "plasma",
    na.value = "transparent"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Hotspots de Mudança do Uso do Solo",
    subtitle = paste0(ano_inicial, "–", ano_final),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(mapa_hotspot)

ggsave(
  "resultados/mapas/hotspots_mudanca.png",
  mapa_hotspot,
  width = 9,
  height = 6,
  dpi = 300
)

taxa_mudanca <- resultado_uso_solo %>%
  group_by(nome) %>%
  arrange(ano) %>%
  mutate(taxa_anual = proporcao_percent - lag(proporcao_percent)) %>%
  ungroup()

write.csv2(taxa_mudanca, "resultados/tabelas/taxa_anual_mudanca.csv", row.names = FALSE, fileEncoding = "Latin1")

cat("\nProcessamento completo finalizado com sucesso! Todos os gráficos, mapas e tabelas foram plotados no R e salvos nas pastas correspondentes.\n")

#Relatório

cat("\nGerando relatório técnico final personalizado em Word...\n")

pacotes_word <- c("officer", "flextable", "dplyr", "readr")
instalar_word <- pacotes_word[!(pacotes_word %in% installed.packages()[, "Package"])]
if (length(instalar_word) > 0) { install.packages(instalar_word) }
library(officer)
library(flextable)
library(dplyr)
library(readr)

tabela_dados <- read.csv2("resultados/tabelas/uso_solo_area_proporcao_por_ano.csv", fileEncoding = "Latin1")

ano_ini <- min(tabela_dados$ano)
ano_fim <- max(tabela_dados$ano)

area_bacia_km2 <- ifelse(exists("area_km2"), round(area_km2, 2), 0)
area_bacia_ha <- round(area_bacia_km2 * 100, 2)

dados_ultimo_ano <- tabela_dados %>%
  filter(ano == ano_fim) %>%
  arrange(desc(proporcao_percent))

doc <- read_docx()

doc <- body_add_par(doc, "RELATÓRIO TÉCNICO DE DIAGNÓSTICO AMBIENTAL", style = "heading 1")
doc <- body_add_par(doc, paste0("Dinâmica de Uso e Cobertura do Solo da Microbacia (", ano_ini, " – ", ano_fim, ")"), style = "heading 2")
doc <- body_add_par(doc, paste("Data de emissão:", format(Sys.Date(), "%d/%m/%Y")), style = "Normal")
doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_par(doc, "1. Caracterização da Área Recortada", style = "heading 2")
texto_area <- paste0(
  "A área de estudo delimitada apresenta uma extensão territorial total de aproximadamente ", 
  area_bacia_km2, " km² (equivalente a ", area_bacia_ha, " hectares). ",
  "O código automatizado gerou o recorte preciso da bacia sobre as bases matriciais ",
  "do MapBiomas, abrangendo toda a série temporal de ", ano_ini, " a ", ano_fim, "."
)
doc <- body_add_par(doc, texto_area, style = "Normal")
doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_par(doc, paste("2. Mapeamento e Vegetação no Ano de", ano_fim), style = "heading 2")
texto_veg <- paste0(
  "O mapa a seguir representa a espacialização das classes de uso e cobertura da terra para o ano mais recente (", ano_fim, "). ",
  "O estudo do uso de ocupação do solo é inprescindível para um futuro diagnóstico, ",
  "o mapa destaca as taxas de uso do solo."
)

doc <- body_add_par(doc, texto_veg, style = "Normal")

caminho_mapa_recente <- file.path("resultados/mapas", paste0("mapa_uso_solo_", ano_fim, ".png"))
if (file.exists(caminho_mapa_recente)) {
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc, paste("Figura 1: Mapeamento de Uso e Cobertura do Solo da microbacia no ano de", ano_fim, "."), style = "Normal")
  doc <- body_add_img(doc, src = caminho_mapa_recente, width = 6.2, height = 4.1)
  doc <- body_add_par(doc, "", style = "Normal")
}

doc <- body_add_par(doc, paste("3. Composição Percentual da Paisagem em", ano_fim), style = "heading 2")
doc <- body_add_par(doc, paste("A tabela abaixo detalha a área em hectares e a respectiva porcentagem de cada classe registrada exclusivamente no ano de", ano_fim, "."), style = "Normal")

tabela_recente_fmt <- dados_ultimo_ano %>%
  select(nome, area_ha, proporcao_percent) %>%
  mutate(
    area_ha = round(area_ha, 2),
    proporcao_percent = paste0(round(proporcao_percent, 2), "%")
  )
names(tabela_recente_fmt) <- c("Classe de Uso / Vegetação", "Área (ha)", "Proporção (%)")

ft <- flextable(tabela_recente_fmt)
ft <- autofit(ft)
doc <- body_add_flextable(doc, value = ft)
doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_par(doc, "4. Identificação de Hotspots (Áreas de Maior Transformação)", style = "heading 2")
texto_hotspot <- paste(
  "Para compreender as regiões com maiores taxas de mudanças o mapa de hotspots demonstra significativamente as regiões que sofreram",
  "mais alterações de uso e cobertura durante os anos estudados. As zonas com intensidade",
  "mais elevada indicam locais com maiores taxas de mudanças no solo da região",
  ", servindo como diagnóstico crítico para análise no estudo do local."
)
doc <- body_add_par(doc, texto_hotspot, style = "Normal")

caminho_hotspot <- file.path("resultados/mapas", "hotspots_mudanca.png")
if (file.exists(caminho_hotspot)) {
  doc <- body_add_par(doc, "Figura 2: Espacialização dos Hotspots de Mudança (áreas de maior instabilidade e transição de uso).", style = "Normal")
  doc <- body_add_img(doc, src = caminho_hotspot, width = 6.2, height = 4.1)
  doc <- body_add_par(doc, "", style = "Normal")
}

doc <- body_add_par(doc, "5. Considerações Finais", style = "heading 2")
doc <- body_add_par(doc, "Os resultados consolidados neste documento fornecem um retrato técnico robusto da microbacia hidrográfica, oferecendo subsídios quantitativos e espaciais essenciais para estudos de impacto ambiental e planejamento territorial.", style = "Normal")

caminho_docx <- file.path("resultados", "Relatorio_Tecnico_Final_Bacia.docx")
print(doc, target = caminho_docx)

cat("\n[SUCESSO] Relatório técnico personalizado gerado e salvo em:", caminho_docx, "\n")



