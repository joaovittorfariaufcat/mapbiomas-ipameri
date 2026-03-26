#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Projeto de Pesquisa: PI05919-2025 - DIAGNÓSTICO DO USO E OCUPAÇÃO DO SOLO NA ÁREA DO ALTO CURSO DA MICROBACIA HIDROGRÁFICA DO RIBEIRÃO VAI-E-VEM NO MUNICÍPIO DE IPAMERI (GO)
# Orientador: RAFAEL DE AVILA RODRIGUES
# Co-Orientador: ANTOVER PANAZZOLO SARMENTO
# Centro: UNIVERSIDADE FEDERAL DE CATALÃO
# Discente: JOAO VITTOR DE FARIA PEREIRA
# Objetivo: Criar um fluxo automatizado para a extração, processamento e análise de dados de uso e cobertura do solo na microbacia do Ribeirão Vai e Vem (Ipameri-GO), utilizando apenas a linguagem R e ferramentas de código aberto.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# ETAPA 1: DELIMITAÇÃO DA ÁREA DE ESTUDO
# Dados ANA: Base Hidrográfica Ottocodificada da Bacia do Rio Paranaíba
# Camadas: Áreas de contribuição hidrográfica + trechos de drenagem

# 1.1 CRIAR PASTA DO PROJETO
############################

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


# 1.2 INSTALAR E CARREGAR PACOTES
#################################

pacotes <- c("sf", "terra", "mapview", "dplyr")

instalar <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]

if (length(instalar) > 0) {
  install.packages(instalar, dependencies = TRUE)
}

invisible(lapply(pacotes, library, character.only = TRUE))

library(sf)
library(dplyr)
library(mapview)

sf_use_s2(FALSE)

cat("Pacotes carregados com sucesso.\n")


# 1.3 ESTRUTURA DE PASTAS
#########################

pastas <- c(
  "dados",
  "dados/bacias",
  "dados/drenagem",
  "resultados",
  "resultados/shapefiles",
  "resultados/mapas",
  "resultados/tabelas"
)

for (p in pastas) {
  if (!dir.exists(p)) {
    dir.create(p, recursive = TRUE)
  }
}

cat("Estrutura de pastas criada.\n")


# 1.4 DOWNLOAD DOS DADOS DA ANA
###############################

url_bacias <- "https://metadados.snirh.gov.br/files/09436656-f4cf-4793-b169-33700b2d40ee/GEOFT_BHO_AREACONTRIBUICAO.zip"
url_drenagem <- "https://metadados.snirh.gov.br/geonetwork/srv/api/records/09436656-f4cf-4793-b169-33700b2d40ee/attachments/geoft_bho_trecho_drenagem.zip"

dest_bacias <- "dados/bacias/bacias.zip"
dest_drenagem <- "dados/drenagem/drenagem.zip"

if (!file.exists(dest_bacias)) {
  download.file(url_bacias, dest_bacias, mode = "wb")
  cat("Download das bacias concluído.\n")
} else {
  cat("Arquivo de bacias já existe.\n")
}

if (!file.exists(dest_drenagem)) {
  download.file(url_drenagem, dest_drenagem, mode = "wb")
  cat("Download da drenagem concluído.\n")
} else {
  cat("Arquivo de drenagem já existe.\n")
}


# 1.5 DESCOMPACTAR
##################

unzip(dest_bacias, exdir = "dados/bacias")
unzip(dest_drenagem, exdir = "dados/drenagem")

cat("Arquivos descompactados.\n")


# 1.6 CARREGAR OS SHAPEFILES CERTOS
###################################

arquivo_bacias <- list.files(
  path = "dados/bacias",
  pattern = "GEOFT_BHO_AREACONTRIBUICAO.*\\.shp$",
  full.names = TRUE,
  recursive = TRUE
)

arquivo_drenagem <- list.files(
  path = "dados/drenagem",
  pattern = "geoft_bho_trecho_drenagem.*\\.shp$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(arquivo_bacias) == 0) {
  stop("Shapefile de bacias não encontrado em dados/bacias.")
}

if (length(arquivo_drenagem) == 0) {
  stop("Shapefile de drenagem não encontrado em dados/drenagem.")
}

cat("Shapefile de bacias encontrado em:\n", arquivo_bacias[1], "\n")
cat("Shapefile de drenagem encontrado em:\n", arquivo_drenagem[1], "\n")

bacias <- st_read(arquivo_bacias[1], quiet = TRUE)
drenagem <- st_read(arquivo_drenagem[1], quiet = TRUE)

cat("Camada de bacias carregada.\n")
cat("Camada de drenagem carregada.\n")

cat("Colunas de bacias:\n")
print(names(bacias))

cat("Colunas de drenagem:\n")
print(names(drenagem))


# 1.6.1 PADRONIZAR CRS E VALIDAR GEOMETRIAS
###########################################

if (is.na(st_crs(bacias))) {
  stop("A camada de bacias está sem CRS definido.")
}

if (is.na(st_crs(drenagem))) {
  stop("A camada de drenagem está sem CRS definido.")
}

drenagem <- st_transform(drenagem, st_crs(bacias))

bacias <- st_make_valid(bacias)
drenagem <- st_make_valid(drenagem)

cat("CRS padronizado e geometrias validadas.\n")


# 1.6.2 CONFERIR CAMPO COBACIA
##############################

if (!"COBACIA" %in% names(bacias)) {
  stop("A camada de bacias não possui a coluna COBACIA.")
}

if (!"COBACIA" %in% names(drenagem)) {
  stop("A camada de drenagem não possui a coluna COBACIA.")
}

bacias$COBACIA <- as.character(bacias$COBACIA)
drenagem$COBACIA <- as.character(drenagem$COBACIA)

cat("Campo COBACIA conferido nas duas camadas.\n")


# 1.7 PONTO DE EXUTÓRIO CORRETO
###############################

lon <- -48.17193060976696
lat <- -17.70561924093522

ponto_exutorio <- st_as_sf(
  data.frame(id = 1, lon = lon, lat = lat),
  coords = c("lon", "lat"),
  crs = 4674
)

ponto_exutorio <- st_transform(ponto_exutorio, st_crs(drenagem))

cat("Ponto do exutório criado com sucesso.\n")
print(st_coordinates(ponto_exutorio))


# 1.8 LOCALIZAR TRECHO DE DRENAGEM MAIS PRÓXIMO
###############################################

dist <- st_distance(ponto_exutorio, drenagem)
trecho_id <- which.min(dist)

if (length(trecho_id) == 0 || is.na(trecho_id)) {
  stop("Não foi possível identificar o trecho de drenagem mais próximo.")
}

trecho_principal <- drenagem[trecho_id, ]

cat("Trecho principal localizado.\n")
cat("COBACIA do trecho principal:", as.character(trecho_principal$COBACIA), "\n")


# 1.8.1 AJUSTAR O PONTO PARA CAIR EXATAMENTE NA DRENAGEM
########################################################

linha_snap <- st_nearest_points(ponto_exutorio, trecho_principal)
pontos_snap <- st_cast(linha_snap, "POINT")

ponto_exutorio_snap <- st_as_sf(
  data.frame(id = 1),
  geometry = st_sfc(pontos_snap[2], crs = st_crs(drenagem))
)

cat("Ponto ajustado para a drenagem.\n")


# 1.9 DELIMITAR TODA A ÁREA A MONTANTE DO EXUTÓRIO
##################################################

# garantir tipo compatível
drenagem$COTRECHO <- as.character(drenagem$COTRECHO)
drenagem$NUTRJUS  <- as.character(drenagem$NUTRJUS)
drenagem$COBACIA  <- as.character(drenagem$COBACIA)
bacias$COBACIA    <- as.character(bacias$COBACIA)

# trecho inicial onde está o exutório
trecho_inicial <- as.character(trecho_principal$COTRECHO)

cat("Trecho inicial do exutório:", trecho_inicial, "\n")

# função para buscar todos os trechos a montante
buscar_montante <- function(trecho_id, drenagem_sf) {
  
  visitados <- character(0)
  fila <- trecho_id
  
  while(length(fila) > 0) {
    atual <- fila[1]
    fila <- fila[-1]
    
    if(atual %in% visitados) next
    
    visitados <- c(visitados, atual)
    
    # trechos que deságuam no trecho atual
    upstream <- drenagem_sf$COTRECHO[drenagem_sf$NUTRJUS == atual]
    upstream <- upstream[!is.na(upstream)]
    upstream <- as.character(upstream)
    
    # adicionar à fila apenas os que ainda não foram visitados
    novos <- setdiff(upstream, visitados)
    fila <- unique(c(fila, novos))
  }
  
  return(unique(visitados))
}

# obter todos os trechos conectados a montante
trechos_montante <- buscar_montante(trecho_inicial, drenagem)

cat("Número de trechos a montante encontrados:", length(trechos_montante), "\n")

# filtrar drenagem da bacia inteira
drenagem_micro <- drenagem %>%
  dplyr::filter(COTRECHO %in% trechos_montante)

cat("Trechos filtrados na drenagem:", nrow(drenagem_micro), "\n")

# pegar todos os códigos de bacia associados a esses trechos
cobacias_montante <- unique(drenagem_micro$COBACIA)
cobacias_montante <- cobacias_montante[!is.na(cobacias_montante)]

cat("Número de COBACIA encontrados:", length(cobacias_montante), "\n")

# selecionar todas as áreas de contribuição correspondentes
bacia_micro <- bacias %>%
  dplyr::filter(COBACIA %in% cobacias_montante)

cat("Número de polígonos de área de contribuição encontrados:", nrow(bacia_micro), "\n")

if(nrow(bacia_micro) == 0){
  stop("Nenhuma área de contribuição foi encontrada para os trechos a montante.")
}

# validar e unir tudo
bacia_micro <- st_make_valid(bacia_micro)
bacia_micro <- st_union(bacia_micro)

bacia_micro <- st_as_sf(
  data.frame(id = 1),
  geometry = st_sfc(bacia_micro, crs = st_crs(bacias))
)

cat("Bacia a montante unida com sucesso.\n")

# opcional: recortar drenagem final pela bacia unida
drenagem_micro <- suppressWarnings(
  st_intersection(drenagem_micro, bacia_micro)
)

# área
bacia_utm <- st_transform(bacia_micro, 31983)
area_km2 <- as.numeric(st_area(bacia_utm)) / 1000000

cat("Área total da bacia delimitada (km²):", round(area_km2, 4), "\n")

# conferir se o exutório está sobre a bacia
cat("O ponto ajustado está dentro da bacia?\n")
print(st_within(ponto_exutorio_snap, bacia_micro, sparse = FALSE))


# 1.10 SALVAR
#############

st_write(
  bacia_micro,
  "resultados/shapefiles/microbacia_vai_e_vem_alto_curso.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

st_write(
  drenagem_micro,
  "resultados/shapefiles/drenagem_microbacia_vai_e_vem.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

st_write(
  ponto_exutorio_snap,
  "resultados/shapefiles/exutorio_vai_e_vem.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

cat("Arquivos salvos com sucesso.\n")

dir.create("resultados/shapefiles", recursive = TRUE, showWarnings = FALSE)

sf::st_write(
  bacia_micro,
  "resultados/shapefiles/microbacia_vai_e_vem_alto_curso.shp",
  delete_layer = TRUE
)

cat("Shapefile salvo com sucesso.\n")


# 1.11 VISUALIZAÇÃO
###################

mapview(bacia_micro,
        col.regions = "lightgreen",
        alpha.regions = 0.35,
        layer.name = "Bacia a montante") +
  mapview(drenagem_micro,
          color = "blue",
          lwd = 2,
          layer.name = "Drenagem") +
  mapview(ponto_exutorio_snap,
          col.regions = "red",
          cex = 6,
          layer.name = "Exutório")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Etapa 2: Extração de Dados de Uso do Solo
#
# Utilizar raster multibanda do MapBiomas (1985 a 2023), já recortado no Google Earth Engine
# e disponibilizado online para download automático no R.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2.1 CARREGAR PACOTE
#####################

if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")

library(terra)

# 2.2 CRIAR PASTAS NECESSÁRIAS
##############################

dir.create("dados/mapbiomas", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/rasters", recursive = TRUE, showWarnings = FALSE)

# 2.3 DOWNLOAD DO RASTER MULTIBANDA
###################################

url_mapbiomas <- "https://raw.githubusercontent.com/joaovittorfariaufcat/mapbiomas-ipameri/main/1985_2023_ipameri.tif"

destino <- "dados/mapbiomas/mapbiomas_microbacia_1985_2023.tif"

if (!file.exists(destino)) {
  
  download.file(
    url = url_mapbiomas,
    destfile = destino,
    mode = "wb"
  )
  
  cat("Raster MapBiomas baixado com sucesso.\n")
  
} else {
  
  cat("Raster já existe na pasta dados/mapbiomas.\n")
}

# 2.4 CARREGAR RASTER
#####################

mapbiomas <- rast(destino)

cat("Raster MapBiomas carregado com sucesso.\n")
cat("Número de bandas encontradas:", nlyr(mapbiomas), "\n")

# 2.5 DEFINIR ANOS E NOMEAR BANDAS
##################################

anos <- 1985:2023

if (nlyr(mapbiomas) != length(anos)) {
  stop(paste0(
    "O raster possui ", nlyr(mapbiomas),
    " bandas, mas o esperado para 1985-2023 é ", length(anos), " bandas."
  ))
}

names(mapbiomas) <- paste0("classification_", anos)

cat("Bandas nomeadas de 1985 a 2023 com sucesso.\n")

# 2.6 RECORTAR O RASTER PARA A MICROBACIA
#########################################

if (!exists("bacia_micro")) {
  stop("O objeto 'bacia_micro' não foi encontrado. Rode a Etapa 1 antes da Etapa 2.")
}

bacia_vect <- vect(bacia_micro)

mapbiomas_crop <- crop(mapbiomas, bacia_vect)
mapbiomas_mask <- mask(mapbiomas_crop, bacia_vect)

cat("Raster recortado para a área da microbacia.\n")

# 2.7 SALVAR RASTER MULTIBANDA RECORTADO
########################################

writeRaster(
  mapbiomas_mask,
  "resultados/rasters/mapbiomas_microbacia_1985_2023_recortado.tif",
  overwrite = TRUE
)

cat("Raster multibanda recortado salvo com sucesso.\n")

# 2.8 EXPORTAR RASTERS ANUAIS
#############################

for (i in 1:nlyr(mapbiomas_mask)) {
  
  ano <- anos[i]
  
  writeRaster(
    mapbiomas_mask[[i]],
    paste0("resultados/rasters/mapbiomas_", ano, ".tif"),
    overwrite = TRUE
  )
}

cat("Rasters anuais exportados com sucesso.\n")

# 2.9 CONFERÊNCIA FINAL
#######################

cat("Resumo final da Etapa 2:\n")
print(mapbiomas_mask)
print(names(mapbiomas_mask))
nlyr(mapbiomas_mask)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Etapa 3: Processamento e Análise Espacial
#
# - Utilizar o raster multibanda do MapBiomas já recortado para a microbacia
# - Classificar os valores de pixel conforme legenda oficial do MapBiomas
# - Calcular área e proporção de cada classe de uso do solo por ano
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3.1 INSTALAR E CARREGAR PACOTES
#################################

pacotes <- c("terra", "sf", "dplyr", "tidyr", "purrr", "readr")

instalar <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]

if(length(instalar) > 0){
  install.packages(instalar, dependencies = TRUE)
}

invisible(lapply(pacotes, library, character.only = TRUE))

# 3.2 CRIAR PASTAS NECESSÁRIAS
##############################

dir.create("resultados", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/tabelas", recursive = TRUE, showWarnings = FALSE)

# 3.3 CARREGAR O RASTER DA ETAPA 2
##################################

caminho_raster <- "resultados/rasters/mapbiomas_microbacia_1985_2023_recortado.tif"

if(!file.exists(caminho_raster)){
  stop("O raster recortado da Etapa 2 não foi encontrado em 'resultados/rasters/mapbiomas_microbacia_1985_2023_recortado.tif'. Rode a Etapa 2 antes da Etapa 3.")
}

mapbiomas <- rast(caminho_raster)

anos <- 1985:2023

if(nlyr(mapbiomas) != length(anos)){
  stop(paste0(
    "O raster possui ", nlyr(mapbiomas),
    " bandas, mas o esperado para 1985-2023 é ", length(anos), " bandas."
  ))
}

names(mapbiomas) <- paste0("classification_", anos)

cat("Raster da Etapa 2 carregado com sucesso.\n")
cat("Número de bandas:", nlyr(mapbiomas), "\n")

# 3.4 CONFERIR A MICROBACIA
###########################

if(!exists("bacia_micro")){
  stop("O objeto 'bacia_micro' não foi encontrado. Rode a Etapa 1 antes da Etapa 3.")
}

# garantir mesmo CRS do raster
bacia_micro <- st_transform(bacia_micro, crs(mapbiomas))
bacia_vect <- vect(bacia_micro)

# recorte extra por segurança
mapbiomas <- crop(mapbiomas, bacia_vect)
mapbiomas <- mask(mapbiomas, bacia_vect)

cat("Raster conferido e mascarado novamente pela microbacia.\n")

# 3.5 LEGENDA MAPBIOMAS
#######################

# Ajuste conforme as classes realmente presentes no seu recorte.
# Mantive uma legenda ampla para o período 1985-2023.
legenda_mapbiomas <- data.frame(
  classe = c(
    3, 4, 5, 6, 9, 11, 12, 13, 15,
    18, 19, 20, 21, 23, 24, 25, 29,
    30, 31, 32, 33, 36, 39, 40, 41, 46, 47, 48, 49, 50, 62
  ),
  nome = c(
    "Formação Florestal",
    "Formação Savânica",
    "Mangue",
    "Floresta Alagável",
    "Floresta Plantada",
    "Área Úmida Natural",
    "Formação Natural não Florestal",
    "Campo Alagado",
    "Pastagem",
    "Agricultura",
    "Lavoura Temporária",
    "Cana",
    "Mosaico de Usos",
    "Praia e Duna",
    "Área Urbana",
    "Outras Áreas Não Vegetadas",
    "Afloramento Rochoso",
    "Mineração",
    "Aquicultura",
    "Apicum",
    "Corpos d'Água",
    "Área Urbana",
    "Soja",
    "Arroz",
    "Outras Lavouras Temporárias",
    "Café",
    "Citrus",
    "Outras Perenes",
    "Silvicultura",
    "Restinga Herbácea",
    "Não Observado"
  ),
  stringsAsFactors = FALSE
)

# 3.6 FUNÇÃO PARA CALCULAR ÁREA POR CLASSE
##########################################

calc_area_ano <- function(raster_ano, ano){
  
  raster_ano[raster_ano == 0] <- NA
  
  freq_tab <- terra::freq(raster_ano)
  
  if(is.null(freq_tab) || nrow(freq_tab) == 0){
    return(NULL)
  }
  
  df <- as.data.frame(freq_tab)
  
  df <- df[, c("value", "count")]
  names(df) <- c("classe", "pixels")
  
  # pixel de 30m = 900 m²
  df$area_m2 <- df$pixels * 900
  df$area_ha <- df$area_m2 / 10000
  df$area_km2 <- df$area_m2 / 1000000
  df$ano <- ano
  
  return(df)
}

# 3.7 CALCULAR ÁREA PARA TODOS OS ANOS
######################################

resultado_bruto <- purrr::map_dfr(
  seq_along(anos),
  ~ calc_area_ano(mapbiomas[[.x]], anos[.x])
)

if(nrow(resultado_bruto) == 0){
  stop("Nenhum resultado foi calculado. Verifique o raster e a microbacia.")
}

cat("Tabela bruta de áreas calculada com sucesso.\n")

# 3.8 JUNTAR COM A LEGENDA
##########################

resultado_uso_solo <- resultado_bruto %>%
  left_join(legenda_mapbiomas, by = "classe") %>%
  mutate(
    nome = ifelse(is.na(nome), paste("Classe", classe), nome)
  )

cat("Legenda associada às classes com sucesso.\n")

# 3.9 CALCULAR PROPORÇÃO POR ANO
################################

resultado_uso_solo <- resultado_uso_solo %>%
  group_by(ano) %>%
  mutate(
    area_total_ha = sum(area_ha, na.rm = TRUE),
    area_total_km2 = sum(area_km2, na.rm = TRUE),
    proporcao_percent = (area_ha / area_total_ha) * 100
  ) %>%
  ungroup()

cat("Proporções calculadas com sucesso.\n")

# 3.10 ORGANIZAR TABELA FINAL
#############################

resultado_uso_solo <- resultado_uso_solo %>%
  select(
    ano,
    classe,
    nome,
    pixels,
    area_ha,
    area_km2,
    proporcao_percent
  ) %>%
  arrange(ano, classe)

# 3.11 GERAR TABELA RESUMIDA (ANO x CLASSE)
###########################################

tabela_resumida <- resultado_uso_solo %>%
  select(ano, nome, proporcao_percent) %>%
  tidyr::pivot_wider(
    names_from = nome,
    values_from = proporcao_percent,
    values_fill = 0
  ) %>%
  arrange(ano)

# 3.12 EXPORTAR CSVs
####################

readr::write_csv(
  resultado_uso_solo,
  "resultados/tabelas/uso_solo_area_proporcao_por_ano.csv"
)

readr::write_csv(
  tabela_resumida,
  "resultados/tabelas/uso_solo_proporcao_resumida_por_ano.csv"
)

cat("Arquivos CSV exportados com sucesso.\n")

# 3.13 CONFERÊNCIA FINAL
########################

cat("\nResumo da Etapa 3:\n")
cat("Número total de registros:", nrow(resultado_uso_solo), "\n")
cat("Anos processados:", min(resultado_uso_solo$ano), "a", max(resultado_uso_solo$ano), "\n")
cat("Classes encontradas:\n")
print(sort(unique(resultado_uso_solo$classe)))

cat("\nPrimeiras linhas da tabela final:\n")
print(head(resultado_uso_solo, 15))

cat("\nSoma das proporções por ano (deve ficar próxima de 100):\n")
print(
  resultado_uso_solo %>%
    group_by(ano) %>%
    summarise(soma_percent = sum(proporcao_percent, na.rm = TRUE))
)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Etapa 4: Visualização e Exportação
#
# - Gerar mapas temáticos com ggplot2
# - Criar gráficos de mudança de uso do solo com ggplot2 e dygraphs
# - Exportar dados tabulares em CSV
# - Gerar mapa de hotspots de mudança
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 4.1 INSTALAR E CARREGAR PACOTES
#################################

pacotes <- c("ggplot2", "dplyr", "tidyr", "sf", "terra", "dygraphs", "xts", "readr", "viridis")

instalar <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]

if(length(instalar) > 0){
  install.packages(instalar, dependencies = TRUE)
}

invisible(lapply(pacotes, library, character.only = TRUE))

# 4.2 CRIAR PASTAS NECESSÁRIAS
##############################

dir.create("resultados", showWarnings = FALSE)
dir.create("resultados/mapas", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/tabelas", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/rasters", recursive = TRUE, showWarnings = FALSE)

# 4.3 CARREGAR OBJETOS NECESSÁRIOS
##################################

# tabela gerada na Etapa 3
if(!exists("resultado_uso_solo")){
  caminho_csv <- "resultados/tabelas/uso_solo_area_proporcao_por_ano.csv"
  
  if(!file.exists(caminho_csv)){
    stop("O objeto 'resultado_uso_solo' não está em memória e o CSV da Etapa 3 não foi encontrado.")
  }
  
  resultado_uso_solo <- readr::read_csv(caminho_csv, show_col_types = FALSE)
}

# raster gerado na Etapa 2
caminho_raster <- "resultados/rasters/mapbiomas_microbacia_1985_2023_recortado.tif"

if(!file.exists(caminho_raster)){
  stop("O raster recortado da Etapa 2 não foi encontrado em 'resultados/rasters/mapbiomas_microbacia_1985_2023_recortado.tif'.")
}

mapbiomas <- rast(caminho_raster)

anos <- 1985:2023
names(mapbiomas) <- paste0("classification_", anos)

# bacia da Etapa 1
if(!exists("bacia_micro")){
  stop("O objeto 'bacia_micro' não foi encontrado. Rode a Etapa 1 antes da Etapa 4.")
}

# 4.4 LEGENDA E CORES MAPBIOMAS
###############################

legenda_mapbiomas <- data.frame(
  classe = c(
    3, 4, 5, 6, 9, 11, 12, 13, 15,
    18, 19, 20, 21, 23, 24, 25, 29,
    30, 31, 32, 33, 36, 39, 40, 41, 46, 47, 48, 49, 50, 62
  ),
  nome = c(
    "Formação Florestal",
    "Formação Savânica",
    "Mangue",
    "Floresta Alagável",
    "Floresta Plantada",
    "Área Úmida Natural",
    "Formação Natural não Florestal",
    "Campo Alagado",
    "Pastagem",
    "Agricultura",
    "Lavoura Temporária",
    "Cana",
    "Mosaico de Usos",
    "Praia e Duna",
    "Área Urbana",
    "Outras Áreas Não Vegetadas",
    "Afloramento Rochoso",
    "Mineração",
    "Aquicultura",
    "Apicum",
    "Corpos d'Água",
    "Área Urbana",
    "Soja",
    "Arroz",
    "Outras Lavouras Temporárias",
    "Café",
    "Citrus",
    "Outras Perenes",
    "Silvicultura",
    "Restinga Herbácea",
    "Não Observado"
  ),
  stringsAsFactors = FALSE
) %>%
  distinct(classe, .keep_all = TRUE)

cores_mapbiomas <- c(
  "Formação Florestal" = "#1f8d49",
  "Formação Savânica" = "#7dc975",
  "Mangue" = "#04381d",
  "Floresta Alagável" = "#026975",
  "Floresta Plantada" = "#7a5900",
  "Área Úmida Natural" = "#519799",
  "Formação Natural não Florestal" = "#d6bc74",
  "Campo Alagado" = "#d89f5c",
  "Pastagem" = "#edde8e",
  "Agricultura" = "#f5b3c8",
  "Lavoura Temporária" = "#ffefc3",
  "Cana" = "#db7093",
  "Mosaico de Usos" = "#ffb3ff",
  "Praia e Duna" = "#ffd966",
  "Área Urbana" = "#d4271e",
  "Outras Áreas Não Vegetadas" = "#db4d4f",
  "Afloramento Rochoso" = "#b39ddb",
  "Mineração" = "#9c0027",
  "Aquicultura" = "#091077",
  "Apicum" = "#fc8114",
  "Corpos d'Água" = "#2532e4",
  "Soja" = "#c71585",
  "Arroz" = "#f54ca9",
  "Outras Lavouras Temporárias" = "#ff69b4",
  "Café" = "#8b4513",
  "Citrus" = "#ff8c00",
  "Outras Perenes" = "#daa520",
  "Silvicultura" = "#4caf50",
  "Restinga Herbácea" = "#c2b280",
  "Não Observado" = "#ffffff"
)

# manter apenas classes nomeadas
resultado_uso_solo <- resultado_uso_solo %>%
  mutate(
    nome = as.character(nome)
  ) %>%
  filter(!is.na(nome))

# 4.5 MAPAS TEMÁTICOS ANO A ANO
###############################

for(a in anos){
  
  r <- mapbiomas[[paste0("classification_", a)]]
  r[r == 0] <- NA
  
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(df) <- c("x", "y", "classe")
  
  df <- df %>%
    left_join(legenda_mapbiomas, by = "classe") %>%
    mutate(
      nome = ifelse(is.na(nome), paste("Classe", classe), nome)
    )
  
  prop_classes <- df %>%
    count(nome, name = "n") %>%
    mutate(
      prop = (n / sum(n)) * 100,
      legenda = paste0(nome, " (", round(prop, 1), "%)")
    )
  
  labels_legenda <- prop_classes$legenda
  names(labels_legenda) <- prop_classes$nome
  
  classes_presentes <- unique(df$nome)
  cores_presentes <- cores_mapbiomas[names(cores_mapbiomas) %in% classes_presentes]
  
  mapa <- ggplot() +
    geom_raster(
      data = df,
      aes(x = x, y = y, fill = nome)
    ) +
    geom_sf(
      data = st_transform(bacia_micro, crs(r)),
      fill = NA,
      color = "black",
      linewidth = 0.8
    ) +
    scale_fill_manual(
      values = cores_presentes,
      breaks = names(labels_legenda),
      labels = labels_legenda,
      name = "Uso do Solo",
      na.translate = FALSE
    ) +
    coord_sf(expand = FALSE) +
    labs(
      title = paste("Uso e Cobertura do Solo -", a),
      subtitle = "Microbacia Ribeirão Vai-e-Vem",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 9),
      legend.position = "right"
    )
  
  ggsave(
    filename = paste0("resultados/mapas/mapa_uso_solo_", a, ".png"),
    plot = mapa,
    width = 9,
    height = 6,
    dpi = 300
  )
}

cat("Mapas anuais exportados com sucesso.\n")

# 4.6 GRÁFICO TEMPORAL DE PROPORÇÃO
###################################

grafico_temporal <- ggplot(
  resultado_uso_solo,
  aes(x = ano, y = proporcao_percent, color = nome)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Mudança do Uso e Cobertura do Solo",
    subtitle = "Microbacia Ribeirão Vai-e-Vem (1985–2023)",
    x = "Ano",
    y = "Proporção (%)",
    color = "Classe"
  ) +
  theme_minimal()

ggsave(
  "resultados/graficos/mudanca_uso_solo.png",
  grafico_temporal,
  width = 10,
  height = 6,
  dpi = 300
)

# 4.7 GRÁFICO INTERATIVO
########################

dados_dy <- resultado_uso_solo %>%
  group_by(ano, nome) %>%
  summarise(
    proporcao_percent = sum(proporcao_percent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = nome,
    values_from = proporcao_percent,
    values_fill = 0
  ) %>%
  arrange(ano)

dados_dy[,-1] <- lapply(dados_dy[,-1], as.numeric)

dados_xts <- xts::xts(
  dados_dy[,-1],
  order.by = as.Date(paste0(dados_dy$ano, "-01-01"))
)

grafico_interativo <- dygraph(dados_xts) %>%
  dyRangeSelector() %>%
  dyOptions(stackedGraph = TRUE)

htmlwidgets::saveWidget(
  widget = grafico_interativo,
  file = "resultados/graficos/mudanca_uso_solo_interativo.html",
  selfcontained = TRUE
)

cat("Gráfico interativo salvo com sucesso.\n")

# 4.8 EXPORTAR TABELA FINAL
###########################

write.csv2(
  resultado_uso_solo,
  "resultados/tabelas/uso_solo_proporcao_por_ano.csv",
  row.names = FALSE
)

# 4.9 COMPARAÇÃO 1985 vs 2023
#############################

comparacao <- resultado_uso_solo %>%
  filter(ano %in% c(1985, 2023))

grafico_barra <- ggplot(
  comparacao,
  aes(x = nome, y = area_ha, fill = factor(ano))
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Comparação do Uso do Solo",
    subtitle = "1985 vs 2023",
    x = "Classe",
    y = "Área (ha)",
    fill = "Ano"
  ) +
  theme_minimal()

ggsave(
  "resultados/graficos/comparacao_1985_2023.png",
  grafico_barra,
  width = 10,
  height = 7,
  dpi = 300
)

# 4.10 MUDANÇA ANUAL POR CLASSE
###############################

mudanca_anual <- resultado_uso_solo %>%
  arrange(nome, ano) %>%
  group_by(nome) %>%
  mutate(
    area_ano_anterior = lag(area_ha),
    mudanca_ha = area_ha - area_ano_anterior,
    mudanca_percent = ifelse(
      !is.na(area_ano_anterior) & area_ano_anterior > 0,
      (mudanca_ha / area_ano_anterior) * 100,
      NA
    )
  ) %>%
  ungroup()

write.csv2(
  mudanca_anual,
  "resultados/tabelas/mudanca_anual_uso_solo.csv",
  row.names = FALSE
)

# 4.11 GRÁFICO DE ÁREA EMPILHADA
###############################

grafico_area_empilhada <- ggplot(
  resultado_uso_solo,
  aes(x = ano, y = proporcao_percent, fill = nome)
) +
  geom_area() +
  labs(
    title = "Evolução do Uso do Solo",
    subtitle = "Microbacia Ribeirão Vai-e-Vem",
    x = "Ano",
    y = "Proporção (%)",
    fill = "Classe"
  ) +
  theme_minimal()

ggsave(
  "resultados/graficos/area_empilhada.png",
  grafico_area_empilhada,
  width = 10,
  height = 6,
  dpi = 300
)

# 4.12 RESUMO ESTATÍSTICO DAS CLASSES
#####################################

resumo_classes <- resultado_uso_solo %>%
  group_by(nome) %>%
  summarise(
    area_media_ha = mean(area_ha, na.rm = TRUE),
    area_maxima_ha = max(area_ha, na.rm = TRUE),
    area_minima_ha = min(area_ha, na.rm = TRUE),
    proporcao_media = mean(proporcao_percent, na.rm = TRUE),
    .groups = "drop"
  )

write.csv2(
  resumo_classes,
  "resultados/tabelas/resumo_estatistico_classes.csv",
  row.names = FALSE
)

# 4.13 MAPA DE HOTSPOTS DE MUDANÇA
##################################

mudancas <- mapbiomas[[1]]
mudancas[] <- 0

for(i in 2:nlyr(mapbiomas)){
  mudou <- mapbiomas[[i]] != mapbiomas[[i - 1]]
  mudou[is.na(mudou)] <- 0
  mudancas <- mudancas + mudou
}

writeRaster(
  mudancas,
  "resultados/rasters/hotspots_mudanca.tif",
  overwrite = TRUE
)

df_hotspot <- as.data.frame(mudancas, xy = TRUE, na.rm = TRUE)
colnames(df_hotspot) <- c("x", "y", "mudancas")

mapa_hotspot <- ggplot() +
  geom_raster(
    data = df_hotspot,
    aes(x = x, y = y, fill = mudancas)
  ) +
  geom_sf(
    data = st_transform(bacia_micro, crs(mudancas)),
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) +
  scale_fill_viridis_c(name = "Nº de mudanças") +
  labs(
    title = "Hotspots de Mudança do Uso do Solo",
    subtitle = "1985–2023",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

ggsave(
  "resultados/mapas/hotspots_mudanca.png",
  mapa_hotspot,
  width = 9,
  height = 6,
  dpi = 300
)

# 4.14 TAXA ANUAL DE MUDANÇA
############################

taxa_mudanca <- resultado_uso_solo %>%
  group_by(nome) %>%
  arrange(ano) %>%
  mutate(
    taxa_anual = proporcao_percent - lag(proporcao_percent)
  ) %>%
  ungroup()

write.csv2(
  taxa_mudanca,
  "resultados/tabelas/taxa_anual_mudanca.csv",
  row.names = FALSE
)

# 4.15 INDICADORES DE MUDANÇA 2000 vs 2023
##########################################

indicadores_2000_2023 <- resultado_uso_solo %>%
  mutate(
    grupo = case_when(
      nome %in% c(
        "Formação Florestal",
        "Formação Savânica",
        "Mangue",
        "Floresta Alagável",
        "Área Úmida Natural",
        "Formação Natural não Florestal",
        "Campo Alagado"
      ) ~ "Coberturas Vegetais",
      nome %in% c(
        "Pastagem",
        "Agricultura",
        "Lavoura Temporária",
        "Mosaico de Usos",
        "Soja",
        "Outras Lavouras Temporárias",
        "Cana",
        "Café",
        "Citrus",
        "Outras Perenes"
      ) ~ "Agricultura e Agropecuária",
      nome %in% c("Área Urbana") ~ "Áreas Urbanas",
      nome %in% c("Corpos d'Água") ~ "Corpos d'Água",
      TRUE ~ "Outros"
    )
  ) %>%
  group_by(ano, grupo) %>%
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
  filter(ano %in% c(2000, 2023)) %>%
  pivot_wider(names_from = ano, values_from = area_ha, values_fill = 0) %>%
  mutate(
    mudanca_ha = `2023` - `2000`,
    mudanca_percentual = ifelse(
      `2000` > 0,
      ((`2023` - `2000`) / `2000`) * 100,
      NA
    )
  )

write.csv2(
  indicadores_2000_2023,
  "resultados/tabelas/indicadores_mudanca_2000_2023.csv",
  row.names = FALSE
)

# 4.16 EXIBIÇÕES FINAIS
#######################

print(indicadores_2000_2023)
print(grafico_temporal)
print(grafico_barra)
print(grafico_area_empilhada)
print(mapa_hotspot)

cat("\nEtapa 4 finalizada com sucesso.\n")
cat("Arquivos gerados em:\n")
cat("- resultados/mapas/\n")
cat("- resultados/graficos/\n")
cat("- resultados/tabelas/\n")
cat("- resultados/rasters/\n")