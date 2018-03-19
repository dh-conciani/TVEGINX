## Dhemerson Conciani - dh.conciani@gmail.com - www.conciani.com

## Pacotes
## São conjuntos de funções prontas para usar (plug and play) 

library (lubridate)  # Extrair datas e usar como variáveis
library (rgdal)      # Ferramentas SIG para vetores 
library (raster)     # Ferramentas SIG para imagens
library (stringr)    # Manipulação de textos (nomes de arquivos)
library (tools)      # Ferramentas para processamento em lotes 

## Abrir shapefile
## dsn= "Caminho da  até a pasta onde encontra-se seu dado vetorial"
## layer= "Nome do arquivo" (sem o .shp no final)
shp =  readOGR (dsn='C:/Users/BSD/Desktop/curso_gis/aulas/aula_5/arquivos/shp',
                layer= 'UCS_ITIRAPINA')

## Plotar seu shapefile
plot (shp)

## Colorir
# col= "Nome da cor"
plot (shp, col='green')

## Consultar o sistema de coordenadas
crs (shp)

## Adicionar grid de coordenadas ao mapa
plot (shp, col='green', axes=TRUE)

## Adicionar um título
plot (shp, col='green', axes=TRUE, main="Estação Ecológica de Itirapina")

## Listar um conjunto de imagens em uma pasta usando um padrão
## path= "Caminho até a pasta onde estão seus dados raster"
## pattern= "Padrão de busca usado para listar as imagens"
## full.names= "Ler o nome completo do arquivo, inclusive o formato"
ndvi = list.files(path = 'H:/curso_gis/aulas/aula_5/arquivos/1985_2016_index',
                   pattern = 'sr_ndvi.tif$', 
                   full.names = T)

ndvi # Exibir a lista 
## Ainda é apenas uma lista de caracteres, e não as imagens. 

## Usar o caminho listado para ler todas as imagens de uma vez
## lapply = abreviação para "list apply" = processamento em lista
## raster = função para ler cada caminho informado como um arquivo raster
ndvi_list = lapply (ndvi, raster)
ndvi_list

## Agora cada caminho em sua lista foi lido como uma imagem raster!

## Vamos extrair as variáveis temporais (meses e anos) elas serão importantes
## Para isso, vamos ler apenas o NOME dos arquivos, sem o caminho
filenames  = file_path_sans_ext(basename(ndvi))

## Checar lista de nomes // Conceito de STRING
filenames

## Vamos separar nossa STRING de data das demais strings
parse  = sapply(strsplit(filenames, split='_', fixed=TRUE), function(x) (x[4]))

## Checar nosso resultado
parse

## Agora precisamos informar o R que essa string é uma DATA no formato YYYY/MM/DD
date = ymd (parse)

## Checar dados
date

## Checar se as datas estão ordenadas no tempo 
plot (date)

## Agora que nossas o R já sabe que são datas, podemos transformar em STRINGS para trabalhar
date = as.character(date)

## Temos um traço "-" separando nossos dias, meses e anos. 
## Vamos usar este padrão para separar as variáveis
years  <- sapply(strsplit(date, split='-', fixed=TRUE), function(x) (x[1]))
months <- sapply(strsplit(date, split='-', fixed=TRUE), function(x) (x[2]))

## Agora que já separamos, vamos definir como variáveis numéricas
years  <- as.numeric (years)
months <- as.numeric (months)

## Checar a consistência
years
months

## Apagar as informações que não vamos mais usar
rm (parse, date)


## Plotar a primeira imagem para conferir
plot (ndvi_list[[1]])

## Checar a resolução de uma imagem para conferir (em metros)
res (ndvi_list[[1]])

## Checar o sistema de coordenadas
crs (ndvi_list[[1]])
## Comparar com o CRS do seu Shapefile
crs (shp)

## Reprojetar todos os rasters para o hemisfério Sul - APENAS SE DIVERGIR
## Vamos usar o sistema de coordenadas do seu shapefile como referência
ref_coord = crs (shp)

## Vamos criar uma FUNÇÃO que aplica seu sistema de coordenadas em todas as imagens
reproject_raster = function (x) projectRaster (x, crs = ref_coord)

## Vamos aplicar esta função em todas as nossas imagens (aproximadamente 1 hora e meia) 
# projected_list = lapply (ndvi_list, reproject_raster)

## Ao invés de aguardar o processo ser concluido, vamos importar as imagens já processadas
## ESTA ETAPA DO PROCESSO NÃO É NECESSÁRIA SE VOCÊ REPROJETAR OS DADOS NO R
ndvi = list.files(path = 'H:/curso_gis/aulas/aula_5/arquivos/reprojected', pattern = 'sr_ndvi.tif$', full.names = T)
projected_list = lapply (ndvi, raster)

## Checar se o sistema de coordenadas foi alterado com sucesso
crs (projected_list[[1]])

## Plotar uma imagem para conferir a distribuição espacial
plot (projected_list[[1]])

## Agora temos nosso vetor (shapefile) e nossas imagens projetados em um mesmo sistema 
## Podemos usar ferramentas de interação e prosseguir com a análise de dados 

## Processar imagens completas é pesado e demanda grande quantidade de tempo
## Para deixar nosso código mais leve, vamos fazer um recorte apenas para área de interesse

## Vamos criar uma FUNÇÃO que recorta as imagens para o tamamho do shapefile
crop_extent = function (x) crop (x, shp)

## Aplicar a FUNÇÃO e recortar todas imagens em sua lista
croped_ndvi = lapply (projected_list, crop_extent)

## Plotar para checar a consistência
plot (croped_ndvi[[1]])

## Nossos dados ainda apresentam "ruídos" de áreas que não são nosso interesse
## Vamos criar uma FUNÇÃO que transforma todos valores fora do shapefile em NA
extract_to = function (x) mask (x, shp) 

## Vamos aplicar esta função em nossos dados já recortados
ndvi_resu = lapply (croped_ndvi, extract_to)

## Plotar para conferir
plot (ndvi_resu[[1]])

## Para realizar cálculos mstemáticos entre imagens é necessário empilhar
## O cálculo é realizado comparando PIXEL A PIXEL sobreposto
ndvi_itirapina = stack (ndvi_resu)

## Plot para checar
## Rasters empilhados são plotados todos ao mesmo tempo
plot (ndvi_itirapina)

## Os valores de NDVI estão fora de escala, precisamos normalizar para todas imagens
ndvi_itirapina = ndvi_itirapina * 0.0001   # Fator de normalização para -1 à +1

## Plot para conferir
plot (ndvi_itirapina)

###################### I N T E R V A L O #########################

## Vamos explorar a variação da cobertura vegetal ao longo das últimas 3 décadas
## Para realizar cálculos precisamos ler o valor numérico de cada pixel por imagem, exceto NA's
values_ndvi = na.omit(values(ndvi_itirapina))
values_ndvi
dim(values_ndvi)

## Cada imagem é uma coluna, portanto, para saber a média da imagem basta calcular a média da coluna
ndvi_means = colMeans(values_ndvi)

## E transformar em uma tabela
ndvi_means <- as.data.frame (ndvi_means)

## Checar dados
ndvi_means ## Ainda está estranho... 

## Vamos adicionar uma coluna com a variável ano que separamos no começo da aula
df = cbind (years, ndvi_means)

## Checar nossos dados
df #Agora sim!!!!!!

## Vamos plotar os dados para examinar melhor
plot (df, main="Cobertura Vegetal", xlab="Tempo (anos)", ylab="NDVI")

## Vamos representar nossa variação em um modelo estatístico
## 1. Linear
mod_1 <- lm (ndvi_means ~ years, data = df)
summary (mod_1)

## Vamos adicionar a linha do modelo ao nosso gráfico
plot (df, main="Cobertura Vegetal", xlab="Tempo (anos)", ylab="NDVI")
abline(lm(ndvi_means ~ years, data=df), col='red')

## Vamos elaborar um MAPA de ganho ou perda de cobertura vegetal neste período
## O calculo é realizado por pixel 

# Cria uma stack com todas as imagens NDVI menos a primeira
ndvi_1 <- stack(ndvi_resu[-1])

# Cria outro stack com todas as imagens ndvi menos a ultima
ndvi_2 <- stack(ndvi_resu[-(length(ndvi_resu))])

# Subtrai os stacks e calcula a diferença entre cada par
diff <- stack (ndvi_1 - ndvi_2)

# Padroniza valores entre -1 e +1
diff <- diff * 0.0001

# Calcula a diferença total por pixel ao longo do tempo
diff_ndvi = calc (diff, fun=sum)

# Plotar resultado
plot (diff_ndvi)

## Descobrir o que não variou (usar desvio padrão)
sd(df$ndvi_means) ## Premissa de não variação

## Vamos classificar para melhor entender
plot (diff_ndvi, 
      breaks= c(-1, 0, 0.05, 0.2, 1),
      col= c("red", "white","darkolivegreen3", "green4"))

# Exportar seu produto
writeRaster (diff_ndvi, filename='diff_ndvi.tif',format='GTiff', overwrite=TRUE)
