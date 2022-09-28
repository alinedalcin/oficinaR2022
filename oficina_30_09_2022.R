#"File" -> "Reopen with encoding" -> "UTF-8", para a acentuação dos comentários aparecer corretamente

setwd("C:/Users/aline/Dropbox/Documentos/Artigos e etc/Oficinas R/Oficina R") # escolher seu diretório de trabalho 
# Cole o caminho da pasta onde está salvo os arquivos. 
# A barra \ deve ser substituída por / ou por \\

getwd() # verificar qual é o diretório de trabalho salvo



#### Calculadora

1 + 1 

18327 - 7981

24/7

5*78

14^2

soma <- 234 +458 # salvar objetos no environment

rm(soma) # excluir objetos do environment



#### Explorar base de dados

install.packages("data.table") # instalar pacotes
library(data.table) # carregar pacotes
# Vamos utilizar o pacote data.table para importar dados. 
# O data.table não é o único pacote para este fim; existem vários pacotes para importar dados.

banco_mundial_2019_2020 <- fread("banco_mundial_2019_2020.csv") # importar dados

print(banco_mundial_2019_2020) # apresenta toda a base de dados
View(banco_mundial_2019_2020) # abre o banco em outra janela

str(banco_mundial_2019_2020) # apresenta a estrutura da base de dados: dimensão, nomes, classe, dados iniciais...

dim(banco_mundial_2019_2020) # apresenta a dimensão da base de dados

names(banco_mundial_2019_2020) # apresenta os nomes das variáveis da base de dados

class(banco_mundial_2019_2020) # apresenta a classe da base de dados ou da variável
class(banco_mundial_2019_2020$continente)
class(banco_mundial_2019_2020$populacao)
class(banco_mundial_2019_2020$dias_necessarios_para_abrir_negocio)
# para transformar a classe de uma variável: as.character(), as.numeric(), as.logical(), as.factor(), as.Date(), etc...
banco_mundial_2019_2020$dias_necessarios_para_abrir_negocio_inteiro <- as.integer(banco_mundial_2019_2020$dias_necessarios_para_abrir_negocio)
class(banco_mundial_2019_2020$dias_necessarios_para_abrir_negocio_inteiro)

head(banco_mundial_2019_2020, 10) # apresenta primeiras observações
head(banco_mundial_2019_2020$pais, 10)
head(banco_mundial_2019_2020$taxa_de_fertilidade, 10)

tail(banco_mundial_2019_2020, 10) # apresenta últimas observações
tail(banco_mundial_2019_2020$pais, 10)
tail(banco_mundial_2019_2020$taxa_de_fertilidade)

table(banco_mundial_2019_2020$renda) # apresenta tabela dos resultados
table(banco_mundial_2019_2020$continente)

summary(banco_mundial_2019_2020) # resumo da base de dados por variavável
summary(banco_mundial_2019_2020$pais)
summary(banco_mundial_2019_2020$taxa_de_fertilidade)

mean(banco_mundial_2019_2020$taxa_de_fertilidade) # apresenta a média da variável
mean(banco_mundial_2019_2020$taxa_de_fertilidade, na.rm=TRUE) # apresenta a média da variável
# Para descartar os missings (NA), colocar na.rm = TRUE 
# Se tiver missing, o resultado será NA
# Outras funções: variância (var), mínimo (min), máximo (max), amplitude (range), mediana (median), quantis (quantile)

any(is.na(banco_mundial_2019_2020$taxa_de_fertilidade)) # saber se existe algum missing (NA)
sum(is.na(banco_mundial_2019_2020$taxa_de_fertilidade)) # saber quantos missings (NA) existem 
any(banco_mundial_2019_2020$taxa_de_fertilidade==3, na.rm=TRUE) 
sum(banco_mundial_2019_2020$taxa_de_fertilidade==3, na.rm=TRUE) 
#Utilizar outras funções is: por exemplo, is.numeric
#Utilizar igualdades ou desigualdades:  >, >=, <, <=, ==, !=



#### Pacote dplyr: 
#selecionar variáveis (select), criar variáveis a partir de outras (mutate), filtrar base de dados (filter), ordenar dados (arrange), agrupar variáveis por soma, média, contagem, etc (summarise)

install.packages("dplyr")
library(dplyr) #pacote para manusear dados

exemplo_filter1 <- filter(banco_mundial_2019_2020, !is.na(banco_mundial_2019_2020$taxa_de_fertilidade)) 
exemplo_filter2 <- filter(banco_mundial_2019_2020, taxa_de_fertilidade>=2) 
exemplo_filter3 <- filter(banco_mundial_2019_2020, taxa_de_fertilidade>=2&continente!="África")
# Mais de uma condição: & (para e), | (para ou)

exemplo_mutate <- mutate(banco_mundial_2019_2020, pib_per_capita=pib_dolares_precos_correntes/populacao) 

exemplo_select <- select(banco_mundial_2019_2020, c("pais", "continente", "pib_dolares_precos_correntes")) 

exemplo_arrange <- arrange(banco_mundial_2019_2020, continente) 
# Ordem decrescente: desc()

exemplo_summarise1 <- summarise(banco_mundial_2019_2020, media=mean(expectativa_de_vida_ao_nascer_mulheres, na.rm=TRUE), minimo=min(expectativa_de_vida_ao_nascer_mulheres, na.rm=TRUE), maximo=max(expectativa_de_vida_ao_nascer_mulheres, na.rm=TRUE))
exemplo_summarise2 <- banco_mundial_2019_2020 %>%
  group_by(continente) %>%
  summarise(media=mean(expectativa_de_vida_ao_nascer_mulheres, na.rm=TRUE), minimo=min(expectativa_de_vida_ao_nascer_mulheres, na.rm=TRUE), maximo=max(expectativa_de_vida_ao_nascer_mulheres, na.rm=TRUE))

exemplo_geral <- banco_mundial_2019_2020 %>% 
  filter(!is.na(banco_mundial_2019_2020$pib_dolares_precos_correntes))%>%
  mutate(pib_per_capita=pib_dolares_precos_correntes/populacao)%>%
  group_by(continente)%>%
  summarise(media=mean(pib_per_capita))%>%
  arrange(continente)



#### Visualização de dados

### Gráfico 1

install.packages(ggplot2)
library(ggplot2) #pacote para gráficos

#banco_mundial_2019_2020 <- fread("banco_mundial_2019_2020.csv")
#rm(exemplo_filter1, exemplo_filter2, exemplo_filter3, exemplo_geral, exemplo_mutate, exemplo_select, exemplo_summarise1, exemplo_summarise2)

banco_mundial_2019_2020 <- mutate(banco_mundial_2019_2020, pib_per_capita=pib_dolares_precos_correntes/populacao)

## Passo 1: Criar gráfico
ggplot(banco_mundial_2019_2020)+
  geom_point(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens))

## Passo 2: Escala logaritimica em x
ggplot(banco_mundial_2019_2020)+
  geom_point(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens))+
  scale_x_log10()

## Passo 3: Tirar a notação da escala de x
ggplot(banco_mundial_2019_2020)+
  geom_point(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens))+
  scale_x_log10(labels=function(x) format(x, scientific = FALSE))
  
## Passo 4: Nomes dos eixos e títulos
ggplot(banco_mundial_2019_2020)+
  geom_point(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens))+
  scale_x_log10(labels=function(x) format(x, scientific = FALSE))+
  labs(y="Expectativa de vida ao nascer para os Homens", x="PIB per capita (base log)", title="Correlação entre expectativa de vida ao nascer e PIB per capita")
  
## Passo 5: Alterar características
ggplot(banco_mundial_2019_2020)+
  geom_point(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens, color=continente, size=populacao))+
  scale_x_log10(labels=function(x) format(x, scientific = FALSE))+
  labs(y="Expectativa de vida ao nascer para os Homens", x="PIB per capita (base log)", title="Correlação entre expectativa de vida ao nascer e PIB per capita", color="Continente", size="População")

# Funções: geom_point, geom_text, geom_col, geom_line, geom_smooth
# características: x, y, color, fill, size, alpha, linetype, label, shape, width

## Passo 6: Outros passos
ggplot(banco_mundial_2019_2020)+
  geom_point(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens, fill=continente, size=populacao), shape=21, color="black", alpha=0.7)+
  geom_smooth(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens), method="lm", se=FALSE, colour="black")+
  geom_text(data=subset(banco_mundial_2019_2020, pais=="Brasil"|pais=="China"|pais=="?ndia"), aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens, label=paste(codigo)), size=4, colour="black")+
  scale_x_log10(labels=function(x) format(x, scientific = FALSE))+
  scale_size_continuous(range = c(2, 10))+
  labs(y="Expectativa de vida ao nascer para os Homens", x="PIB per capita (base log)", fill="Legenda:", title="Correlação entre expectativa de vida ao nascer e PIB per capita")+
  guides(fill = guide_legend(override.aes = list(size=4)), size=FALSE)+
  theme(legend.position = "bottom")



### Gráfico 2

pib <- fread("pib_trimestral_dessazonalizado_indice.csv")

pib$trimestre <- as.Date(pib$trimestre, format="%d/%m/%Y")

#Criar variável categórica, igual a 1 se pib positivo e igual a 0 se pib negativo
pib$positivo[pib$crescimento_pib>0] <- 1 
pib$positivo[pib$crescimento_pib<0] <- 0 
pib$positivo <- as.factor(pib$positivo)

pib_filtrado <- filter(pib, trimestre>=as.Date("01/01/2010", format="%d/%m/%Y"))

ggplot(pib_filtrado)+
  geom_col(data=subset(pib_filtrado, !is.na(crescimento_pib)), aes(x=trimestre, y=crescimento_pib*100, fill=positivo))+
  geom_line(aes(x=trimestre, y=pib), color="black", size=1)+
  scale_y_continuous(sec.axis = sec_axis(~ ./100, name = "Crescimento (%)"))+
  labs(x="Trimestre", y="Número Índice", title="PIB trimestral dessazonalizado")+
  scale_x_date(date_breaks = "6 month", date_labels = "%m/%Y")+
  theme(axis.text.x = element_text(angle=90, hjust=1), legend.position = "none")+
  scale_fill_manual(values=c("red", "springgreen4"))+
  geom_hline(yintercept=0, linetype="dashed")



### Mapa

adequacao_rs <- fread("adequacao_rs.csv")

install.packages("sp")
library(sp) #importar RDS

mapa_rs <- readRDS("MapaRSMunicipios.RDS")

base_mapa <- merge(mapa_rs, adequacao_rs, by="GEOCODIGO")
  
## Passo 1: carregar mapa
install.packages("leaflet")
library(leaflet) #fazer mapas

base_mapa%>%
  leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap)
# Outros providers: names(providers)

## Passo 2: adicionar shape/polígonos
base_mapa%>%
  leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap)%>%
  addPolygons()

## Passo 3: escolher caracter?sticas
gradiente <- colorNumeric(palette = "YlOrRd", domain = base_mapa$ensino_fundamental)
# Outras paletas de cores: viridis, magma, inferno, plasma, YlOrRd, YlOrBr, YlGnBu, YlGn, Reds, RdPu, Purples, PuRd, PuBuGn, Blues, Greens, Greys, Oranges

base_mapa%>%
  leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap)%>%
  addPolygons(weight = 1, color = "grey", fillColor = ~gradiente(base_mapa$ensino_fundamental), fillOpacity = 0.5,
              popup = paste0(base_mapa$cidade, "<br>", "Porcentagem: ", base_mapa$ensino_fundamental, "%"))

# weight = espessura da linha
# color = cor da linha
# fillColor = cor do preenchimento dos pol?gonos
# fillOpacity = grau de transpar?ncia
# popup = caixa de texto
  
## Passo 4: adicionar legenda
base_mapa%>%
  leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap)%>%
  addPolygons(weight = 1, color = "grey", fillColor = ~gradiente(base_mapa$ensino_fundamental), fillOpacity = 0.5,
              popup = paste0(base_mapa$cidade, "<br>", "Porcentagem: ", base_mapa$ensino_fundamental, "%"))%>%
  addLegend(position = "bottomright", pal = gradiente, values = ~ensino_fundamental, title="Legenda:")
# Posições da legenda: topright, bottomright, bottomleft, topleft
    
## Passo 5: adicionar marcadores ou ajustar zoom
base_mapa%>%
leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap)%>%
  addPolygons(weight = 1, color = "grey", fillColor = ~gradiente(base_mapa$ensino_fundamental), fillOpacity = 0.5,
              popup = paste0(base_mapa$cidade, "<br>", "Porcentagem: ", base_mapa$ensino_fundamental, "%"))%>%
  addLegend(position = "bottomright", pal = gradiente, values = ~ensino_fundamental, title="Legenda:") %>%
  addMarkers(lat = -30.027155, lng = -51.175108)%>%
  setView(lng = -51.187634, lat =  -30.012577, zoom = 10)



#### Dashboard

# Em outro script



#### Pnad Contínua

#install.packages("PNADcIBGE")
library(PNADcIBGE) # baixar dados da Pnad Contínua
library(survey) # analisar dados amostrais

dadosPNADc <- get_pnadc(year = 2019, quarter = 2)
View(dadosPNADc$variables)

taxa_desocup <- svymean(~VD4002,dadosPNADc,na.rm=TRUE)
svymean(~VD4002,subset(dadosPNADc, UF=="Rio Grande do Sul"),na.rm=TRUE)
svymean(~VD4002,subset(dadosPNADc, UF=="Rio Grande do Sul"&V1023=="Capital"&V2007=="Mulher"),na.rm=TRUE)

#Outras funções: svytotal, svyratio, svyquantile

