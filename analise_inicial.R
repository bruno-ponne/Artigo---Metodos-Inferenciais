library(readxl)
library(descr)
library(dplyr)
library(readr)
library(ggplot2)

# Etapa A - Carregando os dados do SAEB 2017 e 2019 para escolas de Ensino Médio de Pernambuco

SAEB_PE <- read_excel("data/SAEB_PE.xlsx")

# ND - Número de participantes no SAEB insuficiente para que os resultados sejam divulgados.

SAEB_PE[,7:12] <- sapply(SAEB_PE[,7:12], as.numeric)
SAEB_PE[,7:12] <- sapply(SAEB_PE[,7:12], as.numeric)

# ND e - foram automaticamente convertidos para NA pela função as.numeric.

freq(SAEB_PE$REDE)

# 92% das escolas são estaduais, 5% privadas. Para análise, ficaremos, por enquanto, com as estaduais:

SAEB_PE_ <- filter(SAEB_PE, REDE == "Estadual")

#####################################################################################
# Para executar esse trecho é necessário fazer download do arquivo CENSO_2019.csv
# Alternativamente, pule essa parte e carregue apenas os dados do Censo para PE
# Etapa B - Carregando dados do Censo Escolar 2019:

CENSO_2019 <- read_delim("CENSO_2019.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Filtra-se apenas escolas da rede estadual, que oferecem Ensino Médio em Pernambuco:

CENSO_2019_PE <- filter(CENSO_2019, SG_UF == "PE", TP_DEPENDENCIA == 2, IN_MED == 1)

save(CENSO_2019_PE, file = "CENSO_PE.RData")
######################################################################################
load("data/CENSO_PE.RData")

# Sugestão inicial de representantes de cada eixo:

# Eixo 1: Tempo Integral
# QT_MAT_MED_INT

# Eixo 2: Gestão
# IN_PROF_GESTAO
# QT_PROF_GESTAO

# Eixo 3: Inovação
# IN_ORGAO_NENHUM  - escola não possui órgão colegiado;
# QT_PROF_MONITORES
# IN_AREA_VERDE
# IN_MATERIAL_PED_NENHUM
# IN_MATERIAL_PED_CIENTIFICO
# IN_BIBLIOTECA

# Eixo 4: Infraestrutura
# IN_COZINHA
# IN_BANHEIRO
# IN_SECRETARIA
# IN_SALA_PROFESSOR
# IN_BANDA_LARGA

CENSO_SEL <- select(CENSO_2019_PE,
                    CO_ENTIDADE,
                    QT_MAT_MED_INT,
                    IN_PROF_GESTAO,
                    QT_PROF_GESTAO,
                    QT_PROF_ADMINISTRATIVOS,
                    IN_ORGAO_NENHUM,
                    QT_PROF_MONITORES,
                    IN_AREA_VERDE,
                    IN_MATERIAL_PED_NENHUM,
                    IN_MATERIAL_PED_CIENTIFICO,
                    IN_BIBLIOTECA,
                    IN_COZINHA,
                    IN_BANHEIRO,
                    IN_SECRETARIA,
                    IN_SALA_PROFESSOR,
                    IN_BANDA_LARGA)


summary(CENSO_SEL)

# só NA: IN_PROF_GESTAO, QT_PROF_GESTAO, IN_MATERIAL_PED_NENHUM

CENSO_SEL <- select(CENSO_SEL, -IN_PROF_GESTAO, -QT_PROF_GESTAO, -IN_MATERIAL_PED_NENHUM)


data <- left_join(SAEB_PE_, CENSO_SEL, by = c("CO_ESCOLA" = "CO_ENTIDADE"))

ggplot(data = data, aes(x= QT_MAT_MED_INT, y = MAT2019))+
  geom_point(alpha = .5)+
  geom_smooth(method = "lm", se = FALSE, color = "#d7301f")+
  ylab("Desempenho em Matemática")+
  xlab("Quantidade de Matrículas em Tempo Integral")+
  theme_bw()

ggplot(data = data, aes(x= QT_MAT_MED_INT, y = PORT2019))+
  geom_point(alpha = .5)+
  geom_smooth(method = "lm", se = FALSE, color = "#d7301f")+
  ylab("Desempenho em Português")+
  xlab("Quantidade de Matrículas em Tempo Integral")+
  theme_bw()

ggplot(data = data, aes(x= as.factor(IN_AREA_VERDE), y = MAT2019))+
  geom_boxplot()+
  theme_bw()

ggplot(data = data, aes(x= as.factor(IN_MATERIAL_PED_CIENTIFICO), y = MAT2019))+
  geom_boxplot()+
  theme_bw()

# 9: NÃO INFORMADO

ggplot(data = data, aes(x= as.factor(IN_ORGAO_NENHUM), y = MAT2019))+
  geom_boxplot()+
  theme_bw()

# Modelos Matemática

modelo_01 <- lm(MAT2019~QT_MAT_MED_INT, data = data)

summary(modelo_01)

modelo_02 <- lm(MAT2019~QT_MAT_MED_INT+QT_PROF_MONITORES, data = data)

summary(modelo_02)

modelo_03 <- lm(MAT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_AREA_VERDE), 
                  data = data)

summary(modelo_03)

modelo_04 <- lm(MAT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_COZINHA), 
                data = data)

summary(modelo_04)

modelo_05 <- lm(MAT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_COZINHA)+
                  as.factor(IN_SECRETARIA), 
                data = data)

summary(modelo_05)

modelo_06 <- lm(MAT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_COZINHA)+
                  as.factor(IN_SECRETARIA)+
                  as.factor(IN_SALA_PROFESSOR), 
                data = data)

summary(modelo_06)

IN_MATERIAL_PED_CIENTIFICO

modelo_07 <- lm(MAT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_COZINHA)+
                  as.factor(IN_SECRETARIA)+
                  as.factor(IN_SALA_PROFESSOR)+
                  as.factor(IN_MATERIAL_PED_CIENTIFICO), 
                data = data)

summary(modelo_07)

modelo_08 <- lm(MAT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_COZINHA)+
                  as.factor(IN_SECRETARIA)+
                  as.factor(IN_SALA_PROFESSOR)+
                  as.factor(IN_MATERIAL_PED_CIENTIFICO)+
                  as.factor(IN_BIBLIOTECA), 
                data = data)

summary(modelo_08)

modelo_09 <- lm(MAT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_BANDA_LARGA), 
                data = data)

summary(modelo_09)

modelo_10 <- lm(MAT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_BANDA_LARGA)+
                  QT_PROF_ADMINISTRATIVOS, 
                data = data)

summary(modelo_10)

# Modelos Português

modelo_01 <- lm(PORT2019~QT_MAT_MED_INT, data = data)

summary(modelo_01)

modelo_02 <- lm(PORT2019~QT_MAT_MED_INT+QT_PROF_MONITORES, data = data)

summary(modelo_02)

modelo_03 <- lm(PORT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_AREA_VERDE), 
                data = data)

summary(modelo_03)

modelo_04 <- lm(PORT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_COZINHA), 
                data = data)

summary(modelo_04)

modelo_05 <- lm(PORT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_COZINHA)+
                  as.factor(IN_SECRETARIA), 
                data = data)

summary(modelo_05)

modelo_06 <- lm(PORT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_COZINHA)+
                  as.factor(IN_SECRETARIA)+
                  as.factor(IN_SALA_PROFESSOR), 
                data = data)

summary(modelo_06)

IN_MATERIAL_PED_CIENTIFICO

modelo_07 <- lm(PORT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_COZINHA)+
                  as.factor(IN_SECRETARIA)+
                  as.factor(IN_SALA_PROFESSOR)+
                  as.factor(IN_MATERIAL_PED_CIENTIFICO), 
                data = data)

summary(modelo_07)

modelo_08 <- lm(PORT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_COZINHA)+
                  as.factor(IN_SECRETARIA)+
                  as.factor(IN_SALA_PROFESSOR)+
                  as.factor(IN_MATERIAL_PED_CIENTIFICO)+
                  as.factor(IN_BIBLIOTECA), 
                data = data)

summary(modelo_08)

modelo_09 <- lm(PORT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_BANDA_LARGA), 
                data = data)

summary(modelo_09)

modelo_10 <- lm(PORT2019 ~ QT_MAT_MED_INT+
                  QT_PROF_MONITORES+
                  as.factor(IN_BANDA_LARGA)+
                  as.factor(IN_BIBLIOTECA)+
                  as.factor(IN_AREA_VERDE)+
                  QT_PROF_ADMINISTRATIVOS, 
                data = data)

summary(modelo_10)

