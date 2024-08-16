install.packages("readxl") 
library(readxl)

attack1 <- read_excel("Dados/Dados_Adeanio_Attack_I_27052024.xlsx", sheet = "data")

#Contando o número de ataques
attack1$attack621 <- as.numeric(attack1$attack621)
attack1$attack621 <- ifelse(is.na(attack1$attack621), 0, attack1$attack621)
attack1$attack621

attack1$attack621_v2 <- as.numeric(attack1$attack621_v2)
attack1$attack621_v2 <- ifelse(is.na(attack1$attack621_v2), 0, attack1$attack621_v2)
attack1$attack621_v2

attack1$attack211 <- as.numeric(attack1$attack211)
attack1$attack211 <- ifelse(is.na(attack1$attack211), 0, attack1$attack211)
attack1$attack211

attack1$attack211_v2 <- as.numeric(attack1$attack211_v2)
attack1$attack211_v2 <- ifelse(is.na(attack1$attack211_v2), 0, attack1$attack211_v2)
attack1$attack211_v2

attack1$soma.ataques <- attack1$attack621 + attack1$attack621_v2 + attack1$attack211 + attack1$attack211_v2
attack1$soma.ataques


#VERIFICANDO BARREIRAS RELACIONADAS A EXAMES
#Inserindo o valor TRUE para sim e FALSE para não e mantendo o NA
attack1$attack533

attack1$attack533 <- lapply(attack1$attack533, function(x) ifelse(is.na(x), NA, ifelse(x == "Sim", TRUE, FALSE)))
attack1$attack533 <- unlist(attack1$attack533)
unlist(attack1$attack533)
attack1$attack533

attack1$attack599 <- lapply(attack1$attack599, function(x) ifelse(is.na(x), NA, ifelse(x == "Sim", TRUE, FALSE)))
attack1$attack599 <- unlist(attack1$attack599)
unlist(attack1$attack599)
attack1$attack599

attack1$barreira.exame <- ifelse(is.na(attack1$attack533) & is.na(attack1$attack599),
                       NA,
                       ifelse(is.na(attack1$attack533) & attack1$attack599, attack1$attack599,
                              ifelse(is.na(attack1$attack599) & attack1$attack533, attack1$attack533,
                                     attack1$attack533 | attack1$attack599)))
attack1$barreira.exame

# dificuldade_verdadeiro = sum(attack1$barreira.exame, na.rm=T)
# dificuldade_falso = sum(!attack1$barreira.exame, na.rm=T)

resultado_t_acesso = t.test(attack621_v2 ~ barreira.exame, data=attack1, na.action = na.omit)

# Extraindo os resultados do teste t
t_statistic <- resultado_t_acesso$statistic
p_value <- resultado_t_acesso$p.value
conf_int <- resultado_t_acesso$conf.int
mean_diff <- resultado_t_acesso$estimate
df <- resultado_t_acesso$parameter

# Criando uma tabela de resultados
tabela_resultados <- data.frame(
  "Estatística t" = round(t_statistic, 4),
  "Valor p" = round(p_value, 4),
  "Intervalo de Confiança Inferior" = round(conf_int[1], 4),
  "Intervalo de Confiança Superior" = round(conf_int[2], 4),
  "Diferença de Médias" = round(mean_diff[1] - mean_diff[2], 4),
  "Graus de Liberdade" = df
)

# Exibindo a tabela no formato de Markdown
knitr::kable(tabela_resultados, format = "markdown")


#VERIFICANDO BARREIRAS RELACIONADAS A TRANSPORTE
attack1$attack546

#VERIFICANDO BARREIRAS RELACIONADAS A ATENÇÃO BÁSICA
attack1$attack527

#VERIFICANDO BARREIRAS RELACIONADAS A MEDICAMENTOS
attack1$attack601











