# Packages
install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")
install.packages("treemap")
install.packages("ggplot2")
install.packages("hrbrthemes")
install.packages('xts')
install.packages("dygraphs")
install.packages("plotly")

library(plotly)
library(microdatasus)
library(dplyr)
library(treemap)
library(ggplot2)
library(hrbrthemes)
library(dygraphs)
library(xts)
library(tidyverse)
library(lubridate)
library(latticeExtra)

# Data Import

# dados <- fetch_datasus(year_start = 2017 ,month_start = 1, year_end = 2022,
                       month_end = 12, uf = "GO", information_system = "CNES-LT")

# write.csv(dados, file = "data.csv", row.names = FALSE)



df <- read.csv("data.csv")
str(df)
data.frame(head(df))

# Ajustando coluna de Data

df$ano <- factor(nrow(df))
df$mes <- factor(nrow(df))

df$ano <- as.factor(substr(df$COMPETEN, 1, 4))
df$mes <- as.factor(substr(df$COMPETEN, 5, 6))


df$data <- as.Date(paste0(df$ano, "-", df$mes, "-01"), format = "%Y-%m-%d")
df <- df %>% select(-ano, -mes, -COMPETEN, -CPF_CNPJ, -CNPJ_MAN, -NAT_JUR, -ESFERA_A, -ATIVIDAD, -NATUREZA, -CLIENTEL)

str(df)

### Organização e Limpeza dos dados

data.frame(head(df))

table(is.na(df))

# Removendo colunas nulas ou com sua grande maioria de linhas nula

sapply(df, function(x) {
  table(is.na(x)) #[2]
})


df$MICR_REG <- NULL


df$DISTRSAN <- NULL


df$REGSAUDE <- NULL


df$DISTRADM <- NULL


df$RETENCAO <- NULL


df$NIV_HIER <- NULL


df$TERCEIRO <- NULL



### Realizando Análise dos Dados

data.frame(head(df))

leito_pre <- df[, c("QT_EXIST", "data", "CODUFMUN")]
leito_pre <- leito_pre %>% filter(data >= "2017-01-01", data <= "2019-12-01")

leito_pos <- df[, c("QT_EXIST", "data", "CODUFMUN")]
leito_pos <- leito_pos %>% filter(data >= "2020-01-01", data <= "2022-12-01")     

# Compara a distribuição geográfica dos leitos por município antes e depois da pandemia
treemap(leito_pre,
        title = "Código das UF em Treemap para Medir a quantidade de leitos no pós-covid",
        index="CODUFMUN",
        vSize="QT_EXIST",
        type = "index",
        cex.labels = 15,
        cex.main = 2,
)

treemap(leito_pos,
        title = "Código das UF em Treemap para Medir a quantidade de leitos no pós-covid",
        index="CODUFMUN",
        vSize="QT_EXIST",
        type="index",
        cex.labels = 15,
        cex.main = 2
)

# Evolução do número de leitos no tempo
leito_evolucao <-  aggregate(QT_EXIST ~ data, data = df, FUN = sum)

# p <- ggplot(leito_evolucao, aes(x=data, y=QT_EXIST)) +
#   geom_line( color="steelblue") + 
#   geom_point(show.legend = TRUE) +
#   xlab("") +
#   theme_ipsum() +
#   theme(axis.text.x=element_text(angle=60, hjust=1)) +
#   labs( x = "Tempo", y = "Quantidade") +
#   scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2022-12-01"))) 
#  
# p

# Segunda Versão

don <- xts(x = leito_evolucao$QT_EXIST, order.by = leito_evolucao$data)

p <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)
p

# Comparar a quantidade de leitos SUS e leitos não SUS no tempo e no espaço
sus <- df[,c("data", "QT_SUS")]
sus_n <- df[,c("data", "QT_NSUS")]

sus_sum <-  aggregate(QT_SUS ~ data, data = sus, FUN = sum)
susn_sum <-  aggregate(QT_NSUS ~ data, data = sus_n, FUN = sum)

sus_merge <- merge(sus_sum,susn_sum, by = 'data' )
sus_merge


g <- plot_ly(sus_merge, x = ~data, y = ~QT_SUS, type = "scatter", mode = "lines", name = "Leitos do SUS")

g <- g %>% add_trace(data = sus_merge, x = ~data, y = ~QT_NSUS, name = "Leitos Terceiros", secondary_y = TRUE)

g

# Determinando a capacidade hospitalar de acordo com a quantidade de especialidades atendidas
# E com isso também conseguimos medir o aumento da frequência de especialidades nas datas informadas

df$Count_Espec <- 1
espec <-  aggregate(Count_Espec ~ data, data = df, FUN = sum)

ko <- xts(x = espec$Count_Espec, order.by = espec$data)

q <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)
q
