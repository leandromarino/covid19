library(magrittr)
library(gdtools)
library(extrafont)


#preparando fontes para a plotagem dos gráficos
extrafont::font_import('Baloo_Chettan_2/')
extrafont::fonts()
loadfonts(device = "pdf")
loadfonts()



# funcao para leitura dos dados
read_covid_daily <- function(dia,
                             mes,
                             link_base = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/')
{
  
  link <- paste0(link_base, sprintf('%02d', mes), '-', sprintf('%02d', dia), '-2020.csv')
  link_info <- httr::GET(link)
  
  cat(paste0('Importando dado covid19. Data: ', dia, '/', mes, '/2020\n'))
  
  
  if(link_info$status_code == 404){
    result <- NULL
  }else if(link_info$status_code == 200){
    result <- read.csv(file = link, header = TRUE, as.is = TRUE)
    result$dia <- dia
    result$mes <- mes
    result$ano <- 2020
    result$data_arq <- paste0(2020, sprintf('%02d', mes), sprintf("%02d", dia))
  }else{
    result <- NULL
    warning(paste0('Verificar link. Código de erro: ', link_info$status_code, '\n Dia: ', dia, 'Mês: ', mes, 'não importado!!!!'))
  }
  
  result
  
}







# lendo arquivos
df_dias <- data.frame(dia = c(1:31, 1:29, 1:31, 1:30), mes = rep(1:4,c(31,29,31,30)))
dados_covid <- df_dias %>% apply(., 1, function(x){ read_covid_daily(dia = x[[1]], mes = x[[2]])} )
df_dias$data <- df_dias %>% apply(., 1, function(x) paste0(2020, x[[2]] %>% sprintf('%02d',.), x[[1]] %>% sprintf('%02d',.)))

names(dados_covid) <- df_dias$data

remover_vazios <- dados_covid %>% lapply(., is.null) %>% unlist() %>% which()
dados_covid <- dados_covid[- remover_vazios ]
rm(remover_vazios)

dados_covid <- lapply(dados_covid, plyr::rbind.fill)
dados_covid <- plyr::rbind.fill(dados_covid)


colnames(dados_covid) <- plyr::mapvalues(x = colnames(dados_covid),
                                         from = c("Province.State", "Country.Region", "Last.Update",
                                                  "Confirmed", "Deaths", "Recovered", "dia", "mes",
                                                  "ano", "data_arq", "Latitude", "Longitude"),
                                         to = c('provincia', 'pais', 'atualizacao',
                                                'confirmados', 'mortos', 'recuperados', 'dia', 'mes',
                                                'ano', 'data_arq', 'latitude', 'longitude'))

dados_covid %>% head
dados_covid$pais <- stringr::str_trim(dados_covid$pais)

dados_covid$pais[dados_covid$pais %>% grep('Iran',.)] <- 'Iran'

covid_pais <- dados_covid %>%
  dplyr::group_by(pais, data_arq) %>%
  dplyr::summarise(confirmados = sum(confirmados, na.rm = TRUE),
                   mortos      = sum(mortos     , na.rm = TRUE),
                   recuperados = sum(recuperados, na.rm = TRUE)) %>%
  as.data.frame

# separando arquivo por pais
# para poder o numero de dias de corona.
# cada pais aparece apenas quando confirma o numero de casos
temp <- covid_pais %>% split(., .$pais)
temp <- lapply(temp, function(x){
  x <- x[order(x$data_arq), ]
  x$dia_infec <- 1:nrow(x)
  x
})
covid_pais <- do.call(rbind, temp)
rm(temp)


temp <- covid_pais %>% split(., .$pais)
temp$Italy



setwd('Dropbox (Leandro Marino)/Covid19/')
# pdf(file = paste0('graf_', covid_pais$data_arq %>% max(), 'ITA_BRA_IRA_001casos.pdf'),
#     width = 15/2.54, height = 9/2.54, pointsize = 12, bg = 'white',
#     family = 'Baloo Chettan 2')
# #
png(file = paste0('graf_', covid_pais$data_arq %>% max(), 'ITA_BRA_IRA_001casos.png'),res = 300,
    width = 480*5, height = 480*3, pointsize = 12, bg = 'white',
    family = 'Baloo Chettan 2')

par(mar = c(6, 4, 3, 2) + 0.1)
plot(x = 0, y = 0, type = 'n',
     xlab = 'Dia (a partir do primeiro diagnóstico positivo)',
     ylab = 'Número de casos', lwd = 2, col = 'red',
     xlim = c(0, 30), ylim = c(0, 3000))
lines(x = temp$Italy$dia_infec , y = temp$Italy$confirmados , col = 'red', lwd = 2)
lines(x = temp$Brazil$dia_infec, y = temp$Brazil$confirmados, col = 'blue', lwd = 2)
lines(x = temp$Iran$dia_infec  , y = temp$Iran$confirmados  , col = 'green', lwd = 2)
legend('topleft', legend = c('Itália', 'Brasil', 'Irã'), col = c('red', 'blue', 'green'), lwd = 2, bty = 'n')
title('Série histórica de casos confirmados de covid19\n(a partir do primeiro diagnóstico)')
mtext('Elaborado por: Leandro Marino', side = 1, line = 4.5, adj = 0, cex = 0.8)
mtext('Fonte: https://github.com/CSSEGISandData/COVID-19', side = 1, line = 4.5, adj = 1, cex = 0.8)
dev.off()













temp <- covid_pais %>% .[.$confirmados > 10,] %>% split(., .$pais)
temp <- lapply(temp, function(x){
  x <- x[order(x$data_arq), ]
  x$dia_infec <- 1:nrow(x)
  x
})
covid_pais_10p <- do.call(rbind, temp)
rm(temp)




temp <- covid_pais_10p %>% split(., .$pais)


# pdf(file = paste0('graf_', covid_pais$data_arq %>% max(), 'ITA_BRA_IRA_010casos.pdf'),
#     width = 15/2.54, height = 9/2.54, pointsize = 12, bg = 'white',
#     family = 'Baloo Chettan 2')
#
png(file =  paste0('graf_', covid_pais$data_arq %>% max(), 'ITA_BRA_IRA_010casos.png'),res = 300,
    width = 480*5, height = 480*3, pointsize = 12, bg = 'white',
    family = 'Baloo Chettan 2')

par(mar = c(6, 4, 3, 2) + 0.1)
plot(x = 0, y = 0, type = 'n',
     xlab = 'Dia (a partir do décimo diagnóstico positivo)',
     ylab = 'Número de casos', lwd = 2, col = 'red',
     xlim = c(0, 30), ylim = c(0, 3000))
lines(x = temp$Italy$dia_infec , y = temp$Italy$confirmados , col = 'red', lwd = 2)
lines(x = temp$Brazil$dia_infec, y = temp$Brazil$confirmados, col = 'blue', lwd = 2)
lines(x = temp$Iran$dia_infec  , y = temp$Iran$confirmados  , col = 'green', lwd = 2)
legend('topleft', legend = c('Itália', 'Brasil', 'Irã'), col = c('red', 'blue', 'green'), lwd = 2, bty = 'n')
title('Série histórica de casos confirmados de covid19\n(a partir do décimo diagnóstico)')
mtext('Elaborado por: Leandro Marino', side = 1, line = 4.5, adj = 0, cex = 0.8)
mtext('Fonte: https://github.com/CSSEGISandData/COVID-19', side = 1, line = 4.5, adj = 1, cex = 0.8)
dev.off()













temp <- covid_pais %>% .[.$confirmados > 100,] %>% split(., .$pais)
temp <- lapply(temp, function(x){
  x <- x[order(x$data_arq), ]
  x$dia_infec <- 1:nrow(x)
  x
})
covid_pais_100p <- do.call(rbind, temp)
rm(temp)




temp <- covid_pais_100p %>% split(., .$pais)


# pdf(file = paste0('graf_', covid_pais$data_arq %>% max(), 'ITA_BRA_IRA_0100casos.pdf'),
#     width = 15/2.54, height = 9/2.54, pointsize = 12, bg = 'white',
#     family = 'Baloo Chettan 2')
#
png(file = paste0('graf_', covid_pais$data_arq %>% max(), 'ITA_BRA_IRA_100casos.png'),res = 300,
    width = 480*5, height = 480*3, pointsize = 12, bg = 'white',
    family = 'Baloo Chettan 2')

par(mar = c(6, 4, 3, 2) + 0.1)
plot(x = 0, y = 0, type = 'n',
     xlab = 'Dia (a partir do centésimo diagnóstico positivo)',
     ylab = 'Número de casos', lwd = 2, col = 'red',
     xlim = c(0, 30), ylim = c(0, 3000))
lines(x = temp$Italy$dia_infec , y = temp$Italy$confirmados , col = 'red', lwd = 2)
lines(x = temp$Brazil$dia_infec, y = temp$Brazil$confirmados, col = 'blue', lwd = 2)
lines(x = temp$Iran$dia_infec  , y = temp$Iran$confirmados  , col = 'green', lwd = 2)
legend('topleft', legend = c('Itália', 'Brasil', 'Irã'), col = c('red', 'blue', 'green'), lwd = 2, bty = 'n')
title('Série histórica de casos confirmados de covid19\n(a partir do centésimo diagnóstico)')
mtext('Elaborado por: Leandro Marino', side = 1, line = 4.5, adj = 0, cex = 0.8)
mtext('Fonte: https://github.com/CSSEGISandData/COVID-19', side = 1, line = 4.5, adj = 1, cex = 0.8)
dev.off()

