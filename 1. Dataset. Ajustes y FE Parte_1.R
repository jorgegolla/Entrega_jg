rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
setwd( "C:\\Users\\User\\Documents\\ITBA\\2. Mineria" )

# Cargo el dataset con historia

dataset  <- fread( "./datasets/competencia1_historia_2022.csv.gz" )


# Hago Ajustes y Parte1 del FE


# ------------------------------------------------------------------------
# IPC de 201901 a 202103 (27 meses)
# ------------------------------------------------------------------------

IPC = c(189.6,196.8,206.0,213.1,219.6,225.5,230.5,239.6,253.7,262.1,273.2,283.4,289.8,295.7,305.6,310.1,314.9,322.0,328.2,337.1,346.6,359.7,371.0,385.9,401.5,415.9,435.9)

#variables a ajustar por IPC
varMonetarias = c("mrentabilidad","mrentabilidad_annual","mcomisiones","mactivos_margen","mpasivos_margen","mcuenta_corriente_adicional","mcuenta_corriente","mcaja_ahorro",
                  "mcaja_ahorro_adicional","mcuentas_saldo","mautoservicio","mtarjeta_visa_consumo","mtarjeta_master_consumo","mprestamos_personales",
                  "mprestamos_prendarios","mprestamos_hipotecarios","mplazo_fijo_pesos","minversion1_pesos","minversion2","mpayroll",
                  "mpayroll2","mcuenta_debitos_automaticos","mttarjeta_master_debitos_automaticos","mpagodeservicios","mpagomiscuentas","mcajeros_propios_descuentos",
                  "mtarjeta_visa_descuentos", "mtarjeta_master_descuentos","mcomisiones_mantenimiento","mcomisiones_otras","mtransferencias_recibidas","mtransferencias_emitidas",
                  "mextraccion_autoservicio","mcheques_depositados","mcheques_emitidos","mcheques_depositados_rechazados","mcheques_emitidos_rechazados","matm","matm_other",
                  "Master_mfinanciacion_limite","Master_msaldototal","Master_msaldopesos","Master_mconsumospesos","Master_mlimitecompra",
                  "Master_madelantopesos","Master_mpagado","Master_mpagospesos","Master_mconsumototal","Master_mpagominimo",
                  "Visa_mfinanciacion_limite","Visa_msaldototal","Visa_msaldopesos","Visa_mconsumospesos","Visa_mlimitecompra",
                  "Visa_madelantopesos","Visa_mpagado","Visa_mpagospesos","Visa_mconsumototal","Visa_mpagominimo")


for (var in varMonetarias) { 
  
  dataset[foto_mes == 201901, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[1]), .SDcols = var]
  dataset[foto_mes == 201902, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[2]), .SDcols = var]
  dataset[foto_mes == 201903, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[3]), .SDcols = var]
  dataset[foto_mes == 201904, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[4]), .SDcols = var]
  dataset[foto_mes == 201905, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[5]), .SDcols = var]
  dataset[foto_mes == 201906, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[6]), .SDcols = var]
  dataset[foto_mes == 201907, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[7]), .SDcols = var]
  dataset[foto_mes == 201908, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[8]), .SDcols = var]
  dataset[foto_mes == 201909, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[9]), .SDcols = var]
  dataset[foto_mes == 201910, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[10]), .SDcols = var]
  dataset[foto_mes == 201911, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[11]), .SDcols = var]
  dataset[foto_mes == 201912, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[12]), .SDcols = var]
  dataset[foto_mes == 202001, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[13]), .SDcols = var]
  dataset[foto_mes == 202002, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[14]), .SDcols = var]
  dataset[foto_mes == 202003, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[15]), .SDcols = var]
  dataset[foto_mes == 202004, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[16]), .SDcols = var]
  dataset[foto_mes == 202005, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[17]), .SDcols = var]
  dataset[foto_mes == 202006, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[18]), .SDcols = var]
  dataset[foto_mes == 202007, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[19]), .SDcols = var]
  dataset[foto_mes == 202008, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[20]), .SDcols = var]
  dataset[foto_mes == 202009, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[21]), .SDcols = var]
  dataset[foto_mes == 202010, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[22]), .SDcols = var]
  dataset[foto_mes == 202011, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[23]), .SDcols = var]
  dataset[foto_mes == 202012, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[24]), .SDcols = var]
  dataset[foto_mes == 202101, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[25]), .SDcols = var]
  dataset[foto_mes == 202102, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[26]), .SDcols = var]
  #dataset[foto_mes == 202103, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[27]), .SDcols = var]
  
}

# ------------------------------------------------------------------------
# AJUSTE X Tipo de Cambio Oficial (TCO) 
# ------------------------------------------------------------------------


#TCO de 201901 a 202103 (27 meses)

TCO = c(37.035,38.9983,43.3533,44.01,44.87,42.4483,43.8692,59.075,57.5583,59.7267,59.8633,59.895,60.3312,62.208,64.4697,66.835,68.535,70.455,72.315,74.175,76.175,78.3283,81.2967,84.145,87.2983,89.825,91.985)


#variables a ajustar por TCO

varDolarizadas = c("mcaja_ahorro_dolares", "mplazo_fijo_dolares", "minversion1_dolares", "mforex_buy","mforex_sell","Master_msaldodolares","Master_mconsumosdolares","Master_madelantodolares","Master_mpagosdolares",
                   "Visa_mpagosdolares","Visa_msaldodolares","Visa_mconsumosdolares","Visa_madelantodolares")


for (var in varDolarizadas) { 
  
  dataset[foto_mes == 201901, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[1]), .SDcols = var]
  dataset[foto_mes == 201902, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[2]), .SDcols = var]
  dataset[foto_mes == 201903, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[3]), .SDcols = var]
  dataset[foto_mes == 201904, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[4]), .SDcols = var]
  dataset[foto_mes == 201905, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[5]), .SDcols = var]
  dataset[foto_mes == 201906, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[6]), .SDcols = var]
  dataset[foto_mes == 201907, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[7]), .SDcols = var]
  dataset[foto_mes == 201908, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[8]), .SDcols = var]
  dataset[foto_mes == 201909, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[9]), .SDcols = var]
  dataset[foto_mes == 201910, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[10]), .SDcols = var]
  dataset[foto_mes == 201911, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[11]), .SDcols = var]
  dataset[foto_mes == 201912, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[12]), .SDcols = var]
  dataset[foto_mes == 202001, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[13]), .SDcols = var]
  dataset[foto_mes == 202002, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[14]), .SDcols = var]
  dataset[foto_mes == 202003, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[15]), .SDcols = var]
  dataset[foto_mes == 202004, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[16]), .SDcols = var]
  dataset[foto_mes == 202005, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[17]), .SDcols = var]
  dataset[foto_mes == 202006, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[18]), .SDcols = var]
  dataset[foto_mes == 202007, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[19]), .SDcols = var]
  dataset[foto_mes == 202008, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[20]), .SDcols = var]
  dataset[foto_mes == 202009, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[21]), .SDcols = var]
  dataset[foto_mes == 202010, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[22]), .SDcols = var]
  dataset[foto_mes == 202011, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[23]), .SDcols = var]
  dataset[foto_mes == 202012, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[24]), .SDcols = var]
  dataset[foto_mes == 202101, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[25]), .SDcols = var]
  dataset[foto_mes == 202102, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[26]), .SDcols = var]
  #dataset[foto_mes == 202103, (var) := lapply(.SD, '*', TCO[length(TCO)] / TCO[27]), .SDcols = var]
  
}



# ------------------------------------------------------------------------
# VARIABLES AGREGADAS DE FE Parte1
# ------------------------------------------------------------------------


# 1.Saldo depositos totales en pesos (incluyendo inversiones)
## incluye CtaCte, CA, PF e inversiones

dataset[  , jg_CtaCte_pesos_depo := 0 ]
dataset[ mcuenta_corriente_adicional>=0 , jg_CtaCte_pesos_depo := mcuenta_corriente_adicional ]
dataset[ mcuenta_corriente>=0 ,           jg_CtaCte_pesos_depo := mcuenta_corriente ]
dataset[ , jg_totaldep_pesos          := rowSums( cbind( jg_CtaCte_pesos_depo, mcaja_ahorro, mcaja_ahorro_adicional, mplazo_fijo_pesos, minversion1_pesos, minversion2) , na.rm=TRUE ) ]

dataset[  , jg_CtaCte_pesos_prest := 0 ]
dataset[ mcuenta_corriente_adicional<=0 , jg_CtaCte_pesos_prest := mcuenta_corriente_adicional*-1 ]
dataset[ mcuenta_corriente<=0           , jg_CtaCte_pesos_prest := mcuenta_corriente*-1 ]


# ------------------------------------------------------------------------
# 2.Saldo depositos totales en dolares (incluyendo inversiones)
## incluye CtaCte, CA, PF e inversiones

dataset[ , jg_totaldep_us          := rowSums( cbind( mcaja_ahorro_dolares,  mplazo_fijo_dolares, minversion1_dolares) , na.rm=TRUE ) ]


# ------------------------------------------------------------------------
# 3.Saldo depositos totales (1.toales en $ + 2.totales en US$)

dataset[ , jg_totaldep_tot          := rowSums( cbind( jg_totaldep_pesos,  jg_totaldep_us) , na.rm=TRUE ) ]


# ------------------------------------------------------------------------
# 3.a Saldo de depósitos totales * edad

dataset[ , jg_totaldep_tot_poredad    := jg_totaldep_tot * cliente_edad]


# ------------------------------------------------------------------------
# 4. Cantidad de préstamos
# +1 x la tarjeta o x la ctacte

dataset[  , UNO := 1 ]
dataset[ , jg_cprest_tot          := rowSums( cbind( cprestamos_personales,  cprestamos_prendarios, cprestamos_hipotecarios, UNO) , na.rm=TRUE ) ]
dataset$UNO <-NULL 


# ------------------------------------------------------------------------
# 5. Saldo Total de préstamos

dataset[  , Master := 0 ]
dataset[  , Visa   := 0 ]
dataset[ Master_msaldototal>0 , Master := Master_msaldototal ]
dataset[ Visa_msaldototal>0    , Visa   := Visa_msaldototal ]
dataset[ , jg_prest_tot          := rowSums( cbind( mprestamos_personales,  mprestamos_prendarios, mprestamos_hipotecarios, Master, Visa, jg_CtaCte_pesos_prest) , na.rm=TRUE ) ]
dataset$Master <-NULL
dataset$Visa <-NULL


# ------------------------------------------------------------------------
# 6. Riqueza Neta (3. Saldo depositos totales - 1.Saldo Total de préstamos)

dataset[ , jg_riqueza_neta          :=  jg_totaldep_tot - jg_prest_tot ]


# ------------------------------------------------------------------------
# 7. Riqueza Neta / edad (6. Riqueza Neta / Edad)
## simil Irlanda pero con riqueza

dataset[  , jg_riqueza_neta_sobre_edad   := jg_riqueza_neta / cliente_edad ]


# ------------------------------------------------------------------------
# 8. Apalancamiento 1 (Riqueza Neta / ingresos)

dataset[  , jg_riqueza_neta_sobre_ingresos   := jg_riqueza_neta / mpayroll ]


# ------------------------------------------------------------------------
# 9. Activos / edad ((3. Saldo depositos totales)  / edad)
## simil Irlanda pero con activos

dataset[  , jg_riqueza_bruta_sobre_edad   := jg_totaldep_tot / cliente_edad ]


# ------------------------------------------------------------------------
# 10. Saldo promedio de préstamos (5. Saldo Total de préstamos / 4. Cantidad de préstamos)

dataset[  , jg_prestamo_promedio   := jg_prest_tot / jg_cprest_tot ]


# ------------------------------------------------------------------------
#10.1. Prestamo promedio / edad

dataset[  , jg_prestamo_promedio_sobre_edad   := jg_prestamo_promedio / cliente_edad ]


# ------------------------------------------------------------------------
# 11. Apalancamiento 2 (5. Saldo Total de préstamos / 3.Saldo depositos totales)

dataset[  , jg_prestamos_sobre_depositos   := jg_prest_tot / jg_totaldep_tot ]


# ------------------------------------------------------------------------
# 12. Apalancamiento 3 (5. Saldo Total de préstamos / ingresos )

dataset[  , jg_prestamos_sobre_ingresos   := jg_prest_tot / mpayroll ]


# ------------------------------------------------------------------------
# 13. Cantidad de seguros

dataset[ , jg_cseguros_tot          := rowSums( cbind( cseguro_vida,  cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales) , na.rm=TRUE ) ]


# ------------------------------------------------------------------------
#13.a. Cantidad de seguros * riqueza neta

dataset[  , jg_cseguros_tot_por_RN   := jg_cseguros_tot * jg_riqueza_neta ]


# ------------------------------------------------------------------------
#13.b. Cantidad de seguros * riqueza bruta
dataset[  , jg_cseguros_tot_por_RN   := jg_cseguros_tot * jg_totaldep_tot ]


# ------------------------------------------------------------------------
# 14. Comisiones / ingresos
# 15. Comisiones / 3.Saldo depositos totales
# 16. Comisiones / edad

dataset[  , comis_tot          := rowSums( cbind( mcomisiones_mantenimiento,  mcomisiones_otras) , na.rm=TRUE ) ]
dataset[  , jg_comisiones_sobre_ingresos   := comis_tot / mpayroll ]
dataset[  , jg_comisiones_sobre_riquezabruta   := comis_tot / jg_totaldep_tot ]
dataset[  , jg_comisiones_sobre_edad   := comis_tot / cliente_edad ]
dataset$comis_tot <-NULL


# ------------------------------------------------------------------------
# 16. Cliente sofisticado (es VIP, tiene Caja seguridad, tiene inversiones en US, tiene PF en dolares, tiene seguros, tiene consumo en US tarjeta )

dataset[  , jg_cliente_sofisticado := 0 ]
dataset[ cliente_vip==1 ,                jg_cliente_sofisticado := 1 ]
dataset[ ccaja_seguridad>0 ,             jg_cliente_sofisticado := 1 ]
dataset[ minversion1_dolares>100000 ,    jg_cliente_sofisticado := 1 ]
dataset[ mplazo_fijo_dolares>100000 ,    jg_cliente_sofisticado := 1 ]
dataset[ jg_cseguros_tot>=2 ,            jg_cliente_sofisticado := 1 ]
dataset[ Visa_mconsumosdolares>100000 ,  jg_cliente_sofisticado := 1 ]
dataset[ Master_mconsumosdolares>100000 ,jg_cliente_sofisticado := 1 ]


# ------------------------------------------------------------------------
#16.a Cliente sofisticado por riqueza neta

dataset[  , jg_cliente_sofisticado_por_RN   := jg_cliente_sofisticado * jg_riqueza_neta ]


# ------------------------------------------------------------------------
#16.b Cliente sofisticado por riqueza bruta

dataset[  , jg_cliente_sofisticado_por_Rbruta   := jg_cliente_sofisticado * jg_totaldep_tot ]


# ------------------------------------------------------------------------
#16.c Cliente sofisticado x edad

dataset[  , jg_cliente_sofisticado_por_edad   := jg_cliente_sofisticado * cliente_edad ]


# ------------------------------------------------------------------------
# 17. Transaccional monto total (Transferencia+Cheques)

dataset[  , jg_transaccional_tot          := rowSums( cbind( mtransferencias_recibidas, mtransferencias_emitidas, mcheques_depositados, mcheques_emitidos) , na.rm=TRUE ) ]


# ------------------------------------------------------------------------
# 18. Transaccional cantidad total

dataset[  , jg_ctransaccional_tot          := rowSums( cbind( ctransferencias_recibidas, ctransferencias_emitidas, ccheques_depositados, ccheques_emitidos) , na.rm=TRUE ) ]


# ------------------------------------------------------------------------
# 19. Transaccional / edad

dataset[  , jg_transaccional_tot_sobre_edad   := jg_transaccional_tot / cliente_edad ]


# ------------------------------------------------------------------------
# 20. Transaccional promedio

dataset[  , jg_transaccional_promedio   := jg_transaccional_tot / jg_ctransaccional_tot ]


# ------------------------------------------------------------------------
#21 Transaccional promedio por edad

dataset[  , jg_transaccional_promedio_poredad   := jg_transaccional_promedio * cliente_edad ]


#--------------------------------------------------------------------------------------

# grabo el dataset en disco local y luego subo a GoogleCloud para para el Paso 2.

#grabo el dataset
setwd( "./datasets" )
fwrite( dataset,
        "competencia1_historia_2022_ajustadoIPC2.csv.gz",
        logical01= TRUE,
        sep= "," )
