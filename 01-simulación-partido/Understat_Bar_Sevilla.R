
# ------------------------------------------------
# --------------- Resultado m?s frecuente---------
# ------------------------------------------------

# 1. Lectura del fichero de datos 
# Understat. Barcelona - Atl?tico de Madrid

shots_df <- read.csv(
  file = 'Understat_Barcelona_Sevilla.csv', 
  sep=",", header = TRUE, encoding = 'UTF-8')
shots_df[c(1:10),]

# 2. Caracter?sticas del fichero
colnames(shots_df) # columnas
cat("Número de disparos entre ambos equipos:", 
    nrow(shots_df))
str(shots_df)
# Conocemos algunas conclusiones: xG vs Goles
cat("xG total del Sevilla:", 
    sum(shots_df[shots_df$team == "Sevilla", "xG"]))
cat("xG total del Barcelona:", 
    sum(shots_df[shots_df$team != "Sevilla", "xG"]))
cat("Número de goles:", 
    nrow(shots_df[shots_df$result == 'Goal',]))


# 3. Simulaci?n de partidos

N <- 1000 # n?mero de simulaciones
# Para cada simulaci?n...
results_df <- data.frame()
for (j in 1:N){
  set.seed(j) # Semilla aleatoriedad
  result <- c(NA, NA) # Resultado en la simulaci?n
  k <- 1
  # Para cada equipo...
  for (t in unique(shots_df$team)){
    shots_team <- shots_df[shots_df$team == t, ]
    goals_t <- 0 # n? goles del equipo (inicializamos en 0)
    # Simulamos cada disparo
    for (i in 1:nrow(shots_team)){
      goals_t <- goals_t + rbinom(n=1, size=1, prob=shots_team$xG[i])
    } # Sumamos los ?xitos = n? goles del equipo en la simulaci?n
    result[k] <- goals_t
    k <- k+1
  }
  # Concatenamos resultados
  results_df <- rbind(results_df, result)
  # Renombramos las columnas
  colnames(results_df) <- unique(shots_df$team)
}

head(results_df) # Primeros registros


# 4. Nuevas variables

## Calculamos el resultado de cada partido
results_df$result <- paste(as.character(results_df$Sevilla), 
                           "-", as.character(results_df$`Barcelona`))
## Victoria local, visitante o empate
results_df$win <- ifelse(
  results_df$Sevilla > results_df$`Barcelona`, "Sevilla", 
  ifelse(results_df$Sevilla < results_df$`Barcelona`, "Barcelona", 
         "Empate"))


# 5. ?Cu?l hubiera sido el resultado m?s frecuente? 
results_table <- sort(table(results_df$result), decreasing=TRUE) 
results_table # n? veces que se repite cada resultado 
pos_real_result <- which(names(results_table) == "4 - 1") 
colors <- rep("gray", length(results_table)) 
colors[pos_real_result] <- "firebrick" 
barplot(results_table, 
        col = colors, 
        main = "Resultado más frecuente tras 1000 simulaciones FC Barcelona 1-4 Sevilla", 
        ylab = "Partidos", xlab = "Resultado", las=2)

# 6. Porcentaje de victoria local, visitante o empate

win_proportions <- prop.table(table(results_df$win))
win_proportions # porcentaje de cada posible resultado
colors <- c("royalblue", "firebrick", "forestgreen")
teams <- names(win_proportions)
labels <- paste(teams, round(100*win_proportions, 2), "%")
pie(win_proportions, col = colors, labels = labels,
    main = "Proporción de victoria \ntras 1000 partidos simulados")
legend("bottomleft", names(win_proportions),
       fill = colors, pt.cex = 1, bty = 'n', cex = 0.7)


# 7. C?lculo de xPoints
SEV_xPTS <- as.character(win_proportions["Sevilla"]*3 + 
                           win_proportions["Empate"])
FCB_xPTS <- as.character(win_proportions["Barcelona"]*3 + 
                           win_proportions["Empate"])
cat(paste("xPoints. FC Barcelona", FCB_xPTS, "-", 
          SEV_xPTS, "Sevilla"))
