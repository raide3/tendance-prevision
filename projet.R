# =============================================================================
# Projet d'Analyse Descriptive des Séries Temporelles
# Étude : Popularité de "Station de ski" sur Wikipédia (2014-2025)
# =============================================================================

# --- 0. Chargement des packages ---
packages <- c("readxl", "ggplot2", "forecast", "tseries", "dplyr", "tidyr")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 1. CHARGEMENT
# Assure-toi que le fichier est dans le dossier de travail
df <- readxl::read_excel("ski_dataset.xlsx", sheet = "dataset")
colnames(df) <- c("Date", "Vues")

# Création de la série temporelle
ski_ts <- ts(df$Vues, start = c(2014, 1), frequency = 12)

# =============================================================================
# 2. ANALYSE EXPLORATOIRE - FIGURE 1
# =============================================================================
png("image.png", width = 1000, height = 500, res = 120)
plot.ts(ski_ts, 
        col = "steelblue", 
        lwd = 1.5,
        ylab = "Vues (milliers)",
        xlab = "Année")
title(main = "Vues Wikipédia : Station de ski (2014-2025)") # Titre à part pour éviter l'erreur
grid(col = "gray90", lty = 1)
dev.off()

# =============================================================================
# 3. DÉCOMPOSITION - FIGURE 2
# =============================================================================
decomp <- decompose(ski_ts, type = "additive")

png("image2.png", width = 1000, height = 800, res = 120)
plot(decomp) # plot(decomp) ne supporte pas l'argument main directement
dev.off()

# =============================================================================
# 4. SÉRIE CVS ET TENDANCE - FIGURE 3
# =============================================================================
ski_cvs <- ski_ts - decomp$seasonal
t_index <- 1:length(ski_cvs)
reg_lineaire <- lm(as.numeric(ski_cvs) ~ t_index)

png("image3.png", width = 1000, height = 500, res = 120)
plot.ts(ski_cvs, 
        col = "gray50", 
        lwd = 1.2, 
        main = "Série CVS et ajustement de la tendance")
# Ajout des lignes de tendance
lines(decomp$trend, col = "steelblue", lwd = 2.5)
# Pour la régression linéaire, on crée un objet ts pour l'alignement
tendance_reg_ts <- ts(fitted(reg_lineaire), start = start(ski_ts), frequency = 12)
lines(tendance_reg_ts, col = "tomato", lwd = 2, lty = 2)

legend("topleft",
       legend = c("Série CVS", "MM centrée (tendance)", "Régression linéaire"),
       col = c("gray50", "steelblue", "tomato"),
       lty = c(1, 1, 2), lwd = 2, bty = "n")
dev.off()

# =============================================================================
# 5. DÉSAISONNALISATION PAR RÉGRESSION ET COMPARAISON CVS - FIGURE 4
# =============================================================================

# Création des variables indicatrices pour les mois (Saisonnalité déterministe)
# cycle(ski_ts) donne le mois (1 à 12) pour chaque observation
mois_factor <- factor(cycle(ski_ts))
t_reg <- 1:length(ski_ts)

# Modèle de régression : Tendance linéaire + indicatrices saisonnières
# On enlève l'intercept pour avoir tous les mois ou on garde janvier en référence
modele_reg <- lm(as.numeric(ski_ts) ~ t_reg + mois_factor)

# Extraction de la composante saisonnière par régression
# On récupère les coefficients des mois (2 à 12) et on fixe Janvier à 0 (référence)
coeff_reg <- c(0, coef(modele_reg)[3:13]) 
# Centrage des coefficients (somme égale à 0 comme pour la MM)
coeff_reg_centrees <- coeff_reg - mean(coeff_reg)

# Calcul de la série CVS par régression
# On soustrait à chaque point le coefficient du mois correspondant
sais_reg_complete <- coeff_reg_centrees[cycle(ski_ts)]
ski_cvs_reg <- ts(as.numeric(ski_ts) - sais_reg_complete, start = start(ski_ts), frequency = 12)

# Génération de l'image 4
png("image4_cvs_comparaison.png", width = 1000, height = 500, res = 120)
par(mar = c(5, 4, 4, 2))

# Tracer la CVS par Moyenne Mobile (calculée en partie 4)
plot.ts(ski_cvs, 
        col = "steelblue", 
        lwd = 1.5, 
        main = "Comparaison des séries CVS : Moyenne Mobile vs Régression",
        xlab = "Année",
        ylab = "Vues désaisonnalisées")

# Ajouter la CVS par Régression
lines(ski_cvs_reg, col = "tomato", lwd = 1.5, lty = 2)

# Grille et légende
grid(col = "gray90", lty = 1)
legend("topleft",
       legend = c("CVS (via Moyenne Mobile)", "CVS (via Régression)"),
       col = c("steelblue", "tomato"),
       lty = c(1, 2), 
       lwd = 2, 
       bty = "n")

dev.off()


# =============================================================================
# 6. PRÉVISIONS HOLT-WINTERS - FIGURE 5
# =============================================================================
n_train <- round(length(ski_ts) * 0.80)
ski_train <- window(ski_ts, end = time(ski_ts)[n_train])
ski_test  <- window(ski_ts, start = time(ski_ts)[n_train + 1])

model_hw_add <- HoltWinters(ski_train, seasonal = "additive")
prev_hw_add  <- predict(model_hw_add, n.ahead = length(ski_test))

png("image5_holtwinters.png", width = 1000, height = 500, res = 120)
plot(ski_ts, main = "Prévisions Holt-Winters Additif", col="gray40")
lines(prev_hw_add, col = "tomato", lwd = 2)
lines(ski_test, col = "steelblue", lwd = 2, lty = 2)
legend("topleft", 
       legend = c("Observé", "Prévision", "Réel (Test)"),
       col = c("gray40", "tomato", "steelblue"), lty = 1, lwd = 2)
dev.off()

# =============================================================================
# 7. STABILITÉ SAISONNIÈRE - FIGURE 6
# =============================================================================
# On utilise matplot qui est plus robuste pour les comparaisons
noms_mois <- c("Jan","Fév","Mar","Avr","Mai","Jun","Jul","Aoû","Sep","Oct","Nov","Déc")
d1 <- decompose(window(ski_ts, start=c(2014,1), end=c(2017,12)))$figure
d2 <- decompose(window(ski_ts, start=c(2018,1), end=c(2021,12)))$figure
d3 <- decompose(window(ski_ts, start=c(2022,1), end=c(2025,12)))$figure

png("image6_stabilite_saisonnalite.png", width = 1000, height = 500, res = 120)
matplot(1:12, cbind(d1, d2, d3), type = "b", pch = 16:18, 
        col = c("steelblue", "tomato", "forestgreen"),
        xaxt = "n", xlab = "Mois", ylab = "Coeff",
        main = "Stabilité de la saisonnalité")
axis(1, at = 1:12, labels = noms_mois)
legend("topleft", legend = c("2014-2017", "2018-2021", "2022-2025"),
       col = c("steelblue", "tomato", "forestgreen"), pch = 16:18, bty = "n")
dev.off()

cat("\nOpération terminée. Les images sont prêtes pour LaTeX.\n")