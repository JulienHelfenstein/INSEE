library(dplyr)
library(purrr)
library(ggplot2)
library(psych)
library(devtools)
library(ggbiplot)

# Fonction pour avoir la p-valeur d'une chi2 et d'un V de Cramer
cramerv = function(x,y) {
    tbl = df %>% select(x,y) %>% table()
    chisq_pval = round(chisq.test(tbl)$p.value, 4)
    cramV = round(cramersV(tbl), 4) 
    data.frame(x, y, chisq_pval, cramV) }

# Mode statistique
find_mode = function(var) {
    tail(names(sort(table(var))), 1)
}

# Fonction pour obtenir des informations sur les variables
# Mettre variable sous la forme : dataframe$variable
info = function(variable) {
    if (class(variable) == "factor"){
        classe = class(variable)
        variable_mod = retirer_na(variable)
        modalites = length(levels(variable))
        manquantes = length(variable) - length(variable_mod)
        observations = length(variable_mod)
        
        mat_eff = matrix(0, nrow = 3, ncol = modalites)
        for (i in 1:modalites) {
            mat_eff[1,i] = levels(variable_mod)[i]
            mat_eff[2,i] = length(which(variable_mod == levels(variable_mod)[i]))
            mat_eff[3,i] = paste(as.character(round(100*length(which(variable_mod == levels(variable_mod)[i])) / observations, digits = 2)),"%")
        }
        plus_freq = list(mat_eff[1, which(mat_eff[2, ] == max(as.numeric(mat_eff[2, ])))])
        moins_freq = list(mat_eff[1, which(mat_eff[2, ] == min(as.numeric(mat_eff[2, ])))])
        
        df1 = as.data.frame(cbind(c("Classe", "Nombre d'observations", "Nombre de données manquantes", "Nombre de modalités distinctes", "Modalité la moins fréquente", "Modalité la plus fréquente"),c(classe, observations, manquantes, modalites, moins_freq, plus_freq)), nrow = 6, ncol = 2)
        df2 = as.data.frame(cbind(c("Modalités","Effectifs", "Pourcentage"), mat_eff), nrow = 3, ncol = modalites)
    return(list(df1,df2))
    }

    else if (class(variable) == "numeric") {
        classe = class(variable)
        variable_mod = retirer_na(variable)
        manquantes = length(variable) - length(variable_mod)
        observations = length(variable_mod)
        distinctes = length(unique(variable_mod))
        moyenne = mean(variable_mod, na.rm = TRUE)
        ic = list(IC(variable, 0.05))
        cv = sd(variable) / mean(variable)
        minimum = round(min(variable_mod, na.rm = TRUE))
        maximum = round(max(variable_mod, na.rm = TRUE))
        mediane = round(median(variable_mod, na.rm = TRUE))

        df3 = as.data.frame(cbind(c("Classe", "Nombre d'observations", "Nombre de données manquantes", "Nombre de valeurs distinctes", "Minimum", "Maximum", "Mediane", "Moyenne", "Intervalle de confiance 95%", "Coefficient de variation"), c(classe, observations, manquantes, distinctes, minimum, maximum, mediane, moyenne, ic, cv)), ncol = 10)
    return(df3)
    }

    else if (class(variable) == "character") {
        classe = class(variable)
        variable = as.numeric(variable)
        variable_mod = retirer_na(variable)
        manquantes = length(variable) - length(variable_mod)
        observations = length(variable_mod)
        distinctes = length(unique(variable_mod))
        moyenne = round(mean(variable_mod, na.rm = TRUE))
        minimum = round(min(variable_mod, na.rm = TRUE))
        maximum = round(max(variable_mod, na.rm = TRUE))
        mediane = round(median(variable_mod, na.rm = TRUE))

        df4 = as.data.frame(cbind(c("Classe", "Nombre d'observations", "Nombre de données manquantes", "Nombre de valeurs distinctes", "Moyenne", "Minimum", "Maximum", "Mediane"), c(classe, observations, manquantes, distinctes, moyenne, minimum, maximum, mediane)), ncol = 8)
    return(df4)
    }
}


# Retirer les NA et les -8 d'une variable
retirer_na = function(variable) {
    variable_na = variable[!is.na(variable)]
    variable_na_huit = variable_na[!(variable_na == -8)]
    #return(variable_na_huit)
    return(variable_na)
}

# Fonction pour afficher la matrice de corrélation
correlation = function(df) {
    df_comb = data.frame(t(combn(sort(names(df)), 2)), stringsAsFactors = FALSE)
    df_res = map2_df(df_comb$X1, df_comb$X2, cramerv)

    df_res %>%
    ggplot(aes(x,y,fill=chisq_pval))+
    #geom_text(aes(x,y,label=cramV))+ Permet d'afficher les valeurs sur chaque cases
    geom_tile()+
    scale_fill_gradient(low="red", high="yellow")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}


# Donne le nombre de lignes qui ont des NA ou des -8
na_liste = function(dataframe) {
    compteur = 0
    for (i in 1:dim(df)[1]){
        if (sum(is.na(df[i,])) > 0) {
            compteur = compteur + 1
        }
    }
    res = cat("Le nombre de lignes possedant des NA ou des -8 est de :", compteur, "ce qui représente :", 100*(compteur)/dim(df)[1], "%")
    return(res)
}

# Génération du jeu de données final
jeudedonnes = function(){
    vars_non = c("unempdur", "income", "mmarr", "ymarr", "msepdiv", "ysepdiv", "nociga", "wkabdur", "wkabintdur", "emcc", "workab")
    jd = SD2011[, !names(SD2011) %in% vars_non, drop = F]
    df = na.omit(jd)
    df = df[-(which(df[, 16] == -8)), ]
    return(df)
}

# Calcule le nombre de modalités dans le dataframe
nb_modalites = function(dataframe){
    compteur = 0
    for (i in 1:length(dataframe)){
       compteur = compteur + dim(unique(df[names(df)[i]]))[1]
    }
    return(compteur)
}

# Affiche la classe des variables du dataframe
affiche_classe = function(dataframe){
    cat("Les classes des variables :","\n")
    for (j in 1:length(names(dataframe))) {
        cat(names(dataframe)[j], ":", class(dataframe[, names(dataframe)[j]]),"\n")
    }
}

# Calcul du CIO (variables numériques)
CIO = function(df_org, df_syn) {
    liste = rep(0, length(df_org))
    for (i in 1:length(df_org)) {
        moy_org = mean(df_org[, names(df_org)[i]])
        moy_syn = mean(df_syn[, names(df_syn)[i]])

        sd_org = sd(df_org[, names(df_org)[i]])
        sd_syn = sd(df_syn[, names(df_syn)[i]])
        
        sqrt_org = sqrt(length(df_org[, names(df_org)[i]]))
        sqrt_syn = sqrt(length(df_syn[, names(df_syn)[i]]))

        t = qt(0.975, df = length(df_org[, names(df_org)[i]]) - 1)

        lo = moy_org - t * sd_org / sqrt_org
        uo = moy_org + t * sd_org / sqrt_org
        ls = moy_syn - t * sd_syn / sqrt_syn
        us = moy_syn + t * sd_syn / sqrt_syn
        
        liste[i] = 0.5 * (((min(uo, us) - max(lo, ls))/(uo - lo))+((min(uo, us) - max(lo, ls))/(us - ls)))
    }
    return(mean(liste))
}


IC = function(variable) {
    lb = quantile(variable, probs = 0.025)
    ub = quantile(variable, probs = 0.975)
    intervalle = c(lb, ub)

    return(intervalle)
}

percent = function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

ACP_info = function(df, variable) {
    pc = prcomp(df, center = TRUE, scale = TRUE)
    pc$rotation = -1 * pc$rotation # Les vecteurs propres pointent dans le sens négatif
    pc$x = -1 * pc$x
    resu = summary(pc)

    biplot = biplot(pc, scale = 0)
    
    values = summary(pc)$importance[2, ][1:length(df)]
    barplot = barplot(values, ylim = c(0, 1))
    text(barplot, values + 0.02, percent(values), cex = 1)

    cumsum = plot(cumsum(summary(pc)$importance[2, ]))

    screeplot = screeplot(pc, type = "l", npcs = length(df), main = "Screeplot")

    var_exp = pc$sdev^2 / sum(pc$sdev^2)

    q = qplot(c(1:length(df)), var_exp) + 
        geom_line() + 
        xlab("Composantes principales") + 
        ylab("Variance expliquée") +
        ggtitle("Scree Plot") +
        ylim(0, 1)


    pp = pairs.panels(df, gap = 0, bg = c("green", "blue")[variable], pch = 21)

    g = ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = variable,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68) +
        theme(legend.direction = 'horizontal', legend.position = 'top')
    
    

    return(list(resu, biplot, barplot, cumsum, screeplot, q, pp, g))
}

moycum_graph = function(matrice, i, nom, nbsim) {
    cum_avg = cumsum(matrice[i, ]) / seq_along(matrice[i, ])
    png(filename = paste0("Moyenne_cumulee_", dimnames(matrice)[[1]][i], "_", nom, ".png"), width = 1080, height = 1080)
    plot(1:nbsim, cum_avg, type = "l")
    lines(1:nbsim, rep(mean(matrice[i, ]), nbsim), type = "l", col = "red")
    dev.off()
}

graph_propension = function(df, synd, xvar_ind, yvar_ind) {
    n1 = dim(df)[1]
    n2 = dim(synd)[1]
    N = n1 + n2
    cc = n2 / N
    maxit = 200

    df.prop = rbind(synd, df)
    df.prop = data.frame(df.prop, t = c(rep(1, n2), rep(0, n1)))

    logit.int = as.formula(paste("t ~ ."))
    fit = suppressWarnings(glm(logit.int, data = df.prop, family = "binomial", control = list(maxit = maxit)))
    score = predict(fit, type = "response")

    ggplot(df, aes(x = names(df)[xvar_ind], y = names(df)[yvar_ind], color = score)) +
        geom_point() +
        scale_colour_gradient(low = "red", high = "green")
}

plot_vars_var = function(df, xvar, yvar, var) {
    png(filename = paste0("Plot", yvar, xvar, var, ".png"), width = 1080, height = 1080)
    g = ggplot(df, aes(x = df[, xvar], y = df[, yvar], color = df[, var])) +
        geom_point() +
        labs(title = paste0(yvar, " en fonction de ", xvar, "coloré par ", var), x = xvar, y = yvar, color = var) +
        theme(
            legend.title = element_text(colour = "black", size = 20),
            legend.text = element_text(colour = "black", size = 20),
            legend.key.size = unit(2, "cm"),
            plot.title = element_text(color = "black", size = 25, hjust = 0.5),
            axis.title.x = element_text(color = "black", size = 25, hjust = 0.5),
            axis.title.y = element_text(color = "black", size = 25, hjust = 0.5))
    dev.off()
}

transparent_theme = theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA))