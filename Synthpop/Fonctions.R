library(dplyr)
library(purrr)
library(ggplot2)

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


IC = function(variable, alpha) {
    moy = mean(variable)
    std = sd(variable)
    n = length(variable)
    t = qt(1 - alpha / 2, df = n - 1)

    lb = moy - t * std / sqrt(n)
    ub = moy + t * std / sqrt(n)
    intervalle = c(lb, ub)

    return(intervalle)
}

percent = function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
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
