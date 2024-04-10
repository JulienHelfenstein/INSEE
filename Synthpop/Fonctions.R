library(dplyr)

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
        moyenne = round(mean(variable_mod, na.rm = TRUE))
        minimum = round(min(variable_mod, na.rm = TRUE))
        maximum = round(max(variable_mod, na.rm = TRUE))
        mediane = round(median(variable_mod, na.rm = TRUE))

        df3 = as.data.frame(cbind(c("Classe", "Nombre d'observations", "Nombre de données manquantes", "Nombre de valeurs distinctes", "Moyenne", "Minimum", "Maximum", "Mediane"), c(classe, observations, manquantes, distinctes, moyenne, minimum, maximum, mediane)), ncol = 8)
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
    #variable_na_huit = variable_na[!(variable_na == -8)]
    #return(variable_na_huit)
    return(variable_na)
}

# Fonction pour afficher la matrice de corrélation
correlation = function(df) {
    df_comb = data.frame(t(combn(sort(names(df)), 2)), stringsAsFactors = FALSE)
    df_res = map2_df(df_comb$X1, df_comb$X2, f)

    df_res %>%
    ggplot(aes(x,y,fill=chisq_pval))+
    #geom_text(aes(x,y,label=cramV))+ Permet d'afficher les valeurs sur chaque cases
    geom_tile()+
    scale_fill_gradient(low="red", high="yellow")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

# Calcul du k-anonymat
k_anonymat = function(dataframe, vars) {
    liste = matrix(0, nrow = 1, ncol = length(vars))
    for (i in 1:length(vars)) {
        modalites = unique(vars[i])
        mode = find_mode(dataframe[vars[i]])
        nombre = sum(retirer_na(dataframe[vars[i]] == find_mode(dataframe[vars[i]])))
        liste[i] = nombre
    }
    kano = min(liste)
    return(kano)
}

# Calcul de la l-diversité
l_diversite = function(dataframe, vars, sensis) {
    liste = matrix(0, nrow = 1, ncol = length(vars))
    ldiv = min(liste)
    return(ldiv)
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