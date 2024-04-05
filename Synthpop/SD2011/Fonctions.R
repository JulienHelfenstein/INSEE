# Fonction pour avoir la p-valeur d'une chi2 et d'un V de Cramer
f = function(x,y) {
    tbl = df %>% select(x,y) %>% table()
    chisq_pval = round(chisq.test(tbl)$p.value, 4)
    cramV = round(cramersV(tbl), 4) 
    data.frame(x, y, chisq_pval, cramV) }

# Mode statistique
find_mode = function(x) {
    u = unique(x)
    tab = tabulate(match(x, u))
    u[tab == max(tab, na.rm = TRUE)]
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
    variable_na_huit = variable_na[!(variable_na == -8)]
    return(variable_na_huit)
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
