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
        modalites = length(levels(variable))
        manquantes = sum(is.na(variable))
        observations = length(variable) - manquantes
        
        mat_eff = matrix(0, nrow = 3, ncol = modalites)
        for (i in 1:modalites) {
            mat_eff[1,i] = levels(variable)[i]
            mat_eff[2,i] = length(which(variable == levels(variable)[i]))
            mat_eff[3,i] = 100*length(which(variable == levels(variable)[i])) / observations
        }
        plus_freq = mat_eff[1, which(mat_eff[2, ] == max(as.numeric(mat_eff[2, ])))]
        moins_freq = mat_eff[1, which(mat_eff[2, ] == min(as.numeric(mat_eff[2, ])))]
        
        df1 = as.data.frame(cbind(c("Nombre d'observations", "Nombre de données manquantes", "Nombre de modalités distinctes", "Modalité la moins fréquente", "Modalité la plus fréquente"),c(observations, manquantes, modalites, moins_freq, plus_freq)), ncol = 2)
        df2 = as.data.frame(cbind(c("Modalités","Effectifs", "Pourcentage"), mat_eff), nrow = 3, ncol = modalites)
    return(list(df1,df2))
    }

    else if (class(variable) == "numeric") {
        manquantes = sum(is.na(variable))
        observations = length(variable) - manquantes
        distinctes = length(unique(variable))
        moyenne = mean(variable, na.rm = TRUE)
        minimum = min(variable, na.rm = TRUE)
        maximum = max(variable, na.rm = TRUE)
        mediane = median(variable, na.rm = TRUE)

        df3 = as.data.frame(cbind(c("Nombre d'observations", "Nombre de données manquantes", "Nombre de valeurs distinctes", "Moyenne", "Minimum", "Maximum", "Mediane"), c(observations, manquantes, distinctes, moyenne, minimum, maximum, mediane)), ncol = 7)
    return(df3)
    }

    else if (class(variable) == "character") {
        manquantes = sum(is.na(variable))
        observations = length(variable) - manquantes
        distinctes = length(unique(variable))
        moyenne = mean(variable, na.rm = TRUE)
        minimum = min(variable, na.rm = TRUE)
        maximum = max(variable, na.rm = TRUE)
        mediane = median(variable, na.rm = TRUE)

        df3 = as.data.frame(cbind(c("Nombre d'observations", "Nombre de données manquantes", "Nombre de valeurs distinctes", "Moyenne", "Minimum", "Maximum", "Mediane"), c(observations, manquantes, distinctes, moyenne, minimum, maximum, mediane)), ncol = 7)
    return(df3)
    }
}


# Retirer les NA d'une variable
retirer_na = function(variable) {
    variable_mod = variable[!is.na(variable)]
    return(variable_mod)
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
