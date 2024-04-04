# Fonction pour avoir la p-valeur d'une chi2 et d'un V de Cramer
f = function(x,y) {
    tbl = df %>% select(x,y) %>% table()
    chisq_pval = round(chisq.test(tbl)$p.value, 4)
    cramV = round(cramersV(tbl), 4) 
    data.frame(x, y, chisq_pval, cramV) }

# Mode statistique
find_mode <- function(x) {
    u <- unique(x)
    tab <- tabulate(match(x, u))
    u[tab == max(tab)]
  }

# Fonction pour obtenir des informations sur les variables
# Mettre variable sous la forme : dataframe$variable
info = function(variable){
    if (class(variable) == "factor"){
        modalites = length(levels(variable))
        manquantes = sum(is.na(variable))
        observations = length(variable) - manquantes
        
        mat_eff = matrix(0, nrow = 3, ncol = modalites)
        for (i in 1:modalites){
            mat_eff[1,i] = levels(variable)[i]
            mat_eff[2,i] = length(which(variable == levels(variable)[i]))
            mat_eff[3,i] = 100*length(which(variable == levels(variable)[i]))/observations
        }
        plus_freq = mat_eff[1,which(mat_eff[2,] == min(as.numeric(mat_eff[2,])))]
        moins_freq = mat_eff[1,which(mat_eff[2,] == max(as.numeric(mat_eff[2,])))]
        
        df1 = as.data.frame(c(observations, manquantes, modalites, moins_freq, plus_freq), ncol = 5, row.names=c("Nombre d'observations","Nombre de données manquantes","Nombre de modalités distinctes","Modalité la moins fréquente","Modalité la plus fréquente"))
        df2 = as.data.frame(mat_eff, nrow = 3, ncol = modalites)
    return(c(df1,df2))
    }

    else if (class(variable) == "numeric"){
        manquantes = sum(is.na(variable))
        observations = length(variable) - manquantes
        distinctes = length(unique(variable))
        moyenne = mean(variable)
        minimum = min(variable)
        maximum = max(variable)
        mediane = median(variable)
        mode = find_mode(variable)

        df3 = as.data.frame(c(observations, manquantes, distinctes, moyenne, minimum, maximum, mediane, mode), row.names = c("Nombre d'observations","Nombre de données manquantes","Nombre de valeurs distinctes","Moyenne","Minimum","Maximum","Mediane","Mode"))
    return(df3)
    }
}


# Fonction pour afficher les graphiques des variables
graph = function(variable){

}
