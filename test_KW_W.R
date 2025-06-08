df <- read.csv("wyniki/wyniki.csv")

### Test Kruskala-Wallisa
## Test ten jest nieparametryczną alternatywą jednoczynnikowej analizy wariancji. 
## Za pomocą tego testu porównujemy rozkłady kilku (k) zmiennych.

# H0:F1=⋯=Fk (wszystkie próby pochodzą z jednej populacji)
# H1:∃i,j∈{1,…,k} Fi≠Fj (nie wszystkie próby pochodzą z tych samych populacji).

output <- lapply(unique(df$Dataset), function(x){
  kruskal.test(mean_test_score~Strategia, 
               df[df$Dataset == x,])
  
  statistic <- kruskal.test(mean_test_score~Strategia, 
                            df[df$Dataset == x,])$statistic[[1]]
  
  p.value <- kruskal.test(mean_test_score~Strategia, 
                          df[df$Dataset == x,])$p.value[[1]]
  
  list(x, c(statistic, p.value))
  })

new <- data.frame()
for(i in 1:length(output)){
  new <- rbind(new, unlist(output[[i]]))
}

names(new) <- c("Zbiór danych", "Statystyka Testowa", "P-wartość")    
new$`Statystyka Testowa` <- as.numeric(as.character(new$`Statystyka Testowa`))
new$`P-wartość` <- as.numeric(as.character(new$`P-wartość`))

alfa <- 0.05

new$Decyzja <- ifelse(new$`P-wartość` < alfa, "Odrzucamy H", "Nie ma podstaw do odrzucenia H")

library(xtable)
xtable(new)


df <- read.csv("wyniki.csv")

## Test Wilcoxona

alfa <- 0.05
jaki <- c()
jakim <- c()
ind <- 1
jnd <- 1
knd <- 1
pval <- c()
ind_pval <- c()
dataset_name <- c()


for (dane in unique(df$Dataset)) {
  
  strategie <- unique(df$Strategia[df$Dataset == dane])
  
  for (i in 1:length(strategie)) {
    for (j in 1:length(strategie)) {
      if (i < j) {
        jaki[ind] <- strategie[i]
        jakim[ind] <- strategie[j]
        ind_pval[knd] <- wilcox.test(df[df$Dataset == dane & df$Strategia == strategie[i], c("mean_test_score")], 
                                     df[df$Dataset == dane & df$Strategia == strategie[j], c("mean_test_score")])$p.value[[1]]
        ind <- ind + 1
        jnd <- jnd + 1
        knd <- knd + 1
        dataset_name <- c(dataset_name, dane)
      }
    }
  }
}
results <- data.frame(jaki, jakim, ind_pval, dataset_name)
results$Decyzja <- ifelse(results$ind_pval < alfa/choose(35,2), "Odrzucamy H", "Nie ma podstaw do odrzucenia H")
names(results) <- c("Strategia 1", "Strategia 2", "p.value", "zbiór", "Decyzja")

results$Decyzja[is.na(results$p.value)] <- "Nie ma podstaw do odrzucenia H"

table(results$zbiór, results$Decyzja)

library(xtable)
xtable(table(results$zbiór, results$Decyzja))

