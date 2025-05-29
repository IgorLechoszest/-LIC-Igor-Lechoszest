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



## Test Wilcoxona 
jaki <- c()
jakim <- c()
ind <- 1
jnd <- 1
knd <- 1
pval <- c()
ind_pval <- c()


for (i in unique(df$Strategia[df$Dataset == "phoneme"])){
  for(j in unique(df$Strategia[df$Dataset == "phoneme"])){
    if (i != j){
      jaki[ind] <- i
      jakim[ind] <- j
      ind_pval[knd] <- wilcox.test(df[df$Dataset == "phoneme" & df$Strategia == i,c("mean_test_score")],
                  df[df$Dataset == "phoneme" & df$Strategia == j,c("mean_test_score")])$p.value[[1]]
      ind <- ind + 1
      jnd <- jnd + 1
      knd <- knd + 1
    }
  }
}

results <- data.frame(jaki, jakim, ind_pval)
results$Decyzja <- ifelse(results$ind_pval < alfa, "Odrzucamy H", "Nie ma podstaw do odrzucenia H")
names(results) <- c("Strategia 1", "Strategia 2", "p.value", "Decyzja")

