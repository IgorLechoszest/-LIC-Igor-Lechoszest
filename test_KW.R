df <- read.csv("wyniki.csv")

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
