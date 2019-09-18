####open tables in .txt
###several tables were used because i don't know how to use the filter function
openareas <- read.table("C:/Users/Usuário/Desktop/dados/aberto.txt", header =TRUE)

closedareas <- read.table("C:/Users/Usuário/Desktop/dados/fechado.txt", header =TRUE)

allareas <- read.table("C:/Users/Usuário/Desktop/dados/total.txt", header =TRUE)

predationtotal <- read.table("C:/Users/Usuário/Desktop/dados/preddata.txt", header =TRUE)

predators <- read.table("C:/Users/Usuário/Desktop/dados/definicao.txt", header =TRUE)


####installing packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("car")
install.packages("broom")
install.packages("rcompanion")

####fitting glm models
###it includes the plot and the summary of the results
###openareas
fit1 = glm(fate ~ hcm, data=openareas, family=binomial)
newdat <- data.frame(hcm=seq(min(openareas$hcm), max(openareas$hcm),len=100))
newdat$fate = predict(fit1, newdata=newdat, type="response")
plot(fate~hcm, data=openareas, col="black")
lines(fate ~ hcm, newdat, col="black", lwd=3)

summary(fit1)

nagelkerke(fit1)

###closedareas
fit2 = glm(fate ~ hcm, data=closedareas, family=binomial)
newdat <- data.frame(hcm=seq(min(closedareas$hcm), max(closedareas$hcm),len=100))
newdat$fate = predict(fit2, newdata=newdat, type="response")
plot(fate~hcm, data=closedareas, col="black")
lines(fate ~ hcm, newdat, col="black", lwd=3)

summary(fit2)

nagelkerke(fit2)

###allareas
fit3 = glm(fate ~ hcm, data=allareas, family=binomial)
newdat <- data.frame(hcm=seq(min(allareas$hcm), max(allareas$hcm),len=100))
newdat$fate = predict(fit3, newdata=newdat, type="response")
plot(fate~hcm, data=allareas, col="black")
lines(fate ~ hcm, newdat, col="black", lwd=3)

summary(fit3)

nagelkerke(fit3)

###Plotting boxplots and t-tests
###what kind of predator have attacked in different heights 
ggplot(predators) +
  aes(x = predador, y =hcm   ) +
  geom_boxplot(size = 1, color = c( "black", "grey29")) +
  geom_jitter(aes(color = predador), width = .3, size = 2, alpha = .5) + 
  scale_color_manual(name = "Identificação", labels = c("Invertebrados", "Vertebrados"), values = c("black", "dark gray")) +
  labs(x = "Predador",
       y = "Altura da larva") +
  annotate("text", label = "P-value = 0,02", size = 3, x = 2.35, y = 1.2) +
  scale_x_discrete(labels=c("Invertebrados", "Vertebrados")) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14))

t.test(hcm~ predador, data=predators)  

###predation in different areas 
ggplot(predationtotal) +
  aes(x = Tratamento, y =Nmrpred   ) +
  geom_boxplot(size = 1, color = c( "black", "grey29")) +
  geom_jitter(aes(color = Tratamento), width = .3, size = 2, alpha = .5) + 
  scale_color_manual(name = "Tratamentos", labels = c("Aberta", "Fechado"), values = c("black", "dark gray")) +
  labs(x = "Tratamentos",
       y = "Número total de predações") +
  annotate("text", label = "P-value = 0,1", size = 3, x = 2.39, y = -0.09) +
  scale_x_discrete(labels=c("Aberta", "Fechada")) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14))

t.test(Nmrpred~ Tratamento, data=predationtotal)  

###chisquare
chisq.test(pred$predador, pred$tratamento)

