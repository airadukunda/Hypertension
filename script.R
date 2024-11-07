Frequire(foreign)
data<-read.spss(file.choose())

attach(data)
names(data)
summary( Age)
View(data)
data[.7]
prop.test(x=c(54,66),n=c(300,414))



names(data)
par(mar=c(4.5,4.5,1,1))
t<-table(HTA_A,RisqueMCV)
t<-round(prop.table(t,2)*100,1)
print(t)
colnames(t)<-c("Risque élevé[≥20%]","Risque faible[0-10%[","Risque modéré[10-19%]")
b<-barplot(t,beside=T,col=c("green","red"),ylab="Pourcentage",xlab="Score de Framingham ",ylim=c(0,110))
legend(0.80,100,cex=0.9,bg="lightyellow",pch=15,col=c("red","green"),legend=c("Oui","Non"),title="Hypertension")
text(b,t+4,labels=t)



t<-table(RisqueMCV)
print(t)
##
x <-c( 57,535,122 )
labels<-c("Risque élevé[≥20%]","Risque faible[0-10%[","Risque modéré[10-19%]")

piepercent<-round(100*x/sum(x),1)

# Give the chart file a name.

# Plot the chart.
pie(x, labels = piepercent, main = "City pie chart",col = rainbow(length(x)))
legend("topright", c("London","New York","Singapore","Mumbai"), cex = 0.8,
   fill = rainbow(length(x)))

# Save the file.
dev.off()



















#Regressions
names(data)
attach(data)
table(HTA_A)
mod2<-glm(as.factor(HTA_A)~as.factor(AgeCat1),data=data,family=binomial)
summary(mod2)
library(questionr)
odds.ratio(mod2)
forest_model(mod2)


mod2<-glm(as.factor(HTA_A)~as.factor(Alcol1),data=data,family=binomial)
summary(mod2)
library(questionr)
odds.ratio(mod2)
forest_model(mod2)

mod2<-glm(as.factor(HTA_A)~as.factor(atcdfhta1),data=data,family=binomial)
summary(mod2)
library(questionr)
odds.ratio(mod2)
forest_model(mod2)




mod2<-glm(as.factor(HTA_A)~as.factor(Diabeteconnu),data=data,family=binomial)
summary(mod2)
library(questionr)
odds.ratio(mod2)
forest_model(mod2)


mod2<-glm(as.factor(HTA_A)~as.factor(IMC1),data=data,family=binomial)
summary(mod2)
library(questionr)
odds.ratio(mod2)
forest_model(mod2)


mod2<-glm(as.factor(HTA_A)~as.factor(irc1),data=data,family=binomial)
summary(mod2)
library(questionr)
odds.ratio(mod2)
forest_model(mod2)



mod2<-glm(as.factor(HTA_A)~as.factor(Niveaudeducation1),data=data,family=binomial)
summary(mod2)
library(questionr)
odds.ratio(mod2)
forest_model(mod2)



mod2<-glm(as.factor(HTA_A)~as.factor(Residence1),data=data,family=binomial)
summary(mod2)
library(questionr)
odds.ratio(mod2)
forest_model(mod2)


mod2<-glm(as.factor(HTA_A)~as.factor(Sexe1),data=data,family=binomial)
summary(mod2)
library(questionr)
odds.ratio(mod2)
forest_model(mod2)


mod2<-glm(as.factor(HTA_A)~as.factor(Statutmatrimonial),data=data,family=binomial)
summary(mod2)
library(questionr)
odds.ratio(mod2)
forest_model(mod2)

mod2<-glm(as.factor(HTA_A)~as.factor(Tabac1),data=data,family=binomial)
summary(mod2)
library(questionr)
odds.ratio(mod2)
forest_model(mod2)


 









#
AgeCat1
 antcdfamiliauxdehta
 Diabeteconnu
 IMC1
 irc1
 Niveaudeducation1
 Tabac1

mod2<-glm(as.factor(HTA_A)~as.factor(Niveaudeducation1)
+as.factor(Tabac1)
+as.factor(AgeCat1)
+as.factor(IMC1)
+as.factor(irc1)
+as.factor(Diabeteconnu)
+as.factor(atcdfhta1),data=data,family=binomial)

names(data)

summary(mod2)
library(questionr)
library(forestmodel)
round(odds.ratio(mod2),2)
forest_model(mod2)
step(mod2)

 modelfinal<-glm(formula = as.factor(HTA_A) ~ as.factor(Niveaudeducation1) + 
    as.factor(Tabac1) + as.factor(AgeCat1) + as.factor(IMC1) + 
    as.factor(Diabeteconnu) + as.factor(atcdfhta1), family = binomial, 
    data = data)
library(questionr)
odds.ratio(modelfinal)
round(odds.ratio(modelfinal),2)
forest_model(modelfinal)

plot(modelfinal,xlab="Quantiles théoriques",ylab="résidus standardisées",which=2)
plot(modelfinal,pch=18,wich=c(4))

library(ResourceSelection)
hoslem.test(as.factor(HTA_A), fitted(modelfinal),g=10)
si p-valeur > 0.05, on admet que le modèle est bien adapté aux données.
##3.
X2 <- sum(residuals(modelfinal, type = "pearson")^2) 
ddl <- df.residual(modelfinal)
1 - pchisq(X2, ddl) 

















data2<-cbind(HTA_A,Niveaudeducation1,Tabac1, AgeCat1,IMC1, Diabeteconnu, atcdfhta1)
data3<-data.frame(data2)



Courbe ROC
library(pROC)
library(ROCR)
pr1<-predict(modelfinal,data3,typ="response")
pred1<-prediction(pr1,HTA_A)
evaluation1<-performance(pred1,"acc")
par(mar=c(4.5,4.5,1,1))
p1<-plot(evaluation1,col="darkblue",main="",xlab="1-Specificité",ylab="Sensibilité")
AUC
##Score
 S1 <- predict(modelfinal,newdata=data3,type="response")
library(ROCR)
roc(HTA_A,S1,plot=TRUE,col="red")

#########################Cross validation
library(tree)

library(rpart)


data3<-data.frame(data2)

View(data3)

.rpart<-rpart(HTA_A~., method="class", data=data3)

print(TB.rpart)
printcp(TB.rpart)
names(data3)
colnames(data3)<-c("Hypertension","Niveaudeducation","Tabac","Age","IMC","Diabeteconnu","atcdfHTA")
tree <- rpart(Hypertension ~., data = data3, method = "class") 


#Admission_YN: Dependent Attribute. As admission depends on the factors score, rank of college, etc.
#rpart() returns a Decision tree created for the data
#If you plot this tree, you can see that it is not visible, due to the limitations of the plot window in the R console

plot(tree)
text(tree, pretty = 0)

#To unhance it, let us take some help from rattle:
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(tree, tweak = 1.6)
library(car)
library(base)
library(DescTools)
library(ResourceSelection)

#Distance de Cook
plot(modelfinal, which = 4, id.n = 5)

inf.id.1 = which(cooks.distance(modelfinal)>0.25)
inf.id.1



ols_plot_cooksd_bar(modelfinal)

library(tidyverse)

tibble_ex <- data3
attach(tibble_ex)

names(tibble_ex)
model <- glm(Hypertension ~., family = binomial, data = tibble_ex) 

tibble_ex <- data2 %>%
  mutate(pred = predict(model, type = 'response'))

tibble_ex %>%
  arrange(pred) %>%
  ggplot(aes(x = pred, y = Patient)) +
  stat_smooth(method = 'glm', method.args = list(family = binomial), se = F) +
  geom_abline()






############Validation

##Calcul de lerreur par validation croisée
##Taille de léchantillon.
n<-nrow(data3)
dim(data3)
n<-714
K<-7
###Tirer dune permutation {1,,n}
 alea=runif(n)
rang=rank(alea)
rang[1:7]
 ##Taille de chaque échantillon
taille<-n%/%K
##Composotion des bloques.

block=(rang-1)%/%taille+1
table(block)
block[block==8]=7
block=as.factor(block)
##Calcul de lerreur


arbre=rpart(Hypertension~.,data=data3[block!=K,],method="class")

err.cv=numeric(0)
all.err<-numeric(0)


k<-1:7
 for(k in 1:K)
{
 arbre=rpart(Hypertension~.,data=data3[block!=k,],method="class")
 pred=predict(object = arbre,newdata = data3[block==k,],type="class")
 mc<-table(data2$Hypertension[block==k],pred)
err=1-((mc[1,1]+mc[2,2])/sum(mc))
all.err<-rbind(all.err,err)
}
print(all.err)
err.cv<-mean(all.err)
print(err.cv)














library(car)
vif(modelfinal)
 ##Points leviers
p <- length(modelfinal$coef) 
n <- 714
par(mar=c(4.5,4.5,1,1))
plot(influence(modelfinal)$hat,type="h",ylab="hii") 
abline(h=c(3*p/n),col ="red")





plot(modelfinal,pch=18,wich=c(1),4)
plot(cooks.distance(modelfinal,pch=18,wich=c(5)),type="h",ylab="Distance de Cook",main="Verification des points influents")




############################################################

#### cqlcul des residus
par(mar=c(4.5,4.5,1,1))
 plot(rstudent(modelfinal),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2),col=2)
##Graphique prédiction linéaire/résidus
par(mar=c(4.5,4.5,1,1))
plot(predict(modelfinal),rstudent(mod.final),type="p",cex=0.5,xlab="prévision linéaire",
ylab="Résidus studentisés par VC")



###############################

####Courbe ROC
library(pROC)
library(ROCR)
pr1<-predict(modelfinal,data,typ="response")
pred1<-prediction(pr1,HTA)
evaluation1<-performance(pred1,"acc")
par(mar=c(4.5,4.5,1,1))
p1<-plot(evaluation1,col="darkblue",main="",xlab="1-Specificité",ylab="Sensibilité")
AUC
##Score
 S1 <- predict(modelfinal,newdata=data,type="response")
library(ROCR)
roc(data3$Hypertension ,S1,plot=TRUE,col="green",cex=1)
text(0.5,0.8,text(0.4,0.75,labels="AUC=0.8822",col="purple",cex=1))

##Arbre de prediction.
library(rpart)
arbre <- rpart(modelfinal,data=data)

 
plot(arbre) 
t<- text(arbre,pretty=2)
##Score
 S1 <- predict(mod,newdata=data,type="response")
 S2 <- predict(modelfinal,newdata=data,type="response")
 s3 <- predict(mod.final.int,newdata=data)
##Courbe ROC(a retenir)
library(ROCR)
attach(data)
r<-roc( HTA_A,S2,plot=T,col=2,percent=TRUE,ci=TRUE,quiet = TRUE,smooth = TRUE)
r

grid(lwd=1)##Quadrillage
 text(0.23*100,0.02*100,labels="AUC: 88.16%[95% CI: 85.17%-91.03%] ",col="red",cex=1)


res.stud <- rstudent(modelfinal) # Calcul des résidus
round(res.stud ,2)
# studentisés.
seuil.stud <- qt(0.975,714-6-2) # Calcul du seuil par la
# loi de Student.
cond <- res.stud<(-seuil.stud) | res.stud > seuil.stud
 # Liste des individus susceptibles dêtre considérés comme
# aberrants.

















Additional 
#https://larmarange.github.io/analyse-R/regression-logistique.html 
##Rename of the model
library(GGally)
ggcoef_model(modelfinal, exponentiate = TRUE)

###
Identifier les variables ayant un effet significatif
Les p-values associées aux odds ratios nous indique si un odd ratio est significativement différent de 1, par rapport à la modalité de référence. Mais cela n’indique pas si globalement une variable a un effet significatif sur le modèle. Pour tester l’effet global sur un modèle, on peut avoir recours à la fonction drop1. Cette dernière va tour à tour supprimer chaque variable du modèle et réaliser une analyse de variance (ANOVA, voir fonction anova) pour voir si la variance change significativement.

drop1(modelfinal, test = "Chisq")

Un critère similaire à l’AIC est le critère BIC (Bayesian Information Criterion) appelé aussi SBC (Schwarz information criterion). Il s’obtient avec step an ajoutant l’argument k = log(n) où n est le nombre d’observations inclues dans le modèle que l’on peut obtenir avec nrow(model.matrix(reg)) (pour tenir compte des éventuelles observations manquantes retirées des données pour le calcul du modèle).

reg2_bic <- step(modelfinal, k = log(nrow(model.matrix(modelfinal))))



####Charger les packages requis
library("ggplot2")  # Visualisation des données
library("dplyr")    # Manipulation des donnéesnames(data)
##Préparation des données
##1.Créer un jeu de données
names(data)
attach(data)
 HTA, HTA_A
t<-table(RisqueMCV)
 print(t)
round(prop.table(t)*100,1)
count.data <-data.frame(
            
     RisqueMCV= c( 594,120 ),
           prop = c( 83.2, 16.8 )
             )
count.data
N.B Le nom class peut etre remplacer par le nom de la variable.Une fois faite,remplacer le dans la formule des pie dunot chart
##2.Calculez la position des étiquettes de texte comme étant la somme cumulée de la proportion:
##2.1.Trier la variable de regroupement (class) par ordre décroissant. Ceci est important pour calculer les coordonnées y des étiquettes.
##2.2.Pour placer les étiquettes au centre des tranches, nous utiliserons cumsum(prop) - 0.5*prop comme position des étiquettes.

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(HTA_A)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
count.data
View(data)

##Donut chart
##Le donut chart nest quun simple diagramme circulaire avec un trou à lintérieur.
mycols<-rainbow(2)
##La seule différence entre le code du camembert est que nous avons défini : x = 2 and xlim = c(0.5, 2.5) pour créer le trou à lintérieur du camembert. De plus, largument width dans la fonction geom_bar() nest plus nécessaire.
par(mar=c(4.5,4.5,1,1))
ggplot(count.data, aes(x = 2, y = prop, fill = HTA)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "black")+
  scale_fill_manual(values = mycols) +
  theme_void()+xlim(0.5, 2.5)




p<-prop.test(120,714, p = NULL,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)
p$conf.int*100


##############################################################################



##############################################################################


library(shinydashboard)
library(shiny)
library(rsconnect)
library(openxlsx)
library(RJSONIO)
library(pROC)
library(shinyWidgets)
### chargement de donnees #Dataset4.3
library(foreign)
data<-read.spss(file.choose())
attach(data)
names(data)
#data <- read.csv ("datapp.csv")
data2<-cbind(HTA_A,Niveaudeducation1,Tabac1, AgeCat1,IMC1, Diabeteconnu, atcdfhta1)
data3<-data.frame(data2)
colnames(data3)<-c("HTA","Niveduca","Tabac1","AgeCat1","IMC1","Diabete_connu","atcdf_hta1")
View(data3)

### I. partie Interface d'un utilisateur ligne 15 a 117 donc tous les arguments de dashboardpage


ui <- dashboardPage(

  ### l'entete de l'aplication avec la fonction dashboardHeader        
    dashboardHeader(title = "HIGH BLOOD PRESSURE RISK APPLICATION", titleWidth = 400),
  
  ### la partie de gauche ou on met les menus: chaque menu a son contenu pour le     
  ### cas on a cree 3 menus l'une pour l'application ,l'autre pour la dscription 
  ### et enfin une menu pour la performance de l'application 
  
  ### la fonction pour creer les menus c'est sidebarMenu chaque menu cree est geree par
  ### la fonction menuItem dont le premier argument est l'etiquette du menu et l'id de ce menu 
    dashboardSidebar(
       sidebarMenu(
          menuItem("HBP RISK APP",tabName = "APP"),
          menuItem("DESCRIPTION DE L'APPLICATION",tabName = "DESCR"),
          menuItem("APP PERFORMANCE", tabName = "PERF")
            
        )),
    
  ### creation de ce qui apparaittra quand on ouvre une menu qu'on a cree ci-haut
 ### la  fonction setBackgroundImage permet de prendre une image comme l'arriere plan 
 ### de l'application 
 ### ce qui lie le contenu d'un menu(cad menuItem et tabItem  )c'est l'argument 
 ###  tabName
    dashboardBody(
      
      setBackgroundImage(src = "C:/Users/HP/Documents/TRAVAUX/Arnaud2.jpg", shinydashboard = TRUE ), 
      
      
                  
      tabItems(
        
        ### creation du contenu du menu(menuItem) description de l'application 
        
        tabItem(tabName = "DESCR",
                
                ### fluidRow permet de creer les elements que l'on mettra sur 
                ## une meme ligne:
                
                ### infoBox est une boite dans laquelle on peut mettre un court text etc                   
                fluidRow(infoBox('BIENVENUE',"VOICI LES DETAILS DE CETTE APPLICATION",width = 12,color = "navy",fill=TRUE)),
                
                ### textOutput permet de signaler qu'il y aura contenu du texte dans le menu 
                                  
                fluidRow(box(textOutput("description"),collapsible = FALSE,width = 12))),
                 
                ### si on veut qu'il y ait un graphique dans le contenu du menu 
                ### on ecrirat plotOutput finalement la formule est 
                ###  ***Output ou *** est le nom de ce qu'on veut afficher 
                           
        tabItem(tabName = "PERF",
                                   fluidRow(plotOutput("roc"))),
                           
                           
        tabItem(tabName = "APP",
                                   fluidRow(infoBox('These de Doctorat en Medecine',"HIGH BLOOD PRESSURE RISK APPLICATION",subtitle="Iradukunda. A",width = 12,color='teal',fill=FALSE)),
                                   
                                   fluidRow(
                                        
                                     ### column est un argument qui est dans la fonction fluidRow qui indique les carac
                                     ### teristiques d'un element a afficher dans le menu (tabItem)
                                     
                                     
                                     ### le numero 4 indique la largeur de cette element a afficher
                                     ### selectInput permet de dire que l'element(input) a afficher conciste a selectionner un input parmis plusieurs possibles
                                     ### choices indique la liste des elements sur lesquels on fait la selection
                                     ### multiple peut FALSE si le choix est unique et TRUE si on peut choisir plusieurs elements de la liste
                                     
                                     
                                     ### Neanmoins si l'input est numeric on ecrira numericInput 
                                     ### de facon generale on ecrira ***Input ou *** est le nom de ce qu'on veut entrer
                                       
                                       column(4, selectInput ( "niveduc" , "NIVEAU D'EDUCATION" ,choices =c("Universite", "Secondaire", "Sans/primaire"), multiple=FALSE)),
                                       column(4, selectInput ( "tabac" ,"CONSOMMATION DU TABAC" ,choices =c("Oui", "Non"), multiple=FALSE)),
                                       column(4, selectInput ( "age" ,"AGE" ,choices =c("60 et +", "40-59ans", "15-39ans"), multiple=FALSE))
                                       
                                   ),
                                   
                                   fluidRow(
                                       
                                       column(4, selectInput ( "imc" ,"INDICE DE MASSE CORPORELLE" ,choices =c("Normal", "Obese", "Surpoids"), multiple=FALSE)),
                                       column(4, selectInput ( "diaconnu" ,"DIABETE CONNU" ,choices =c("Non", "Oui"), multiple=FALSE)),
                                       column(4, selectInput ( "antecehta" ,"ANTECEDENT FAMILIAL HTA" ,choices =c("Oui", "Non"), multiple=FALSE))
                                       
                                   ),
                                   
                                   box(title=' RESULT of Risk in %',
                                       solidHeader = TRUE,
                                       background = "blue",
                                       collapsible = TRUE,
                                       fluidRow(
                                           column(8, tableOutput("table"))
                                           
                                           )    
                                           
                                       ),
                                   fluidRow(
                                     column(12, plotOutput("grap"))
                                   )
                           )
    )
    )
)

### II. PRTIE SERVEUR QUI PERMET DE TRANSFORMER LES INPUTS EN OUTPUTS 


server <- function(input, output){

  #### LA FONCTION UTILISEE POUR RENVOYER LES RESULTATS C'EST RENDER*** ou *** est le nom du resultat 
#### dont on veut envoyer et pour indiquer la ou on va mettre les resultats on utilise output$*** 
### ou *** est le nom de la place ou on va mettre le resultat. Par exemple au 
### niveau du ligne 59 on avait appele description le lieu ou on va mettre le texte  
  
    output$description <- renderText({"This application put in production
      the model which estimated in paper available in this link https://www.arnaud/"})
    
 #### renvoie du graphique roc dans l'interface graphique au menu APP PERFORMANCE   
    
    output$roc <- renderPlot({
      
      modelfinal <- glm (as.factor(HTA)~as.factor(Niveduca)+
                                                   as.factor(Tabac1)+as.factor(AgeCat1)+as.factor(IMC1)+
                                                   as.factor(Diabete_connu)+as.factor(atcdf_hta1),data, 
                                                 family = "binomial")
    oroc1 <- roc ( data $ HTA=="Oui" , fitted(modelfinal))
    plot (1- oroc1$specificities , oroc1$sensitivities, main = "Courbe ROC ", xlab = "Taux de faux positif", 
          ylab = "Taux de vrai positif", type = "l", col = "blue")
    legend (0.2,0.07,legend = "Probabilite que l'application estime correctement le risque = 88.22% ", box.col = "black" )
    abline (0,1, col = "red")})
    
    #### renvoyer le graphique dans les resultats dans le menu APP
    
    output$grap <- renderPlot({modelfinal <- glm (as.factor(HTA)~as.factor(Niveduca)+
                                                    as.factor(Tabac1)+as.factor(AgeCat1)+as.factor(IMC1)+
                                                    as.factor(Diabete_connu)+as.factor(atcdf_hta1),data, 
                                                  family = "binomial")
    oroc1 <- roc ( data $ HTA=="Oui" , fitted(modelfinal))
    plot (1- oroc1$specificities , oroc1$sensitivities, main = "Courbe ROC ", xlab = "Taux de faux positif", 
          ylab = "Taux de vrai positif", type = "l", col = "blue")
    legend (0.2,0.07,legend = "Probabilite que l'application estime correctement le risque = 88.22% ", box.col = "black" )
    abline (0,1, col = "red")})
    
    ### renvoyer les resultats sous forme d'une table
    
    output$table <- renderTable({
  
        modelfinal <- glm (as.factor(HTA)~as.factor(Niveduca)+
                             as.factor(Tabac1)+as.factor(AgeCat1)+as.factor(IMC1)+
                             as.factor(Diabete_connu)+as.factor(atcdf_hta1),data, 
                           family = "binomial")
      
        #### recuperation des donnees entrees par l'utilisateur   
     
        nouval <- data.frame(Niveduca=input$niveduc, Tabac1=input$tabac, AgeCat1=input$age, IMC1=input$imc,
                             Diabete_connu=input$diaconnu, atcdf_hta1=input$antecehta)
        
        ### prevoir le risque selon les donnees entrees par l'utilisateur 
                          
        prevrisk <- 100*round(predict(modelfinal, newdata=nouval, type="response"),4)
        
        ### le resultat a afficher selon le risque obtenu
        
        if(prevrisk < 0.10){
          print(data.frame('Risk'  = prevrisk, Category="Low"))}
        else if (prevrisk > 0.17)
        {print(data.frame('Risk'   = prevrisk , Category= "High"))}    
        
        else
        {print(data.frame('Risk' = prevrisk , Category= "Medium"))}
    })
}
shinyApp( ui , server)       













