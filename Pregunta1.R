#Pregunta 1

#1.1

#ensaig de Bernoulli
x<- c(0,1)
f<- c(0.68,0.32) #definim la nostra variable aleatòria i la seva funció de densitat

plot(f~x,type = "h", ylim=c(0,1), col ="red") #fem un plot per verure-ho gràficament.

#ara ens demanen la probabilitat de que en una mostra de 43 llars, 13 responguin que SÍ que tenen com a mínim 2 teles

#ara si ens fixem però la nostra variable aleatòria ha canviat --> hem de crear una nova variable amb una nova funció de densitat i tal. 

#ara tenim el què es diu una VA Binomial

#si ens fixem les possbiles respostes seran --> 0,1,2,3,4,5,6 ... , 43 (el nº de llars que contesten que sí a l'enquesta)
#és a dir és com si fessim 43 mostres de la variable anterior i les sumessim totes (recordem que si diuen que SÍ --> 1)

#si anessim a calcular la probabilitat veiem que seria --> ex  P(X = 0) = P(0intersec0intersec0...) -> com que són indepes seria fer 0.68*0.68..... = 0.68^43

#però i si volem calcular P(X=13)?



n<-43
sample(x,n,f, replace = TRUE) #això seria una mostra aleatòria del que estudiem
sum(sample(x,n,f, replace = TRUE)) #això suma els compos -> ens dona el valor que pren la nostra variable aleatòria (quantes families tenen 2 teles o més en una mostra de 43)
#hauriem de replicar això molts cops per veure les probabilitats de cada cosa:

#ho fem així:

Y<-function(i){sum(sample(x,n,f,replace = TRUE))}

enquestes <- sapply(1:40,Y)
table(enquestes)
#veiem que en aquest cas el 13 ens ha sortit 9 cops, per tant si fessim l'experiment 40 cops trobariem que la probabilitat de que X = 13 seria de 9/40

#anem a fer-ho més cops

enquestes <- sapply(1:400000,Y)
table(enquestes) #ara veiem que ja ens dona valors més grans --> seria més real

fr<- table(enquestes)/400000
fr #amb això estariem veient bàsicament la probabilitat si haguessim fet 400000 mostres

barplot(fr)
fr["13"] #amb això extraiem el valor que ens interessa


# però clar tot això son com aproximacions i valors que no acaben de ser reals

#per fer-ho real aquí a R farem

dbinom(13,43,0.32) #això s'interpreta com la probabilitat de que obtinguem 13 respostes positives en una enquesta de 43 sabent que la probabilitat d'obtenir una resposta afirmativa és 0.32
y<- c(0:43)
plot(y,dbinom(y,43,0.32), type= "h", col = "red") #amb això estem fent el gràfic de la funció de densitat de la nostra variable aleatòria

#en la següent pregunta la resposta l'obtindriem fent:
dbinom(17,44,0.32) #fiquem 44 ja que ara ens diuen que l'enquesta és de 44

#fem el gràfic general:
y<-c(0:44)
plot(y,dbinom(y,43,0.32), type= "h", col = "red")

#però ens estant demanant la probabilitat de que menys de 17 llars responguin que sÍ 
#veiem que en el fons ens demanen F(y<= 16) --> Funció de DISTRIBUCIÓ o si ho fessim respecte la funció de densitat ens demanen f(0)+f(1)+f(2)+...+f(16)

pbinom(16,44,0.32) #es fa amb aquest comando. 

qbinom(0.5,44,0.32) #amb això calculariem la mediana

qbinom(0.25,44,0.32) #amb això calculem el primer quartil

#(això últim no ens ho demanen però ho fem per saber-ho)

#pregunta 3

esperança<- 24*0.68  #ho ha justficat a la pissarra, bàsicament seria que partim d'una variable de bernoulli com abans, on volem saber llars que tenen com a màxim 1 tele ( és lu d'abans que tenia P  = 0.68), llavors seria a partir d'això fer 24 enquestes i veure els resultats -> això seria la meva nova VA i a partir d'aquí hauriem de calulcar l'esperança i tal...
Variança<- 24*0.68*0.32
#el quantil quart de X és el primer quartil de X
primerquartil<- qbinom(0.25,24,0.68) 
esperança
Variança
primerquartil

#última pregunta

#aquí hauiem de fer una simulació tipus la que hem fet abans

n<- 46
Y<-function(i){sum(sample(x,n,f,replace = TRUE))}
enquestes <- sapply(1:400000,Y)
table(enquestes)
mean(enquestes)
