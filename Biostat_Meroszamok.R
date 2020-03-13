#Doctor Strange készít egy saját varázsuniverzumot amiben
#csak és kizárólag unkák vannak
#De abból "végtelen" sok
#Mivel õ kezeli az unkák paraméterét, a hosszukat a következõk alapján határozza meg
set.seed(1156421456) #véletlenszám generátor beállítása
POP_SIZES = 1000000 #populációméretek
par(mfrow = c(1,1))
#hosszúságok normáleloszlásának paraméterei
voros_MU = 25
voros_SIGMA = 3

sarga_MU = 20
sarga_SIGMA = 5

#random generált hosszúságok egy-egy millió brekihez
voros_pop = rnorm(mean = voros_MU, sd = voros_SIGMA, n = POP_SIZES)
sarga_pop = rnorm(mean = sarga_MU, sd = sarga_SIGMA, n = POP_SIZES)

#Hisztogramok
hist(voros_pop, xlim=c(0,45), col="red")
hist(sarga_pop, add=T, col=rgb(1,1,0, 0.5))


#Sûrûségfüggvények
voros_density <- density(voros_pop) # returns the density data
sarga_density <- density(sarga_pop) # returns the density data
plot(voros_density, col="red", lwd=4,xlim=c(0,45),
     main="Unkahosszúságok eloszlása",
     xlab="Hossz (cm)")
lines(sarga_density, col="orange",lwd=4)
#hist(veres_pop, freq = FALSE , xlim=c(0,45), col="red", main="Unkahosszúságok eloszlása")
#hist(sarga_pop, freq = FALSE , add=T, col=rgb(1,1,0, 0.5))


#KÍSÉRLET 1 --> boxplot magyarázat
set.seed(1) #véletlenszám generátor beállítása
#Vegyünk egy vöröshasú és egy sárgahasú mintát, mindkettõ 100-100-as mintamérettel
voros_sample=sample(voros_pop, size=100)
sarga_sample=sample(sarga_pop, size=100)

df=data.frame(
  hossz=c(voros_sample,sarga_sample),
  szin= c(rep("voroshasu",100), rep("sargahasu", 100))
)

#Nézzük meg a minták hisztogramja és a boxplotok közötti kapcsolatot
par(mfrow = c(2,1))
hist(voros_sample, breaks=10, xlim=c(0,45),ylim=c(0,25), col="red",
     main="100 elemes unkaminták eloszlása (hisztogram)",
     xlab="Hossz (cm)")
hist(sarga_sample, breaks=10, add=T, col=rgb(1,1,0, 0.5))

boxplot(df$hossz~df$szin, boxfill=c("yellow","red"),ylim=c(0,45),
        main="100 elemes unkaminták eloszlása (boxplot)", horizontal=TRUE,
        xlab="Hossz (cm)")

#Alap mérõszámok
#középértékek
mean(voros_sample)
median(voros_sample)
voros_MU #a valódi populációs érték más!
mean(voros_pop) #nyilván a véges populáció is mutat minimális eltérést
median(voros_pop)


mean(sarga_sample)
median(sarga_sample)
sarga_MU #a valódi populációs érték más!
mean(sarga_pop)#nyilván a véges populáció is mutat minimális eltérést
median(sarga_pop)

#szórások <-- megintsak eltér egymástól a populáció valódi szórása és a mintából becsült szórás
sd(voros_sample)
voros_SIGMA
sd(voros_pop)

sd(sarga_sample)
sarga_SIGMA
sd(sarga_pop)



#KÍSÉRLET 2 --> Standard Error avagy az átlagok szórása
set.seed(2) #véletlenszám generátor beállítása
par(mfrow = c(2,1))
#Most koncentráljunk csak a vöröshasú unkákra,
#vegyünk megint 100 elemes mintákat, de ezt csináljuk meg 10000-szer,
#MINDIG CSAK AZ ÁTLAGOT VESSZÜK!
voros_multisample=replicate(10000, mean(sample(voros_pop, 100)))
voros_multisample

hist(voros_multisample, xlim=c(0,45), col="purple",
     main="10000 db 100-elemû unkaminta ÁTLAGAINAK eloszlása (hisztogram)",
     xlab="Átlagos hossz (cm)")

voros_simasample=sample(voros_pop, 10000)
hist(voros_simasample, xlim=c(0,45), col="red",
     main="10000 db unka hosszúságának eloszlása (hisztogram)",
     xlab="Hossz (cm)")


sd(voros_multisample) #<---az átlagok szórása (standard error) a 10000 db 100-as mintában
#mint láthatjuk köze sincs a rendes szóráshoz
voros_SIGMA
#vagyishát van valami köze:
voros_SIGMA/(sqrt(100)) #<--- az átlagok szórása kb. ennyi





#"Kísérlet" 3
#Konfidenciaintervallumok:
#Vegyük újfent a vöröshasú unkákat, abból egy 50 db-os mintát
set.seed(2) #véletlenszám generátor beállítása
n=50

voros_minisample=sample(voros_pop, n)
mean(voros_minisample)
sd(voros_minisample)

#Meg szeretnénk állítani ez alapján hogy az eredeti populáció középértéke (MU) mennyi lehet
#Nyilván nagyjából a minta átlaga környékén helyezkedhet el
#De nem akarunk túl pongyolák lenni, inkább egy tartományt jelölünk ki
#99%-os konfidencia intervallum --> 99% hogy ott van az átlag
#ELSÕDLEGES kérdés:
#Ismerjük a populáció átlagok szórását meghatározó paramétert?


#Doctor Strange ismeri SIGMA=3 --> SE=SIGMA/sqrt(n)
S0=voros_SIGMA
ukrit1 = qnorm(0.005, mean = 0, sd = 1)
ukrit2 = qnorm(0.995, mean = 0, sd = 1)
mean(voros_minisample) + ukrit1*(S0/sqrt(n)) #alsó határ
mean(voros_minisample) + ukrit2*(S0/sqrt(n)) #felsõ határ


#Tony Stark nem tudja, de cserébe okos
s=sd(voros_minisample)
tkrit1 = qt(0.005, df = n-1)
tkrit2 = qt(0.995, df = n-1)
mean(voros_minisample) + tkrit1*(s/sqrt(n))
mean(voros_minisample) + tkrit2*(s/sqrt(n))

