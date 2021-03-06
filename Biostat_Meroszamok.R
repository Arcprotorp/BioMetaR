#Doctor Strange k�sz�t egy saj�t var�zsuniverzumot amiben
#csak �s kiz�r�lag unk�k vannak
#De abb�l "v�gtelen" sok
#Mivel � kezeli az unk�k param�ter�t, a hosszukat a k�vetkez�k alapj�n hat�rozza meg
set.seed(1156421456) #v�letlensz�m gener�tor be�ll�t�sa
POP_SIZES = 1000000 #popul�ci�m�retek
par(mfrow = c(1,1))
#hossz�s�gok norm�leloszl�s�nak param�terei
voros_MU = 25
voros_SIGMA = 3

sarga_MU = 20
sarga_SIGMA = 5

#random gener�lt hossz�s�gok egy-egy milli� brekihez
voros_pop = rnorm(mean = voros_MU, sd = voros_SIGMA, n = POP_SIZES)
sarga_pop = rnorm(mean = sarga_MU, sd = sarga_SIGMA, n = POP_SIZES)

#Hisztogramok
hist(voros_pop, xlim=c(0,45), col="red")
hist(sarga_pop, add=T, col=rgb(1,1,0, 0.5))


#S�r�s�gf�ggv�nyek
voros_density <- density(voros_pop) # returns the density data
sarga_density <- density(sarga_pop) # returns the density data
plot(voros_density, col="red", lwd=4,xlim=c(0,45),
     main="Unkahossz�s�gok eloszl�sa",
     xlab="Hossz (cm)")
lines(sarga_density, col="orange",lwd=4)
#hist(veres_pop, freq = FALSE , xlim=c(0,45), col="red", main="Unkahossz�s�gok eloszl�sa")
#hist(sarga_pop, freq = FALSE , add=T, col=rgb(1,1,0, 0.5))


#K�S�RLET 1 --> boxplot magyar�zat
set.seed(1) #v�letlensz�m gener�tor be�ll�t�sa
#Vegy�nk egy v�r�shas� �s egy s�rgahas� mint�t, mindkett� 100-100-as mintam�rettel
voros_sample=sample(voros_pop, size=100)
sarga_sample=sample(sarga_pop, size=100)

df=data.frame(
  hossz=c(voros_sample,sarga_sample),
  szin= c(rep("voroshasu",100), rep("sargahasu", 100))
)

#N�zz�k meg a mint�k hisztogramja �s a boxplotok k�z�tti kapcsolatot
par(mfrow = c(2,1))
hist(voros_sample, breaks=10, xlim=c(0,45),ylim=c(0,25), col="red",
     main="100 elemes unkamint�k eloszl�sa (hisztogram)",
     xlab="Hossz (cm)")
hist(sarga_sample, breaks=10, add=T, col=rgb(1,1,0, 0.5))

boxplot(df$hossz~df$szin, boxfill=c("yellow","red"),ylim=c(0,45),
        main="100 elemes unkamint�k eloszl�sa (boxplot)", horizontal=TRUE,
        xlab="Hossz (cm)")

#Alap m�r�sz�mok
#k�z�p�rt�kek
mean(voros_sample)
median(voros_sample)
voros_MU #a val�di popul�ci�s �rt�k m�s!
mean(voros_pop) #nyilv�n a v�ges popul�ci� is mutat minim�lis elt�r�st
median(voros_pop)


mean(sarga_sample)
median(sarga_sample)
sarga_MU #a val�di popul�ci�s �rt�k m�s!
mean(sarga_pop)#nyilv�n a v�ges popul�ci� is mutat minim�lis elt�r�st
median(sarga_pop)

#sz�r�sok <-- megintsak elt�r egym�st�l a popul�ci� val�di sz�r�sa �s a mint�b�l becs�lt sz�r�s
sd(voros_sample)
voros_SIGMA
sd(voros_pop)

sd(sarga_sample)
sarga_SIGMA
sd(sarga_pop)



#K�S�RLET 2 --> Standard Error avagy az �tlagok sz�r�sa
set.seed(2) #v�letlensz�m gener�tor be�ll�t�sa
par(mfrow = c(2,1))
#Most koncentr�ljunk csak a v�r�shas� unk�kra,
#vegy�nk megint 100 elemes mint�kat, de ezt csin�ljuk meg 10000-szer,
#MINDIG CSAK AZ �TLAGOT VESSZ�K!
voros_multisample=replicate(10000, mean(sample(voros_pop, 100)))
voros_multisample

hist(voros_multisample, xlim=c(0,45), col="purple",
     main="10000 db 100-elem� unkaminta �TLAGAINAK eloszl�sa (hisztogram)",
     xlab="�tlagos hossz (cm)")

voros_simasample=sample(voros_pop, 10000)
hist(voros_simasample, xlim=c(0,45), col="red",
     main="10000 db unka hossz�s�g�nak eloszl�sa (hisztogram)",
     xlab="Hossz (cm)")


sd(voros_multisample) #<---az �tlagok sz�r�sa (standard error) a 10000 db 100-as mint�ban
#mint l�thatjuk k�ze sincs a rendes sz�r�shoz
voros_SIGMA
#vagyish�t van valami k�ze:
voros_SIGMA/(sqrt(100)) #<--- az �tlagok sz�r�sa kb. ennyi





#"K�s�rlet" 3
#Konfidenciaintervallumok:
#Vegy�k �jfent a v�r�shas� unk�kat, abb�l egy 50 db-os mint�t
set.seed(2) #v�letlensz�m gener�tor be�ll�t�sa
n=50

voros_minisample=sample(voros_pop, n)
mean(voros_minisample)
sd(voros_minisample)

#Meg szeretn�nk �ll�tani ez alapj�n hogy az eredeti popul�ci� k�z�p�rt�ke (MU) mennyi lehet
#Nyilv�n nagyj�b�l a minta �tlaga k�rny�k�n helyezkedhet el
#De nem akarunk t�l pongyol�k lenni, ink�bb egy tartom�nyt jel�l�nk ki
#99%-os konfidencia intervallum --> 99% hogy ott van az �tlag
#ELS�DLEGES k�rd�s:
#Ismerj�k a popul�ci� �tlagok sz�r�s�t meghat�roz� param�tert?


#Doctor Strange ismeri SIGMA=3 --> SE=SIGMA/sqrt(n)
S0=voros_SIGMA
ukrit1 = qnorm(0.005, mean = 0, sd = 1)
ukrit2 = qnorm(0.995, mean = 0, sd = 1)
mean(voros_minisample) + ukrit1*(S0/sqrt(n)) #als� hat�r
mean(voros_minisample) + ukrit2*(S0/sqrt(n)) #fels� hat�r


#Tony Stark nem tudja, de cser�be okos
s=sd(voros_minisample)
tkrit1 = qt(0.005, df = n-1)
tkrit2 = qt(0.995, df = n-1)
mean(voros_minisample) + tkrit1*(s/sqrt(n))
mean(voros_minisample) + tkrit2*(s/sqrt(n))

