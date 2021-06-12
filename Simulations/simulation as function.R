# Rozsprzestrzenianie sie chorób na przestrzeni 2-wymiarowej
# High - wysokosc macierzy (int)
# Width - dlugosc macierzy (int)
# Population - Populacja (sugerowana 1/10 przestrzeni) (int)
# first_vacc - iteracja w ktorej rozpoczynaja sie szczepienia (int) 
# dose - losowana liczba dawek z rozkladu normalnego podanej sredniej (dose) i odchylenia standardowego doseSD()
# doseSD - losowana liczba dawek z rozkladu normalnego podanej sredniej (dose) i odchylenia standardowego doseSD()
# baseDrate - losowane prawdopodobienstwo zgonu z sredniej baseDrate i odchylenia baseDrateSD  
# baseDrateSD - losowane prawdopodobienstwo zgonu z sredniej baseDrate i odchylenia baseDrateSD
# baseINFrate - losowane prawdopodobienstwo z sredniej baseINFrate i odchylenia baseINFrateSD
# baseINFrateSD - losowane prawdopodobienstwo z sredniej baseINFrate i odchylenia baseINFrateSD
# totalts - liczba iteracji -1
# name - nazwa pliku do zapisu animacji gif lub jego sciezka ( bez rozszczenienia '.gif')
#parametry proponowane
High = 100
Width = 100
Population = 1000 
first_vacc = 100
dose = 3
doseSD = 1
baseDrate = 0.0015
baseDrateSD = 0.001
baseINFrate = 0.35
baseINFrateSD = 0.075
totalts = 150
nameoutput = 'simulation'
library(ggplot2)
library(gganimate)
Simulate = function(High,Width,Population,first_vacc,dose,doseSD,baseDrate,baseDrateSD,baseINFrate,baseINFrateSD,nameoutput){
  library(plot.matrix)

  # PARAMETRY ZMIENNE
  High = High
  Width = Width
  Population = Population
  
  # Utworzenie
  total = c(c(rep(1,Population)),                           # 0 - pusta przestrzeń
            c(rep(0,High * Width - Population)))            # 1 - zajęta przestrzeń przez człowieka
  
  Total = sample(x = total,                                 # Losowo wyznaczone miejsca puste i zajęte                       
                 size = length(total),
                 replace = F)
  Matrix = matrix(data = Total,                             # Stworzenie przestrzeni 2 wymiarowej
                  nrow = High,
                  ncol = Width)

  #___Dodanie do zbioru osoby zakażonej w losowo położonym punkcie nie zajetym przez czlowieka___
  Vectorized_Matrix = as.vector(Matrix)
  Randomspot_vectorized = sample(x = c(High*Width),
                                 size = 1)
  while (Vectorized_Matrix[Randomspot_vectorized] == 1){
    Randomspot_vectorized = sample(x = c(High*Width),
                                   size = 1)}
  Vectorized_Matrix[Randomspot_vectorized] = 2
  Matrix = matrix(data = Vectorized_Matrix,                 # Aktualizacja przestrzeni o osobe zarażoną                   
                  nrow = High,
                  ncol = Width)

  vaccination = function(first_vacc, doses){
    if (t>first_vacc){
      vec = as.vector(Matrix)
      if(doses<length(Matrix[Matrix==1])){
        coords_vacc  = sample(which(vec == 1, arr.ind=TRUE),doses)
      } else{
        coords_vacc = sample(which(vec == 1, arr.ind=TRUE),length(Matrix[Matrix==1]))
      }
      for (i in coords_vacc){
        vec[i] = 3
      }
      Matrix <<- matrix(vec,
                        nrow = High,
                        ncol = Width)
    } else{
      
    }
  } # Funkcja etapu wakcynacji, firstvacc =- okres pierwszej wakcynacji , jezeli t jest od niego wieksze, szczepi 'doses' losowo wybranych jednostek
  deathtest = function(deathrate){
    vec = as.vector(Matrix)
    coords_ppl  = which(vec == 2, arr.ind=TRUE)
    for (i in coords_ppl){
      testvalue = sample(x = c(1,0),size = 1,prob = c(deathrate,(1-deathrate)))  
      if (testvalue == 1){
        vec[i] = 0
        Matrix <<- matrix(vec,
                          nrow = High,
                          ncol = Width)# 0 poniewaz umiera i zostawia pustą przestrzeń
      }
    }
  } # funckja etapu testowania smiertelnosci. deathrate to prawdopodobienstwo zgonu zakazonej jednostki.
  healtest = function(heal_chance){
    vec = as.vector(Matrix)
    coords_ppl  = which(vec == 2, arr.ind=TRUE)
    for (i in coords_ppl){
      healtestvalue = sample(x = c(1,0),size = 1,prob = c(heal_chance,(1-heal_chance)))   #1 -  wyzdrowial
      if (healtestvalue == 1){
        vec[i] = 1
        Matrix <<- matrix(vec,
                          nrow = High,
                          ncol = Width) # 1 - poniewaz wraca do bycia zdrowym
      }
    }
  } # funckja etapu testowania powrotu do zdrowia. heal_chance to prawdopodobienstwo wyzdrowienia jednostki, 
  # zdefiniowane jako odsetek populacji zachorowanej 14 dni temu do populacji nowo zakazonej
  infectiontest = function(inf_rate){
    #Wyznaczenie Przestrzeni rozprzestrzeniania sie, podstawa , coordynat zarazonego daje obszar mozliwosci zakazenia 5x5
    coords  = which(Matrix == 2, arr.ind=TRUE)
    #Ustanowienie obszaru zarażonych, z wyjątkami dla ścian bocznych
    sickrows = data.frame()
    sickcols = data.frame()
    for (i in coords[,1]){
      if (i %in% c(3:(High-2))){ sickrows = rbind(sickrows,c(i-2,i+2)) }
      if (i == 1){ sickrows = rbind(sickrows,c(i,i+2)) }
      if (i == 2){ sickrows = rbind(sickrows,c(i-1,i+2)) }
      if (i == (High-1)){ sickrows = rbind(sickrows,c(i-2,i+1)) }
      if (i == High){ sickrows = rbind(sickrows,c(i-2,i)) }
    }
    for (i in coords[,2]){
      if (i %in% c(3:(Width-2))){ sickcols = rbind(sickcols,c(i-2,i+2)) }
      if (i == 1){ sickcols = rbind(sickcols,c(i,i+2)) }
      if (i == 2){ sickcols = rbind(sickcols,c(i-1,i+2)) }
      if (i == (Width-1)){ sickcols = rbind(sickcols,c(i-2,i+1)) }
      if (i == Width){ sickcols = rbind(sickcols,c(i-2,i)) }
    }
    sickcoords = cbind(sickrows,sickcols)
    colnames(sickcoords)= c('left','right','upper','bottom')
    sickarea = data.frame(matrix(0,High,Width))
    for (i in 1:nrow(sickcoords)){
      sickarea[sickcoords$left[i]:sickcoords$right[i],sickcoords$upper[i]:sickcoords$bottom[i]] = 1
    }
    #wygenerowanie obszaru na ktorym znajduja sie ludzie ktorych trzeba poddac testowi na zarazenie
    sickarea[sickarea == T] = 1 # seting area of sick ppl influence to 1 to check with relations of other dataframe
    sickarea[sickarea == F] = 9 # number producing false
    df3 = Matrix[] == sickarea[]
    coords2  = which(df3 == 1, arr.ind=TRUE)
    coords2
    coords2 = cbind(coords2,sample(c(T,F),nrow(coords2),replace = T,prob = c(inf_rate,(1-inf_rate))))
    coords2 = as.data.frame(coords2)
    ppl_in_risk_area = nrow(coords2)
    if (ppl_in_risk_area > 0){
      for (i in 1:nrow(coords2)){
        if (nrow(coords2)>0){
          if (coords2$V3[i] == 1){  
            Matrix[coords2$row[i],coords2$col[i]] <<- c(2)
          }
          else {
            Matrix[coords2$row[i],coords2$col[i]] <<- c(1)
            
          }
        }
      }
    }
  } # test infekcji nowych jednostek ktore nie sa zakazone i nie
  
  
  
  t = 1
  first_vacc = first_vacc
  infected_pop = c(1)
  dose = dose
  doseSD = doseSD
  baseDrate = baseDrate
  baseDrateSD = baseDrateSD
  baseINFrate = baseINFrate
  baseINFrateSD = baseINFrateSD
  
  liczba_zarazonych = length(Matrix[Matrix==2])
  liczba_zaszczepionych = length(Matrix[Matrix==3])
  liczebnosc_populacji = length(Matrix[Matrix == 1])+liczba_zarazonych+liczba_zaszczepionych
  ts = c(1)
  while (t<totalts){
    #parametry
    currently_infected = length(Matrix[Matrix == 2])
    doses = abs(round(rnorm(1,dose,doseSD))) #losowanie liczby dostepnych dawek w okresie
    deathrate = abs(rnorm(1,baseDrate,baseDrateSD)) #losowanie prawdopodobienstwa zgonu
    infrate = rnorm(1,baseINFrate,baseINFrateSD)
    if (length(infected_pop)<15){ #heal_chance moze byc wiekszy od zera po 14 iteracjii
      heal_chance = 0
    } else{
      heal_chance =  abs(infected_pop[2]-infected_pop[1])/currently_infected
      if (heal_chance>1){heal_chance= 1}
    }
    #mainloop _ wykorzystanie funkcji
    infectiontest(infrate)
    deathtest(deathrate)
    healtest(heal_chance)
    vaccination(first_vacc,doses)
    Sys.sleep(1)
    #random replacement
    Matrixasvector = as.vector(Matrix)
    Matrixasvector = sample(Matrix,length(Matrix),replace = F)
    Matrix = matrix(Matrixasvector,High,Width)
    
    #podbicie okresu chorobowego, iteracji
    t= t+1
    infected_pop = c(infected_pop,length(Matrix[Matrix == 2]))
    if (length(infected_pop)==16){ # jezeli len =  14 wyrzuca pierwszą
      infected_pop = infected_pop[-1]
    }
    #plot(Matrix,                                              # Wykres rozmieszczenia
    #     col = c('white','blue','red','green'),
    #     xlab = 'Geom Width',
    #     ylab = 'Geom_High')
    #Sys.sleep(5)
    zarazeni = length(Matrix[Matrix==2])
    zaszczepieni = length(Matrix[Matrix==3])
    poopulacjacala = length(Matrix[Matrix == 1])+zarazeni+zaszczepieni
    ts = c(ts,t)
    liczba_zarazonych = c(liczba_zarazonych,zarazeni)
    liczba_zaszczepionych = c(liczba_zaszczepionych,zaszczepieni)
    liczebnosc_populacji = c(liczebnosc_populacji,poopulacjacala)
    summarystats <<- data.frame(ts, liczebnosc_populacji,liczba_zarazonych,liczba_zaszczepionych)
  }
}

Simulate(High,Width,Population,first_vacc,
         dose,doseSD,
         baseDrate,baseDrateSD,
         baseINFrate,baseINFrateSD,
         totalts)
print("Simulation end.")
#print(summarystats)
#ggplot(summarystats, aes(x=ts)) + 
#  geom_line(aes(y = liczebnosc_populacji), color = "black") + 
#  geom_line(aes(y = liczba_zarazonych), color="red") +
#  geom_line(aes(y = liczba_zaszczepionych), color="green") + 
#  transition_reveal(ts)
#
#anim_save(paste0(nameoutput,".gif"))
#
#ggplot(summarystats, aes(x=ts)) + 
#  geom_line(aes(y = liczebnosc_populacji), color = "black") + 
#  geom_line(aes(y = liczba_zarazonych), color="red") +
#  geom_line(aes(y = liczba_zaszczepionych), color="green")