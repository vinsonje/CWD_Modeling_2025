######################
####Define Parameters
######################
thyme = 12*20

track.pop = FALSE

###########################
#grid/landscape parameters
###########################
grid.xmax = 100
grid.ymax = 100
cell.x.size = 1.0
cell.y.size = 1.0
density = 10 #density per km2
area = grid.xmax * grid.ymax #total area of the grid

###########################
#host demographic parameters
###########################
N0 = density*area #initial population size
K = N0 #carrying capacity for whole population
fs = 15 #average family size

lifespan = 12*10
Pbd = 1.5
birth.times = seq(6, thyme, 12)

###########################
#host relocation parameters
###########################
shift = c(0.657, 2.175) #shape and rate of the gamma distribution that defines how far they relocate on the landscape
inc = 1.65 #home range size of the population (basically, if they are drawn to relocate lower than this number they don't relocate)
max.den = 100
move.strat = "maxden"

##########################
#host dispersal parameters
##########################
dispersal = 0.006
disp.dist = c(1.572, 0.239) 

disp.times = seq(11, thyme, by = 12)
# disp.times = c(10:12, 22:24, 34:36,
#                46:48, 58:60, 70:72)

###########################
#Epidemiological parameters
###########################
I0 = 1

#shed = 0.002 #the shedding rate of infected deer (mean of poisson distribution); you changed this from 0.017
#0.017 originally too high; 0.005 good for ~2.5% prev.
shed = 0.0001 #slow: 0.002 -> 5% after 20 years
lat.period = 3
inf.period = 20

#########################
#Direct Transmission Parameters
#########################
#F1 = 0.03 #within same cell; was 0.002; 0.0025; 0.09 too high?; 0.02 too low; 0.05 too high; 0.03 good for ~2.5% prev
F1 = 0.08 #slow: 
B1 = F1*0.75 #between cells

# F1 = 0
# B1 = 0

##########################
#Prion transmission parameters
########################
B1P.m = -10
B1P.inter = 1.0

# B1P.m = 0
# B1P.inter = 0

corpse.burst = shed*10

prion.lifespan = 12*7

############################
####Define direct contacts
############################
setwd("E:/CWD_Modeling/Deer_Data/Clean Data/ctmm_data")
all.contacts.data = read.csv("sum_contacts_hrdiff.csv")

all.contacts.data.binom = all.contacts.data
all.contacts.data.binom$direct.cont = (all.contacts.data.binom$direct.cont>0)*1
all.contacts.data.binom$indirect.cont = (all.contacts.data.binom$indirect.cont>0)*1
all.contacts.data.binom$hr.diff.km = all.contacts.data.binom$hr.diff.m/1000

F2 = glm(direct.cont ~ hr.diff.km, family = "binomial", data = all.contacts.data.binom)

F2_int = coef(F2)[1]
F2_B = coef(F2)[2]

#############################
####Define indirect contacts
#############################
F2i = glm(indirect.cont ~ hr.diff.km, family = "binomial", data = all.contacts.data.binom)

F2i_int = coef(F2i)[1]
F2i_B = coef(F2i)[2]

######################
#Sharpshooting parameters
######################
ss.times = c(seq(61, thyme, 12),
             seq(62, thyme, 12),
             seq(63, thyme, 12))

# ss.times = 9999
# ss.times = 0
ss.shooters = 30
ss.radius = 1.0
ss.num = 20
ss.strat = "priority"
ss.laccess = 1.0
ss.laccess.dist = 1.0
ss.edge.weight = 0.5

######################
#Harvesting parameters
######################
h.times = c(seq(10, thyme, 12),
            seq(11, thyme, 12),
            seq(12, thyme, 12),
            seq(13, thyme, 12))

# h.times = -999

h.harvest = 5000 #20,000 individuals per harvesting season

######################
#Surveillance parameters
######################
test.rate = 0.58
true.pos.E = 0
true.pos.I = 0.999
true.neg = 0.999
# sur.start = -999
sur.start = seq(13, thyme, by = 12)
