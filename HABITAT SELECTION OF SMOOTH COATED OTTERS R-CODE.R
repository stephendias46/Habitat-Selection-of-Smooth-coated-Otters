# CODE TO ACCOMPANY THE ARTICLE...
# Habitat Selection of Smooth Coated Otters (Lutrogale perspicillata) in the peri-coastal, urbanized landscape of Goa, India
# Authors: Stephen Jonah Dias, Patrick James Ciaran White, Atul Sinai Borker, Nandini Vaz Fernandes

# DATA ARE IN THE FILE "SMOOTH COATED OTTERS IN GOA DATA.csv"
sco<-read.csv(file.choose(),stringsAsFactors = T)

# THE BELOW PACKAGES ARE REQUIRED, PLEASE INSTALL FIRST
require(car)
require(MuMIn)
require(ggeffects)
require(sjPlot)

# TESTING VARIANCE INFLATION FACTORS FOR THE GLOBAL MODEL
model<-glm(OTTER.SIGNS~NITRATES+PHOSPHATES+SULPHATES+SALINITY+EMC..h.+FISHING,"binomial",data=sco)
vif(model)

# CANDIDATE SET OF MODELS (see Table 1 in article)
model1<-glm(OTTER.SIGNS~1,"binomial",data=sco)
model2<-glm(OTTER.SIGNS~NITRATES,"binomial",data=sco)
model3<-glm(OTTER.SIGNS~PHOSPHATES,"binomial",data=sco)
model4<-glm(OTTER.SIGNS~SULPHATES,"binomial",data=sco)
model5<-glm(OTTER.SIGNS~NITRATES+PHOSPHATES+SULPHATES,"binomial",data=sco) 
model6<-glm(OTTER.SIGNS~SALINITY,"binomial",data=sco)
model7<-glm(OTTER.SIGNS~EMC..h.,"binomial",data=sco)
model8<-glm(OTTER.SIGNS~FISHING,"binomial",data=sco)
model9<-glm(OTTER.SIGNS~SALINITY+EMC..h.+FISHING,"binomial",data=sco)
model10<-glm(OTTER.SIGNS~NITRATES+PHOSPHATES+SULPHATES+SALINITY+EMC..h.+FISHING,"binomial",data=sco)

# CREATION OF MODEL SELECTION TABLE USING MuMIn (see Table 2 in article)
mod.list<-list(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10)
model.sel(mod.list)
mod.tab<-model.sel(mod.list)

# MODEL AVERAGED PARAMETER ESTIMATES (see Table 3b in article)
modav<-model.avg(mod.tab)
round(coefTable(modav,full=F),3)

# MANGRIVE COVER (see Figure 4b in article)
y.lym=c(0,1)
x.lym=c(0,45)
plot_model(model = model9,type="pred",terms="EMC..h. [all]",
           axis.title = c("Mangrove cover (%)","Probability of otter presence"),
           colors="bw",title="(b)",axis.lim=list(x.lym,y.lym))

# SALINITY PLOT (see Figure 4a in article)
y.lym=c(0,1)
x.lym=c(0,25)
plot_model(model = model9,type="pred",terms="SALINITY [all]",
           axis.title = c("Salinity (g/L)","Probability of otter presence"),
           colors="bw",title="(a)",axis.lim=list(x.lym,y.lym))

# FISHING PLOT (see Figure 4c in article)
y.lym=c(0,1)
x.lym=c(0,1)
plot_model(model = model9,type="pred",terms="FISHING [all]",
           axis.title = c("Fishing presence","Probability of otter presence"),
           colors="bw",title="(c)",axis.lim=list(x.lym,y.lym))
points(x=c(0,1),y=c(0.47,0.8))


