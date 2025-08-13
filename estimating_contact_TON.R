setwd("E:/CWD_Modeling/Deer_Data/Clean Data/ctmm_data")
all.contacts.data = read.csv("sum_contacts_hrdiff.csv")

all.contacts.data$hr.diff.km = all.contacts.data$hr.diff.m/1000

ggplot(subset(all.contacts.data, direct.cont > 0)) + geom_point(aes(x = hr.diff.km, y = direct.cont)) 

F2 = glm(direct.cont ~ hr.diff.km, family = Gamma, data = subset(all.contacts.data, direct.cont > 0))

new.data = data.frame(hr.diff.km = seq(0, max(all.contacts.data$hr.diff.km), 0.01))
F2.pred = data.frame(direct.cont = predict.glm(F2, new.data, type = "response"), new.data)

ggplot() + geom_point(data = subset(all.contacts.data, direct.cont > 0), aes(x = hr.diff.km, y = direct.cont)) + 
  geom_line(data = F2.pred, aes(x = hr.diff.km, y = direct.cont), col = "red")

#indirects
ggplot(subset(all.contacts.data, indirect.cont > 0)) + geom_point(aes(x = hr.diff.km, y = indirect.cont)) 

F2 = glm(indirect.cont ~ hr.diff.km, family = Gamma, data = subset(all.contacts.data, indirect.cont > 0))

new.data = data.frame(hr.diff.km = seq(0, max(all.contacts.data$hr.diff.km), 0.01))
F2.pred = data.frame(indirect.cont = predict.glm(F2, new.data, type = "response"), new.data)

ggplot() + geom_point(data = subset(all.contacts.data, indirect.cont > 0), aes(x = hr.diff.km, y = indirect.cont)) + 
  geom_line(data = F2.pred, aes(x = hr.diff.km, y = indirect.cont), col = "red")
