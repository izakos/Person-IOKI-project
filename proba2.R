data$learner_id=factor(data$learner_id)
data$unit=factor(data$unit)

# Najpierw sprawdzimy, które rozdzia³y s¹ najchêtniej, a które najrzadziej wybierane


ggplot(data = data) + 
  geom_bar(mapping = aes(x = unit)) +
  coord_flip() +
  ggtitle("Number of people who choose each unit")

(licznoscirozdz=table(data$unit))
sort(licznoscirozdz)
procrozdz=prop.table(licznoscirozdz)
barplot(sort(procrozdz))
barplot(sort(licznoscirozdz))


#widzimy, ¿e im dalszy rozdzia³, tym rzadziej wybierany. ozanacza to, ¿e uczniowie nie przechodz¹ kursu do koñca
#dodatkowo czêœæ uczniów rezygnuje z VP, czeœæ omija powtórki

#sprawdŸmy jak w zale¿noœci od rozdzia³u, zale¿¹ wyniki

aov(data$avg_score~data$unit)
summary(aov(data$avg_score~data$unit)) #isotny wp³yw

summarise(group_by(data,unit),mean(avg_score,na.rm=T))
group_size(group_by(data,unit))
boxplot(data$avg_score~data$unit)
vioplot(data$avg_score~data$unit)
#czy nauczyciel jest istotny

t.test(data$avg_score~data$in_course) #f 0,76  t 0,80
boxplot(data$avg_score~data$in_course)

ggplot(data = data) + 
  geom_point(mapping = aes(x = data$avg_score, y = data$inv_rate,size=5)) + 
  facet_wrap(~data$in_course)

cbind(count(group_by(data,in_course)),count(group_by(data,in_course))[,2]/nrow(data))

ggplot(data = data) + 
  geom_point(mapping = aes(x = data$avg_score, y = data$inv_rate)) + 
  geom_smooth(mapping = aes(x = data$avg_score, y = data$inv_rate))
#kraj

boxplot(data$avg_score~data$country) #ró¿nice spore - mo¿na by dopasowaæ kurs do kraju
View(summarise(group_by(data,country),mean(avg_score,na.rm=T)))
summarise(group_by(data,country),mean(avg_score,na.rm=T))
summarise(group_by(data,country),count(country,na.rm=T)         
          
#czy zdolnoœci ucznia maj¹ wp³yw na wyniki

summarise(group_by(data,learner_id),mean(avg_score,na.rm=T))

#zale¿noœæ wyników od postêpowania wed³ug rad ekspertów
ggplot(data = data) + 
  geom_point(mapping = aes(x = data$avg_score, y = data$inv_rate)) + 
  geom_smooth(mapping = aes(x = data$avg_score, y = data$inv_rate))

ggplot(data = data) + 
  geom_bar(mapping = aes(x = cut(data$inv_rate,20))) +
  coord_flip() +
 ggtitle("How people uses the expert advice")

#œrednie uzyskiwane wyniki - licznoœæ osób

ggplot(data = data) + 
  geom_bar(mapping = aes(x = cut(data$avg_score,20))) +
  coord_flip() +
  ggtitle("œrednie uzyskiwane wyniki")


#stopieñ kompletaacji a wynik
plot(data$completion,data$avg_score)

ggplot(data = data) + 
  geom_point(mapping = aes(x =data$completion , y = data$avg_score),cex=0.75)


#czy zalecenia expertów s³uszne
plot(data$inv_rate,data$avg_score)
aov(data$avg_score~data$inv_rate)
summary(aov(data$avg_score,data$inv_rate))

#w których krajach popularne
summarise(group_by(data,country))
summary(group_by(data,country))
count(group_by(data,country))
View(count(group_by(data,country))) #reklama

#œrednia iloœæ rozdz ucznia - zrób wykres ko³owy

# ile uczniów wybiera dany rozdzia³
ggplot(data=data) + 
  geom_boxplot(mapping=(aes(x = learner_id, y = unit())))
  
table(data$learner_id)
barplot(table(data$learner_id))
table(table(data$learner_id))
barplot(table(table(data$learner_id)))

#gêstoœæ
hist(data$avg_score,prob=T)
lines(density(data$avg_score,na.rm=T))
summary(data)

#ile osob sie stosuje w jakim proc
table(cut(data$inv_rate,20))
plot(table(cut(data$inv_rate,20)))


# ró¿nice w poziomie wyników dla nauczyciela i bez dla wysokich wyników
filter(data,avg_score>=0.8)
count(group_by(filter(data,avg_score>=0.8),in_course))


