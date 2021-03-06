data$learner_id=factor(data$learner_id)
data$unit=factor(data$unit)

# Najpierw sprawdzimy, kt�re rozdzia�y s� najch�tniej, a kt�re najrzadziej wybierane


ggplot(data = data) + 
  geom_bar(mapping = aes(x = unit)) +
  coord_flip() +
  ggtitle("Number of people who choose each unit")

(licznoscirozdz=table(data$unit))
sort(licznoscirozdz)
procrozdz=prop.table(licznoscirozdz)
barplot(sort(procrozdz))
barplot(sort(licznoscirozdz))


#widzimy, �e im dalszy rozdzia�, tym rzadziej wybierany. ozanacza to, �e uczniowie nie przechodz� kursu do ko�ca
#dodatkowo cz�� uczni�w rezygnuje z VP, cze�� omija powt�rki

#sprawd�my jak w zale�no�ci od rozdzia�u, zale�� wyniki

aov(data$avg_score~data$unit)
summary(aov(data$avg_score~data$unit)) #isotny wp�yw

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

boxplot(data$avg_score~data$country) #r�nice spore - mo�na by dopasowa� kurs do kraju
View(summarise(group_by(data,country),mean(avg_score,na.rm=T)))
summarise(group_by(data,country),mean(avg_score,na.rm=T))
summarise(group_by(data,country),count(country,na.rm=T)         
          
#czy zdolno�ci ucznia maj� wp�yw na wyniki

summarise(group_by(data,learner_id),mean(avg_score,na.rm=T))

#zale�no�� wynik�w od post�powania wed�ug rad ekspert�w
ggplot(data = data) + 
  geom_point(mapping = aes(x = data$avg_score, y = data$inv_rate)) + 
  geom_smooth(mapping = aes(x = data$avg_score, y = data$inv_rate))

ggplot(data = data) + 
  geom_bar(mapping = aes(x = cut(data$inv_rate,20))) +
  coord_flip() +
 ggtitle("How people uses the expert advice")

#�rednie uzyskiwane wyniki - liczno�� os�b

ggplot(data = data) + 
  geom_bar(mapping = aes(x = cut(data$avg_score,20))) +
  coord_flip() +
  ggtitle("�rednie uzyskiwane wyniki")


#stopie� kompletaacji a wynik
plot(data$completion,data$avg_score)

ggplot(data = data) + 
  geom_point(mapping = aes(x =data$completion , y = data$avg_score),cex=0.75)


#czy zalecenia expert�w s�uszne
plot(data$inv_rate,data$avg_score)
aov(data$avg_score~data$inv_rate)
summary(aov(data$avg_score,data$inv_rate))

#w kt�rych krajach popularne
summarise(group_by(data,country))
summary(group_by(data,country))
count(group_by(data,country))
View(count(group_by(data,country))) #reklama

#�rednia ilo�� rozdz ucznia - zr�b wykres ko�owy

# ile uczni�w wybiera dany rozdzia�
ggplot(data=data) + 
  geom_boxplot(mapping=(aes(x = learner_id, y = unit())))
  
table(data$learner_id)
barplot(table(data$learner_id))
table(table(data$learner_id))
barplot(table(table(data$learner_id)))

#g�sto��
hist(data$avg_score,prob=T)
lines(density(data$avg_score,na.rm=T))
summary(data)

#ile osob sie stosuje w jakim proc
table(cut(data$inv_rate,20))
plot(table(cut(data$inv_rate,20)))


# r�nice w poziomie wynik�w dla nauczyciela i bez dla wysokich wynik�w
filter(data,avg_score>=0.8)
count(group_by(filter(data,avg_score>=0.8),in_course))


