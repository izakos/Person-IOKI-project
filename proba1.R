
#za³adowanie danych

library(readr)
data <- read_csv("C:/Users/HP/Desktop/wa¿ne/audition_project/data.csv")
View(data)

#faktoryzacja 
data$learner_id=factor(data$learner_id)
data$unit=factor(data$unit)

anova_avg_inv=aov(data$avg_score~inv_rate)
summary(aov(data$avg_score~inv_rate)) #p-wart < 0,05 - avg_score istotnie zale¿y od inv_rate (postepowania wed³ug zaleceñ ekspertów)

anova_avg_inv2=aov # dodaj inn¹ hipoteze alternatywna
summary(anova_avg_inv)

anova=aov(avg_score~unit,data)
summary(anova) # p-wart <0,05 zale¿y - Ÿle 

anova2=aov(data$avg_score~.,data=data) # ???
summary(anova2)

anova3=aov(avg_score~completion,data)
summary(anova3) # istotne 


anova4=aov(avg_score~in_course,data)
summary(anova4)

drop_na(data) #10 bez niczego

group_size(group_by(data,unit))# iloœæ poszczególnych rozdzia³ów odpaleñ

mean(data$avg_score[data$in_course!='t'],na.rm=T) #0,7551668

mean(data$avg_score[data$in_course=='t']) #0,7951297

plot(group_size(group_by(data,unit)))

