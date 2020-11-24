#Problem2
library(tidyr)
library(dplyr)
library(ggplot2)
v1 <- c(11.10,11.22,11.29,11.49)
v2 <- c(11.32,11.40,11.71)
v3 <- c(11.60,11.78,12.05)
v4 <- c(10.61,10.88,11.12,11.24,11.43)
v5 <- c(10.92,11.20,11.30,11.62,11.70)
v6 <- c(11.70,11.79,11.91,12.15)
v7 <- c(11.33,11.41,11.62,12.15,12.30)
v8 <- c(11.32,11.65,11.96,12.15)
v9 <- c(11.54,11.89,12.04)
v10 <- c(10.93,11.01,11.08,11.12,11.28,11.37)
v11 <- c(11.35,11.43,11.50,11.57,11.92)
v12 <- c(11.95,12.01,12.25,12.30,12.39)
b1 <- rep("Rib16",4)
b2 <- rep("Gastralia",3)
b3 <- rep("Gastralia",3)
b4 <- rep("Dorsalvertebra",5)
b5 <- rep("Dorsalvertebra",5)
b6 <- rep("Femur",4)
b7 <- rep("Tibia",5)
b8 <- rep("Metatarsal",4)
b9 <- rep("Phalange",3)
b10 <- rep("Proximalcaudal",6)
b11 <- rep("Mid-caudal",5)
b12 <- rep("Distalcaudal",5)
bone <- c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12)
oxygen <- c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12)
bone_oxygen <- list(bone=bone,oxygen=oxygen)
bone_oxygen2 <-as_tibble(bone_oxygen)
anova_one_way <- aov(oxygen ~ bone, data = bone_oxygen2)
summary(anova_one_way)

# MingYANG recommended：
# if you must want to input the by hand ,try this below:
Bones<-"initiate"
Bones[1:4]<-"Rib_16"
Bones[5:7]<-"Gastralia"
Bones[8:10]<-"Gastralia2"
Bones[11:15]<-"Dorsal_vertebra"
Bones[16:20]<-"Dorsal_vertebra2"
Bones[21:24]<-"Femur"
Bones[25:29]<-"Tibia"
Bones[30:33]<-"Metatarsal"
Bones[34:36]<-"Phalange"
Bones[37:42]<-"Proximal_caudal"
Bones[43:47]<-"Mid_caudal"
Bones[48:52]<-"Distal_caudal"

Rib_16<-c(11.10, 11.22, 11.29, 11.49)
Gastralia<-c(11.32,	11.40,	11.71)
Gastralia2<-c(11.60,	11.78,	12.05)
Dorsal_vertebra<-c(10.61,	10.88,	11.12,	11.24,	11.43)
Dorsal_vertebra2<-c(10.92,	11.20, 11.30,	11.62,	11.70)
Femur<-c(11.70,	11.79,	11.91,	12.15)
Tibia<-c(11.33,	11.41,	11.62,	12.15,	12.30)
Metatarsal<-c(11.32,	11.65,	11.96,	12.15)
Phalange<-c(11.54,	11.89,	12.04)
Proximal_caudal<-c(10.93,	11.01,	11.08,	11.12,	11.28,	11.37)
Mid_caudal<-c(11.35,	11.43,	11.50,	11.57,	11.92)
Distal_caudal<-c(11.95,	12.01,	12.25,	12.30,	12.39)

oxygen<-c(Rib_16,Gastralia,Gastralia2,Dorsal_vertebra,Dorsal_vertebra2,Femur,Tibia,Metatarsal,Phalange,Proximal_caudal,Mid_caudal,Distal_caudal)
bone_oxygen<-cbind(Bones,oxygen)
bone_oxygen_tb1<-as_tibble(bone_oxygen)

bone_oxygen_tb1 %>%
  group_by(Bones)%>%
  ggplot(aes(x = Bones, y = oxygen, fill = Bones)) +
  geom_boxplot() +
  theme_classic()
# and don`t forget to draw a qualified picture!
# the end
