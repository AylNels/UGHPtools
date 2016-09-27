setwd("C:/Users/Shit Happens/Desktop/UGHP/UGHPtools")
phenotype=read.csv("Phenotyping_data2.csv")

genotype=read.csv("Genotype_data.csv")

#merging the data sets
colnames(phenotype)
colnames(genotype)
genotype$Sample_Name=NULL
genotype$Directory= NULL
genotype$Plate_Position= NULL
genotype$Column= NULL
genotype$X=NULL
colnames(genotype)
table(genotype$QC)
table(genotype$Note)
#optional code only required with formating issues, gone in Phenotyping_data2.csv file 
#phenotype[7:73]= list(NULL)




UGHP_data = merge(phenotype, genotype, by = c("SeedID","Geno_Cross"))

##TEST
colnames(UGHP_data)
typeof(UGHP_data$Date)
typeof(UGHP_data$germ_date)
head(UGHP_data$Date, 10)

head(UGHP_data$germ_date, 10)





#Making the date to be set as.date in readable function.
UGHP_data$Date <- as.Date(UGHP_data$Date, "%m/%d/%Y")
UGHP_data$germ_date<-as.Date(UGHP_data$germ_date, "%m/%d/%Y")

#confirm that DAG works
UGHP_data$DAG = UGHP_data$Date-UGHP_data$germ_date
head(UGHP_data$DAG, 10)




#confirming, QC = geno cross is note is TRUE
#Start with duplication
UGHP_data$QC_Notes=UGHP_data$QC
#New column Parameters
UGHP_data$QC=NULL
UGHP_data$QC=UGHP_data$Geno_Cross

#consider ifelse statements
#fix this part its F'd-up
levels(UGHP_data$QC)= c(levels(QC), "NA")
UGHP_data[UGHP_data$Geno_Cross ]
UGHP_data$QC<-if(UGHP_data$Note == "good") {print(UGHP_data$Geno_Cross)} else {print(NA)} 

UGHP_data$QC<-if(UGHP_data$Note == "fail") {print("NA")}

#closest statement
UGHP_data$QC<- ifelse(UGHP_data$Note == "good", TRUE, NA)

    # takes x from specific to general category of religion
  
  
#saving NEW FILE
write.csv(UGHP_data, "UGHP_data.csv")

read.csv("UGHP_data.csv")


#isolate selection
UGHP_data=subset(UGHP_data, Isolate == "1.2.16" | Isolate == "MEA PGG"| Isolate == "Control"| Isolate == "1.3.19"| Isolate == "Noble Rot")
UGHP_data=subset(UGHP_data, !Isolate == "NA" | !Isolate == " ")
UGHP_data=subset(UGHP_data, Note == "good")

UGHP_data2=subset(a2, !Note == "NA")
fail=subset(UGHP_data, Note == "fail")
good=subset(UGHP_data, Note == "good")


#Good & fail genotypes
plot(good$Geno_Cross, main = "good", na.rm= TRUE)
par(mfrow= c(2,1))
plot(fail$Geno_Cross, main = "fail")

#good & fail
plot(good$Isolate)
table(good$Isolate)
Plot(good$X96_Well_Plate_1)

A212 = subset(UGHP_data, SeedID == "2A12")

#percentage that worked for each genotype
barplot(a2$note)

table(good$Isolate)
plot(good$Flat)

#Tray divisions set up
#NEED TO MAKE THE DATES ACTUAL DATES (NOT NUMERIC)
#NEED TO MAKE SEEDid's INTO ACTUAL GENO_CROSS & GOOD VS. FAIL
#Place each tray subset with its corresponding tray
Tray3= subset(UGHP_data, Tray =="3")
Tray4= subset(UGHP_data, Tray == "4")
Tray5= subset(UGHP_data, Tray == "5")
Tray6= subset(UGHP_data, Tray == "6")
Tray7= subset(UGHP_data, Tray == "7")
Tray8= subset(UGHP_data, Tray == "8")



#Making plots for Tray3 based on SeedID
library("ggplot2")
library("dplyr")


#TESTING: TRAY 3 finding dips in growth & other errors/ cleaning up data by genotype
#table(Tray3$Geno_Cross)
Tray3_test=subset(Tray3, Geno_Cross == "B - Col/ Ws")
qplot(DAG, Total_Area.cm.,data= Tray3_test, geom = "line", color = SeedID, main = "Tray 3 test")

Tray3_test=subset(Tray3, Geno_Cross == "C - Ws/Ler")
qplot(DAG, Total_Area.cm.,data= Tray3_test, geom = "line", color = SeedID, main = "Tray 3 test")

Tray3_test=subset(Tray3,  Geno_Cross == "D - Ler/ Ws")
qplot(DAG, Total_Area.cm.,data= Tray3_test, geom = "line", color = SeedID, main = "Tray 3 test")

Tray3_test=subset(Tray3,  Geno_Cross == "F - Col/Ler")
qplot(DAG, Total_Area.cm.,data= Tray3_test, geom = "line", color = SeedID, main = "Tray 3 test")

Tray3_test=subset(Tray3,  Geno_Cross == "G - Ler/Col")
qplot(DAG, Total_Area.cm.,data= Tray3_test, geom = "line", color = SeedID, main = "Tray 3 test")

table(Tray3$Geno_Cross)

##TESTING: TRAY 3 summary
#table(Tray3$Geno_Cross)
table(Tray3$Geno_Cross)
qplot(DAG, Total_Area.cm.,data= Tray3, geom = "line", color = SeedID, main = "Tray 3")


#TESTING: TRAY 4 finding dips in growth & other errors/ cleaning up data by genotype
#table(Tray4$Geno_Cross)
Tray4_test=subset(Tray4, Geno_Cross == "A - Ws/Col")
qplot(DAG, Total_Area.cm.,data= Tray4_test, geom = "line", color = SeedID, main = "Tray 4 test")

Tray4_test=subset(Tray4, Geno_Cross == "B - Col/ Ws")
qplot(DAG, Total_Area.cm.,data= Tray4_test, geom = "line", color = SeedID, main = "Tray 4 test")

Tray4_test=subset(Tray4,  Geno_Cross == "D - Ler/ Ws")
qplot(DAG, Total_Area.cm.,data= Tray4_test, geom = "line", color = SeedID, main = "Tray 4 test")

Tray4_test=subset(Tray4,  Geno_Cross == "F - Col/Ler")
qplot(DAG, Total_Area.cm.,data= Tray4_test, geom = "line", color = SeedID, main = "Tray 4 test")

Tray4_test=subset(Tray4,  Geno_Cross == "G - Ler/Col")
qplot(DAG, Total_Area.cm.,data= Tray4_test, geom = "line", color = SeedID, main = "Tray 4 test")

##TESTING: TRAY 4 summary
table(Tray4$Geno_Cross)
qplot(DAG, Total_Area.cm.,data= Tray4, geom = "line", color = SeedID, main = "Tray 4" )


#TESTING: TRAY 5 finding dips in growth & other errors/ cleaning up data by genotype
#table(Tray5$Geno_Cross)
Tray5_test=subset(Tray5,  Geno_Cross == "D - Ler/ Ws")
qplot(DAG, Total_Area.cm.,data= Tray5_test, geom = "line", color = SeedID, main = "Tray 5 test")

Tray5_test=subset(Tray5,  Geno_Cross == "F - Col/Ler")
qplot(DAG, Total_Area.cm.,data= Tray5_test, geom = "line", color = SeedID, main = "Tray 5 test")

Tray5_test=subset(Tray5,  Geno_Cross == "G - Ler/Col")
qplot(DAG, Total_Area.cm.,data= Tray5_test, geom = "line", color = SeedID, main = "Tray 5 test")

##TESTING: TRAY 5 summary
table(Tray5$Geno_Cross)
qplot(DAG, Total_Area.cm.,data= Tray5, geom = "line", color = SeedID, main = "Tray 5" )


#TESTING: TRAY 6 finding dips in growth & other errors/ cleaning up data by genotype
#table(Tray6$Geno_Cross)
Tray6_test=subset(Tray6, Geno_Cross == "A - Ws/Col")
qplot(DAG, Total_Area.cm.,data= Tray6_test, geom = "line", color = SeedID, main = "Tray 6 test")

Tray6_test=subset(Tray6, Geno_Cross == "B - Col/ Ws")
qplot(DAG, Total_Area.cm.,data= Tray6_test, geom = "line", color = SeedID, main = "Tray 6 test")

Tray6_test=subset(Tray6,  Geno_Cross == "F - Col/Ler")
qplot(DAG, Total_Area.cm.,data= Tray6_test, geom = "line", color = SeedID, main = "Tray 6 test")

Tray6_test=subset(Tray6,  Geno_Cross == "G - Ler/Col")
qplot(DAG, Total_Area.cm.,data= Tray6_test, geom = "line", color = SeedID, main = "Tray 6 test")

##TESTING: TRAY 6 summary
table(Tray6$Geno_Cross)
qplot(DAG, Total_Area.cm.,data= Tray6, geom = "line", color = SeedID, main = "Tray 6" )


#TESTING: TRAY 7 finding dips in growth & other errors/ cleaning up data by genotype
#table(Tray7$Geno_Cross)
Tray7_test=subset(Tray7, Geno_Cross == "A - Ws/Col")
qplot(DAG, Total_Area.cm.,data= Tray7_test, geom = "line", color = SeedID, main = "Tray 7 test")

Tray7_test=subset(Tray7, Geno_Cross == "B - Col/ Ws")
qplot(DAG, Total_Area.cm.,data= Tray7_test, geom = "line", color = SeedID, main = "Tray 7 test")

Tray7_test=subset(Tray7,  Geno_Cross == "D - Ler/ Ws")
qplot(DAG, Total_Area.cm.,data= Tray7_test, geom = "line", color = SeedID, main = "Tray 7 test")

Tray7_test=subset(Tray7,  Geno_Cross == "F - Col/Ler")
qplot(DAG, Total_Area.cm.,data= Tray7_test, geom = "line", color = SeedID, main = "Tray 7 test")

Tray7_test=subset(Tray7,  Geno_Cross == "G - Ler/Col")
qplot(DAG, Total_Area.cm.,data= Tray7_test, geom = "line", color = SeedID, main = "Tray 7 test")

##TESTING: TRAY 7 summary
table(Tray7$Geno_Cross)
qplot(DAG, Total_Area.cm.,data= Tray7, geom = "line", color = SeedID, main = "Tray 7" )


#TESTING: TRAY 8 finding dips in growth & other errors/ cleaning up data by genotype
Tray8_test=subset(Tray8, Geno_Cross == "A - Ws/Col")
qplot(DAG, Total_Area.cm.,data= Tray8_test, geom = "line", color = SeedID, main = "Tray 8 test")

Tray8_test=subset(Tray8, Geno_Cross == "B - Col/ Ws")
qplot(DAG, Total_Area.cm.,data= Tray8_test, geom = "line", color = SeedID, main = "Tray 8 test")

Tray8_test=subset(Tray8, Geno_Cross == "C - Ws/Ler")
qplot(DAG, Total_Area.cm.,data= Tray8_test, geom = "line", color = SeedID, main = "Tray 8 test")

Tray8_test=subset(Tray8,  Geno_Cross == "D - Ler/ Ws")
qplot(DAG, Total_Area.cm.,data= Tray8_test, geom = "line", color = SeedID, main = "Tray 8 test")

Tray8_test=subset(Tray8,  Geno_Cross == "F - Col/Ler")
qplot(DAG, Total_Area.cm.,data= Tray8_test, geom = "line", color = SeedID, main = "Tray 8 test")

Tray8_test=subset(Tray8,  Geno_Cross == "G - Ler/Col")
qplot(DAG, Total_Area.cm.,data= Tray8_test, geom = "line", color = SeedID, main = "Tray 8 test")

##TESTING: TRAY 8 summary
table(Tray8$Geno_Cross)
qplot(DAG, Total_Area.cm.,data= Tray8, geom = "line", color = SeedID, main = "Tray 8" )




##TESTING: Seed geno_cross
qplot(DAG, Total_Area.cm.,data= Tray6, geom = "line", color = Geno_Cross, main = "Tray 6" )

### Growth curve for individual plants, color'd by genotype, eacch plant shown in graph....
ggplot(UGHP_data,aes(x = DAG, y = Total_Area.cm., colour= Geno_Cross, group=SeedID)) + geom_line ()+ ggtitle("Growth curve of individual plants grouped")

#Tray 3
ggplot(Tray3,aes(x = DAG, y = Total_Area.cm., colour= Geno_Cross, group=SeedID)) + geom_line ()+ ggtitle("Tray3:Growth curve of individual plants grouped")


###Growth of curve for individual plants by tray
ggplot(UGHP_data,aes(x = DAG, y = Total_Area.cm., colour= Tray, group=SeedID)) + geom_line ()+ ggtitle("Growth curve of individual plants grouped")

colnames(Tray3)


#Making subsets within each tray
Tray3_Growth_A=subset(Tray3_Growth, SeedId == "2A31"|SeedID== "2A41"|SeedID == "2A61"|SeedID== "")


unique(Tray3_Growth$Position)

colnames(UGHP_data)


####