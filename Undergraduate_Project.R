setwd("C:/Users/Shit Happens/Desktop/UGHP/UGHPtools")
phenotype=read.csv("Phenotyping_data.csv")
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

UGHP_data = merge(phenotype, genotype, by = c("SeedID","Geno_Cross"))

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

#saving new file
write.csv(UGHP_data, "UGHP_data.csv")




#isolate selection
UGHP_data2=subset(UGHP_data, Isolate == "1.2.16" | Isolate == "MEA PGG"| Isolate == "Control"| Isolate == "1.3.19"| Isolate == "Noble Rot")
UGHP_data2=subset(UGHP_data2, !Isolate == "NA" | !Isolate == " ")


UGHP_data2=subset(a2, !Note == "NA")
fail=subset(UGHP_data2, Note == "fail")
good=subset(UGHP_data2, Note == "good")


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
table(UGHP_data2$SeedID, na.rm = TRUE)
Tray3= subset(UGHP_data2, Tray == "3")
Tray3_Growth=subset(Tray3, Total_Area.cm. <= "400" )

Tray4= subset(UGHP_data2, Tray == "4")
Tray4_Growth=subset(Tray4, Total_Area.cm. <= "400" )

Tray5= subset(UGHP_data2, Tray == "5")
Tray5_Growth=subset(Tray5, !Total_Area.cm. == "0")


Tray6= subset(UGHP_data2, Tray == "6")
Tray6_Growth=subset(Tray6, !Total_Area.cm. == "0" )

Tray7= subset(UGHP_data2, Tray == "7")
Tray7_Growth=subset(Tray7, !Total_Area.cm. == "0" )

Tray8= subset(UGHP_data2, Tray == "8")
Tray8_Growth=subset(Tray7, !Total_Area.cm. == "0" )






#Making plots for Tray3 based on SeedID
library("ggplot2")
install.packages("dplyr")
library("dplyr")


Tray3_Growth$Date= as.numeric(Tray3_Growth$Date)
qplot(DAG, Total_Area.cm.,data= Tray3_Growth, geom = "line", color = SeedID, main = "Tray 3")


#Makingp lot for Tray4 based on SeedID
Tray4_Growth$Date= as.numeric(Tray4_Growth$Date)
qplot(DAG, Total_Area.cm.,data= Tray4_Growth, geom = "line", color = SeedID, main = "Tray 4" )

#Making plot for Tray5 based on SeedID
Tray5_Growth$Date= as.numeric(Tray5_Growth$Date)
qplot(DAG, Total_Area.cm.,data= Tray5_Growth, geom = "line", color = SeedID, main = "Tray 5" )

#Making plot for Tray6 based on SeedID = EMPTY
Tray6_Growth$Date= as.numeric(Tray6_Growth$Date)
qplot(DAG, Total_Area.cm.,data= Tray6_Growth, geom = "point", color = SeedID, main = "Tray 6" )

#Making plot for Tray7 based on SeedID= EMPTY- sorta
Tray7_Growth$Date= as.numeric(Tray7_Growth$Date)
qplot(DAG, Total_Area.cm.,data= Tray7_Growth, geom = "point", color = SeedID, main = "Tray 7" )

#Making plot for Tray8 based on SeedID= EMPTY
Tray8_Growth$Date= as.numeric(Tray8_Growth$Date)
qplot(DAG, Total_Area.cm.,data= Tray8_Growth, geom = "line", color = SeedID, main = "Tray 8" )


#Making subsets within each tray
Tray3_Growth_A=subset(Tray3_Growth, SeedId == "2A31"|SeedID== "2A41"|SeedID == "2A61"|SeedID== "")


unique(Tray3_Growth$Position)

colnames(UGHP_data)
