getwd()

# Importacion de datos y paquetes ----------------------------------------------------
clientes<-read.csv("Clientes_Banco_Reducido.csv",header = TRUE,sep = ",")
install.packages("tidyr")
library(tidyr)

# Ejercicio 1 -------------------------------------------------------------
#1.1
clientes
#1.2
class(clientes)
#1.3
str(clientes)
#1.4
head(clientes,18)
#1.5
tail(clientes,25)
#1.6
dim(clientes)
dim.data.frame(clientes)
#1.7
colnames(clientes)
#1.8
sum(colSums(is.na(clientes))>0)
sum(colSums(is.na(clientes)==TRUE)>0)
#1.9

class(clientes$Marital_Status)
unique(clientes$Marital_Status)

Marital_vector<-c(clientes$Marital_Status)
Marital_levels<-c("Married","Single","Unknown","Divorced","Widower")
Marital_labels<-c("M","S","UNK","D","W")
Marital_factor<-factor(Marital_vector,levels = Marital_levels,labels = Marital_labels)
clientes$Marital_Status=Marital_factor
clientes
class(clientes$Marital_Status)
#1.10


class(clientes$Card_Category)
unique(clientes$Card_Category)

Card_vector<-c(clientes$Card_Category)
Card_levels<-c("Platinum","Gold","Silver","Blue")
Card_labels<-c("Platino_1","Oro_2","Plata_3","Azul_4")
Card_factor<-factor(Card_vector,exclude = NA,ordered = TRUE,levels = Card_levels,labels = Card_labels)
clientes$Card_Category=Card_factor
class(clientes$Card_Category)
clientes


#1.11
mean(clientes$Customer_Age,na.rm = TRUE)
#1.12
vector_medio<-c((mean(clientes$Months_on_book,na.rm = TRUE)),(mean(clientes$Credit_Limit,na.rm = TRUE)))
vector_medio

#1.13
sum((lapply(clientes,is.numeric))==TRUE)
eee<-lapply(clientes,is.numeric)==TRUE
vector_numeric<-eee[eee==TRUE]
vector_numeric

#1.14
columnas_numericas<-clientes[1,lapply(clientes,is.numeric)==TRUE]
columnas_numericas

quitar_na_columnas<-function(x){
  columna_sin_na<-na.omit(x)
  return(mean(columna_sin_na))
}
lapply(columnas_numericas, quitar_na_columnas)


#He creado esta funcion para que se pueda aplicar la funcion mean correctamente, ya que con NAs daba errores
colMeans(columnas_numericas,na.rm = TRUE,dims = 1)
#Aqui con la funcionn colmeans

#1.15
clientes[1:100,1:((ncol(clientes))-4)]
clientes[1:100,1:12]

ncol(clientes)
#1.16

clientes[-(nrow(clientes)):((-(nrow(clientes)+11))+nrow(clientes)),-ncol(clientes):-((ncol(clientes))+3)]
clientes[-10127:-11,-16:-13]

nrow(clientes)
ncol(clientes)
#1.17
quantile(clientes$Customer_Age,na.rm = TRUE)
#1.18
quantile(clientes$Customer_Age,probs = seq(0,1,0.10),na.rm = TRUE)
str(clientes$Customer_Age)
#1.19
lapply(clientes,summary)
#1.20
length(unique(clientes$Income_Category))
#1.21
nrow(clientes["Months_on_book">30])
#1.22
clientes$Attrition_Flag
cambio_de_valores<-function(vector_columna){
  for(i in 1:length(vector_columna)){
    if (vector_columna[i]=="Existing Customer"){
      vector_columna[i]=0
    }
    else{
      vector_columna[i]=1
    }
  }
  return(vector_columna)
}
clientes$Attrition_Flag<-cambio_de_valores(clientes$Attrition_Flag)
clientes
clientes$Attrition_Flag[1]
help(sort)
#1.23
clientes$Months_on_book
sort(clientes$Months_on_book,decreasing = TRUE,na.last = TRUE)
#1.24
help("order")
clientes <- clientes[order(clientes$Total_Relationship_Count), ]
head(clientes,25)
#1.25
clientes_avg_month_book<-clientes[clientes["Months_on_book"]>(mean(clientes$Months_on_book)),]
clientes_avg_month_book$CLIENTNUM
clientes$CLIENTNUM[clientes$Months_on_book>(mean(clientes$Months_on_book))]

which(clientes$Months_on_book>(mean(clientes$Months_on_book)))
#1.26
boxplot(clientes$Customer_Age)
#Tiene dos valores atipicos
#1.27
hist(clientes$Months_Inactive_12_mon)
#1.28
meses_de_antiguedad<-function(meses,limite){
  if(meses<=limite){
    return("Cliente reciente")
  
  }
  else{
    return("Cliente")
  }
}
meses_de_antiguedad(16,36)
meses_de_antiguedad(46,36)

#1.29
clientes$cliente_antiguedad<-lapply(clientes$Months_on_book,meses_de_antiguedad,limite=36)
head(clientes,5)


# Ejercicio 2 -------------------------------------------------------------

#2.1
clientes_2<-unite(clientes,id_cliente_antiguedad,c("CLIENTNUM","cliente_antiguedad"),sep ="-" )

head(clientes_2,10)

#2.2
id_unico_<-function(vector_columna2){
  for(i in 1:length(vector_columna2)){
    vector_columna2[i]=i
  }
  return(vector_columna2)
}
clientes$ID_Unico<-id_unico_(clientes$CLIENTNUM)
head(clientes,10)
#2.3
clientes_long<-clientes[,c("CLIENTNUM","Card_Category","Customer_Age")]
clientes_long<-spread(clientes_long,key = Card_Category,value = Customer_Age)
head(clientes_long,20)
#2.4
clientes_wide<-gather(clientes_long,value=Customer_Age,key =Card_Category,c( "Azul_1" ,"Plata_2", "Oro_3", "Platino_4"))
clientes_wide
#2.5
unique(clientes$Education_Level)
head(clientes,10)
df_calculos<-clientes[,c("Income_Category","Education_Level")]
df_calculos <- df_calculos %>%
  group_by(Income_Category, Education_Level) %>%
  summarise(n = n())
df_calculos

#2.6
df_calculos_ordenado=arrange(df_calculos,desc(n))
df_calculos_ordenado
#2.7
head(clientes,10)
sum_tram_inc<-clientes[,c("Income_Category","Credit_Limit")]
sum_tram_inc<-group_by(sum_tram_inc,Income_Category) %>% summarise(Suma_credito=sum(Credit_Limit))
sum_tram_inc
#2.8
pers_dependentes<-clientes[,c("CLIENTNUM","Gender","Dependent_count")] %>% group_by(Gender,CLIENTNUM) %>%
  summarise(Numero_de_personas=sum(Dependent_count))%>%
  filter(Gender=="F",Numero_de_personas==max(Numero_de_personas)) 

pers_dependentes
#2.9
head(clientes,10)

ultimofiltro<-clientes[,c("Marital_Status","Attrition_Flag","Dependent_count","Months_Inactive_12_mon","Contacts_Count_12_mon","Credit_Limit")] %>% filter(Marital_Status=="M",Attrition_Flag==1,Dependent_count>=3) %>% 
  summarise(MAx_meses_inact=max(Months_Inactive_12_mon),Media_contactos_12_meses=mean(Contacts_Count_12_mon),Suma_total_credito=sum(Credit_Limit))
ultimofiltro
clientes[clientes$Attrition_Flag==0 & clientes$Gender=="M",]
clientes
