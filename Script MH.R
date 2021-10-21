#librerias a instalar (ver en packages si están cargadas)------

library(tidyverse)
library(DataExplorer)
library(googlesheets4)
library(ggplot2)
library(forcats)
library(gargle)
library(scales)
library(dplyr)
library(readr)
library(readxl)


# Datos | cargamos encuesta bajo el nombre "estudio"--------

estudio <- read.csv("Estudio de Modalidad Hibrida de Trabajo.csv",
                    encoding = "UTF-8",
                    sep = ";")

# trabajar desde googlesheet (me tira error)

estudio_googlesheets <- googlesheets4::read_sheet("1YDLbgdpYBAJMHiDq7tx9zjs3F76mnICnxIBW-7tGC0c")
estudio_googlesheets


# install.packages("forcats")
# install.packages("gargle")
# install.packages("scales")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("DataExplorer")
# install.packages("googlesheets4")
# install.packages("readxl")

# para ver nombres de las columnas o variables -------

names(estudio)
variable.names(estudio)
colnames(estudio)

#para ver excel------------

View(estudio)

# para ver los datos de una variable-------

table(estudio$estudio)

# renombrar las variables ------

estudio <- estudio %>%
  rename(email = Direcci.f3.n.de.mail,
         estudio = Nivel.de.Estudios,
         provincia = X.U.00BF.D.f3.nde.vivis.,
         modalidad_preferida = X.U.00BF.Qu.e9..modalidad.prefer.ed.s.,
         busqueda_pasado = X.U.00BF.Buscabas.activamente.un.nuevo.trabajo.antes.de.la.Pandemia.Covid.19.,
         busqueda_hoy = Actualmente..U.00BF.est.e1.s.buscando.trabajo.activamente.,
         rechazo_oferta = X.U.00BF.Rechazar.ed.as.alguna.oferta.laboral.por.no.contar.con.la.modalidad.teletrabajo.,
         profesion_remota = Tu.profesi.f3.n..U.00BF.se.puede.hacer.de.forma.remota.,
         trabaja = X.U.00BF.Actualmente.Trabajas.,
         rubro = Selecciona.el.rubro.de.tu.empresa,
         puesto = Selecciona.tu.puesto.de.trabajo,
         empleador_tt_antes = X.U.00BF.Ofrec.ed.a.teletrabajo.antes.de.la.Pandemia.Covid.19.,
         teletrabajando_hoy = Actualmente..U.00BF.estas.haciendo.teletrabajo.,
         acepta_por_teletrabajo = Si.te.ofrecieran.el.mismo.trabajo.con.modalidad.hibrida.en.otra.empresa..U.00BF.aceptar.ed.as.,
         dias_preferencia = X.U.00BF.Cu.e1.ntos.d.ed.as.te.gustar.ed..U.00AD.a.trabajar.de.forma.presencial.y.cuantos.en.teletrabajo.,
         valora_balance = Ordena.las.opciones.seg.fa.n.tu.preferencia..Siendo.5.el.mas.valorado.y.1.el.menos.valorado..Que.te.gusta.o.valoras.mas.de.la.modalidad.hibrida.de.trabajo....Balance.entre.vida.laboral.y.personal..,
         valora_lugar = Ordena.las.opciones.seg.fa.n.tu.preferencia..Siendo.5.el.mas.valorado.y.1.el.menos.valorado..Que.te.gusta.o.valoras.mas.de.la.modalidad.hibrida.de.trabajo....Elegir.tu.lugar.de.trabajo.,
         valora_flexibilidad = Ordena.las.opciones.seg.fa.n.tu.preferencia..Siendo.5.el.mas.valorado.y.1.el.menos.valorado..Que.te.gusta.o.valoras.mas.de.la.modalidad.hibrida.de.trabajo....Flexibilidad.horaria..,
         valora_viaticos = Ordena.las.opciones.seg.fa.n.tu.preferencia..Siendo.5.el.mas.valorado.y.1.el.menos.valorado..Que.te.gusta.o.valoras.mas.de.la.modalidad.hibrida.de.trabajo....Ahorro.en.viáticos..,
         valora_compensaciones = Ordena.las.opciones.seg.fa.n.tu.preferencia..Siendo.5.el.mas.valorado.y.1.el.menos.valorado..Que.te.gusta.o.valoras.mas.de.la.modalidad.hibrida.de.trabajo....Compensaciones..reintegro.internet..silla.ergonómica..escritorio..kit.seguridad..,
         resigna_sueldo = X.U.00BF.Estar.ed.as.dispuesto.a.resignar.un.5..de.tu.sueldo.para.continuar.con.una.modalidad.hibrida.teletrabajo.)

# Agrega columna de id
estudio$id <- rep(1:nrow(estudio))

estudio <- estudio %>%
  rename(edad = Edad,
         genero = Genero,
         marca.temporal = Marca.temporal) %>% 
  select(id, everything())        # Para poner primero la columna id y luego el resto


# quitar datos nulos con:( , na.rm=T)

# contar datos nulos:

sum(!is.na(estudio$X))
sum(!is.na(estudio$X.U.00BF.Cu.e1.ntos.d.ed.as.te.gustar.ed..U.00AD.a.trabajar.de.forma.presencial.y.cuantos.en.teletrabajo..1))
length(estudio$X.U.00BF.Cu.e1.ntos.d.ed.as.te.gustar.ed..U.00AD.a.trabajar.de.forma.presencial.y.cuantos.en.teletrabajo..1)
range(estudio$X.U.00BF.Cu.e1.ntos.d.ed.as.te.gustar.ed..U.00AD.a.trabajar.de.forma.presencial.y.cuantos.en.teletrabajo..1, na.rm = T)
mean(estudio$Edad, na.rm=T)
summary(estudio)

# Elimina columnas con datos nulos
estudio <- estudio %>% 
  select(id:resigna_sueldo)

# Pivotear el dataseet ----

names(estudio)
glimpse(estudio)

# Creo una tabla temporal
est_temp <- estudio

# Convierto las columnas numéricas en character
est_temp <- est_temp %>% 
  mutate_if(is.numeric, as.character)

# Pivoteo el dataset
estudio_long <- est_temp %>% 
  pivot_longer(cols = c(8:25),
               names_to = "pregunta",
               values_to = "respuesta")

# Elimino la tabla temporal
rm(est_temp)



#ver algunos datos----

glimpse(estudio)

# Cuenta la cantidad de personas por género y rango de edad
estudio %>% 
  select(edad, genero) %>% 
  group_by(genero, edad) %>%
  tally()

estudio %>% 
  count(edad, genero, estudio)

table(estudio$acepta_por_teletrabajo)

# hacer gráfico:

ggplot(estudio, aes(x = estudio, na.rm=T)) +
  geom_bar()

ggplot(estudio, aes(x = genero, na.rm=T)) +
  geom_bar()

ggplot(estudio, aes(x = edad, na.rm=T)) +
  geom_bar()

ggplot(estudio, aes(x = modalidad_preferida, na.rm=T)) +
  geom_bar()

#ggplot(estudio, aes(x = modalidad_preferida, na.rm=T)) +
#  geom_bar()

# guardar gráfico en PNG --------

ggsave(filename = "nivel.de.estudio.png")
ggsave(filename= "genero.png")
ggsave(filename= "edad.png")

#analizar algunos datos-----

est3 <- estudio %>%
  filter(!is.na(acepta_por_teletrabajo)) %>%
  mutate(acepta_x_tele = if_else(acepta_por_teletrabajo == "SI", 1, 0))

est3
create_report(est3, y = "acepta_x_teletrabajo")

# abreviar y cambiar nombres carácteres (sergio)-------

est4 <- estudio %>%
  mutate(modalidad_preferida = factor(modalidad_preferida,
                                    levels = c("Hibrida (mixto presencial y teletrabajo)",
                                               "Presencial", "Teletrabajo"),
                                    labels = c("mh", "pres", "tt")))

summary(est4$modalidad_preferida)
#table(est4$modalidad_preferida) Es redundante

# convertir variables con carácteres a números---(apunte arturo) -----

#Edad:

unique(estudio$edad)
niv.tf<-c("18 a 27", "28 a 40", "41 a 52", "Mayo de 53")
estudio$edad<-factor(estudio$edad, labels=niv.tf)
summary(estudio$edad)

#Género:
  
unique(estudio$genero)
niv.tf<-c("F","M")
estudio$genero<-factor(estudio$genero, labels=niv.tf)
summary(estudio$genero)

#Estudios:
  
unique(estudio$estudio)
niv.tf<-c("Otr","Sec", "Ter", "Uni")
estudio$estudio<-factor(estudio$estudio, labels=niv.tf)
summary(estudio$estudio)

#vive:
  
  unique(estudio$provincia)
niv.tf<-c("Bs.As.", "Río Negro", "Misiones", "cABA", "Córdoba", "Corrientes", "Entre Ríos", "Mendoza", "Neuquén", "Tierra del Fuego", "San Juan", "Salta", "Chubut", "Santa Fe", "Chaco")
estudio$provincia <-factor(estudio$provincia, labels=niv.tf)
summary(estudio$provincia)

#Modalidad:
  
  unique(estudio$modalidad_preferida)
niv.tf<-c("híbrida", "presencial","tt")
estudio$modalidad_preferida<-factor(estudio$modalidad_preferida, labels=niv.tf)
summary(estudio$modalidad_preferida)

#buscabas trabajo antes Covid-19

unique(estudio$busqueda_pasado)
niv.tf<-c("", "no", "si")
estudio$busqueda_pasado <-factor(estudio$busqueda_pasado, labels=niv.tf)
summary(estudio$busqueda_pasado)

#actualmente estás buscando trabajo?
  
  unique(estudio$busqueda_hoy)
niv.tf<-c("","no","si")
estudio$busqueda_hoy<-factor(estudio$busqueda_hoy, labels = niv.tf)
summary(estudio$busqueda_hoy)

#Rechazarías una oferta laboral por no contar con tt:
  
  unique(estudio$rechazo_oferta)
niv.tf<-c("", "no","si")
estudio$rechazo_oferta<-factor(estudio$rechazo_oferta, labels = niv.tf)
summary(estudio$rechazo_oferta)

#Tu profesión ¿se puede hacer de forma remota?
  
  unique(estudio$profesion_remota)
niv.tf<-c("", "no", "si")
estudio$profesion_remota<-factor(estudio$profesion_remota,labels = niv.tf)
summary(estudio$profesion_remota)

#¿Actualmente Trabajas?
  
  unique(estudio$trabaja)
niv.tf<-c("", "no", "si")
estudio$trabaja<-factor(estudio$trabaja,labels = niv.tf)
summary(estudio$trabaja)

#Selecciona el rubro de tu empresa

unique(estudio$rubro)
niv.tf<-c("", "Adm.pública", "c.masivo", "otros", "servicios", "tecno")
estudio$rubro<-factor(estudio$rubro,labels = niv.tf)
summary(estudio$rubro)

#Selecciona tu puesto de trabajo:
  
  unique(estudio$puesto)
niv.tf<-c("", "Ana/adm.","Gte/Dir", "Jef/Sup", "Ope")
estudio$puesto<-factor(estudio$puesto,labels = niv.tf)
summary(estudio$puesto)

#¿Ofrecía teletrabajo antes de la Pandemia Covid-19?
  
  unique(estudio$empleador_tt_antes)
niv.tf<-c("", "no", "si")
estudio$empleador_tt_antes<-factor(estudio$empleador_tt_antes,labels = niv.tf)
summary(estudio$empleador_tt_antes)

#Actualmente ¿estas haciendo teletrabajo?
  
  
  unique(estudio$teletrabajando_hoy)
niv.tf<-c("", "no, volví presencial", "sí, beneficio", "sí, x pandemia")
estudio$teletrabajando_hoy<-factor(estudio$teletrabajando_hoy,labels = niv.tf)
summary(estudio$teletrabajando_hoy)

#Si te ofrecieran el mismo trabajo con modalidad hibrida en otra empresa ¿aceptarías?
  
  unique(estudio$acepta_por_teletrabajo)
niv.tf<-  c("", "no", "si")
estudio$acepta_por_teletrabajo<-factor(estudio$acepta_por_teletrabajo,labels = niv.tf)
summary(estudio$acepta_por_teletrabajo)


#¿Cuántos dí­as te gustarí­a trabajar de forma presencial y cuantos en teletrabajo? 
  
  unique(estudio$dias_preferencia)
niv.tf<-  c("ns/nc", "1p/4tt", "2p/3tt", "3p/2tt", "4p/1tt")
estudio$dias_preferencia<-factor(estudio$dias_preferencia,labels = niv.tf)
summary(estudio$dias_preferencia)

# Valora Balance vida

unique(estudio$valora_balance)
mode(estudio$valora_balance)
class(estudio$valora_balance)
length(estudio$valora_balance)
mean(estudio$valora_balance)
range(estudio$valora_balance)
range(estudio$valora_balance, na.rm = T)
mean(estudio$valora_balance, na.rm = T)
sum(!is.na(estudio$valora_balance))


#Valora flexibilidad

range(estudio$valora_flexibilidad, na.rm = T)
mean(estudio$valora_flexibilidad, na.rm = T)

#valor elegir lugar de trabajo

range(estudio$valora_lugar, na.rm = T)
mean(estudio$valora_lugar, na.rm = T)

#Valor viáticos

range(estudio$valora_viaticos, na.rm=T)
mean(estudio$valora_viaticos,na.rm=T)

#valora compensaciones

range(estudio$valora_compensaciones, na.rm=T)
mean(estudio$valora_compensaciones,na.rm=T)


#estás dispuesto a resignar:
  
  unique(estudio$resigna_sueldo)
niv.tf<- c("", "no", "si")
estudio$resigna_sueldo<-factor(estudio$resigna_sueldo,labels = niv.tf)
summary(estudio$resigna_sueldo)  


# ver los datos de las variables----
  
table(estudio$acepta_por_teletrabajo)
table(estudio$edad)
table(estudio$genero)
table(estudio$estudio)
table(estudio$provincia)
table(estudio$modalidad_preferida)
table(estudio$busqueda_pasado)
summary(estudio)



