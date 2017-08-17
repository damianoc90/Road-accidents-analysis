########################################################## CONFIGURAZIONE

if (!require("e1071")) { install.packages("e1071") }
if (!require("rpart")) { install.packages("rpart") }
if (!require("MASS")) { install.packages("MASS") }
if (!require("fpc")) { install.packages("fpc") }
if (!require("ggplot2")) { install.packages("ggplot2") }
if (!require("cluster")) { install.packages("cluster") }
if (!require("class")) { install.packages("class") }
if (!require("graphics")) { install.packages("graphics") }
#MAPS
if (!require("ggmap")) { install.packages("ggmap") }
if (!require("sp")) { install.packages("sp") }
if (!require("rgdal")) { install.packages("rgdal") }

cat("\014")
options(warn=-1)
dir.create("plots", showWarnings=F, mode="0777")

db = read.csv("dataset.csv", header=T, col.names=c("id", "easting", "northing", "veicoli_coinvolti", "persone_coinvolte", "data", "ora", "classe_strada", "condizioni_asfalto", "illuminazione", "meteo", "classe_vittima", "gravità_vittima", "sesso", "età", "tipo_veicolo"))
db$data = as.Date(db$data, format="%d/%m/%Y")
db$tipo_veicolo = replace(db$tipo_veicolo, db$tipo_veicolo == 90, 23)
db$tipo_veicolo = replace(db$tipo_veicolo, db$tipo_veicolo == 97, 24)

########################################################## SOMMARIO E STATISTICHE

cat("______________________________________________________\n\n")
cat("SOMMARIO E STATISTICHE\n\n")
print(summary(db))
#istogrammi
png(filename="plots/Histogram.png", units="px", width=1600, height=1000, res=100)
par(mfrow=c(3,3))
barplot(table(db$veicoli_coinvolti), main="Veicoli coinvolti", xlab="veicoli_coinvolti")
barplot(table(db$persone_coinvolte), main="Persone coinvolti", xlab="persone_coinvolte")
barplot(table(db$classe_strada), main="Classe strada", xlab="classe_strada")
barplot(table(db$illuminazione), main="Illuminazione", xlab="illuminazione")
barplot(table(db$classe_vittima), main="Classe vittima", xlab="classe_vittima")
barplot(table(db$gravità_vittima), main="Gravità vittima", xlab="gravità_vittima")
barplot(table(db$età), main="Età persone coinvolte", xlab="età", breaks=range(db$età))
barplot(table(db$sesso), main="Sesso delle persone coinvolte", xlab="sesso")
barplot(table(db$tipo_veicolo), main="Tipo veicolo", xlab="tipo_veicolo")
dev.off()
#incidenti per data
ggplot(data=db, aes(db$data)) + geom_histogram() + scale_x_date(date_breaks="1 month", date_labels="%b")
ggsave(filename="plots/Incidenti per data.png", width=8,  dpi=300)
#tipo_veicolo
ggplot(data=db, aes(db$tipo_veicolo)) + geom_histogram() + scale_x_continuous(breaks=c(1:24))
ggsave(filename="plots/Tipo veicolo.png", width=8)
#gravità_vittima e classe_vittima
ggplot(data=db, aes(x=gravità_vittima, fill=factor(classe_vittima, labels=c("Conducente","Passeggero", "Pedone")), group=classe_vittima)) + geom_bar(position="dodge") + scale_x_continuous(breaks=c(1:3)) + scale_fill_brewer(palette="Set1") + labs(fill="classe_vittima") + theme(legend.position="bottom")
ggsave(filename="plots/Classe vittima e gravita vittima.png", dpi=300)
#gravità_vittima e classe_strada
ggplot(data=db, aes(x=gravità_vittima, fill=factor(classe_strada, labels=c("Autostrada","A(M)", "A", "B", "C", "Non classificato")), group=classe_strada)) + geom_bar(position="stack") + scale_x_continuous(breaks=c(1:6)) + scale_fill_brewer(palette="Set1") + labs(fill="classe_strada") + theme(legend.position="bottom")
ggsave(filename="plots/Classe strada e gravita vittima.png", dpi=300)

cat("Dagli istogrammi delle vittime si evince che è più probabile che negli incidenti venga coinvolto un numero basso di persone. L'età media di una persona incidentata è 35 anni.")
cat("\n\n______________________________________________________\n\n")

########################################################## MAPPATURA INCIDENTI

cat("MAPPATURA INCIDENTI\n\n")
png(filename="plots/Mappatura incidenti.png", units="px", width=1600, height=1000, res=100)
par(mfrow=c(1,1))
plot(db$easting, db$northing, main="Mappatura incidenti", xlab="easting", ylab="northing", type="p")
dev.off()

coords = cbind(easting = as.numeric(as.character(db$easting)), northing=as.numeric(as.character(db$northing)))
coords = SpatialPointsDataFrame(coords, data=data.frame(db$id), proj4string=CRS("+init=epsg:27700"))
coords = spTransform(coords, CRS("+init=epsg:4326"))
coords = as.data.frame(coords@coords)
map = qmap("Leeds", zoom=11, maptype="roadmap")
png(filename="plots/Mappatura incidenti (google maps).png", units="px", width=1000, height=1000, res=100)
plot(map + geom_point(data=coords, aes(easting, northing), color="red", size=2, alpha=0.4))
dev.off()
par(c(2,2))
#classe_vittima
ggplot(data=db, aes(x=easting)) + geom_point(aes(easting, northing, colour=factor(classe_vittima, labels=c("Conducente", "Passeggero", "Pedone")))) + scale_color_brewer(palette="Set1") + theme(legend.position="bottom", legend.title=element_blank()) + ggtitle("classe_vittima")
ggsave(filename="plots/Mappatura classe_vittima.png", width=11, dpi=350)
#gravità_vittima
ggplot(data=db, aes(x=easting)) + geom_point(aes(easting, northing, colour=factor(gravità_vittima, labels=c("Fatale", "Grave", "Leggero")))) + scale_color_brewer(palette="Set1") + theme(legend.position="bottom", legend.title=element_blank()) + ggtitle("gravità_vittima")
ggsave(filename="plots/Mappatura gravità_vittima.png", width=11, dpi=350)
#condizioni_asfalto
ggplot(data=db, aes(x=easting)) + geom_point(aes(easting, northing, colour=factor(condizioni_asfalto, labels=c("Secco", "Bagnato/umido", "Neve", "Brina/ghiaccio", "Inondazione")))) + scale_color_brewer(palette="Set1") + theme(legend.position="bottom", legend.title=element_blank()) + ggtitle("condizioni_asfalto")
ggsave(filename="plots/Mattapura condizioni_asfalto.png", width=11, dpi=350)
#classe_strada
ggplot(data=db, aes(x=easting)) + geom_point(aes(easting, northing, colour=factor(classe_strada, labels=c("Autostrada","A(M)", "A", "B", "C", "Non classificato")))) + scale_color_brewer(palette="Set1") + theme(legend.position="bottom", legend.title=element_blank()) + ggtitle("classe_strada")
ggsave(filename="plots/Mattapura classe_strada.png", width=11, dpi=350)

cat("Dalla distribuzione degli incidenti riportate sul plot si può notare che attorno alla posizione centrali del grafico (429857,434336), ottenuta dalla media dei velori, avvengono il maggior numero di incidenti. Si può pensare quindi che potrebbe trattarsi di un centro urbano o le condizioni stradali o di luminosità sono pessime.")
cat("\n\n______________________________________________________\n\n")

########################################################## MANIPOLAZIONE

db$data = NULL
db$id = NULL
TS = head(db, 0.5*nrow(db))
CS = tail(db, 0.5*nrow(db))

########################################################## PCA - ANALISI COMPONENTI PRINCIPALI

cat("PCA - ANALISI COMPONENTI PRINCIPALI\n\n")
pca = prcomp(log(db), center=T, scale.=T) 
print(summary(pca))
png(filename="plots/PCA.png", units="px", width=1600, height=1000, res=100)
plot(pca, type="l", main="PCA - Analisi componenti principali")
dev.off()
cat("\n\n______________________________________________________\n\n")

########################################################## CLUSTERING

cat("CLUSTERING SU CLASSE_STRADA\n\n")

#clustering gerarchico ward.D
distance_matrix = dist(db, method="euclidean")
fit = hclust(distance_matrix, method="ward.D")
groups1 = cutree(fit, k=6)
cat("● Clustering gerarchico con metodo ward.D ( corretti:", sum(diag(table(db$classe_strada, groups1))), "su", sum(table(db$classe_strada, groups1)), ")\n")
print(table(db$classe_strada, groups1))
cat("\n")
png(filename=paste("plots/Clustering gerarchico con metodo wardD.png", sep=""), units="px", width=1600, height=1000, res=100)
plot(fit, main="Clustering gerarchico", col=groups1)
plot(db$easting, db$northing, main="Clustering gerarchico (metodo ward.D)", xlab="easting", ylab="northing", pch=15, cex=3, col=groups1)
dev.off()
png(filename=paste("plots/Dendogramma incidenti con metodo wardD.png", sep=""), units="px", width=1600, height=1000, res=100)
plot(as.dendrogram(fit), main="Dendogramma incidenti (metodo ward.D)", col=groups1, leaflab="none")
rect.hclust(fit, k=6, border=groups1)
dev.off()
#clustering gerarchico complete
distance_matrix = dist(db, method="euclidean")
fit = hclust(distance_matrix, method="complete")
groups2 = cutree(fit, k=6)
cat("● Clustering gerarchico con metodo complete ( corretti:", sum(diag(table(db$classe_strada, groups2))), "su", sum(table(db$classe_strada, groups2)), ")\n")
print(table(db$classe_strada, groups2))
cat("\n")
png(filename=paste("plots/Clustering gerarchico con metodo complete.png", sep=""), units="px", width=1600, height=1000, res=100)
plot(fit, main="Clustering gerarchico", col=groups2)
plot(db$easting, db$northing, main="Clustering gerarchico (metodo complete)", xlab="easting", ylab="northing", pch=15, cex=3, col=groups2)
dev.off()
png(filename=paste("plots/Dendogramma incidenti con metodo complete.png", sep=""), units="px", width=1600, height=1000, res=100)
plot(as.dendrogram(fit), main="Dendogramma incidenti (metodo complete)", col=groups2, leaflab="none")
rect.hclust(fit, k=6, border=groups2)
dev.off()

cat("● Comparo i due cluster:")
cat("\nMedia diametri cluster group1: ", mean(cluster.stats(distance_matrix, groups1)$diameter))
cat("\nMedia diametri cluster group2: ", mean(cluster.stats(distance_matrix, groups2)$diameter),"\n")
cat("\nPoichè il peggior cluster è quello che ha il diametro maggiore (in quanto i punti che appartengono ad esso sono più dispersi), possiamo dire che il cluster col metodo \"complete\" è più efficace.\n")
#str(cluster.stats(distance_matrix, groups1, groups2))

#kmeans
kmeans = kmeans(db, 6, iter.max=500, trace=F)
cat("\n\n● k-means ( corretti:", sum(diag(table(db$classe_strada, kmeans$cluster))), "su", sum(table(db$classe_strada, kmeans$cluster)), ")\n")
print(table(db$classe_strada, kmeans$cluster))
png(filename="plots/Clustering k-means.png", units="px", width=1600, height=1000, res=100)
plot(db$easting, db$northing, col=kmeans$cluster, main="Clustering k-means", xlab="easting", ylab="northing")
points(kmeans$centers, col=1:6, pch=13, cex=4)
dev.off()

cat("\n______________________________________________________\n\n")

########################################################## CLASSIFICAZIONE

cat("CLASSIFICAZIONE CLASSE_VITTIMA IN BASE AGLI ATTRIBUTI PIU CORRELATI\n\n")
cat("● Coefficienti di Pearson:\n\n")
pearson = cor(db$classe_vittima, db[, -10], method = "pearson")
print(pearson)
cat("\nA seguire gli attributi ordinati dal più correlato al meno correlato con la colonna 'classe_vittima':\n")
cat(names(pearson[, order(pearson, decreasing=F)]))

formula = as.formula("as.factor(classe_vittima) ~ veicoli_coinvolti + età + gravità_vittima + condizioni_asfalto")

model = naiveBayes(formula, data=TS)
prediction = predict(model, TS)
cat("\n\n● Matrice di confusione Naive Bayes ( probabilità errore training:", (1 - sum(diag(prop.table(table(TS$classe_vittima, prediction))))),")\n")
print(prop.table(table(TS$classe_vittima, prediction)))
prediction = predict(model, CS)
cat("\n\n● Matrice di confusione Naive Bayes ( probabilità errore classificazione:", (1 - sum(diag(prop.table(table(CS$classe_vittima, prediction))))),")\n")
print(prop.table(table(CS$classe_vittima, prediction)))

model = svm(formula, data=TS)
prediction = predict(model, TS)
cat("\n\n● Matrice di confusione SVM ( probabilità errore training:", (1 - sum(diag(prop.table(table(TS$classe_vittima, prediction))))),")\n")
print(prop.table(table(TS$classe_vittima, prediction)))
prediction = predict(model, CS)
cat("\n\n● Matrice di confusione SVM ( probabilità errore classificazione:", (1 - sum(diag(prop.table(table(CS$classe_vittima, prediction))))),")\n")
print(prop.table(table(CS$classe_vittima, prediction)))

model = lda(formula, data=TS)
prediction = predict(model, TS, type="class")$class
cat("\n\n● Matrice di confusione Fisher ( probabilità errore training:", (1 - sum(diag(prop.table(table(TS$classe_vittima, prediction))))),")\n")
print(prop.table(table(TS$classe_vittima, prediction)))
prediction = predict(model, CS, type="class")$class
cat("\n\n● Matrice di confusione Fisher ( probabilità errore classificazione:", (1 - sum(diag(prop.table(table(CS$classe_vittima, prediction))))),")\n")
print(prop.table(table(CS$classe_vittima, prediction)))

model = rpart(formula, data=TS, method="class", control=rpart.control(minsplit=20, cp=0.002))
png(filename="plots/Albero decisionale (classe_vittima).png", units="px", width=1600, height=1000, res=100)
plot(model, main="Albero decisionale (classe_vittima)", margin=.1)
text(model, cex=.8)
dev.off()
prediction = predict(model, TS, type="class")
cat("\n● Matrice di confusione albero decisionale ( probabilità errore training:", (1 - sum(diag(prop.table(table(TS$classe_vittima, prediction))))),")\n")
print(prop.table(table(TS$classe_vittima, prediction)))
prediction = predict(model, CS, type="class")
cat("\n● Matrice di confusione albero decisionale ( probabilità errore classificazione:", (1 - sum(diag(prop.table(table(CS$classe_vittima, prediction))))),")\n")
print(prop.table(table(CS$classe_vittima, prediction)))

#knn
etichetteVere = factor(TS[, 10])
indiceME = multiedit(TS, etichetteVere, k=3, V=2, I=10, trace=F) #multiediting: rimozione elementi ambigui o outliers
TS_editato = TS[indiceME, ]
etichette_editato = etichetteVere[indiceME]
indiciCondensati = condense(TS_editato, etichette_editato, trace=F) #condensing: rimozione elementi ridondanti
TS_condensato = TS_editato[indiciCondensati, ]
etichette_condensato = etichette_editato[indiciCondensati]

knn = knn(TS_condensato, CS, cl=etichette_condensato, k=1, l=1, prob=T)
cat("\n● K-nn ( corretti:", sum(diag(table(knn, CS[, 10]))), "su", sum(table(knn, CS[, 10])), ")\n")
print(table(knn, CS[, 10]))
png(filename="plots/Classificazione knn (classe_vittima).png", units="px", width=1600, height=1000, res=100)
plot(TS$easting, TS$northing, main="Classificazione knn (classe_vittima)", xlab="easting", ylab="northing")
points(TS_editato[, c(1,2)], pch=4, col="red")
points(TS_condensato[, c(1,2)], pch=13, cex=4, col="green")
points(CS[, c(1,2)], pch=2, col="gray")
dev.off()

cat("\n______________________________________________________\n\n")

########################################################## CLASSIFICAZIONE

cat("CLASSIFICAZIONE ILLUMINAZIONE IN BASE A TUTTI GLI ATTRIBUTI\n")

formula = as.formula("as.factor(illuminazione) ~ .")

model = naiveBayes(formula, data=TS)
prediction = predict(model, TS)
cat("\n\n● Matrice di confusione Naive Bayes ( probabilità errore training:", (1 - sum(diag(prop.table(table(TS$illuminazione, prediction))))),")\n")
print(prop.table(table(TS$illuminazione, prediction)))
prediction = predict(model, CS)
cat("\n\n● Matrice di confusione Naive Bayes ( probabilità errore classificazione:", (1 - sum(diag(prop.table(table(CS$illuminazione, prediction))))),")\n")
print(prop.table(table(CS$illuminazione, prediction)))

model = svm(formula, data=TS)
prediction = predict(model, TS)
cat("\n\n● Matrice di confusione SVM ( probabilità errore training:", (1 - sum(diag(prop.table(table(TS$illuminazione, prediction))))),")\n")
print(prop.table(table(TS$illuminazione, prediction)))
prediction = predict(model, CS)
cat("\n\n● Matrice di confusione SVM ( probabilità errore classificazione:", (1 - sum(diag(prop.table(table(CS$illuminazione, prediction))))),")\n")
print(prop.table(table(CS$illuminazione, prediction)))

model = lda(formula, data=TS)
prediction = predict(model, TS, type="class")$class
cat("\n\n● Matrice di confusione Fisher ( probabilità errore training:", (1 - sum(diag(prop.table(table(TS$illuminazione, prediction))))),")\n")
print(prop.table(table(TS$illuminazione, prediction)))
prediction = predict(model, CS, type="class")$class
cat("\n\n● Matrice di confusione Fisher ( probabilità errore classificazione:", (1 - sum(diag(prop.table(table(CS$illuminazione, prediction))))),")\n")
print(prop.table(table(CS$illuminazione, prediction)))

model = rpart(formula, data=TS, method="class", control=rpart.control(minsplit=20, cp=0.01), parms=list(split = "gini"))
prediction = predict(model, TS, type="class")
cat("\n● Matrice di confusione albero decisionale ( probabilità errore training:", (1 - sum(diag(prop.table(table(TS$illuminazione, prediction))))),")\n")
print(prop.table(table(TS$illuminazione, prediction)))
prediction = predict(model, CS, type="class")
cat("\n● Matrice di confusione albero decisionale ( probabilità errore classificazione:", (1 - sum(diag(prop.table(table(CS$illuminazione, prediction))))),")\n")
print(prop.table(table(CS$illuminazione, prediction)))
png(filename="plots/Albero decisionale (illuminazione).png", units="px", width=1600, height=1000, res=100)
plot(model, main="Albero decisionale (illuminazione)", margin=0.1)
text(model, use.n=F, cex=.8)
dev.off()

#knn
etichetteVere = factor(TS[, 8])
indiceME = multiedit(TS, etichetteVere, k=3, V=2, I=10, trace=F) #multiediting: rimozione elementi ambigui o outliers
TS_editato = TS[indiceME, ]
etichette_editato = etichetteVere[indiceME]
indiciCondensati = condense(TS_editato, etichette_editato, trace=F) #condensing: rimozione elementi ridondanti
TS_condensato = TS_editato[indiciCondensati, ]
etichette_condensato = etichette_editato[indiciCondensati]

knn = knn(TS_condensato, CS, cl=etichette_condensato, k=1, l=1, prob=T)
cat("\n● K-nn ( corretti:", sum(diag(table(knn, CS[, 8]))), "su", sum(table(knn, CS[, 8])), ")\n")
print(table(knn, CS[, 8]))
png(filename="plots/Classificazione knn (illuminazione).png", units="px", width=1600, height=1000, res=100)
plot(TS$easting, TS$northing, main="Classificazione knn (illuminazione)", xlab="easting", ylab="northing")
points(TS_editato[, c(1,2)], pch=4, col="red")
points(TS_condensato[, c(1,2)], pch=13, cex=4, col="green")
points(CS[, c(1,2)], pch=2, col="gray")
dev.off()

cat("\n______________________________________________________\n\n")
