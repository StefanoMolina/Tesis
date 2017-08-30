Cementeras


fit <- ts(rowSums(tsSmooth(StructTS(ts(Cementeras[,2])))))
Cementeras[,2]<-fit
fit <- ts(rowSums(tsSmooth(StructTS(ts(Cementeras[,3])))))
Cementeras[,3]<-fit
fit <- ts(rowSums(tsSmooth(StructTS(ts(Cementeras[,4])))))
Cementeras[,4]<-fit

%Otra manera es con:
  
fit<-na.kalman(Cementeras[,2], model=="StructTS", smooth = TRUE)

plot(ts(Cementeras[,2:4]))
plot(ts(Cementeras1[,2:4]))