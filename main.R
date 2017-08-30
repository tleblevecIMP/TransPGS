

facies<-reading_facies_logs("latemar60m.csv",3)
props<-proportion_2D(facies,5)
transio<-transio_analysis_log("latemar60m.csv",3,10,1)
gaussian_transio_interact(props,transio,1,1,100,100)
