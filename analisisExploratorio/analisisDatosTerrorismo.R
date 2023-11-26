data = read.csv(file = "E:/Descargas/Repos/AMD-2024-1/globalterrorismdb_0718dist.csv", sep=",", header = T, stringsAsFactors = F)
select(data)
library(dplyr)

rod <- select(data,nwoundus, nwoundte, property, propextent, propextent_txt, propvalue, propcomment, ishostkid, nhostkid, nhostkidus, nhours, ndays, divert, kidhijcountry, ransom, ransomamt, ransomamtus, ransompaid, ransompaidus, ransomnote, hostkidoutcome, hostkidoutcome_txt, nreleased, addnotes, scite1, scite2, scite3, dbsource, INT_LOG, INT_IDEO, INT_MISC, INT_ANY, related
)
summary(rod)

library(skimr)
skim(rod)


