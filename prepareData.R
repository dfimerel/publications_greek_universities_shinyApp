# this file prepares the data (downloaded as xml from pubmed on the 6 June 2018)
# run this file ONLY if you want to update the dataa

library(reshape2)
library(XML)

#################################
# read in the XML data
theFile="~/Downloads/pubmed_result(1).xml" #5 years
newData <- xmlParse(theFile)
records <- getNodeSet(newData, "//PubmedArticle")
pmid <- xpathSApply(newData,"//MedlineCitation/PMID", xmlValue)
#authLast <- lapply(records, xpathSApply, ".//Author/LastName", xmlValue)
#authLast[sapply(authLast, is.list)] <- NA
year <- lapply(records, xpathSApply, ".//PubDate/Year", xmlValue) 
year[sapply(year, is.list)] <- NA
journal <- lapply(records, xpathSApply, ".//ISOAbbreviation", xmlValue) 
journal[sapply(journal, is.list)] <- NA
affiliations <- lapply(records, xpathSApply, ".//Author/AffiliationInfo/Affiliation", xmlValue)
affiliations[sapply(affiliations, is.list)] <- NA


# create the list and final data frame
mylist=c()
for (i in 1:length(records)) {
	mylist[[i]]=c(pmid[i],affiliations[i],year[i],journal[i])
}


newlist=c()
for (i in 1:length(records)) {
    ll=length(affiliations[i][[1]])
    dd=data.frame(matrix(ncol=4,nrow=ll))
    dd[,1]=rep(pmid[i],ll)
    dd[,2]=affiliations[[i]]
    dd[,3]=rep(year[[i]],ll)
    dd[,4]=rep(journal[[i]],ll)
    newlist[[i]]=dd
}


newdf=c()
for (i in 1:length(records)) {
    zz=newlist[[i]]
    newdf=rbind(newdf,zz[grep("Greece",zz[,2]),])
}

#save(newdf,file="greek5year.RData")


#########################################################
# read in data and make everything ready for the plots
load("greek5year.RData")
colnames(newdf)=c("pmid","affiliation","year","journal")
pmid=unique(newdf$pmid) #33941 unique pmid


# names of Greek universities
universities=c("National and Kapodistrian University of Athens","University of Thessaly","Democritus University of Thrace","Harokopio University","Aristotle University of Thessaloniki","Technical University of Crete","University of Ioannina","Agricultural University of Athens","University of Crete","University of Patras","Ionian University","National Technical University of Athens","University of the Aegean","University of Peloponnese","University of Piraeus","University of Western Macedonia","Athens School of Fine Arts","Athens University of Economics and Business","Hellenic Open University","International Hellenic University","Panteion University of Social and Political Sciences","University of Macedonia") #"University of Athens","Aristotle University" # these will be missing from the list, need to add them also

# names of Greek teis
teis=c("Alexander Technological Educational Institute of Thessaloniki","Technological Educational Institute of Piraeus","School of Pedagogical and Technological Education","Technological Educational Institute of Athens","Technological Educational Institute of Central Greece","Technological Educational Institute of Central Macedonia","Technological Educational Institute of Crete","Technological Educational Institute of Eastern Macedonia and Thrace","Technological Educational Institute of Epirus","Technological Educational Institute of the Ionian Islands","Technological Educational Institute of Peloponnese","Technological Educational Institute of Thessaly","Technological Educational Institute of Western Greece","Technological Educational Institute of Western Macedonia")


#####################################################
# PLOT 1
# get how many papers are published in each journal
journals=c()
for (i in 1:length(pmid)) {
    journals=c(journals,newdf[newdf$pmid==pmid[i],"journal"][1])
}

tt=table(journals)
tt1=melt(tt)
topJournals=head(tt1[order(tt1$value,decreasing=T),],30) # for visualization efficacy, use only top 30 journals
#save(topJournals,file="topJournals.RData")

#ggplot(topJournals,aes(x=reorder(journals,-value),y=value))+geom_bar(stat="identity",fill="darkgrey")+theme_bw()+theme(axis.text.x=element_text(angle = 60, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+labs(x="",y="Number of publications",title="Number of publications in each journal")



#####################################################
# PLOT 2
# get how may unique publications each University has
unisPubIds=data.frame(matrix(ncol=2,nrow=length(universities)))
colnames(unisPubIds)=c("university","NumPub")
unisPubIds[,1]=universities
for (i in 1:length(universities)) {	  
    unisPubIds[i,2]=length(unique(newdf[grep(universities[i],newdf$affiliation),"pmid"]))
}
#save(unisPubIds,file="unisPubIds.RData")

#ggplot(unisPubIds,aes(x=reorder(university,-NumPub),y=NumPub))+geom_bar(stat="identity",fill="darkgrey")+theme_bw()+theme(axis.text.x=element_text(angle = 70, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+labs(x="",y="Number of publications",title="Number of publications in each University")


#####################################################
# PLOT 3
# pmids per affiliation per year
yearList=list()

for(j in 1:length(universities)) {
	p1=unique(newdf[grep(universities[j],newdf$affiliation),"pmid"]) # unique pmid for affiliation #1
	zz=c()
	for (i in 1:length(p1)) {
			zz=c(zz,newdf[newdf$pmid==p1[i],"year"][1])
	}
	unisYear=melt(table(zz))
	colnames(unisYear)=c("Year","NumPub")
	yearList[[universities[j]]]=unisYear
}
#save(yearList,file="yearList.Rdata")
#i=10
#ggplot(yearList[[i]],aes(x=Year,y=NumPub))+geom_bar(stat="identity",fill="darkgrey")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+labs(x="",y="Number of publications",title=paste("Number of publications per year for\n",names(yearList[i]),sep=""))+scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018),labels=c("2013","2014","2015","2016","2017","2018"))






