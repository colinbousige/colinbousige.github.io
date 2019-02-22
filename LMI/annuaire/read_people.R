check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

check.packages('knitr')
check.packages('dplyr')
check.packages('rcrossref')

read_people <- function(path, file){
    # people       <- read.xlsx(file.path(path, file), sheetName = "people")
    people       <- read.table(file.path(path, file), header=TRUE, sep=";")
    NOM          <- people[,"NOM"]
    Prenom       <- people[,"Prénom"]
    people       <- people[order(NOM),]
    foldername   <- tolower(paste(NOM,Prenom,sep="_"))
    foldername   <- iconv(foldername,from="UTF-8",to="ASCII//TRANSLIT")
    foldername   <- sub("\'","",foldername)
    foldername   <- sub("\`","",foldername)
    foldername   <- sub("\\^","",foldername)
    foldername   <- sub("\\\"","",foldername)
    foldername   <- sub(" ","_",foldername)
    foldername   <- sub("-","_",foldername)
    people$Nom   <- paste("[",NOM," ",Prenom,"](../personnel/",foldername,"/)",sep="")
    people$Email <- paste("[",gsub("@.*","",people$Email),"](mailto:",people$Email,")", sep="")
    people$Tél.  <- paste("[", people$Tél.,"](tel:",people$Tél.,")", sep="")
    people$Tél.  <- gsub(" ", "", people$Tél.)
    colnames(people)[which(colnames(people)=="Page.Perso")] <- "Page Perso"
    people[people=="<NA>"] <- NA
    people[is.na(people)] <- ""
    people
}

display_pers_infos <- function(people, NOM, english=FALSE){
    infos <- people[grep(tolower(NOM), tolower(people[,"Nom"])),]
    infos <- infos[,c("Equipe","Tél.","Email","Etage","Bureau","Page Perso")]
    if(english){
        colnames(infos) <- c("Team","Phone","Email","Floor","Office","Homepage")
    }
    if(length(which(infos[1,]==""))>0){
        infos <- infos[,-which(infos[1,]=="")]
    }
    infos <- t(infos)
    kable(infos, col.names="")
}

display_annuaire <- function(people, english=FALSE){
    infos <- people[,c("Nom","Equipe","Statut","Tél.")]
    if(english){
        colnames(infos) <- c("Name","Team","Status","Phone")
    }
    kable(infos)
}

print_ref <- function(bib, NOM=NULL){
    title <- gsub("\\{","",bib$title)
    title <- gsub("\\}","",title)
    toprint <- paste("[*",title,"*](",bib$url,")", sep="")
    author  <- gsub("\\{\\\\'\\{e\\}\\}","é", bib$author)
    author  <- gsub("\\{\\\\`\\{e\\}\\}","è", author)
    author  <- gsub("\\{\\\\c\\{c\\}\\}","ç", author)
    author  <- gsub("\\{\\\\^\\{o\\}\\}","ô", author)
    author  <- gsub("e\\{\\\\`\\}","è", author)
    author  <- unlist(strsplit(author," and "))
    author  <- sapply(1:length(author), function(i){
        auth <- unlist(strsplit(author[i], " "))
        paste(substr(auth[1],1,1),". ", paste(auth[-1], collapse=" "),sep="")
    })
    if(!is.null(NOM)){
        author[grep(tolower(NOM), tolower(author))] <- paste("**",author[grep(tolower(NOM), tolower(author))],"**", sep="")
    }
    author <- unique(author)
    author <- paste(author, collapse=", ")
    if(bib$entry=="article"){
        if(length(bib$journal)>0) toprint <- paste(toprint, "<br>*",bib$journal,"*", sep="")
        if(length(bib$volume)>0) toprint <- paste(toprint, " **",bib$volume,"**", sep="")
        if(length(bib$pages)>0) toprint <- paste(toprint, ", ",bib$pages, sep="")
        if(length(bib$year)>0) toprint <- paste(toprint, " (",bib$year,")", sep="")
        if(length(author)>0){
            toprint <- paste(toprint, "<br>",author,"", sep="")
        } 
    }
    if(bib$entry=="inproceedings"){
        if(length(bib$publisher)>0) {
            publisher <- gsub("\\{","",bib$publisher)
            publisher <- gsub("\\}","",publisher)
            toprint   <- paste(toprint, "<br>*",publisher,"*", sep="")
        }
        if(length(bib$year)>0) toprint <- paste(toprint, " (",bib$year,")", sep="")
        if(length(author)>0){
            toprint <- paste(toprint, "<br>",author,"", sep="")
        } 
    }
    toprint <- paste(toprint, "<br><br>", sep="")
    toprint
}

print_ref_list <- function(NOM, path, file, english=FALSE){
    people <- read.table(file.path(path, file), header=TRUE, sep=";")
    person <- people[grep(tolower(NOM), tolower(people[,"NOM"])),]
    DOI    <- paste(unique(person[,"DOI"]))
    if(DOI!=""){
        if(english){
            toprint <- "\n\n### Highlights \n\n"
        }else{toprint <- "\n\n### Articles choisis \n\n"}
        DOI <- paste(DOI, collapse=",")
        DOI <- unlist(strsplit(DOI, ","))
        bib <- cr_cn(dois = DOI, format = "bibentry")
        if (length(DOI)>1){
            years <- rev(sort(as.numeric(unique(sapply(1:length(bib), function(i) bib[[i]]$year)))))
        }else{years <- bib$year}
        for (year in years){#year=2019
            toprint <- paste(toprint,"\n\n#### ",year,"\n\n", sep="")
            if (length(DOI)>1){
                for (i in 1:length(bib)) {
                    if(as.numeric(bib[[i]]$year) == year){
                        toprint <- paste(toprint,print_ref(bib[[i]], NOM=NOM),sep="")
                    }
                }
            }else{
                toprint <- paste(toprint,print_ref(bib, NOM=NOM),sep="")
            }
        }
        cat(toprint)
    }
}






# print_ref(bib[[3]], NOM="Ferro")
# print_ref_list(NOM="Ferro", path=file.path("content","annuaire"),file="people.csv")
# NOM="CAROLE"
# path="content/annuaire"
# file="people.csv"