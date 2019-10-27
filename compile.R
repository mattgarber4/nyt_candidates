#### transcript cleaner

setwd("YOUR DIRECTORY HERE")
library(myUtils) # devtools::install_github('mattgarber4/myUtils')

load("raw/compiled.Rdata")
load("candidates.Rdata")
cleaned <- list()

for (i in 1:length(transcripts)) {
    if (i %!in% c(7, 16, 17)) {
        transcripts[[i]]$raw <- NULL
        transcripts[[i]]$time <- NULL
        dta <- data.frame(t(matrix(unlist(transcripts[[i]]))), stringsAsFactors = F)
        colnames(dta) <- names(transcripts[[i]])
        per <- regexpr("[[:punct:]]", dta$question[1])[1]
        dta$number <- substr(dta$question[1], start = 1, stop = per - 1)
        dta$question <- substr(dta$question[1], start = per + 1, stop = nchar(dta$question[1]))
        if (i == 15) {
            dta$hickenlooper <- ""
        }
        dta <- dta[, c("number", 
                       "question",
                       sort(colnames(dta)[colnames(dta) %!in%
                                              c("number", "question")]))]
        colnames(dta) <- gsub("'|'|[[:punct:]]", "", colnames(dta))
        cleaned[[i]] <- dta
        write.csv(dta, file = paste0("clean/q", i, ".csv"))
    } else {
        dta <- read.csv(paste0("clean/q", i, ".csv"), stringsAsFactors = F)
        dta <- dta[1, c("number", 
                        "question",
                        sort(colnames(dta)[colnames(dta) %!in%
                                               c("number", "question")]))]
        colnames(dta) <- gsub("'|'|[[:punct:]]", "", colnames(dta))
        cleaned[[i]] <- dta
    }
    cat(i, "\n")
}


answers <- myBind(cleaned)


answers[,colnames(answers) != "number"] <- 
    data.frame(apply(answers[,colnames(answers) != "number"], c(1,2), trimws))


write.csv(answers, file = "final_nyt_candidate_answers.csv")





