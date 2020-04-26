library(dplyr)
library(testthat)
library(knitr)

# wwsd tables
bib <- read.csv('bibliography.csv', sep="\t", stringsAsFactors = FALSE)
db <- read.csv('database.csv', stringsAsFactors = FALSE)

# order for the results
db <- db %>% arrange(script); rownames(db) <- NULL
bib <- bib %>% arrange(script); rownames(bib) <- NULL

# Which identifiers don't match?
db.s <- db %>% select(script) %>% distinct()
bib.s <- bib %>% select(script) %>% distinct()

## Entries in the database not in the bibliography index
expect_true(all(db.s$script %in% bib.s$script))
kable(db.s[which(!(db.s$script %in% bib.s$script)),])

## Entries in the bibliography not in the database
expect_true(all(bib.s$script %in% db.s$script))
kable(bib.s[which(!(bib.s$script %in% db.s$script)),])

# Which urls don't match (some of these are discrepencies between http and https, others have an extra space)
# TODO: fix these
db.url <- db %>% select(omniglot) %>% distinct()
bib.url <- bib %>% select(omniglot) %>% distinct()

db.url[which(!(db.url$omniglot %in% bib.url$omniglot)),]
bib.url[which(!(bib.url$omniglot %in% db.url$omniglot)),]

