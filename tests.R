library(dplyr)
library(knitr)

# wwsd tables
bib <- read.csv('bibliography.csv', sep="\t")
db <- read.csv('database.csv')

# Which identifiers don't match?

## Entries in the database not in the bibliography index
kable(db[which(!(unique(db$script) %in% unique(bib$script))),])

## Entries in the bibliography not in the database
kable(bib[which(!(unique(bib$script) %in% unique(db$script))),])

# Which urls don't match
db.url <- db %>% select(omniglot) %>% distinct()
bib.url <- bib %>% select(omniglot) %>% distinct()

bib.url[which(!(unique(bib$omniglot) %in% unique(db$omniglot))),]
db.url[which(!(db.url$omniglot %in% bib.url$omniglot)),]
