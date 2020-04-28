library(dplyr)
library(testthat)
library(knitr)

# wwsd tables
bib <- read.csv('bibliography.csv', sep="\t", stringsAsFactors = FALSE)
db <- read.csv('database.csv', stringsAsFactors = FALSE)

# Ordered for results
db <- db %>% arrange(script); rownames(db) <- NULL
bib <- bib %>% arrange(script); rownames(bib) <- NULL

# Which identifiers don't match?
db.s <- db %>% select(script) %>% distinct()
bib.s <- bib %>% select(script) %>% distinct()

## Entries in the database that are not in the bibliography index
expect_true(all(db.s$script %in% bib.s$script))
kable(db.s[which(!(db.s$script %in% bib.s$script)),])

## Entries in the bibliography that are not in the database
expect_true(all(bib.s$script %in% db.s$script))
kable(bib.s[which(!(bib.s$script %in% db.s$script)),])

# Which urls don't match (some of these are discrepencies between http and https, others have an extra space)
db.url <- db %>% select(omniglot) %>% distinct()
bib.url <- bib %>% select(omniglot) %>% distinct()

db.url[which(!(db.url$omniglot %in% bib.url$omniglot)),]
bib.url[which(!(bib.url$omniglot %in% db.url$omniglot)),]

# Recreate the database.csv file with the updated (omniglot) fields in the bibliography.csv
df <- left_join(db, bib, by=c("script"="script"))
df <- df %>% select(script, grapheme, IPA, comment, source.x, omniglot.y)
df <- df %>% rename(source=source.x, omniglot=omniglot.y)
## Check the results of the omniglot url updates
scripts.urls <- df %>% select(script, omniglot) %>% distinct() %>% arrange(script)
distinct.scripts.in.db <- db %>% select(script) %>% distinct()
expect_true(nrow(distinct.scripts.in.db)==nrow(scripts.urls))
## For visual inspection
write.csv(scripts.urls, file="scripts_urls.csv", row.names = FALSE)
## Write new file
write.csv(df, file="database.csv", row.names = FALSE, quote=FALSE)


