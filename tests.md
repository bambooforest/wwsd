Tests for WWSD data
================
Steven Moran
04 May, 2020

``` r
library(dplyr)
library(testthat)
library(knitr)
```

``` r
db <- read.csv('database.csv', stringsAsFactors = FALSE)
bib <- read.csv('bibliography.csv', sep="\t", stringsAsFactors = FALSE)
```

Check for matching fields across the db and the bibliography
============================================================

Ordered for results

``` r
db <- db %>% arrange(script); rownames(db) <- NULL
bib <- bib %>% arrange(script); rownames(bib) <- NULL
```

Which identifiers don't match?

``` r
db.s <- db %>% select(script) %>% distinct()
bib.s <- bib %>% select(script) %>% distinct()
```

Entries in the database that are not in the bibliography index

``` r
expect_true(all(db.s$script %in% bib.s$script))
# If the test above fails, uncomment this line to show which entries are not in the bib file.
# kable(db.s[which(!(db.s$script %in% bib.s$script)),])
```

Entries in the bibliography that are not in the database

``` r
# TODO: The follow test fails until the data is fixed.
# expect_true(all(bib.s$script %in% db.s$script))
kable(bib.s[which(!(bib.s$script %in% db.s$script)),])
```

| x          |
|:-----------|
| Catalan    |
| Chechen    |
| French     |
| Italian    |
| Moldovan   |
| Portuguese |
| Romanian   |
| Romansch   |
| Spanish    |

Which urls don't match (some of these are discrepencies between http and https, others have an extra space)

``` r
db.url <- db %>% select(omniglot) %>% distinct()
bib.url <- bib %>% select(omniglot) %>% distinct()

db.url[which(!(db.url$omniglot %in% bib.url$omniglot)),] %>% kable()
```

| x                                                        |
|:---------------------------------------------------------|
| <https://www.omniglot.com/writing/ainu.htm>              |
| httpsː//www.omniglot.com/writing/archi.htm               |
| <http://www.omniglot.com/writing/avar.htm>               |
| <http://omniglot.com/writing/belarusian.htm>             |
| <http://omniglot.com/writing/bulgarian.htm>              |
| <https://www.omniglot.com/writing/hawaiian.htm>          |
| <http://www.omniglot.com/writing/hindi.htm>              |
| <https://www.omniglot.com/writing/igbo.htm>              |
| <https://www.omniglot.com/writing/itbayat.htm>           |
| <https://www.omniglot.com/writing/japanese_hiragana.htm> |
| <https://www.omniglot.com/writing/japanese_katakana.htm> |
| <http://www.omniglot.com/writing/kabardian.htm>          |
| <http://omniglot.com/writing/kazakh.htm>                 |
| <http://omniglot.com/writing/khmer.htm>                  |
| <https://www.omniglot.com/writing/kichwa.htm>            |
| <http://omniglot.com/writing/kirghiz.htm>                |
| <http://omniglot.com/writing/latvian.htm>                |
| <http://omniglot.com/writing/lithuanian.htm>             |
| <http://omniglot.com/writing/macedonian.htm>             |
| <http://omniglot.com/writing/mongolian.htm>              |
| <https://www.omniglot.com/writing/nko.htm>               |
| <https://omniglot.com/writing/braille.htm>               |
| <https://www.omniglot.com/writing/pukapukan.htm>         |
| <https://www.omniglot.com/writing/rarotongan.htm>        |
| <https://www.omniglot.com/writing/rennellese.htm>        |
| <https://www.omniglot.com/writing/tsuutina.htm>          |
| <http://omniglot.com/writing/serbian.htm>                |
| <https://www.omniglot.com/writing/sikaiana.htm>          |
| <http://www.omniglot.com/writing/sinhala.htm>            |
| <http://omniglot.com/writing/tajik.htm>                  |
| <https://www.omniglot.com/writing/yami.htm>              |
| <https://www.omniglot.com/writing/tikopia.htm>           |
| <https://www.omniglot.com/writing/tokelauan.htm>         |
| <https://www.omniglot.com/writing/tuvaluan.htm>          |
| <http://omniglot.com/writing/ukrainian.htm>              |
| <http://omniglot.com/writing/uzbek.htm>                  |
| <https://www.omniglot.com/writing/vaeakautaumako.htm>    |
| <https://www.omniglot.com/writing/vai.htm>               |
| <https://www.omniglot.com/writing/wallisian.htm>         |
| <http://www.omniglot.com/writing/yoruba.htm>             |
| <https://omniglot.com/writing/zulu.htm>                  |
| <https://omniglot.com/writing/zulu.htm>                  |

``` r
bib.url[which(!(bib.url$omniglot %in% db.url$omniglot)),] %>% kable()
```

| x                                                            |
|:-------------------------------------------------------------|
| NA                                                           |
| <https://www.omniglot.com/writing/catalan.htm>               |
| <https://www.omniglot.com/writing/chechen.htm>               |
| <https://www.omniglot.com/writing/french.htm>                |
| <https://www.omniglot.com/writing/hindi.htm>                 |
| <https://www.omniglot.com/writing/italian.htm>               |
| <https://www.omniglot.com/writing/japanese+AF8-hiragana.htm> |
| <https://www.omniglot.com/writing/japanese+AF8-katakana.htm> |
| <https://www.omniglot.com/writing/kabardian.htm>             |
| <https://omniglot.com/writing/kichwa.htm>                    |
| <https://www.omniglot.com/writing/moldovan.htm>              |
| <https://www.omniglot.com/writing/portuguese.htm>            |
| <https://www.omniglot.com/writing/romanian.htm>              |
| <https://www.omniglot.com/writing/romansh.htm>               |
| <https://www.omniglot.com/writing/sinhala.htm>               |
| <https://www.omniglot.com/writing/spanish.htm>               |

Recreate the database.csv file with the updated (omniglot) fields in the bibliography.csv

``` r
df <- left_join(db, bib, by=c("script"="script"))
df <- df %>% select(script, grapheme, IPA, comment, source.x, omniglot.y)
df <- df %>% rename(source=source.x, omniglot=omniglot.y)
```

Check the results of the omniglot url updates

``` r
scripts.urls <- df %>% select(script, omniglot) %>% distinct() %>% arrange(script)
distinct.scripts.in.db <- db %>% select(script) %>% distinct()

# TODO: this follow test fails until the data is fixed.
# expect_true(nrow(distinct.scripts.in.db)==nrow(scripts.urls))
```

For visual inspection

``` r
# write.csv(scripts.urls, file="scripts_urls.csv", row.names = FALSE)
```

Write new file

``` r
# write.csv(df, file="database.csv", row.names = FALSE)
```

Now inspect the actual g2p data
===============================

Are there any NULL IPA rows? No.

``` r
expect_equal(nrow(db %>% filter(is.na(IPA))), 0)
```

Are there empty IPA rows? Yes.

``` r
db %>% filter(IPA=="") %>% select(script, grapheme, IPA, comment) %>% kable()
```

| script               | grapheme | IPA | comment                                                                                                                                                           |
|:---------------------|:---------|:----|:------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Acehnese             | -        |     | initial                                                                                                                                                           |
| Acehnese             | -        |     | medial                                                                                                                                                            |
| Acehnese             | -        |     | initial                                                                                                                                                           |
| Acehnese             | -        |     | medial                                                                                                                                                            |
| Acehnese             | -        |     | initial                                                                                                                                                           |
| Acehnese             | -        |     | medial                                                                                                                                                            |
| Acehnese             | -        |     | initial                                                                                                                                                           |
| Acehnese             | -        |     | medial                                                                                                                                                            |
| Acehnese             | -        |     | initial                                                                                                                                                           |
| Acehnese             | -        |     | medial                                                                                                                                                            |
| Acehnese             | -        |     | initial                                                                                                                                                           |
| Acehnese             | -        |     | medial                                                                                                                                                            |
| Acehnese             | -        |     | initial                                                                                                                                                           |
| Acehnese             | -        |     | medial                                                                                                                                                            |
| Acehnese             | -        |     | initial                                                                                                                                                           |
| Acehnese             | -        |     | medial                                                                                                                                                            |
| Acehnese             | -        |     | initial                                                                                                                                                           |
| Acehnese             | -        |     | medial                                                                                                                                                            |
| Acehnese             | -        |     | initial                                                                                                                                                           |
| Acehnese             | -        |     | medial                                                                                                                                                            |
| Acehnese             | -        |     | initial                                                                                                                                                           |
| Acehnese             | -        |     | initial                                                                                                                                                           |
| Acehnese             | -        |     | medial                                                                                                                                                            |
| Acehnese             | -        |     | medial                                                                                                                                                            |
| Ainu Katakana        | ナ       |     |                                                                                                                                                                   |
| Ainu Latin           |          |     |                                                                                                                                                                   |
| Amharic              | ና        |     |                                                                                                                                                                   |
| Armenian             | Ա        |     |                                                                                                                                                                   |
| Armenian             | ա        |     |                                                                                                                                                                   |
| Armenian             | Հ        |     |                                                                                                                                                                   |
| Armenian             | հ        |     |                                                                                                                                                                   |
| Armenian             | Ո        |     |                                                                                                                                                                   |
| Armenian             | ո        |     |                                                                                                                                                                   |
| Armenian             | Ր        |     |                                                                                                                                                                   |
| Armenian             | ր        |     |                                                                                                                                                                   |
| Bengali              | ्        |     | supresses inherent vowel                                                                                                                                          |
| Dari                 | -        |     | medial                                                                                                                                                            |
| Dari                 | -        |     | initial                                                                                                                                                           |
| Dari                 | -        |     | medial                                                                                                                                                            |
| Dari                 | -        |     | initial                                                                                                                                                           |
| Dari                 | -        |     | medial                                                                                                                                                            |
| Dari                 | ﻩ        |     | Isolated glyph                                                                                                                                                    |
| Dhivehi (Thaana)     | ް        |     | absence of a vowel                                                                                                                                                |
| Dhivehi (Thaana)     | އ‎       |     | carrier for vowels;                                                                                                                                               |
| Dhivehi (Thaana)     | އ‎       |     | when used with a grapheme indicating the absence of a sound, denotes gemination of the following consonant                                                        |
| Ewe                  | w        |     | intervocally, sometimes                                                                                                                                           |
| Ewe                  | y        |     | intervocally, sometimes                                                                                                                                           |
| Faroese              | Ð        |     | never at the beginning of the word                                                                                                                                |
| Hangul (Korean)      | ᄋ       |     |                                                                                                                                                                   |
| Hangul (Korean)      | ᄒ       |     |                                                                                                                                                                   |
| Hindi (Devanāgarī)   | ्        |     | supresses inherent vowel                                                                                                                                          |
| Japanese (Hiragana)  | な       |     |                                                                                                                                                                   |
| Japanese (Katakana)  | ナ       |     |                                                                                                                                                                   |
| Kannada              | ್        |     | supresses the inherent vowel                                                                                                                                      |
| Kurdish              | -        |     | initial                                                                                                                                                           |
| Kurdish              | -        |     | medial                                                                                                                                                            |
| Kurdish              | -        |     | initial                                                                                                                                                           |
| Kurdish              | -        |     | medial                                                                                                                                                            |
| Kurdish              | -        |     | initial                                                                                                                                                           |
| Kurdish              | -        |     | medial                                                                                                                                                            |
| Kurdish              | -        |     | initial                                                                                                                                                           |
| Kurdish              | -        |     | medial                                                                                                                                                            |
| Kurdish              | -        |     | medial                                                                                                                                                            |
| Kurdish              | -        |     | initial                                                                                                                                                           |
| Kurdish              | -        |     | medial                                                                                                                                                            |
| Kurdish              | -        |     | initial                                                                                                                                                           |
| Kurdish              | -        |     | medial                                                                                                                                                            |
| Kurdish              | -        |     | initial                                                                                                                                                           |
| Kurdish              | -        |     | medial                                                                                                                                                            |
| Kurdish              | -        |     | initial                                                                                                                                                           |
| Kurdish              | -        |     | medial                                                                                                                                                            |
| Kurdish              | -        |     | initial                                                                                                                                                           |
| Kurdish              | -        |     | medial                                                                                                                                                            |
| Kurdish              | -        |     | final                                                                                                                                                             |
| Kurdish              | -        |     | medial                                                                                                                                                            |
| Kurdish              | -        |     | initial                                                                                                                                                           |
| Kurdish              | -        |     | medial                                                                                                                                                            |
| Lezgi                | щ        |     | in russian loanwords                                                                                                                                              |
| Lezgi                | ы        |     | in russian loanwords                                                                                                                                              |
| Lezgi                | ь        |     | in russian loanwords                                                                                                                                              |
| Lezgi                | Щ        |     | in russian loanwords                                                                                                                                              |
| Lezgi                | Ы        |     | in russian loanwords                                                                                                                                              |
| Lezgi                | Ь        |     | in russian loanwords                                                                                                                                              |
| Malayalam            | ്        |     | supresses the inherent vowel                                                                                                                                      |
| Mkhedruli (Georgian) | ვ        |     |                                                                                                                                                                   |
| Mkhedruli (Georgian) | ლ        |     |                                                                                                                                                                   |
| Mkhedruli (Georgian) | რ        |     |                                                                                                                                                                   |
| Mkhedruli (Georgian) | ჰ        |     |                                                                                                                                                                   |
| Nganasan             | В        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | в        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | Ж        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | ж        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | З        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | з        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | Ф        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | ф        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | Ц        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | ц        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | Ч        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | ч        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | Ш        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | ш        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | Щ        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | щ        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | Ъ        |     | Не обозначает звука, употребляется как и в русском                                                                                                                |
| Nganasan             | ъ        |     | Не обозначает звука, употребляется как и в русском                                                                                                                |
| Nganasan             | Ь        |     | Не обозначает звука, употребляется как и в русском                                                                                                                |
| Nganasan             | ь        |     | Не обозначает звука, употребляется как и в русском                                                                                                                |
| Nganasan             | Э        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan             | э        |     | Такой звук отсутствует в нганасанском языке. Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Norwegian            | J        |     |                                                                                                                                                                   |
| Norwegian            | j        |     |                                                                                                                                                                   |
| Pacoh                | Th       |     |                                                                                                                                                                   |
| Panjabi (Gurmukhi)   | ੳ        |     | vowel bearer for u uː o                                                                                                                                           |
| Panjabi (Gurmukhi)   | ਅ        |     | vowel bearer for ɑ ɑː ɛ ɔ                                                                                                                                         |
| Panjabi (Gurmukhi)   | ੲ        |     | vowel bearer for i iː e                                                                                                                                           |
| Panjabi (Gurmukhi)   | ੱ        |     | gemination                                                                                                                                                        |
| Panjabi (Gurmukhi)   | ੰ        |     | marker of nasal consonant or nalisation of a vowel                                                                                                                |
| Panjabi (Gurmukhi)   | ਂ        |     | vowel nazalisation                                                                                                                                                |
| Pashto               | -        |     | medial                                                                                                                                                            |
| Pashto               | -        |     | initial                                                                                                                                                           |
| Pashto               | -        |     | medial                                                                                                                                                            |
| Pashto               | -        |     | initial                                                                                                                                                           |
| Pashto               | -        |     | initial                                                                                                                                                           |
| Pashto               | -        |     | initial                                                                                                                                                           |
| Pashto               | -        |     | medial                                                                                                                                                            |
| Pashto               | -        |     | medial                                                                                                                                                            |
| Pashto               | -        |     | medial                                                                                                                                                            |
| Sinhala              | ්        |     | supresses the inherent vowel                                                                                                                                      |
| Somali Abjad         | -        |     | initial                                                                                                                                                           |
| Somali Abjad         | -        |     | medial                                                                                                                                                            |
| Somali Abjad         | -        |     | initial                                                                                                                                                           |
| Somali Abjad         | -        |     | medial                                                                                                                                                            |
| Somali Abjad         | -        |     | initial                                                                                                                                                           |
| Somali Abjad         | -        |     | medial                                                                                                                                                            |
| Somali Abjad         | -        |     | final                                                                                                                                                             |
| Somali Abjad         | -        |     | final                                                                                                                                                             |
| Somali Abjad         | -        |     | medial                                                                                                                                                            |
| Somali Abjad         | -        |     | medial                                                                                                                                                            |
| Sorani               | -        |     | medial                                                                                                                                                            |
| Sorani               | -        |     | initial                                                                                                                                                           |
| Sorani               | -        |     | medial                                                                                                                                                            |
| Telugu               | ్        |     | supresses the vowel                                                                                                                                               |
| Tigre Abjad          | -        |     | initial                                                                                                                                                           |
| Tigre Abjad          | -        |     | medial                                                                                                                                                            |
| Tigre Abjad          | -        |     | medial                                                                                                                                                            |
| Tigre Geez           | ና        |     |                                                                                                                                                                   |
| Tigrinya             | ና        |     |                                                                                                                                                                   |
| Urdu                 | -        |     | initial                                                                                                                                                           |
| Urdu                 | -        |     | medial                                                                                                                                                            |
| Urdu                 | -        |     | initial                                                                                                                                                           |
| Urdu                 | -        |     | medial                                                                                                                                                            |
| Urdu                 | -        |     | initial                                                                                                                                                           |
| Urdu                 | -        |     | medial                                                                                                                                                            |
| Urdu                 | -        |     | initial                                                                                                                                                           |
| Urdu                 | -        |     | medial                                                                                                                                                            |
| Urdu                 | -        |     | initial                                                                                                                                                           |
| Urdu                 | -        |     | medial                                                                                                                                                            |
| Urdu                 | -        |     | initial                                                                                                                                                           |
| Urdu                 | -        |     | medial                                                                                                                                                            |
| Urdu                 | -        |     | initial                                                                                                                                                           |
| Urdu                 | -        |     | medial                                                                                                                                                            |
| Urdu                 | -        |     | initial                                                                                                                                                           |
| Urdu                 | -        |     | medial                                                                                                                                                            |
| Urdu                 | -        |     | initial                                                                                                                                                           |
| Urdu                 | -        |     | initial                                                                                                                                                           |
| Urdu                 | -        |     | medial                                                                                                                                                            |
| Urdu                 | -        |     | medial                                                                                                                                                            |
| Uzbek                | Ь        |     |                                                                                                                                                                   |
| Uzbek                | ь        |     |                                                                                                                                                                   |

What are the "-" in graphemes?

``` r
db %>% filter(grapheme=="-") %>% select(script, grapheme, IPA, comment) %>% kable()
```

| script       | grapheme | IPA | comment |
|:-------------|:---------|:----|:--------|
| Acehnese     | -        |     | initial |
| Acehnese     | -        |     | medial  |
| Acehnese     | -        |     | initial |
| Acehnese     | -        |     | medial  |
| Acehnese     | -        |     | initial |
| Acehnese     | -        |     | medial  |
| Acehnese     | -        |     | initial |
| Acehnese     | -        |     | medial  |
| Acehnese     | -        |     | initial |
| Acehnese     | -        |     | medial  |
| Acehnese     | -        |     | initial |
| Acehnese     | -        |     | medial  |
| Acehnese     | -        |     | initial |
| Acehnese     | -        |     | medial  |
| Acehnese     | -        |     | initial |
| Acehnese     | -        |     | medial  |
| Acehnese     | -        |     | initial |
| Acehnese     | -        |     | medial  |
| Acehnese     | -        |     | initial |
| Acehnese     | -        |     | medial  |
| Acehnese     | -        |     | initial |
| Acehnese     | -        |     | initial |
| Acehnese     | -        |     | medial  |
| Acehnese     | -        |     | medial  |
| Dari         | -        |     | medial  |
| Dari         | -        |     | initial |
| Dari         | -        |     | medial  |
| Dari         | -        | z   | initial |
| Dari         | -        | z   | medial  |
| Dari         | -        | ɽ   | initial |
| Dari         | -        | ɽ   | medial  |
| Dari         | -        | z   | initial |
| Dari         | -        | z   | medial  |
| Dari         | -        | ʒ   | initial |
| Dari         | -        | ʒ   | medial  |
| Dari         | -        |     | initial |
| Dari         | -        |     | medial  |
| Kurdish      | -        |     | initial |
| Kurdish      | -        |     | medial  |
| Kurdish      | -        |     | initial |
| Kurdish      | -        |     | medial  |
| Kurdish      | -        |     | initial |
| Kurdish      | -        |     | medial  |
| Kurdish      | -        | ɛː  | medial  |
| Kurdish      | -        |     | initial |
| Kurdish      | -        |     | medial  |
| Kurdish      | -        | ŋ   | final   |
| Kurdish      | -        | ŋ   | medial  |
| Kurdish      | -        |     | medial  |
| Kurdish      | -        |     | initial |
| Kurdish      | -        |     | medial  |
| Kurdish      | -        |     | initial |
| Kurdish      | -        |     | medial  |
| Kurdish      | -        |     | initial |
| Kurdish      | -        |     | medial  |
| Kurdish      | -        |     | initial |
| Kurdish      | -        |     | medial  |
| Kurdish      | -        |     | initial |
| Kurdish      | -        |     | medial  |
| Kurdish      | -        |     | final   |
| Kurdish      | -        |     | medial  |
| Kurdish      | -        |     | initial |
| Kurdish      | -        |     | medial  |
| Pashto       | -        |     | medial  |
| Pashto       | -        |     | initial |
| Pashto       | -        |     | medial  |
| Pashto       | -        |     | initial |
| Pashto       | -        |     | initial |
| Pashto       | -        |     | initial |
| Pashto       | -        |     | medial  |
| Pashto       | -        |     | medial  |
| Pashto       | -        |     | medial  |
| Somali Abjad | -        |     | initial |
| Somali Abjad | -        |     | medial  |
| Somali Abjad | -        |     | initial |
| Somali Abjad | -        |     | medial  |
| Somali Abjad | -        |     | initial |
| Somali Abjad | -        |     | medial  |
| Somali Abjad | -        |     | final   |
| Somali Abjad | -        |     | final   |
| Somali Abjad | -        |     | medial  |
| Somali Abjad | -        |     | medial  |
| Sorani       | -        |     | medial  |
| Sorani       | -        |     | initial |
| Sorani       | -        |     | medial  |
| Tigre Abjad  | -        |     | initial |
| Tigre Abjad  | -        |     | medial  |
| Tigre Abjad  | -        |     | medial  |
| Urdu         | -        |     | initial |
| Urdu         | -        |     | medial  |
| Urdu         | -        |     | initial |
| Urdu         | -        |     | medial  |
| Urdu         | -        |     | initial |
| Urdu         | -        |     | medial  |
| Urdu         | -        |     | initial |
| Urdu         | -        |     | medial  |
| Urdu         | -        |     | initial |
| Urdu         | -        |     | medial  |
| Urdu         | -        |     | initial |
| Urdu         | -        |     | medial  |
| Urdu         | -        |     | initial |
| Urdu         | -        |     | medial  |
| Urdu         | -        |     | initial |
| Urdu         | -        |     | medial  |
| Urdu         | -        |     | initial |
| Urdu         | -        |     | initial |
| Urdu         | -        |     | medial  |
| Urdu         | -        |     | medial  |

Empty grapheme rows.

``` r
db %>% filter(grapheme=="") %>% kable() 
```

| script       | grapheme | IPA | comment | source                                                        | omniglot                                       |
|:-------------|:---------|:----|:--------|:--------------------------------------------------------------|:-----------------------------------------------|
| Ainu Latin   |          |     |         | Everhart 2009                                                 | <https://www.omniglot.com/writing/ainu.htm>    |
| Armenian     |          | ə   |         | Dum-Tragut, Jasmine (2009). Armenian: Modern Eastern Armenian | <http://www.omniglot.com/writing/armenian.htm> |
| Somali Latin |          | ʔ   |         | Saeed 1999                                                    | <http://www.omniglot.com/writing/somali.htm>   |
| Võro         |          | ◌ʲ  |         | <https://linguapedia.info/languages/voro.html>                | <http://www.omniglot.com/writing/voro.htm>     |

What about graphemes?

``` r
expect_equal(nrow(db %>% filter(is.na(grapheme))), 0)
db %>% filter(grapheme=="") %>% select(script, grapheme, IPA, comment) %>% kable()
```

| script       | grapheme | IPA | comment |
|:-------------|:---------|:----|:--------|
| Ainu Latin   |          |     |         |
| Armenian     |          | ə   |         |
| Somali Latin |          | ʔ   |         |
| Võro         |          | ◌ʲ  |         |

Check for duplicate graphemes and IPA relations

``` r
dups <- db %>% filter(IPA != "") %>% group_by(script, grapheme, IPA) %>% filter(n()>1) %>% select(script, grapheme, IPA, comment) %>% arrange(script, grapheme, IPA)
dups %>% kable()
```

| script              | grapheme | IPA      | comment                                                                                                              |
|:--------------------|:---------|:---------|:---------------------------------------------------------------------------------------------------------------------|
| Abkhaz              | ӷ        | ʁ        |                                                                                                                      |
| Abkhaz              | ӷ        | ʁ        |                                                                                                                      |
| Abkhaz              | ӷә       | ʁʲʷ      |                                                                                                                      |
| Abkhaz              | ӷә       | ʁʲʷ      |                                                                                                                      |
| Abkhaz              | ӷь       | ʁʲ       |                                                                                                                      |
| Abkhaz              | ӷь       | ʁʲ       |                                                                                                                      |
| Abkhaz              | ԥ        | pʰ       |                                                                                                                      |
| Abkhaz              | ԥ        | pʰ       |                                                                                                                      |
| Acehnese            | ء‬       | ʔ        | isolated                                                                                                             |
| Acehnese            | ء‬       | ʔ        | final                                                                                                                |
| Arabic              | ء        | ʔ        | isolated                                                                                                             |
| Arabic              | ء        | ʔ        | final                                                                                                                |
| Arabic              | ـا       | aː       | final                                                                                                                |
| Arabic              | ـا       | aː       | medial                                                                                                               |
| Arabic              | ـا       | i        | final                                                                                                                |
| Arabic              | ـا       | i        | medial                                                                                                               |
| Arabic              | ـا       | u        | final                                                                                                                |
| Arabic              | ـا       | u        | medial                                                                                                               |
| Arabic              | ـا       | ʔ        | final                                                                                                                |
| Arabic              | ـا       | ʔ        | medial                                                                                                               |
| Arabic              | ﺍ        | aː       | isolated                                                                                                             |
| Arabic              | ﺍ        | aː       | initial                                                                                                              |
| Arabic              | ﺍ        | i        | isolated                                                                                                             |
| Arabic              | ﺍ        | i        | initial                                                                                                              |
| Arabic              | ﺍ        | u        | isolated                                                                                                             |
| Arabic              | ﺍ        | u        | initial                                                                                                              |
| Arabic              | ﺍ        | ʔ        | isolated                                                                                                             |
| Arabic              | ﺍ        | ʔ        | initial                                                                                                              |
| Arabic              | ـد       | d        | final                                                                                                                |
| Arabic              | ـد       | d        | medial                                                                                                               |
| Arabic              | ﺩ        | d        | isolated                                                                                                             |
| Arabic              | ﺩ        | d        | initial                                                                                                              |
| Arabic              | ـذ       | ð        | final                                                                                                                |
| Arabic              | ـذ       | ð        | medial                                                                                                               |
| Arabic              | ﺫ        | ð        | isolated                                                                                                             |
| Arabic              | ﺫ        | ð        | initial                                                                                                              |
| Arabic              | ـر       | r        | final                                                                                                                |
| Arabic              | ـر       | r        | medial                                                                                                               |
| Arabic              | ﺭ        | r        | isolated                                                                                                             |
| Arabic              | ﺭ        | r        | initial                                                                                                              |
| Arabic              | ـز       | z        | final                                                                                                                |
| Arabic              | ـز       | z        | medial                                                                                                               |
| Arabic              | ﺯ        | z        | isolated                                                                                                             |
| Arabic              | ﺯ        | z        | initial                                                                                                              |
| Arabic              | ﻑ        | f        | isolated                                                                                                             |
| Arabic              | ﻑ        | f        | final                                                                                                                |
| Arabic              | ﻕ        | q        | isolated                                                                                                             |
| Arabic              | ﻕ        | q        | final                                                                                                                |
| Arabic              | ﻙ        | k        | isolated                                                                                                             |
| Arabic              | ﻙ        | k        | final                                                                                                                |
| Arabic              | ﻝ        | l        | isolated                                                                                                             |
| Arabic              | ﻝ        | l        | final                                                                                                                |
| Arabic              | ﻡ        | m        | isolated                                                                                                             |
| Arabic              | ﻡ        | m        | final                                                                                                                |
| Arabic              | ﻨ        | n        | initial                                                                                                              |
| Arabic              | ﻨ        | n        | medial                                                                                                               |
| Arabic              | ﻥ        | n        | isolated                                                                                                             |
| Arabic              | ﻥ        | n        | final                                                                                                                |
| Arabic              | ﻭ        | uː       | isolated                                                                                                             |
| Arabic              | ﻭ        | uː       | final                                                                                                                |
| Arabic              | ﻭ        | uː       | initial                                                                                                              |
| Arabic              | ﻭ        | uː       | medial                                                                                                               |
| Arabic              | ﻭ        | w        | isolated                                                                                                             |
| Arabic              | ﻭ        | w        | final                                                                                                                |
| Arabic              | ﻭ        | w        | initial                                                                                                              |
| Arabic              | ﻭ        | w        | medial                                                                                                               |
| Comorian            | ـا       | aː       | final                                                                                                                |
| Comorian            | ـا       | aː       | medial                                                                                                               |
| Comorian            | ـا       | i        | final                                                                                                                |
| Comorian            | ـا       | i        | medial                                                                                                               |
| Comorian            | ـا       | u        | final                                                                                                                |
| Comorian            | ـا       | u        | medial                                                                                                               |
| Comorian            | ـا       | ʔ        | final                                                                                                                |
| Comorian            | ـا       | ʔ        | medial                                                                                                               |
| Comorian            | ﺍ        | aː       | isolated                                                                                                             |
| Comorian            | ﺍ        | aː       | initial                                                                                                              |
| Comorian            | ﺍ        | i        | isolated                                                                                                             |
| Comorian            | ﺍ        | i        | initial                                                                                                              |
| Comorian            | ﺍ        | u        | isolated                                                                                                             |
| Comorian            | ﺍ        | u        | initial                                                                                                              |
| Comorian            | ﺍ        | ʔ        | isolated                                                                                                             |
| Comorian            | ﺍ        | ʔ        | initial                                                                                                              |
| Comorian            | ـد       | d        | final                                                                                                                |
| Comorian            | ـد       | d        | medial                                                                                                               |
| Comorian            | ﺩ        | d        | isolated                                                                                                             |
| Comorian            | ﺩ        | d        | initial                                                                                                              |
| Comorian            | ـذ       | ð        | final                                                                                                                |
| Comorian            | ـذ       | ð        | medial                                                                                                               |
| Comorian            | ﺫ        | ð        | isolated                                                                                                             |
| Comorian            | ﺫ        | ð        | initial                                                                                                              |
| Comorian            | ـر       | r        | final                                                                                                                |
| Comorian            | ـر       | r        | medial                                                                                                               |
| Comorian            | ﺭ        | r        | isolated                                                                                                             |
| Comorian            | ﺭ        | r        | initial                                                                                                              |
| Comorian            | ـز       | z        | final                                                                                                                |
| Comorian            | ـز       | z        | medial                                                                                                               |
| Comorian            | ﺯ        | z        | isolated                                                                                                             |
| Comorian            | ﺯ        | z        | initial                                                                                                              |
| Comorian            | ﻑ        | f        | isolated                                                                                                             |
| Comorian            | ﻑ        | f        | final                                                                                                                |
| Comorian            | ﻙ        | k        | isolated                                                                                                             |
| Comorian            | ﻙ        | k        | final                                                                                                                |
| Comorian            | ﻝ        | l        | isolated                                                                                                             |
| Comorian            | ﻝ        | l        | final                                                                                                                |
| Comorian            | ﻡ        | m        | isolated                                                                                                             |
| Comorian            | ﻡ        | m        | final                                                                                                                |
| Comorian            | ﻨ        | n        | initial                                                                                                              |
| Comorian            | ﻨ        | n        | medial                                                                                                               |
| Comorian            | ﻥ        | n        | isolated                                                                                                             |
| Comorian            | ﻥ        | n        | final                                                                                                                |
| Comorian            | ﻭ        | o        | isolated                                                                                                             |
| Comorian            | ﻭ        | o        | final                                                                                                                |
| Comorian            | ﻭ        | o        | initial                                                                                                              |
| Comorian            | ﻭ        | o        | medial                                                                                                               |
| Comorian            | ﻭ        | u        | isolated                                                                                                             |
| Comorian            | ﻭ        | u        | final                                                                                                                |
| Comorian            | ﻭ        | u        | initial                                                                                                              |
| Comorian            | ﻭ        | u        | medial                                                                                                               |
| Comorian            | ﻭ        | w        | isolated                                                                                                             |
| Comorian            | ﻭ        | w        | final                                                                                                                |
| Comorian            | ﻭ        | w        | initial                                                                                                              |
| Comorian            | ﻭ        | w        | medial                                                                                                               |
| Cornish             | W        | w        |                                                                                                                      |
| Cornish             | W        | w        |                                                                                                                      |
| Dari                | -        | ɽ        | initial                                                                                                              |
| Dari                | -        | ɽ        | medial                                                                                                               |
| Dari                | -        | z        | initial                                                                                                              |
| Dari                | -        | z        | medial                                                                                                               |
| Dari                | -        | z        | initial                                                                                                              |
| Dari                | -        | z        | medial                                                                                                               |
| Dari                | -        | ʒ        | initial                                                                                                              |
| Dari                | -        | ʒ        | medial                                                                                                               |
| Dari                | ‍ر‎      | ɽ        | Isolated glyph                                                                                                       |
| Dari                | ‍ر‎      | ɽ        | final                                                                                                                |
| Dari                | ق        | q        | Isolated glyph                                                                                                       |
| Dari                | ق        | q        | final                                                                                                                |
| Dari                | ـق       | q        | initial                                                                                                              |
| Dari                | ـق       | q        | medial                                                                                                               |
| Dari                | ‍ل‎      | l        | Isolated glyph                                                                                                       |
| Dari                | ‍ل‎      | l        | final                                                                                                                |
| Dari                | ﻟ        | l        | initial                                                                                                              |
| Dari                | ﻟ        | l        | medial                                                                                                               |
| Dari                | م‎       | m        | Isolated glyph                                                                                                       |
| Dari                | م‎       | m        | final                                                                                                                |
| Dari                | ﻣ        | m        | initial                                                                                                              |
| Dari                | ﻣ        | m        | medial                                                                                                               |
| Dari                | ‍ن‎      | n        | Isolated glyph                                                                                                       |
| Dari                | ‍ن‎      | n        | final                                                                                                                |
| Dari                | ﻧ        | n        | initial                                                                                                              |
| Dari                | ﻧ        | n        | medial                                                                                                               |
| Dari                | ﻳ        | e        | initial                                                                                                              |
| Dari                | ﻳ        | e        | medial                                                                                                               |
| Dari                | ﻳ        | i        | initial                                                                                                              |
| Dari                | ﻳ        | i        | medial                                                                                                               |
| Dari                | ﻳ        | j        | initial                                                                                                              |
| Dari                | ﻳ        | j        | medial                                                                                                               |
| English (BrE)       | a        | ə        |                                                                                                                      |
| English (BrE)       | a        | ə        |                                                                                                                      |
| English (BrE)       | A        | ə        |                                                                                                                      |
| English (BrE)       | A        | ə        |                                                                                                                      |
| English (BrE)       | g        | no sound | as in sign                                                                                                           |
| English (BrE)       | g        | no sound | as in gnome                                                                                                          |
| English (BrE)       | o        | ə        |                                                                                                                      |
| English (BrE)       | o        | ə        |                                                                                                                      |
| English (BrE)       | or       | ɜː       |                                                                                                                      |
| English (BrE)       | or       | ɜː       | as in word                                                                                                           |
| English (BrE)       | q        | k        |                                                                                                                      |
| English (BrE)       | q        | k        |                                                                                                                      |
| English (BrE)       | u        | ə        |                                                                                                                      |
| English (BrE)       | u        | ə        |                                                                                                                      |
| Ewe                 | ʋ        | β        |                                                                                                                      |
| Ewe                 | ʋ        | β        |                                                                                                                      |
| Eyak                | a        | a:       |                                                                                                                      |
| Eyak                | a        | a:       |                                                                                                                      |
| Faroese             | ey       | ɛ        |                                                                                                                      |
| Faroese             | ey       | ɛ        | before \[tʃː\]                                                                                                       |
| Faroese             | Ey       | ɛ        |                                                                                                                      |
| Faroese             | Ey       | ɛ        | before \[tʃː\]                                                                                                       |
| Faroese             | k        | ʧ        |                                                                                                                      |
| Faroese             | k        | ʧ        | before i, y, e, ey                                                                                                   |
| Fula                | '        | ʔ        |                                                                                                                      |
| Fula                | '        | ʔ        |                                                                                                                      |
| Icelandic           | g        | k        | in initial position before other vowels and consonants; in medial position before \[l, n\]                           |
| Icelandic           | g        | k        | before /l, n/                                                                                                        |
| Japanese (Katakana) | ズ       | dzɯ      |                                                                                                                      |
| Japanese (Katakana) | ズ       | dzɯ      |                                                                                                                      |
| Kapingamarangi      | l        | l        |                                                                                                                      |
| Kapingamarangi      | l        | l        |                                                                                                                      |
| Kapingamarangi      | L        | l        |                                                                                                                      |
| Kapingamarangi      | L        | l        |                                                                                                                      |
| Kurdish             | -        | ŋ        | final                                                                                                                |
| Kurdish             | -        | ŋ        | medial                                                                                                               |
| Kurdish             | ء        | ʔ        | isolated                                                                                                             |
| Kurdish             | ء        | ʔ        | final                                                                                                                |
| Kurdish             | ﺙ        | t        | isolated                                                                                                             |
| Kurdish             | ﺙ        | t        | final                                                                                                                |
| Kurdish             | ـحـ      | ħ        | medial                                                                                                               |
| Kurdish             | ـحـ      | ħ        | medial                                                                                                               |
| Kurdish             | حـ       | ħ        | initial                                                                                                              |
| Kurdish             | حـ       | ħ        | initial                                                                                                              |
| Kurdish             | ـح       | ħ        | final                                                                                                                |
| Kurdish             | ـح       | ħ        | final                                                                                                                |
| Kurdish             | ـهـ      | h        | final                                                                                                                |
| Kurdish             | ـهـ      | h        | medial                                                                                                               |
| Kurdish             | ﻫ        | h        | isolated                                                                                                             |
| Kurdish             | ﻫ        | h        | initial                                                                                                              |
| Manx                | o        | ɔ        |                                                                                                                      |
| Manx                | o        | ɔ        |                                                                                                                      |
| Maori               | ā        | a:       | есть в омниглоте, нет в грамматиках                                                                                  |
| Maori               | ā        | a:       | есть в омниглоте, нет в грамматиках                                                                                  |
| Mongolian           | ᠡ        | e        | isolated                                                                                                             |
| Mongolian           | ᠡ        | e        | separated final                                                                                                      |
| Mongolian           | ᠥ᠊       | ʏ        | initial                                                                                                              |
| Mongolian           | ᠥ᠊       | ʏ        | medial                                                                                                               |
| Mongolian           | ᠧ‍       | ɛ        | initial                                                                                                              |
| Mongolian           | ᠧ‍       | ɛ        | medial                                                                                                               |
| Mongolian           | ‍ᠩ‍      | ŋ        | medial                                                                                                               |
| Mongolian           | ‍ᠩ‍      | ŋ        | final                                                                                                                |
| Mongolian           | ᠪᠣ‍      | bo       | initial                                                                                                              |
| Mongolian           | ᠪᠣ‍      | bo       | medial                                                                                                               |
| Mongolian           | ᠪᠣ‍      | bu       | initial                                                                                                              |
| Mongolian           | ᠪᠣ‍      | bu       | medial                                                                                                               |
| Mongolian           | ᠫᠠ       | pa       | isolated                                                                                                             |
| Mongolian           | ᠫᠠ       | pa       | final                                                                                                                |
| Mongolian           | ᠫᠠ‍      | pa       | initial                                                                                                              |
| Mongolian           | ᠫᠠ‍      | pa       | medial                                                                                                               |
| Mongolian           | ᠫᠢ       | pi       | isolated                                                                                                             |
| Mongolian           | ᠫᠢ       | pi       | final                                                                                                                |
| Mongolian           | ᠫᠣ‍      | pu       | initial                                                                                                              |
| Mongolian           | ᠫᠣ‍      | pu       | medial                                                                                                               |
| Mongolian           | ᠫᠥ‍      | pœ       | initial                                                                                                              |
| Mongolian           | ᠫᠥ‍      | pœ       | medial                                                                                                               |
| Mongolian           | ᠫᠥ‍      | pʏ       | initial                                                                                                              |
| Mongolian           | ᠫᠥ‍      | pʏ       | medial                                                                                                               |
| Mongolian           | ᠬᠡ       | ɡe       | isolated                                                                                                             |
| Mongolian           | ᠬᠡ       | ɡe       | final                                                                                                                |
| Mongolian           | ᠬᠢ       | ɡi       | isolated                                                                                                             |
| Mongolian           | ᠬᠢ       | ɡi       | final                                                                                                                |
| Mongolian           | ᠬᠢ       | ki       | isolated                                                                                                             |
| Mongolian           | ᠬᠢ       | ki       | final                                                                                                                |
| Mongolian           | ᠭᠦ‍      | ɡʏ       | initial                                                                                                              |
| Mongolian           | ᠭᠦ‍      | ɡʏ       | medial                                                                                                               |
| Mongolian           | ᠭᠦ‍      | kʏ       | initial                                                                                                              |
| Mongolian           | ᠭᠦ‍      | kʏ       | medial                                                                                                               |
| Mongolian           | ᠹᠧ‍      | fɛ       | initial                                                                                                              |
| Mongolian           | ᠹᠧ‍      | fɛ       | medial                                                                                                               |
| Mongolian           | ᠺ‍       | ɡ        | loanwords only, initial                                                                                              |
| Mongolian           | ᠺ‍       | ɡ        | loanwords only, medial                                                                                               |
| Mongolian           | ᠺ‍       | k        | loanwords only, initial                                                                                              |
| Mongolian           | ᠺ‍       | k        | loanwords only, medial                                                                                               |
| Mongolian           | ᠺᠧ‍      | ɡɛ       | initial                                                                                                              |
| Mongolian           | ᠺᠧ‍      | ɡɛ       | medial                                                                                                               |
| Mongolian           | ᠺᠧ‍      | kɛ       | initial                                                                                                              |
| Mongolian           | ᠺᠧ‍      | kɛ       | medial                                                                                                               |
| Nganasan            | ’’       | ʔ        | Глухой гортанный смычный. Добавляется к предшествующему звуку, если слово обозначает множественное число             |
| Nganasan            | ’’       | ʔ        |                                                                                                                      |
| Nganasan            | б        | b        |                                                                                                                      |
| Nganasan            | б        | b        |                                                                                                                      |
| Nganasan            | Б        | b        |                                                                                                                      |
| Nganasan            | Б        | b        |                                                                                                                      |
| Nganasan            | г        | ɡ        |                                                                                                                      |
| Nganasan            | г        | ɡ        |                                                                                                                      |
| Nganasan            | Г        | ɡ        |                                                                                                                      |
| Nganasan            | Г        | ɡ        |                                                                                                                      |
| Nganasan            | д        | d        |                                                                                                                      |
| Nganasan            | д        | d        |                                                                                                                      |
| Nganasan            | Д        | d        |                                                                                                                      |
| Nganasan            | Д        | d        |                                                                                                                      |
| Nganasan            | е        | je       |                                                                                                                      |
| Nganasan            | е        | je       |                                                                                                                      |
| Nganasan            | Е        | je       |                                                                                                                      |
| Nganasan            | Е        | je       |                                                                                                                      |
| Nganasan            | ё        | jo       | Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan            | ё        | jo       |                                                                                                                      |
| Nganasan            | Ё        | jo       | Буква для обозначения этого звука введена в алфавит для правильного написания слов, заимствованных из русского языка |
| Nganasan            | Ё        | jo       |                                                                                                                      |
| Nganasan            | з̌        | ð        |                                                                                                                      |
| Nganasan            | з̌        | ð        |                                                                                                                      |
| Nganasan            | З̌        | ð        |                                                                                                                      |
| Nganasan            | З̌        | ð        |                                                                                                                      |
| Nganasan            | и        | i        | После переднеязычных согласных приобретает характер более заднего звука между гласными И и Ы                         |
| Nganasan            | и        | i        |                                                                                                                      |
| Nganasan            | И        | i        | После переднеязычных согласных приобретает характер более заднего звука между гласными И и Ы                         |
| Nganasan            | И        | i        |                                                                                                                      |
| Nganasan            | й        | j        |                                                                                                                      |
| Nganasan            | й        | j        |                                                                                                                      |
| Nganasan            | Й        | j        |                                                                                                                      |
| Nganasan            | Й        | j        |                                                                                                                      |
| Nganasan            | к        | k        |                                                                                                                      |
| Nganasan            | к        | k        |                                                                                                                      |
| Nganasan            | К        | k        |                                                                                                                      |
| Nganasan            | К        | k        |                                                                                                                      |
| Nganasan            | л        | l        |                                                                                                                      |
| Nganasan            | л        | l        |                                                                                                                      |
| Nganasan            | Л        | l        |                                                                                                                      |
| Nganasan            | Л        | l        |                                                                                                                      |
| Nganasan            | м        | m        |                                                                                                                      |
| Nganasan            | м        | m        |                                                                                                                      |
| Nganasan            | М        | m        |                                                                                                                      |
| Nganasan            | М        | m        |                                                                                                                      |
| Nganasan            | н        | n        |                                                                                                                      |
| Nganasan            | н        | n        |                                                                                                                      |
| Nganasan            | Н        | n        |                                                                                                                      |
| Nganasan            | Н        | n        |                                                                                                                      |
| Nganasan            | ӈ        | ŋ        |                                                                                                                      |
| Nganasan            | ӈ        | ŋ        |                                                                                                                      |
| Nganasan            | Ӈ        | ŋ        |                                                                                                                      |
| Nganasan            | Ӈ        | ŋ        |                                                                                                                      |
| Nganasan            | о        | o        |                                                                                                                      |
| Nganasan            | о        | o        |                                                                                                                      |
| Nganasan            | О        | o        |                                                                                                                      |
| Nganasan            | О        | o        |                                                                                                                      |
| Nganasan            | п        | p        |                                                                                                                      |
| Nganasan            | п        | p        |                                                                                                                      |
| Nganasan            | П        | p        |                                                                                                                      |
| Nganasan            | П        | p        |                                                                                                                      |
| Nganasan            | р        | r        |                                                                                                                      |
| Nganasan            | р        | r        |                                                                                                                      |
| Nganasan            | Р        | r        |                                                                                                                      |
| Nganasan            | Р        | r        |                                                                                                                      |
| Nganasan            | с        | s        |                                                                                                                      |
| Nganasan            | с        | s        |                                                                                                                      |
| Nganasan            | С        | s        |                                                                                                                      |
| Nganasan            | С        | s        |                                                                                                                      |
| Nganasan            | т        | t        |                                                                                                                      |
| Nganasan            | т        | t        |                                                                                                                      |
| Nganasan            | Т        | t        |                                                                                                                      |
| Nganasan            | Т        | t        |                                                                                                                      |
| Nganasan            | у        | u        |                                                                                                                      |
| Nganasan            | у        | u        | Үү = ʲy; y                                                                                                           |
| Nganasan            | У        | u        |                                                                                                                      |
| Nganasan            | У        | u        |                                                                                                                      |
| Nganasan            | х        | x        |                                                                                                                      |
| Nganasan            | х        | x        |                                                                                                                      |
| Nganasan            | Х        | x        |                                                                                                                      |
| Nganasan            | Х        | x        |                                                                                                                      |
| Nganasan            | ы        | ɨ        |                                                                                                                      |
| Nganasan            | ы        | ɨ        |                                                                                                                      |
| Nganasan            | Ы        | ɨ        |                                                                                                                      |
| Nganasan            | Ы        | ɨ        |                                                                                                                      |
| Nganasan            | ю        | ju       |                                                                                                                      |
| Nganasan            | ю        | ju       |                                                                                                                      |
| Nganasan            | Ю        | ju       |                                                                                                                      |
| Nganasan            | Ю        | ju       |                                                                                                                      |
| Paraguayan Guarani  | E        | e        |                                                                                                                      |
| Paraguayan Guarani  | E        | e        |                                                                                                                      |
| Pashto              | آ        | ɑ        | isolated                                                                                                             |
| Pashto              | آ        | ɑ        | initial                                                                                                              |
| Pashto              | ـا       | ɑ        | final                                                                                                                |
| Pashto              | ـا       | ɑ        | medial                                                                                                               |
| Pashto              | ا        | ɑ        | isolated                                                                                                             |
| Pashto              | ا        | ɑ        | initial                                                                                                              |
| Pashto              | د        | d̪        | isolated                                                                                                             |
| Pashto              | د        | d̪        | initial                                                                                                              |
| Pashto              | ـد       | d̪        | final                                                                                                                |
| Pashto              | ـد       | d̪        | medial                                                                                                               |
| Pashto              | ـذ       | z        | final                                                                                                                |
| Pashto              | ـذ       | z        | medial                                                                                                               |
| Pashto              | ذ        | z        | isolated                                                                                                             |
| Pashto              | ذ        | z        | initial                                                                                                              |
| Pashto              | ـډ       | ɖ        | final                                                                                                                |
| Pashto              | ـډ       | ɖ        | medial                                                                                                               |
| Pashto              | ډ        | ɖ        | isolated                                                                                                             |
| Pashto              | ډ        | ɖ        | initial                                                                                                              |
| Pashto              | ـر       | r        | final                                                                                                                |
| Pashto              | ـر       | r        | medial                                                                                                               |
| Pashto              | ر        | r        | isolated                                                                                                             |
| Pashto              | ر        | r        | initial                                                                                                              |
| Pashto              | ز        | z        | isolated                                                                                                             |
| Pashto              | ز        | z        | initial                                                                                                              |
| Pashto              | ـز       | z        | final                                                                                                                |
| Pashto              | ـز       | z        | medial                                                                                                               |
| Pashto              | ړ        | ɭ̆        | isolated                                                                                                             |
| Pashto              | ړ        | ɭ̆        | initial                                                                                                              |
| Pashto              | ړ        | ɺ̢        | isolated                                                                                                             |
| Pashto              | ړ        | ɺ̢        | initial                                                                                                              |
| Pashto              | ړ        | ɻ        | isolated                                                                                                             |
| Pashto              | ړ        | ɻ        | initial                                                                                                              |
| Pashto              | ـړ       | ɭ̆        | final                                                                                                                |
| Pashto              | ـړ       | ɭ̆        | medial                                                                                                               |
| Pashto              | ـړ       | ɺ̢        | final                                                                                                                |
| Pashto              | ـړ       | ɺ̢        | medial                                                                                                               |
| Pashto              | ـړ       | ɻ        | final                                                                                                                |
| Pashto              | ـړ       | ɻ        | medial                                                                                                               |
| Pashto              | ـږ       | ɡ (N)    | final                                                                                                                |
| Pashto              | ـږ       | ɡ (N)    | medial                                                                                                               |
| Pashto              | ـږ       | ʝ (C)    | final                                                                                                                |
| Pashto              | ـږ       | ʝ (C)    | medial                                                                                                               |
| Pashto              | ـږ       | ʐ (S)    | final                                                                                                                |
| Pashto              | ـږ       | ʐ (S)    | medial                                                                                                               |
| Pashto              | ږ        | ɡ (N)    | isolated                                                                                                             |
| Pashto              | ږ        | ɡ (N)    | initial                                                                                                              |
| Pashto              | ږ        | ʝ (C)    | isolated                                                                                                             |
| Pashto              | ږ        | ʝ (C)    | initial                                                                                                              |
| Pashto              | ږ        | ʐ (S)    | isolated                                                                                                             |
| Pashto              | ږ        | ʐ (S)    | initial                                                                                                              |
| Pashto              | ـژ       | d͡z       | final                                                                                                                |
| Pashto              | ـژ       | d͡z       | medial                                                                                                               |
| Pashto              | ـژ       | ʒ        | final                                                                                                                |
| Pashto              | ـژ       | ʒ        | medial                                                                                                               |
| Pashto              | ژ        | d͡z       | isolated                                                                                                             |
| Pashto              | ژ        | d͡z       | initial                                                                                                              |
| Pashto              | ژ        | ʒ        | isolated                                                                                                             |
| Pashto              | ژ        | ʒ        | initial                                                                                                              |
| Pashto              | ۀ        | ə        | isolated                                                                                                             |
| Pashto              | ۀ        | ə        | final                                                                                                                |
| Pashto              | و        | o        | isolated                                                                                                             |
| Pashto              | و        | o        | initial                                                                                                              |
| Pashto              | و        | u        | isolated                                                                                                             |
| Pashto              | و        | u        | initial                                                                                                              |
| Pashto              | و        | w        | isolated                                                                                                             |
| Pashto              | و        | w        | initial                                                                                                              |
| Persian             | ء        | ʔ        | Isolated                                                                                                             |
| Persian             | ء        | ʔ        | Isolated                                                                                                             |
| Persian             | آ        | ɒ        | Isolated                                                                                                             |
| Persian             | آ        | ɒ        | Initial                                                                                                              |
| Persian             | ـأ       | ʔ        | Final                                                                                                                |
| Persian             | ـأ       | ʔ        | Final                                                                                                                |
| Persian             | ـئ       | ʔ        | Final                                                                                                                |
| Persian             | ـئ       | ʔ        | Final                                                                                                                |
| Persian             | ـئـ      | ʔ        | Medial                                                                                                               |
| Persian             | ـئـ      | ʔ        | Medial                                                                                                               |
| Persian             | ئـ       | ʔ        | Initial                                                                                                              |
| Persian             | ئـ       | ʔ        | Initial                                                                                                              |
| Persian             | ـا       | ɒ        | Final/Medial                                                                                                         |
| Persian             | ـا       | ɒ        | Final                                                                                                                |
| Persian             | ـا       | ɒ        | Medial                                                                                                               |
| Persian             | ا        | ɒ        | Isolated                                                                                                             |
| Persian             | ا        | ɒ        | Initial                                                                                                              |
| Persian             | ـب       | b        | Final                                                                                                                |
| Persian             | ـب       | b        | Final                                                                                                                |
| Persian             | بـ       | b        | Initial                                                                                                              |
| Persian             | بـ       | b        | Initial                                                                                                              |
| Persian             | ـبـ      | b        | Medial                                                                                                               |
| Persian             | ـبـ      | b        | Medial                                                                                                               |
| Persian             | ب        | b        | Isolated                                                                                                             |
| Persian             | ب        | b        | Isolated                                                                                                             |
| Persian             | پـ       | p        | Initial                                                                                                              |
| Persian             | پـ       | p        | Initial                                                                                                              |
| Persian             | ـپـ      | p        | Medial                                                                                                               |
| Persian             | ـپـ      | p        | Medial                                                                                                               |
| Persian             | پ        | p        | Isolated                                                                                                             |
| Persian             | پ        | p        | Isolated                                                                                                             |
| Persian             | ـپ       | p        | Final                                                                                                                |
| Persian             | ـپ       | p        | Final                                                                                                                |
| Persian             | ـت       | t        | Final                                                                                                                |
| Persian             | ـت       | t        | Final                                                                                                                |
| Persian             | ت        | t        | Isolated                                                                                                             |
| Persian             | ت        | t        | Isolated                                                                                                             |
| Persian             | تـ       | t        | Initial                                                                                                              |
| Persian             | تـ       | t        | Initial                                                                                                              |
| Persian             | ـتـ      | t        | Medial                                                                                                               |
| Persian             | ـتـ      | t        | Medial                                                                                                               |
| Persian             | ث        | s        | Isolated                                                                                                             |
| Persian             | ث        | s        | Isolated                                                                                                             |
| Persian             | ثـ       | s        | Initial                                                                                                              |
| Persian             | ثـ       | s        | Initial                                                                                                              |
| Persian             | ـثـ      | s        | Medial                                                                                                               |
| Persian             | ـثـ      | s        | Medial                                                                                                               |
| Persian             | ـث       | s        | Final                                                                                                                |
| Persian             | ـث       | s        | Final                                                                                                                |
| Persian             | جـ       | d͡ʒ       | Initial                                                                                                              |
| Persian             | جـ       | d͡ʒ       | Initial                                                                                                              |
| Persian             | ـجـ      | d͡ʒ       | Medial                                                                                                               |
| Persian             | ـجـ      | d͡ʒ       | Medial                                                                                                               |
| Persian             | ـج       | d͡ʒ       | Final                                                                                                                |
| Persian             | ـج       | d͡ʒ       | Final                                                                                                                |
| Persian             | ج        | d͡ʒ       | Isolated                                                                                                             |
| Persian             | ج        | d͡ʒ       | Isolated                                                                                                             |
| Persian             | ـچـ      | t͡ʃ       | Medial                                                                                                               |
| Persian             | ـچـ      | t͡ʃ       | Medial                                                                                                               |
| Persian             | چ        | t͡ʃ       | Isolated                                                                                                             |
| Persian             | چ        | t͡ʃ       | Isolated                                                                                                             |
| Persian             | چـ       | t͡ʃ       | Initial                                                                                                              |
| Persian             | چـ       | t͡ʃ       | Initial                                                                                                              |
| Persian             | ـچ       | t͡ʃ       | Final                                                                                                                |
| Persian             | ـچ       | t͡ʃ       | Final                                                                                                                |
| Persian             | ـحـ      | h        | Medial                                                                                                               |
| Persian             | ـحـ      | h        | Medial                                                                                                               |
| Persian             | ح        | h        | Isolated                                                                                                             |
| Persian             | ح        | h        | Isolated                                                                                                             |
| Persian             | حـ       | h        | Initial                                                                                                              |
| Persian             | حـ       | h        | Initial                                                                                                              |
| Persian             | ـح       | h        | Final                                                                                                                |
| Persian             | ـح       | h        | Final                                                                                                                |
| Persian             | خ        | x        | Isolated                                                                                                             |
| Persian             | خ        | x        | Isolated                                                                                                             |
| Persian             | خـ       | x        | Initial                                                                                                              |
| Persian             | خـ       | x        | Initial                                                                                                              |
| Persian             | ـخ       | x        | Final                                                                                                                |
| Persian             | ـخ       | x        | Final                                                                                                                |
| Persian             | ـخـ      | x        | Medial                                                                                                               |
| Persian             | ـخـ      | x        | Medial                                                                                                               |
| Persian             | د        | d        | Initial/Isolated                                                                                                     |
| Persian             | د        | d        | Isolated                                                                                                             |
| Persian             | د        | d        | Initial                                                                                                              |
| Persian             | ـد       | d        | Final/Medial                                                                                                         |
| Persian             | ـد       | d        | Final                                                                                                                |
| Persian             | ـد       | d        | Medial                                                                                                               |
| Persian             | ـذ       | z        | Final/Medial                                                                                                         |
| Persian             | ـذ       | z        | Final                                                                                                                |
| Persian             | ـذ       | z        | Medial                                                                                                               |
| Persian             | ذ        | z        | Initial/Isolated                                                                                                     |
| Persian             | ذ        | z        | Isolated                                                                                                             |
| Persian             | ذ        | z        | Initial                                                                                                              |
| Persian             | ـر       | ɾ        | Final/Medial                                                                                                         |
| Persian             | ـر       | ɾ        | Final                                                                                                                |
| Persian             | ـر       | ɾ        | Medial                                                                                                               |
| Persian             | ر        | ɾ        | Initial/Isolated                                                                                                     |
| Persian             | ر        | ɾ        | Initial                                                                                                              |
| Persian             | ر        | ɾ        | Isolated                                                                                                             |
| Persian             | ز        | z        | Initial/Isolated                                                                                                     |
| Persian             | ز        | z        | Isolated                                                                                                             |
| Persian             | ز        | z        | Initial                                                                                                              |
| Persian             | ـز       | z        | Final/Medial                                                                                                         |
| Persian             | ـز       | z        | Medial                                                                                                               |
| Persian             | ـز       | z        | Final                                                                                                                |
| Persian             | ـژ       | ʒ        | Final/Medial                                                                                                         |
| Persian             | ـژ       | ʒ        | Final                                                                                                                |
| Persian             | ـژ       | ʒ        | Medial                                                                                                               |
| Persian             | ژ        | ʒ        | Initial/Isolated                                                                                                     |
| Persian             | ژ        | ʒ        | Isolated                                                                                                             |
| Persian             | ژ        | ʒ        | Initial                                                                                                              |
| Persian             | ـسـ      | s        | Medial                                                                                                               |
| Persian             | ـسـ      | s        | Medial                                                                                                               |
| Persian             | س        | s        | Isolated                                                                                                             |
| Persian             | س        | s        | Isolated                                                                                                             |
| Persian             | ـس       | s        | Final                                                                                                                |
| Persian             | ـس       | s        | Final                                                                                                                |
| Persian             | سـ       | s        | Initial                                                                                                              |
| Persian             | سـ       | s        | Initial                                                                                                              |
| Persian             | ـشـ      | ʃ        | Medial                                                                                                               |
| Persian             | ـشـ      | ʃ        | Medial                                                                                                               |
| Persian             | ش        | ʃ        | Isolated                                                                                                             |
| Persian             | ش        | ʃ        | Isolated                                                                                                             |
| Persian             | ـش       | ʃ        | Final                                                                                                                |
| Persian             | ـش       | ʃ        | Final                                                                                                                |
| Persian             | شـ       | ʃ        | Initial                                                                                                              |
| Persian             | شـ       | ʃ        | Initial                                                                                                              |
| Persian             | ص        | s        | Isolated                                                                                                             |
| Persian             | ص        | s        | Isolated                                                                                                             |
| Persian             | صـ       | s        | Initial                                                                                                              |
| Persian             | صـ       | s        | Initial                                                                                                              |
| Persian             | ـصـ      | s        | Medial                                                                                                               |
| Persian             | ـصـ      | s        | Medial                                                                                                               |
| Persian             | ـص       | s        | Final                                                                                                                |
| Persian             | ـص       | s        | Final                                                                                                                |
| Persian             | ـضـ      | z        | Medial                                                                                                               |
| Persian             | ـضـ      | z        | Medial                                                                                                               |
| Persian             | ض        | z        | Isolated                                                                                                             |
| Persian             | ض        | z        | Isolated                                                                                                             |
| Persian             | ضـ       | z        | Initial                                                                                                              |
| Persian             | ضـ       | z        | Initial                                                                                                              |
| Persian             | ـض       | z        | Final                                                                                                                |
| Persian             | ـض       | z        | Final                                                                                                                |
| Persian             | ـطـ      | t        | Medial                                                                                                               |
| Persian             | ـطـ      | t        | Medial                                                                                                               |
| Persian             | ط        | t        | Isolated                                                                                                             |
| Persian             | ط        | t        | Isolated                                                                                                             |
| Persian             | ـط       | t        | Final                                                                                                                |
| Persian             | ـط       | t        | Final                                                                                                                |
| Persian             | طـ       | t        | Initial                                                                                                              |
| Persian             | طـ       | t        | Initial                                                                                                              |
| Persian             | ظـ       | z        | Initial                                                                                                              |
| Persian             | ظـ       | z        | Initial                                                                                                              |
| Persian             | ـظ       | z        | Final                                                                                                                |
| Persian             | ـظ       | z        | Final                                                                                                                |
| Persian             | ظ        | z        | Isolated                                                                                                             |
| Persian             | ظ        | z        | Isolated                                                                                                             |
| Persian             | ـظـ      | z        | Medial                                                                                                               |
| Persian             | ـظـ      | z        | Medial                                                                                                               |
| Persian             | ـعـ      | ʔ        | Medial                                                                                                               |
| Persian             | ـعـ      | ʔ        | Medial                                                                                                               |
| Persian             | ع        | ʔ        | Isolated                                                                                                             |
| Persian             | ع        | ʔ        | Isolated                                                                                                             |
| Persian             | ـع       | ʔ        | Final                                                                                                                |
| Persian             | ـع       | ʔ        | Final                                                                                                                |
| Persian             | عـ       | ʔ        | Initial                                                                                                              |
| Persian             | عـ       | ʔ        | Initial                                                                                                              |
| Persian             | غـ       | ɣ        | Initial                                                                                                              |
| Persian             | غـ       | ɣ        | Initial                                                                                                              |
| Persian             | غ        | ɣ        | Isolated                                                                                                             |
| Persian             | غ        | ɣ        | Isolated                                                                                                             |
| Persian             | ـغ       | ɣ        | Final                                                                                                                |
| Persian             | ـغ       | ɣ        | Final                                                                                                                |
| Persian             | ـغـ      | ɣ        | Medial                                                                                                               |
| Persian             | ـغـ      | ɣ        | Medial                                                                                                               |
| Persian             | فـ       | f        | Initial                                                                                                              |
| Persian             | فـ       | f        | Initial                                                                                                              |
| Persian             | ـف       | f        | Final                                                                                                                |
| Persian             | ـف       | f        | Final                                                                                                                |
| Persian             | ـفـ      | f        | Medial                                                                                                               |
| Persian             | ـفـ      | f        | Medial                                                                                                               |
| Persian             | ف        | f        | Isolated                                                                                                             |
| Persian             | ف        | f        | Isolated                                                                                                             |
| Persian             | قـ       | ɢ        | Initial                                                                                                              |
| Persian             | قـ       | ɢ        | Initial                                                                                                              |
| Persian             | ـقـ      | ɢ        | Medial                                                                                                               |
| Persian             | ـقـ      | ɢ        | Medial                                                                                                               |
| Persian             | ق        | ɢ        | Isolated                                                                                                             |
| Persian             | ق        | ɢ        | Isolated                                                                                                             |
| Persian             | ـق       | ɢ        | Final                                                                                                                |
| Persian             | ـق       | ɢ        | Final                                                                                                                |
| Persian             | ـکـ      | k        | Medial                                                                                                               |
| Persian             | ـکـ      | k        | Medial                                                                                                               |
| Persian             | ـک       | k        | Final                                                                                                                |
| Persian             | ـک       | k        | Final                                                                                                                |
| Persian             | کـ       | k        | Initial                                                                                                              |
| Persian             | کـ       | k        | Initial                                                                                                              |
| Persian             | ک        | k        | Isolated                                                                                                             |
| Persian             | ک        | k        | Isolated                                                                                                             |
| Persian             | گـ       | ɡ        | Initial                                                                                                              |
| Persian             | گـ       | ɡ        | Initial                                                                                                              |
| Persian             | ـگ       | ɡ        | Final                                                                                                                |
| Persian             | ـگ       | ɡ        | Final                                                                                                                |
| Persian             | گ        | ɡ        | Isolated                                                                                                             |
| Persian             | گ        | ɡ        | Isolated                                                                                                             |
| Persian             | ـگـ      | ɡ        | Medial                                                                                                               |
| Persian             | ـگـ      | ɡ        | Medial                                                                                                               |
| Persian             | ل        | l        | Isolated                                                                                                             |
| Persian             | ل        | l        | Isolated                                                                                                             |
| Persian             | لـ       | l        | Initial                                                                                                              |
| Persian             | لـ       | l        | Initial                                                                                                              |
| Persian             | ـل       | l        | Final                                                                                                                |
| Persian             | ـل       | l        | Final                                                                                                                |
| Persian             | ـلـ      | l        | Medial                                                                                                               |
| Persian             | ـلـ      | l        | Medial                                                                                                               |
| Persian             | م        | m        | Isolated                                                                                                             |
| Persian             | م        | m        | Isolated                                                                                                             |
| Persian             | مـ       | m        | Initial                                                                                                              |
| Persian             | مـ       | m        | Initial                                                                                                              |
| Persian             | ـمـ      | m        | Medial                                                                                                               |
| Persian             | ـمـ      | m        | Medial                                                                                                               |
| Persian             | ـم       | m        | Final                                                                                                                |
| Persian             | ـم       | m        | Final                                                                                                                |
| Persian             | نـ       | n        | Initial                                                                                                              |
| Persian             | نـ       | n        | Initial                                                                                                              |
| Persian             | ـن       | n        | Final                                                                                                                |
| Persian             | ـن       | n        | Final                                                                                                                |
| Persian             | ـنـ      | n        | Medial                                                                                                               |
| Persian             | ـنـ      | n        | Medial                                                                                                               |
| Persian             | ن        | n        | Isolated                                                                                                             |
| Persian             | ن        | n        | Isolated                                                                                                             |
| Persian             | هـ       | h        | Initial                                                                                                              |
| Persian             | هـ       | h        | Initial                                                                                                              |
| Persian             | ـهـ      | h        | Medial                                                                                                               |
| Persian             | ـهـ      | h        | Medial                                                                                                               |
| Persian             | ه        | h        | Isolated                                                                                                             |
| Persian             | ه        | h        | Isolated                                                                                                             |
| Persian             | ـه       | a        | Final                                                                                                                |
| Persian             | ـه       | a        | Final                                                                                                                |
| Persian             | ـه       | e        | Final                                                                                                                |
| Persian             | ـه       | e        | Final                                                                                                                |
| Persian             | ـه       | h        | Final                                                                                                                |
| Persian             | ـه       | h        | Final                                                                                                                |
| Persian             | و        | ow       | Initial/Isolated                                                                                                     |
| Persian             | و        | ow       | Isolated                                                                                                             |
| Persian             | و        | ow       | Initial                                                                                                              |
| Persian             | و        | u        | Initial/Isolated                                                                                                     |
| Persian             | و        | u        | Isolated                                                                                                             |
| Persian             | و        | u        | Initial                                                                                                              |
| Persian             | و        | v        | Initial/Isolated                                                                                                     |
| Persian             | و        | v        | Isolated                                                                                                             |
| Persian             | و        | v        | Initial                                                                                                              |
| Persian             | ـو       | o        | Final/Medial                                                                                                         |
| Persian             | ـو       | o        | Final                                                                                                                |
| Persian             | ـو       | o        | Medial                                                                                                               |
| Persian             | ـو       | ow       | Final/Medial                                                                                                         |
| Persian             | ـو       | ow       | Final                                                                                                                |
| Persian             | ـو       | ow       | Medial                                                                                                               |
| Persian             | ـو       | u        | Final/Medial                                                                                                         |
| Persian             | ـو       | u        | Final                                                                                                                |
| Persian             | ـو       | u        | Medial                                                                                                               |
| Persian             | ـو       | v        | Final/Medial                                                                                                         |
| Persian             | ـو       | v        | Final                                                                                                                |
| Persian             | ـو       | v        | Medial                                                                                                               |
| Persian             | یـ       | ey       | Initial                                                                                                              |
| Persian             | یـ       | ey       | Initial                                                                                                              |
| Persian             | یـ       | i        | Initial                                                                                                              |
| Persian             | یـ       | i        | Initial                                                                                                              |
| Persian             | یـ       | y        | Initial                                                                                                              |
| Persian             | یـ       | y        | Initial                                                                                                              |
| Persian             | ـیـ      | ey       | Medial                                                                                                               |
| Persian             | ـیـ      | ey       | Medial                                                                                                               |
| Persian             | ـیـ      | i        | Medial                                                                                                               |
| Persian             | ـیـ      | i        | Medial                                                                                                               |
| Persian             | ـیـ      | y        | Medial                                                                                                               |
| Persian             | ـیـ      | y        | Medial                                                                                                               |
| Persian             | ی        | ey       | Isolated                                                                                                             |
| Persian             | ی        | ey       | Isolated                                                                                                             |
| Persian             | ی        | i        | Isolated                                                                                                             |
| Persian             | ی        | i        | Isolated                                                                                                             |
| Persian             | ی        | y        | Isolated                                                                                                             |
| Persian             | ی        | y        | Isolated                                                                                                             |
| Persian             | ـی       | ey       | Final                                                                                                                |
| Persian             | ـی       | ey       | Final                                                                                                                |
| Persian             | ـی       | i        | Final                                                                                                                |
| Persian             | ـی       | i        | Final                                                                                                                |
| Persian             | ـی       | y        | Final                                                                                                                |
| Persian             | ـی       | y        | Final                                                                                                                |
| Rarotongan          | h        | h        |                                                                                                                      |
| Rarotongan          | h        | h        | Manihiki and Penrhyn dialects                                                                                        |
| Rarotongan          | H        | h        |                                                                                                                      |
| Rarotongan          | H        | h        | Manihiki and Penrhyn dialects                                                                                        |
| Somali Abjad        | آ        | æ:       | isolated                                                                                                             |
| Somali Abjad        | آ        | æ:       | initial                                                                                                              |
| Somali Abjad        | آ        | ɑ:       | isolated                                                                                                             |
| Somali Abjad        | آ        | ɑ:       | initial                                                                                                              |
| Somali Abjad        | ـا       | a        | final                                                                                                                |
| Somali Abjad        | ـا       | a        | medial                                                                                                               |
| Somali Abjad        | ـا       | æ:       | final                                                                                                                |
| Somali Abjad        | ـا       | æ:       | medial                                                                                                               |
| Somali Abjad        | ـا       | ʔ        | final                                                                                                                |
| Somali Abjad        | ـا       | ʔ        | medial                                                                                                               |
| Somali Abjad        | ﺍ        | a        | isolated                                                                                                             |
| Somali Abjad        | ﺍ        | a        | initial                                                                                                              |
| Somali Abjad        | ﺍ        | æ:       | isolated                                                                                                             |
| Somali Abjad        | ﺍ        | æ:       | initial                                                                                                              |
| Somali Abjad        | ﺍ        | ʔ        | isolated                                                                                                             |
| Somali Abjad        | ﺍ        | ʔ        | initial                                                                                                              |
| Somali Abjad        | ـد       | d        | final                                                                                                                |
| Somali Abjad        | ـد       | d        | medial                                                                                                               |
| Somali Abjad        | ﺩ        | d        | isolated                                                                                                             |
| Somali Abjad        | ﺩ        | d        | initial                                                                                                              |
| Somali Abjad        | ـر       | r        | final                                                                                                                |
| Somali Abjad        | ـر       | r        | medial                                                                                                               |
| Somali Abjad        | ﺭ        | r        | isolated                                                                                                             |
| Somali Abjad        | ﺭ        | r        | initial                                                                                                              |
| Somali Abjad        | ﻑ        | f        | isolated                                                                                                             |
| Somali Abjad        | ﻑ        | f        | final                                                                                                                |
| Somali Abjad        | ﻕ        | ɢ        | isolated                                                                                                             |
| Somali Abjad        | ﻕ        | ɢ        | final                                                                                                                |
| Somali Abjad        | ﻙ        | k        | isolated                                                                                                             |
| Somali Abjad        | ﻙ        | k        | final                                                                                                                |
| Somali Abjad        | ﻝ        | l        | isolated                                                                                                             |
| Somali Abjad        | ﻝ        | l        | final                                                                                                                |
| Somali Abjad        | ﻡ        | m        | isolated                                                                                                             |
| Somali Abjad        | ﻡ        | m        | final                                                                                                                |
| Somali Abjad        | ﻨ        | n        | initial                                                                                                              |
| Somali Abjad        | ﻨ        | n        | medial                                                                                                               |
| Somali Abjad        | ﻥ        | n        | isolated                                                                                                             |
| Somali Abjad        | ﻥ        | n        | final                                                                                                                |
| Somali Abjad        | ﻭ        | u        | isolated                                                                                                             |
| Somali Abjad        | ﻭ        | u        | final                                                                                                                |
| Somali Abjad        | ﻭ        | u        | initial                                                                                                              |
| Somali Abjad        | ﻭ        | u        | medial                                                                                                               |
| Somali Abjad        | ﻭ        | ʉ        | isolated                                                                                                             |
| Somali Abjad        | ﻭ        | ʉ        | final                                                                                                                |
| Somali Abjad        | ﻭ        | ʉ        | initial                                                                                                              |
| Somali Abjad        | ﻭ        | ʉ        | medial                                                                                                               |
| Somali Abjad        | وٓ       | u:       | isolated                                                                                                             |
| Somali Abjad        | وٓ       | u:       | final                                                                                                                |
| Somali Latin        | a        | ɑ        |                                                                                                                      |
| Somali Latin        | a        | ɑ        |                                                                                                                      |
| Somali Latin        | A        | æ        |                                                                                                                      |
| Somali Latin        | A        | æ        |                                                                                                                      |
| Somali Latin        | ee       | e:       |                                                                                                                      |
| Somali Latin        | ee       | e:       |                                                                                                                      |
| Somali Latin        | Ee       | ɛ:       |                                                                                                                      |
| Somali Latin        | Ee       | ɛ:       |                                                                                                                      |
| Sorani              | ـا       | ʔ        | final                                                                                                                |
| Sorani              | ـا       | ʔ        | medial                                                                                                               |
| Sorani              | ـپـ      | p        | medial                                                                                                               |
| Sorani              | ـپـ      | p        | medial                                                                                                               |
| Sorani              | پ        | p        | isolated                                                                                                             |
| Sorani              | پ        | p        | isolated                                                                                                             |
| Sorani              | ـپ       | p        | final                                                                                                                |
| Sorani              | ـپ       | p        | final                                                                                                                |
| Sorani              | ـت       | t        | final                                                                                                                |
| Sorani              | ـت       | t        | final                                                                                                                |
| Sorani              | ـتـ      | t        | medial                                                                                                               |
| Sorani              | ـتـ      | t        | medial                                                                                                               |
| Sorani              | ــد      | d        | final                                                                                                                |
| Sorani              | ــد      | d        | medial                                                                                                               |
| Sorani              | ﺩ        | d        | isolated                                                                                                             |
| Sorani              | ﺩ        | d        | initial                                                                                                              |
| Sorani              | ـذ       | z        | final                                                                                                                |
| Sorani              | ـذ       | z        | medial                                                                                                               |
| Sorani              | ‍ذ‎      | z        | isolated                                                                                                             |
| Sorani              | ‍ذ‎      | z        | initial                                                                                                              |
| Sorani              | ڔ        | r        | isolated                                                                                                             |
| Sorani              | ڔ        | r        | initial                                                                                                              |
| Sorani              | ـڕ       | r        | final                                                                                                                |
| Sorani              | ـڕ       | r        | medial                                                                                                               |
| Sorani              | ـژ       | ʒ        | final                                                                                                                |
| Sorani              | ـژ       | ʒ        | medial                                                                                                               |
| Sorani              | ﮊ        | ʒ        | isolated                                                                                                             |
| Sorani              | ﮊ        | ʒ        | initial                                                                                                              |
| Sorani              | ـهـ      | h        | final                                                                                                                |
| Sorani              | ـهـ      | h        | medial                                                                                                               |
| Sorani              | ﻫ        | h        | isolated                                                                                                             |
| Sorani              | ﻫ        | h        | initial                                                                                                              |
| Sorani              | ﮫ        | a        | final                                                                                                                |
| Sorani              | ﮫ        | a        | medial                                                                                                               |
| Sorani              | ـو       | œ        | final                                                                                                                |
| Sorani              | ـو       | œ        | medial                                                                                                               |
| Sorani              | ـو       | w        | final                                                                                                                |
| Sorani              | ـو       | w        | medial                                                                                                               |
| Sorani              | ﻭ        | w        | isolated                                                                                                             |
| Sorani              | ﻭ        | w        | initial                                                                                                              |
| Sorani              | ـوﻭ      | u:       | final                                                                                                                |
| Sorani              | ـوﻭ      | u:       | medial                                                                                                               |
| Sorani              | ـۈ       | ʉː       | final                                                                                                                |
| Sorani              | ـۈ       | ʉː       | medial                                                                                                               |
| Sorani              | ۈ        | ʉː       | isolated                                                                                                             |
| Sorani              | ۈ        | ʉː       | initial                                                                                                              |
| Swedish             | sch      | ɧ        |                                                                                                                      |
| Swedish             | sch      | ɧ        |                                                                                                                      |
| Tabasaran           | ӏ        | ʔ        |                                                                                                                      |
| Tabasaran           | ӏ        | ʔ        |                                                                                                                      |
| Tamil               | ு        | u        | with ஞ் ண் த் ந் ல் ற் ன்                                                                                            |
| Tamil               | ு        | u        | with ட் ம் ர் ழ் ள்                                                                                                  |
| Tamil               | ு        | u        | with ங் ச் ப் ய் வ்                                                                                                  |
| Tamil               | ு        | u        | with க்                                                                                                              |
| Tamil               | ூ        | uː       | with ஞ் ண் த் ந் ல் ற் ன்                                                                                            |
| Tamil               | ூ        | uː       | with ட் ம் ர் ழ் ள்                                                                                                  |
| Tamil               | ூ        | uː       | with ங் ப் ய் வ்                                                                                                     |
| Tamil               | ூ        | uː       | with ச்                                                                                                              |
| Tamil               | ூ        | uː       | with க்                                                                                                              |
| Tigre Abjad         | ـا       | ∅        | final                                                                                                                |
| Tigre Abjad         | ـا       | ∅        | medial                                                                                                               |
| Tigre Abjad         | ـا       | ʔ        | final                                                                                                                |
| Tigre Abjad         | ـا       | ʔ        | medial                                                                                                               |
| Tigre Abjad         | د        | d̪        | isolated                                                                                                             |
| Tigre Abjad         | د        | d̪        | initial                                                                                                              |
| Tigre Abjad         | ـد       | d̪        | final                                                                                                                |
| Tigre Abjad         | ـد       | d̪        | medial                                                                                                               |
| Tigre Abjad         | ـذ       | z        | final                                                                                                                |
| Tigre Abjad         | ـذ       | z        | medial                                                                                                               |
| Tigre Abjad         | ذ        | z        | isolated                                                                                                             |
| Tigre Abjad         | ذ        | z        | initial                                                                                                              |
| Tigre Abjad         | ـر       | r        | final                                                                                                                |
| Tigre Abjad         | ـر       | r        | medial                                                                                                               |
| Tigre Abjad         | ر        | r        | isolated                                                                                                             |
| Tigre Abjad         | ر        | r        | initial                                                                                                              |
| Tigre Abjad         | ز        | z        | isolated                                                                                                             |
| Tigre Abjad         | ز        | z        | initial                                                                                                              |
| Tigre Abjad         | ـز       | z        | final                                                                                                                |
| Tigre Abjad         | ـز       | z        | medial                                                                                                               |
| Tigre Abjad         | و        | w        | isolated                                                                                                             |
| Tigre Abjad         | و        | w        | final                                                                                                                |
| Tigre Abjad         | و        | w        | initial                                                                                                              |
| Ukrainian           | б        | b        |                                                                                                                      |
| Ukrainian           | б        | b        |                                                                                                                      |
| Ukrainian           | Б        | b        |                                                                                                                      |
| Ukrainian           | Б        | b        |                                                                                                                      |
| Ukrainian           | в        | w        |                                                                                                                      |
| Ukrainian           | в        | w        |                                                                                                                      |
| Ukrainian           | В        | w        |                                                                                                                      |
| Ukrainian           | В        | w        |                                                                                                                      |
| Ukrainian           | г        | ɦ        |                                                                                                                      |
| Ukrainian           | г        | ɦ        |                                                                                                                      |
| Ukrainian           | Г        | ɦ        |                                                                                                                      |
| Ukrainian           | Г        | ɦ        |                                                                                                                      |
| Ukrainian           | ґ        | ɡ        |                                                                                                                      |
| Ukrainian           | ґ        | ɡ        |                                                                                                                      |
| Ukrainian           | Ґ        | ɡ        |                                                                                                                      |
| Ukrainian           | Ґ        | ɡ        |                                                                                                                      |
| Ukrainian           | ж        | ʒ        |                                                                                                                      |
| Ukrainian           | ж        | ʒ        |                                                                                                                      |
| Ukrainian           | Ж        | ʒ        |                                                                                                                      |
| Ukrainian           | Ж        | ʒ        |                                                                                                                      |
| Ukrainian           | л        | l        |                                                                                                                      |
| Ukrainian           | л        | l        |                                                                                                                      |
| Ukrainian           | Л        | l        |                                                                                                                      |
| Ukrainian           | Л        | l        |                                                                                                                      |
| Ukrainian           | м        | m        |                                                                                                                      |
| Ukrainian           | м        | m        |                                                                                                                      |
| Ukrainian           | М        | m        |                                                                                                                      |
| Ukrainian           | М        | m        |                                                                                                                      |
| Ukrainian           | н        | n        |                                                                                                                      |
| Ukrainian           | н        | n        |                                                                                                                      |
| Ukrainian           | Н        | n        |                                                                                                                      |
| Ukrainian           | Н        | n        |                                                                                                                      |
| Ukrainian           | о        | ɔ        |                                                                                                                      |
| Ukrainian           | о        | ɔ        |                                                                                                                      |
| Ukrainian           | О        | ɔ        |                                                                                                                      |
| Ukrainian           | О        | ɔ        |                                                                                                                      |
| Ukrainian           | р        | r        |                                                                                                                      |
| Ukrainian           | р        | r        |                                                                                                                      |
| Ukrainian           | Р        | r        |                                                                                                                      |
| Ukrainian           | Р        | r        |                                                                                                                      |
| Ukrainian           | х        | x        |                                                                                                                      |
| Ukrainian           | х        | x        |                                                                                                                      |
| Ukrainian           | Х        | x        |                                                                                                                      |
| Ukrainian           | Х        | x        |                                                                                                                      |
| Ukrainian           | ш        | ʃ        |                                                                                                                      |
| Ukrainian           | ш        | ʃ        |                                                                                                                      |
| Ukrainian           | Ш        | ʃ        |                                                                                                                      |
| Ukrainian           | Ш        | ʃ        |                                                                                                                      |
| Urdu                | ء ‎      | ∅        | isolated glyph                                                                                                       |
| Urdu                | ء ‎      | ∅        | final                                                                                                                |
| Urdu                | ء ‎      | ∅        | initial                                                                                                              |
| Urdu                | ء ‎      | ∅        | medial                                                                                                               |
| Urdu                | ء ‎      | ʔ        | isolated glyph                                                                                                       |
| Urdu                | ء ‎      | ʔ        | final                                                                                                                |
| Urdu                | ء ‎      | ʔ        | initial                                                                                                              |
| Urdu                | ء ‎      | ʔ        | medial                                                                                                               |
| Urdu                | ب‍       | b        | initial                                                                                                              |
| Urdu                | ب‍       | b        | medial                                                                                                               |
| Urdu                | ب        | b        | Isolated glyph                                                                                                       |
| Urdu                | ب        | b        | final                                                                                                                |
| Urdu                | پ‍       | p        | initial                                                                                                              |
| Urdu                | پ‍       | p        | medial                                                                                                               |
| Urdu                | پ        | p        | Isolated glyph                                                                                                       |
| Urdu                | پ        | p        | final                                                                                                                |
| Urdu                | ث        | s        | Isolated glyph                                                                                                       |
| Urdu                | ث        | s        | final                                                                                                                |
| Urdu                | ث‍‎      | s        | initial                                                                                                              |
| Urdu                | ث‍‎      | s        | medial                                                                                                               |
| Urdu                | ٹ‎       | ʈ        | Isolated glyph                                                                                                       |
| Urdu                | ٹ‎       | ʈ        | final                                                                                                                |
| Urdu                | ٹ‍‎      | ʈ        | initial                                                                                                              |
| Urdu                | ٹ‍‎      | ʈ        | medial                                                                                                               |
| Urdu                | ‍ج‎      | d͡ʒ       | Isolated glyph                                                                                                       |
| Urdu                | ‍ج‎      | d͡ʒ       | final                                                                                                                |
| Urdu                | ج‍‎      | d͡ʒ       | initial                                                                                                              |
| Urdu                | ج‍‎      | d͡ʒ       | medial                                                                                                               |
| Urdu                | ‍چ‎      | t͡ʃ       | Isolated glyph                                                                                                       |
| Urdu                | ‍چ‎      | t͡ʃ       | final                                                                                                                |
| Urdu                | چ‍‎      | t͡ʃ       | initial                                                                                                              |
| Urdu                | چ‍‎      | t͡ʃ       | medial                                                                                                               |
| Urdu                | ح‍       | h        | initial                                                                                                              |
| Urdu                | ح‍       | h        | medial                                                                                                               |
| Urdu                | ح‍       | ɦ        | initial                                                                                                              |
| Urdu                | ح‍       | ɦ        | medial                                                                                                               |
| Urdu                | ‍ح‎      | h        | Isolated glyph                                                                                                       |
| Urdu                | ‍ح‎      | h        | final                                                                                                                |
| Urdu                | ‍ح‎      | ɦ        | Isolated glyph                                                                                                       |
| Urdu                | ‍ح‎      | ɦ        | final                                                                                                                |
| Urdu                | ‍خ‎      | x        | Isolated glyph                                                                                                       |
| Urdu                | ‍خ‎      | x        | final                                                                                                                |
| Urdu                | خ‍‎      | x        | initial                                                                                                              |
| Urdu                | خ‍‎      | x        | medial                                                                                                               |
| Urdu                | ‍د‎      | d        | Isolated glyph                                                                                                       |
| Urdu                | ‍د‎      | d        | final                                                                                                                |
| Urdu                | ‍ذ‎      | z        | Isolated glyph                                                                                                       |
| Urdu                | ‍ذ‎      | z        | final                                                                                                                |
| Urdu                | ‍ڈ‎      | ɖ        | Isolated glyph                                                                                                       |
| Urdu                | ‍ڈ‎      | ɖ        | final                                                                                                                |
| Urdu                | ‍ر‎      | r        | Isolated glyph                                                                                                       |
| Urdu                | ‍ر‎      | r        | final                                                                                                                |
| Urdu                | ‍ز‎      | z        | Isolated glyph                                                                                                       |
| Urdu                | ‍ز‎      | z        | final                                                                                                                |
| Urdu                | ‍ڑ‎      | ɽ        | Isolated glyph                                                                                                       |
| Urdu                | ‍ڑ‎      | ɽ        | final                                                                                                                |
| Urdu                | ‍ژ‎      | ʒ        | Isolated glyph                                                                                                       |
| Urdu                | ‍ژ‎      | ʒ        | final                                                                                                                |
| Urdu                | س‍       | s        | initial                                                                                                              |
| Urdu                | س‍       | s        | medial                                                                                                               |
| Urdu                | ‍س‎      | s        | Isolated glyph                                                                                                       |
| Urdu                | ‍س‎      | s        | final                                                                                                                |
| Urdu                | ش‍‎      | ʃ        | initial                                                                                                              |
| Urdu                | ش‍‎      | ʃ        | medial                                                                                                               |
| Urdu                | ‍ش‎      | ʃ        | Isolated glyph                                                                                                       |
| Urdu                | ‍ش‎      | ʃ        | final                                                                                                                |
| Urdu                | ص‍       | ɬ        | initial                                                                                                              |
| Urdu                | ص‍       | ɬ        | medial                                                                                                               |
| Urdu                | ‍ص‎      | ɬ        | Isolated glyph                                                                                                       |
| Urdu                | ‍ص‎      | ɬ        | final                                                                                                                |
| Urdu                | ض‍‎      | d̪z̪       | initial                                                                                                              |
| Urdu                | ض‍‎      | d̪z̪       | medial                                                                                                               |
| Urdu                | ‍ض‎      | d̪z̪       | Isolated glyph                                                                                                       |
| Urdu                | ‍ض‎      | d̪z̪       | final                                                                                                                |
| Urdu                | ‍ط‎      | t̪s̪       | Isolated glyph                                                                                                       |
| Urdu                | ‍ط‎      | t̪s̪       | final                                                                                                                |
| Urdu                | ‍ط‎      | t̪s̪       | initial                                                                                                              |
| Urdu                | ‍ط‎      | t̪s̪       | medial                                                                                                               |
| Urdu                | ‍ظ‎      | ɮ        | Isolated glyph                                                                                                       |
| Urdu                | ‍ظ‎      | ɮ        | final                                                                                                                |
| Urdu                | ‍ظ‎      | ɮ        | initial                                                                                                              |
| Urdu                | ‍ظ‎      | ɮ        | medial                                                                                                               |
| Urdu                | ‍ف‎      | f        | Isolated glyph                                                                                                       |
| Urdu                | ‍ف‎      | f        | final                                                                                                                |
| Urdu                | ‍ق‎      | q        | Isolated glyph                                                                                                       |
| Urdu                | ‍ق‎      | q        | final                                                                                                                |
| Urdu                | ق‍       | q        | initial                                                                                                              |
| Urdu                | ق‍       | q        | medial                                                                                                               |
| Urdu                | ک‍       | k        | initial                                                                                                              |
| Urdu                | ک‍       | k        | medial                                                                                                               |
| Urdu                | گ        | ɡ        | Isolated glyph                                                                                                       |
| Urdu                | گ        | ɡ        | final                                                                                                                |
| Urdu                | ‍ل‎      | l        | Isolated glyph                                                                                                       |
| Urdu                | ‍ل‎      | l        | final                                                                                                                |
| Urdu                | ل‍       | l        | initial                                                                                                              |
| Urdu                | ل‍       | l        | medial                                                                                                               |
| Urdu                | ‍م‍‎     | m        | initial                                                                                                              |
| Urdu                | ‍م‍‎     | m        | medial                                                                                                               |
| Urdu                | م‎       | m        | Isolated glyph                                                                                                       |
| Urdu                | م‎       | m        | final                                                                                                                |
| Urdu                | ‍ن‎      | n        | Isolated glyph                                                                                                       |
| Urdu                | ‍ن‎      | n        | final                                                                                                                |
| Urdu                | ‍ن‎      | ɲ        | Isolated glyph                                                                                                       |
| Urdu                | ‍ن‎      | ɲ        | final                                                                                                                |
| Urdu                | ‍ن‎      | ɳ,       | isolated glyph                                                                                                       |
| Urdu                | ‍ن‎      | ɳ,       | final                                                                                                                |
| Urdu                | ‍ن‎      | ŋ        | isolated glyph                                                                                                       |
| Urdu                | ‍ن‎      | ŋ        | final                                                                                                                |
| Urdu                | ھ‬       | ʰ        | isolated glyph                                                                                                       |
| Urdu                | ھ‬       | ʰ        | initial                                                                                                              |
| Urdu                | ھ‬       | ʱ        | isolated glyph                                                                                                       |
| Urdu                | ھ‬       | ʱ        | initial                                                                                                              |
| Urdu                | ـ‍ھ‍‍‎   | ʰ        | final                                                                                                                |
| Urdu                | ـ‍ھ‍‍‎   | ʰ        | medial                                                                                                               |
| Urdu                | ـ‍ھ‍‍‎   | ʱ        | final                                                                                                                |
| Urdu                | ـ‍ھ‍‍‎   | ʱ        | medial                                                                                                               |
| Urdu                | ‍و‎      | oː       | isolated glyph                                                                                                       |
| Urdu                | ‍و‎      | oː       | final                                                                                                                |
| Urdu                | ‍و‎      | oː       | initial                                                                                                              |
| Urdu                | ‍و‎      | oː       | medial                                                                                                               |
| Urdu                | ‍و‎      | ɔː       | isolated glyph                                                                                                       |
| Urdu                | ‍و‎      | ɔː       | final                                                                                                                |
| Urdu                | ‍و‎      | ɔː       | initial                                                                                                              |
| Urdu                | ‍و‎      | ɔː       | medial                                                                                                               |
| Urdu                | ‍و‎      | uː       | isolated glyph                                                                                                       |
| Urdu                | ‍و‎      | uː       | final                                                                                                                |
| Urdu                | ‍و‎      | uː       | initial                                                                                                              |
| Urdu                | ‍و‎      | uː       | medial                                                                                                               |
| Urdu                | ‍و‎      | ʋ        | isolated glyph                                                                                                       |
| Urdu                | ‍و‎      | ʋ        | final                                                                                                                |
| Urdu                | ‍و‎      | ʋ        | initial                                                                                                              |
| Urdu                | ‍و‎      | ʋ        | medial                                                                                                               |
| Urdu                | ‍ی‍‎     | ɑː       | initial                                                                                                              |
| Urdu                | ‍ی‍‎     | ɑː       | medial                                                                                                               |
| Võro                | x        | ks       |                                                                                                                      |
| Võro                | x        | ks       |                                                                                                                      |
| Võro                | X        | ks       |                                                                                                                      |
| Võro                | X        | ks       |                                                                                                                      |

Note that the segments do not follow a consistent IPA convention (cf. <http://phoible.github.io/conventions/>). For example:

-   ejectives use different Unicode appostrophes
-   aspiration is marked with <h> or <ʰ>
-   diacritic ordering is ambiguous

``` r
db %>% filter(grepl("^p", IPA)) %>% select(IPA) %>% distinct() %>% arrange(IPA)
```

    ##      IPA
    ## 1      p
    ## 2      p̚
    ## 3      p͈
    ## 4     p:
    ## 5     p'
    ## 6  p'(ə)
    ## 7    p'a
    ## 8    p'ä
    ## 9    p'e
    ## 10   p'i
    ## 11   p'o
    ## 12   p'u
    ## 13    p’
    ## 14   p’a
    ## 15   p’e
    ## 16   p’ə
    ## 17   p’i
    ## 18   p’ɨ
    ## 19   p’o
    ## 20   p’u
    ## 21  p(ə)
    ## 22   p~b̥
    ## 23    pː
    ## 24    pa
    ## 25    pá
    ## 26    pä
    ## 27   pai
    ## 28    pe
    ## 29    pə
    ## 30    pɛ
    ## 31   p̪fʼ
    ## 32    ph
    ## 33    pʰ
    ## 34   pha
    ## 35   pʰa
    ## 36   pʰá
    ## 37   phə
    ## 38   pʰə
    ## 39  pʰjɔ
    ## 40  pʰlɔ
    ## 41   pʰɔ
    ## 42  pʰrɔ
    ## 43  pʰʈɔ
    ## 44    pi
    ## 45    pī
    ## 46    pɨ
    ## 47    pʲ
    ## 48   pʲa
    ## 49   pʲi
    ## 50  pʲi]
    ## 51   pʲɨ
    ## 52   pʲo
    ## 53   pjɔ
    ## 54   plɔ
    ## 55   pnɔ
    ## 56    po
    ## 57    pœ
    ## 58    pɔ
    ## 59    pp
    ## 60   ppɔ
    ## 61   prɔ
    ## 62    ps
    ## 63    pʃ
    ## 64   pʃʰ
    ## 65   pʃɔ
    ## 66   ptɔ
    ## 67   pʈɔ
    ## 68    pu
    ## 69    pɯ
    ## 70    pʷ
    ## 71    pʏ
    ## 72    pˀ
    ## 73    pʼ
    ## 74   pʼʷ

Remove duplicates and empties

``` r
# TODO: clean up the segments, write to disk, and update the CSV file(s)
db.clean <- db %>% filter(IPA != "") %>% filter(grapheme!="") %>% filter(grapheme!="-") %>% distinct(script, grapheme, IPA, .keep_all = TRUE)
```
