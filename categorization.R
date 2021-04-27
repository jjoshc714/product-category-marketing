library(tidyverse)
library(janitor)  # clean_names
library(tidytext) # unnest_tokens
library(text2vec) # vectorize text data
library(data.table)
library(glmnet)
library(magrittr)

setwd("C:/Users/Josh/Desktop")

##### --------------- Data Prep --------------- #####

# read data in
dat = read.csv("data.csv")

# subset UK data
uk = dat %>% filter(Country == "United Kingdom")

# clean names
uk = uk %>% clean_names()

# create new sales column
uk = uk %>% mutate(sales = quantity * unit_price)

# checking text frequency for categorization
desc = tibble(line = 1:length(desc), text = uk$description)
desc_words = desc %>% unnest_tokens("words", text)

data("stop_words")

desc_count = desc_words %>% 
        rename(word = words) %>% 
        anti_join(stop_words) %>%  
        count(word, sort = T)


ggplot(data = desc_count[1:15,], aes(x = reorder(word, n), y = n)) + 
     geom_bar(stat = "identity") + coord_flip() +
        ylab("Frequency") + xlab("") + ggtitle("Most Common Words")

# attempt to pinpoint category by desc or stock code
grep("BAG", uk$description, value = T)
# View(uk$description[grepl("BAG", uk$description)]) # reveals bags ~ 20,000, charms ~ 90,000 for sku

# can align first 2 digits w/ categories from uk amazon
# View(uk$description[grepl("16169", uk$stock_code)]) # Kitchen & Home
# View(uk$description[grepl("16156", uk$stock_code)]) # Kitchen & Home

view_desc = function(x){
     View(uk$description[grepl(x, uk$stock_code)], 
          title = as.character(x))
}
# view_desc(16169) ;view_desc(16156)

# looking at stock codes with letters
# View(table(grep("[a-zA-Z]", uk$stock_code, value = T)))

# removing letters and reducing stock code to lowest thousand place
round_dat = uk
round_dat$stock_code = as.numeric(gsub("[a-zA-Z]", "", uk$stock_code))
round_dat$stock_code = floor(round_dat$stock_code/1000)*1000

cat_no = sort(unique(round_dat$stock_code))

# function to view descriptions based on stock code
view_round_desc = function(x){
     View(round_dat %>% 
               filter(stock_code == cat_no[x]) %>% 
               select(description), 
          title = as.character(x))
}

# loop view_round_desc function through all unique rounded values (categories)
# ! WARNING: brings up 33 R tabs
# for(i in 1:length(cat_no)){
#     view_round_desc(i)
# }

# view_round_desc(2)

# after reading through data, assigning category names for each value w/ adjustments
#   https://services.amazon.co.uk/services/sell-online/categories.html

#
#
# using grouped stock codes to look at and form general groups
key = cbind(cat_no, category = c(
     "Other", 
     "Office Products",       # inflatable globe etc may belong elsewhere
     "Office Products",
     "Garden & Outdoors",     # fans etc may belong elsewhere
     "Office Products",       # eggs, handbags, dog rubber etc may belong elsewhere
     "Kitchen & Home",        # purses etc may belong elsewhere
     "Kitchen & Home",
     "Apparel & Accessories", # parasols, sponges, notebooks, trinket box etc may belong elsewhere
     "Kitchen & Home",        # cards, dominoes etc may belong elsewhere
     "Kitchen & Home",        # category 10 (wraps, pirate chest, doll etc may belong elsewhere)
     "Kitchen & Home",        # wraps may belong elsewhere
     "Kitchen & Home",
     "Kitchen & Home",
     "Lighting",
     "Kitchen & Home",        # lampshades may belong elsewhere
     "Kitchen & Home",
     "Kitchen & Home",
     "Toys & Games",
     "Kitchen & Home",
     "Office Products",       # category 20
     "Apparel & Accessories", # combined "Apparel" & "Shoes & Accessories"
     "Sporting Goods",        # combine w/ "Garden & Outdoors"
     "Lighting",
     "Kitchen & Home", # candles and smaller lights may fall under this category rather than lighting,
     "Beauty & Personal Care", # combined "Beauty" & "Health & Personal Care" & "Personal Care Appliances", also note: chimes may belong in "Garden & Outdoors"
     "Garden & Outdoors",
     "Kitchen & Home",
     "Kitchen & Home", # lights may belong in "Lighting"
     "Kitchen & Home",
     "Kitchen & Home",        # category 30
     "Kitchen & Home",
     "Kitchen & Home", # note: t-lights may belong in "Kitchen & Home"
     "Jewelry"
))

# using keys to specify categorization rules
uk$category = 
        ifelse(grepl("MUG|CUP|LUNCH|BOWL|COOKIE|SHOPPING|JUMBO BAG|CAKE|SPOON|JAM|TEA|RECIPE|JAM|STORAGE JAR|BREAD|JUG|BATHROOM|TRAY|PAPER NAPKINS", uk$description) == TRUE, "Kitchen", # regex(//< //>) takes only whole words
               ifelse(grepl("CANDLE|T-LIGHT HOLDER|OIL BURNER|TREE|FILLER PAD|CUSHION|LAMPSHADE|LANTERNS|LANTERN|CLOCK|FRAME|CABINET|METAL SIGN|HANGER|TOWEL|STORAGE|DOORMAT|LIGHTS|WOOLLY|MIRROR|KNOB|CLOTHES|PEGS|PAPER CHAIN|INCENSE|DOOR|BUILDING BLOCK|ORNAMENT|WASTEPAPER|COAT RACK|COAT|COAT HANGER|CUPID|TOILET", uk$description) == TRUE, "Home",
                      ifelse(grepl("TEA TIME|DECORATIONS|DECORATION|HAPPY BIRTHDAY|EGG|PLAYHOUSE|RIBBON|\\<DEC\\>|DOLL|BLOCKS|GAMES|GAME|CARD GAME|PARTY CONES|PLAYING CARDS|PUPPET|FLOWER|BALLOON|HELICOPTER|CUP GAME", uk$description) == TRUE, "Toys & Games",
                             ifelse(grepl("\\<PEN\\>|\\<PENS\\>|JOURNAL|PENCILS|SCISSOR|ERASER|CARD|GIFT BAG|GLOBE|TAPE|NOTEPAD|BLACKBOARD|RULER|CALCULATOR", uk$description) == TRUE, "Office Products", 
                                    ifelse(grepl("HANDBAG|\\<HAND BAG\\>|SOMBRERO|SHOULDER BAG|CHARLOTTE BAG|PURSE", uk$description) == TRUE, "Apparel & Accessories",
                                           ifelse(grepl("TRINKET|NECKLACE|DIAMANTE|EARRINGS|BROOCH|ENAMEL|CRYSTAL|GEMSTONE|CHARM|JEWELLERY|TIARA", uk$description) == TRUE, "Jewelry", 
                                                  ifelse(grepl("WARMER|PARASOL|WIND|UMBRELLA|BICYCLE|WATER BOTTLE|\\<NESTING\\>|CHIME|PICNIC|PICNIC BASKET|GARDEN|BACKPACK", uk$description) == TRUE, "Garden & Outdoors",
                                                         "NA")))))))
table(uk$category)

# reduce kitchen & home to 30,000 each for equivalent rep during training
uk = uk %>% mutate(id = as.character(1:nrow(uk))) %>% select(id, 1:11)

h_index = uk %>% filter(category == "Home") %>% select(id)
k_index = uk %>% filter(category == "Kitchen") %>% select(id)
t_index = uk %>% filter(category == "Toys & Games") %>% select(id)

set.seed(123)
h_random = sample(h_index$id, size = nrow(h_index)-30000)
k_random = sample(k_index$id, size = nrow(k_index)-30000)
t_random = sample(t_index$id, size = nrow(t_index)-30000)

uk$category[as.numeric(h_random)] = "NA"
uk$category[as.numeric(k_random)] = "NA"
uk$category[as.numeric(t_random)] = "NA"

table(uk$category)

# separate categorized and NA datasets into training and test
cats = uk %>% filter(category != "NA")
cats$cat_no = ifelse(cats$category == "Apparel & Accessories", 1,
                     ifelse(cats$category == "Garden & Outdoors", 2, 
                            ifelse(cats$category == "Home", 3, 
                                   ifelse(cats$category == "Jewelry", 4,
                                          ifelse(cats$category == "Kitchen", 5,
                                                 ifelse(cats$category == "Office Products", 6,
                                                        7)))))) # "Toys & Games"
cats = cats %>% select(-c(category, invoice_no, country, sales))

nas = uk %>% filter(category == "NA")
nas = nas  %>% 
        select(-c(category, invoice_no, country, sales))

# attempt to vectorize training data
setDT(cats)
setkey(cats, id)
set.seed(123)
ids = cats$id
index = sample(seq_len(nrow(cats)), size = floor(0.8*nrow(cats)))
train = cats[index,]
test = cats[-index,] # KiM there will be much bias in testing (this is just as a check for SVM)

prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(train$description, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)

vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)

identical(rownames(dtm_train), train$id) # check

# run classification, KiM: ctrl + shift + c to rid of comments
NFOLDS = 4
# glmnet_classifier = cv.glmnet(x = dtm_train, y = train[['cat_no']],
#                               family = 'multinomial',
#                               # L1 penalty
#                               alpha = 1,
#                               type.measure = "auc",
#                               # 4-fold cross-validation
#                               nfolds = NFOLDS,
#                               # high value is less accurate, but has faster training
#                               thresh = 1e-3,
#                               # again lower number of iterations for faster training
#                               maxit = 1e3)

# save(glmnet_classifier, file = "glmnet_classifier.RData")
load("glmnet_classifier.RData")
plot(glmnet_classifier)

print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

# testing
test = nas #! to actually get values for nas (exclude to validate)

it_test = tok_fun(prep_fun(test$description))
# turn off progressbar because it won't look nice in rmd
it_test = itoken(it_test, ids = test$id, progressbar = FALSE)


dtm_test = create_dtm(it_test, vectorizer)

preds = predict(glmnet_classifier, dtm_test, type = 'response')
glmnet:::auc(test$cat_no, preds)

View(preds); View(nas)

# impute predicted category for nas dataset
cat_dat = as.data.frame(cbind(category = sort(unique(uk$category)[-1]),
                              cat_no = colnames(preds)))

preds_dat = as.data.frame(cbind(cat_no = as.character(apply(preds, 1, which.max)),
                                preds = apply(preds, 1, max)))

preds_dat = left_join(preds_dat, cat_dat, by = "cat_no")

uk[as.numeric(nas$id),]$category = preds_dat$category

head(uk)

# check previously taken out "Home" and "Kitchen" values are correct
table(uk$category[as.numeric(h_random)])
table(uk$category[as.numeric(k_random)]) # ok
table(uk$category[as.numeric(t_random)]) # ok

hk = uk[as.numeric(h_random),] %>% filter(category == "Kitchen") # should be home
uk[as.numeric(hk$id),]$category = "Home"

# check that all UK data accounted for
nrow(dat %>% filter(Country == "United Kingdom")) == nrow(uk)

# sanity check too see if categorization is sufficient
view_cats_desc = function(x){
        View(table(uk %>% 
                           filter(category == names(table(uk$category)[x])) %>% 
                           select(description)), names(table(uk$category)[x]))
}

# final manual clean-up for outliers
# "Uncategorized" category e.g blanks and ? in "Home"

uk[which(uk$description %in% ""),]$category = "Uncategorized"
table(uk[grepl("\\?", uk$description),]$description)
uk[grepl("\\?", uk$description),]$category = "Uncategorized"

# for(i in 1:length(table(uk$category))){
#        view_cats_desc(i)
# }

##### --------------- Data Analysis --------------- #####

# create csv for analysis
# write.csv(uk, file = "uk_sales.csv", row.names = F)

# SEE POWER BI ("uk sales analysis")
