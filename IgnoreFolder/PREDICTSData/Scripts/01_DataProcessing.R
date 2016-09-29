library(yarg)

dat <- readRDS("~/Dropbox/PhD_Copy/Wildlife Garden/PREDICTSData/Data/diversity-2015-11-10-03-36-22.rds")

uk_dat <- dat[dat$Country %in% c("Isle of Man","United Kingdom"),]

uk_dat <- droplevels(uk_dat)

levels(uk_dat$Predominant_habitat)

unique(uk_dat$SS[uk_dat$Predominant_habitat == "Urban"])

# [1] AD1_2004__Darvill 1   -- NO
#     AD1_2009__Knight 1    -- NO     
 # [3] AD1_2011__Bates 1    -- NO     
   # AD1_2011b_Hanley 1        -- NO 
 # [5] DB1_2004__Helden 1      -- NO
 # SC1_2014__Fowler 1         -- NO
 # [9] VB1_2012__LeightonGoodall 1 
 # VB1_2013a_Jones 1   -- NO