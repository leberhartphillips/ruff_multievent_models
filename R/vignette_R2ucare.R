## ---- message=FALSE, warning=FALSE---------------------------------------
library(R2ucare)

## ---- message=FALSE, warning=FALSE---------------------------------------
# # read in text file as described at pages 50-51 in http://www.phidot.org/software/mark/docs/book/pdf/app_3.pdf
library(RMark)
data_path <- "/Library/Frameworks/R.framework/Versions/3.4/Resources/library/RMark/extdata/"
dipper <- import.chdata(paste0(data_path,"dipper.txt"),field.names=c("ch","sex"),header=FALSE)
data(dipper)
dipper <- as.data.frame(table(dipper))
str(dipper)

## ---- message=FALSE, warning=FALSE---------------------------------------
dip.hist = matrix(as.numeric(unlist(strsplit(as.character(dipper$ch),""))),nrow=length(dipper$ch),byrow=T)
dip.freq = dipper$Freq
dip.group = dipper$sex
head(dip.hist)
head(dip.freq)
head(dip.group)
# 
# ## ---- message=FALSE, warning=FALSE---------------------------------------
# dipper <- system.file("extdata", "ed.txt", package = "R2ucare")
# dipper <- read_headed(dipper)
# 
# ## ---- message=FALSE, warning=FALSE---------------------------------------
# dip.hist <- dipper$encounter_histories
# dip.freq <- dipper$sample_size
# dip.group <- dipper$groups
# head(dip.hist)
# head(dip.freq)
# head(dip.group)
# 
# ## ---- message=FALSE, warning=FALSE---------------------------------------
# dipper = system.file("extdata", "ed.inp", package = "R2ucare")
# dipper = read_inp(dipper,group.df=data.frame(sex=c('Male','Female')))
ruff <- 
  read_inp("/Users/leberhart/Documents/Academic/Postdoc_Bart/projects/ruff_multievent_models/data/cooked/HC_age_sex_morph.inp", 
           group.df = data.frame(sex = c('AFI', 'JFI', 'AFS', 'JFS', 'AMI', 'JMI', 'AMS', 'JMS')))

ruff.hist = ruff$encounter_histories
ruff.freq = ruff$sample_size
ruff.group = ruff$groups
head(ruff.hist)
head(ruff.freq)
head(ruff.group)

# extract groups
# 1) Adult Female Independents
mask = (ruff.group == 'AFI')
ruff.AFI.hist = ruff.hist[mask,]
ruff.AFI.freq = ruff.freq[mask]

# 2) Juvenile Female Independents
mask = (ruff.group == 'JFI')
ruff.JFI.hist = ruff.hist[mask,]
ruff.JFI.freq = ruff.freq[mask]

# 3) Adult Female Satellites
mask = (ruff.group == 'AFS')
ruff.AFS.hist = ruff.hist[mask,]
ruff.AFS.freq = ruff.freq[mask]

# 4) Juvenile Female Satellites
mask = (ruff.group == 'JFS')
ruff.JFS.hist = ruff.hist[mask,]
ruff.JFS.freq = ruff.freq[mask]

# 5) Adult Male Independents
mask = (ruff.group == 'AMI')
ruff.AMI.hist = ruff.hist[mask,]
ruff.AMI.freq = ruff.freq[mask]

# 6) Juvenile Male Independents
mask = (ruff.group == 'JMI')
ruff.JMI.hist = ruff.hist[mask,]
ruff.JMI.freq = ruff.freq[mask]

# 7) Adult Male Satellites
mask = (ruff.group == 'AMS')
ruff.AMS.hist = ruff.hist[mask,]
ruff.AMS.freq = ruff.freq[mask]

# 8) Juvenile Male Satellites
mask = (ruff.group == 'JMS')
ruff.JMS.hist = ruff.hist[mask,]
ruff.JMS.freq = ruff.freq[mask]

# test for trap-happiness
test2ct_AFI = test2ct(ruff.AFI.hist, ruff.AFI.freq)$test2ct %>% t() %>% as.data.frame()
test2ct_JFI = test2ct(ruff.JFI.hist, ruff.JFI.freq)$test2ct %>% t() %>% as.data.frame()
test2ct_AFS = test2ct(ruff.AFS.hist, ruff.AFS.freq)$test2ct %>% t() %>% as.data.frame()
test2ct_JFS = test2ct(ruff.JFS.hist, ruff.JFS.freq)$test2ct %>% t() %>% as.data.frame()
test2ct_AMI = test2ct(ruff.AMI.hist, ruff.AMI.freq)$test2ct %>% t() %>% as.data.frame()
test2ct_JMI = test2ct(ruff.JMI.hist, ruff.JMI.freq)$test2ct %>% t() %>% as.data.frame()
test2ct_AMS = test2ct(ruff.AMS.hist, ruff.AMS.freq)$test2ct %>% t() %>% as.data.frame()
test2ct_JMS = test2ct(ruff.JMS.hist, ruff.JMS.freq)$test2ct %>% t() %>% as.data.frame()
test2ct_df <- 
  data.frame(group = c("AFI", "JFI", "AFS", "JFS", "AMI", "JMI", "AMS", "JMS")) %>% 
  bind_cols(., bind_rows(test2ct_AFI, test2ct_JFI, test2ct_AFS, test2ct_JFS, 
                         test2ct_AMI, test2ct_JMI,test2ct_AMS, test2ct_JMS)) %>% 
  mutate(test = "test2ct")

# test for transience
test3sr_AFI = test3sr(ruff.AFI.hist, ruff.AFI.freq)$test3sr %>% t() %>% as.data.frame()
test3sr_JFI = test3sr(ruff.JFI.hist, ruff.JFI.freq)$test3sr %>% t() %>% as.data.frame()
test3sr_AFS = test3sr(ruff.AFS.hist, ruff.AFS.freq)$test3sr %>% t() %>% as.data.frame()
test3sr_JFS = test3sr(ruff.JFS.hist, ruff.JFS.freq)$test3sr %>% t() %>% as.data.frame()
test3sr_AMI = test3sr(ruff.AMI.hist, ruff.AMI.freq)$test3sr %>% t() %>% as.data.frame()
test3sr_JMI = test3sr(ruff.JMI.hist, ruff.JMI.freq)$test3sr %>% t() %>% as.data.frame()
test3sr_AMS = test3sr(ruff.AMS.hist, ruff.AMS.freq)$test3sr %>% t() %>% as.data.frame()
test3sr_JMS = test3sr(ruff.JMS.hist, ruff.JMS.freq)$test3sr %>% t() %>% as.data.frame()
test3sr_df <- 
  data.frame(group = c("AFI", "JFI", "AFS", "JFS", "AMI", "JMI", "AMS", "JMS")) %>% 
  bind_cols(., bind_rows(test3sr_AFI, test3sr_JFI, test3sr_AFS, test3sr_JFS, 
                         test3sr_AMI, test3sr_JMI,test3sr_AMS, test3sr_JMS)) %>% 
  mutate(test = "test3sr")

test3sr_AFI = test3sr(ruff.AFI.hist, ruff.AFI.freq)
test3sm_AFI = test3sm(ruff.AFI.hist, ruff.AFI.freq)
test2ct_AFI = test2ct(ruff.AFI.hist, ruff.AFI.freq)
test2cl_AFI = test2cl(ruff.AFI.hist, ruff.AFI.freq)
# display results:
test3sr_females
test3sm_females
test2ct_females
test2cl_females

## ---- message=FALSE, warning=FALSE---------------------------------------
overall_CJS(dip.fem.hist, dip.fem.freq)

## ---- message=FALSE, warning=FALSE---------------------------------------
geese = system.file("extdata", "geese.inp", package = "R2ucare")
geese = read_inp(geese)

## ---- message=FALSE, warning=FALSE---------------------------------------
geese.hist = geese$encounter_histories
geese.freq = geese$sample_size

## ---- message=FALSE, warning=FALSE---------------------------------------
test3Gsr(geese.hist,geese.freq)
test3Gsm(geese.hist,geese.freq)
test3Gwbwa(geese.hist,geese.freq)
testMitec(geese.hist,geese.freq)
testMltec(geese.hist,geese.freq)

## ---- message=FALSE, warning=FALSE---------------------------------------
overall_JMV(geese.hist,geese.freq)

## ---- message=FALSE, warning=FALSE---------------------------------------
# Assuming the geese dataset has been read in R (see above):
geese.hist[geese.hist>1] = 1

## ---- message=FALSE, warning=FALSE---------------------------------------
# Assuming the geese dataset has been read in R (see above):
geese.hist[geese.hist==3]=2 # all 3s become 2s

## ---- message=FALSE, warning=FALSE,eval=FALSE----------------------------
#  # Assuming the female dipper dataset has been read in R (see above):
#  t(apply(dip.fem.hist,1,rev))

## ---- message=FALSE, warning=FALSE,eval=FALSE----------------------------
#  # Assuming the female dipper dataset has been read in R (see above):
#  mask = (apply(dip.fem.hist,1,sum)>0 & dip.fem.freq>0) # select non-empty histories, and histories with at least one individual
#  sum(!mask) # how many histories are to be dropped?
#  dip.fem.hist[mask,] # drop these histories from dataset
#  dip.fem.freq[mask] # from counts

## ---- message=FALSE, warning=FALSE, eval=FALSE---------------------------
#  # Assuming the female dipper dataset has been read in R (see above):
#  dip.fem.hist[,c(1,4,6)] # pick occasions 1, 4 and 6 (might be a good idea to clean the resulting dataset)
#  gather_146 = apply(dip.fem.hist[,c(1,4,6)],1,max) # gather occasions 1, 4 and 6 by taking the max
#  dip.fem.hist[,1] = gather_146 # replace occasion 1 by new occasion
#  dip.fem.hist = dip.fem.hist[,-c(4,6)] # drop occasions 4 and 6

## ---- message=FALSE, warning=FALSE, eval=FALSE---------------------------
#  # Assuming the geese dataset has been read in R (see above):
#  for (i in 1:nrow(geese.hist)){
#  occasion_first_encounter = min(which(geese.hist[i,]!=0)) # look for occasion of first encounter
#  geese.hist[i,occasion_first_encounter] = 0 # replace the first non zero by a zero
#  }
#  # delete empty histories from the new dataset
#  mask = (apply(geese.hist,1,sum)>0) # select non-empty histories
#  sum(!mask) # how many histories are to be dropped?
#  geese.hist[mask,] # drop these histories from dataset
#  geese.freq[mask] # from counts

