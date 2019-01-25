###########################################
# program: sample_addhealth
# file: sample.txt
# author: Angelo Mele
# date created: 07/20/2009
# last modified: 01/21/2019
# description: this file modifies the AddHealth data
#              from the csv format to construct the
#              sample to be used in estimation.
#              
############################################



########################################
#  STEP 1: FRIEND FILES
#######################################


# clean memory
rm(list=ls())

# upload file with friends links
xx<-read.csv(file="E:/backup_phoenix/addhealth/intermediate/friendraw.csv")

# create friends list
# for each respondent i, create list of his friends as a vector containing on the first 
# column the id of respondent and on the second the id of nominated friend
f<-array(0,c(207450,2))
i=1
for (k in 1:20745) {
  for (j in 3:12) {
    f[i,1]<-xx[k,2] 
    f[i,2]<-xx[k,j]
    i = i + 1
  }
}


# get rid of people not on the roster
# get rid of missing values
g<-f[!is.na(f[,2]),]
# get rid of nominations in sample school but not on list
g<-g[g[,2]!=99999999,]
# get rid of nominations not in sample school and not in sister school
g<-g[g[,2]!=77777777,]
# get rid of nominations in sister school but not on list
g<-g[g[,2]!=88888888,]
# get rid of nominations that are also partners (this is kind of arbitrary)
g<-g[g[,2]!=55555555,]


# write file to a csv
write.csv(g, file="E:/backup_phoenix/addhealth/intermediate/friendlist.csv")

# clean memory
rm(list=ls())



##########################################
#  STEP 2: INDIVIDUAL ATTRIBUTES
##########################################

# clean memory
rm(list=ls())

# upload file with attributes data
xx<-read.csv(file="E:/backup_phoenix//addhealth/intermediate/attributesraw.csv")

# attach dataset
attach(xx)


##########   RACIAL VARIABLES    ###########

# DEFINITION 1 : hispanic, non-hisp whites, non-hisp blacks
#               non-hisp asians, non-hisp others (contains native americans)
#               RACE IDENTIFIED BY INTERVIEWER, hispanic identified by respondent

# hispanic
hisp <- 1*(H1GI4==1)
# non-hispanic whites
white <- 1*(H1GI9==1)*(hisp==0)
# non-hispanic blacks
black <- 1*(H1GI9==2)*(hisp==0)
# non-hispanic asians
asian <- 1*(H1GI9==4)*(hisp==0)
# non-hispanic others
other <- 1*(H1GI9==5)*(hisp==0) + 1*(H1GI9==3)*(hisp==0)


# DEFINITION 2 : whites, blacks, asians, native amer, others (as in Census 1990)
#               RACE IDENTIFIED BY INTERVIEWER

# whites
white2 <- 1*(H1GI9==1)
# blacks
black2 <- 1*(H1GI9==2)
# asians
asian2 <- 1*(H1GI9==4)
# native americans
natam2 <- 1*(H1GI9==3)
# others
other2 <- 1*(H1GI9==5)


# DEFINITION 3 : whites, blacks, asians, native amer, others (as in Census 1990)
#                individual can indicate multiple racial groups
#               RACE IDENTIFIED BY RESPONDENT

# whites
white3 <- 1*(H1GI6A==1)
# blacks
black3 <- 1*(H1GI6B==1)
# asians
asian3 <- 1*(H1GI6D==1)
# native americans
natam3 <- 1*(H1GI6C==1)
# others
other3 <- 1*(H1GI6E==1)



# RACE VARIABLE 1 : 1=white, 2=black, 3=asian, 4=hispanic, 5=other (including nat. am)
#                   using definition 1

# hispanic
race <- 4*(hisp==1)
# non-hispanic whites
race <- race + 1*(white==1)*(hisp==0)
# non-hispanic blacks
race <- race + 2*(black==1)*(hisp==0)
# non-hispanic asians
race <- race + 3*(asian==1)*(hisp==0)
# non-hispanic others
race <- race + 5*(other==1)*(hisp==0)



# RACE VARIABLE 2 : 1=whites, 2=blacks, 3=asians, 4=native amer, 5=others (as in Census 1990)
#                   using definition 2

# whites
race2 <- 1*(white2==1)
# blacks
race2 <- 2*(black2==1)
# asians
race2 <- 3*(asian2==1)
# native americans
race2 <- 4*(natam2==1)
# others
race2 <- 5*(other2==1)


# RACE VARIABLE 3 : 1=whites, 2=blacks, 3=asians, 4=native amer, 5=others (as in Census 1990)
#                   using definition 3

# whites
race3 <- 1*(white3==1)
# blacks
race3 <- 2*(black3==1)
# asians
race3 <- 3*(asian3==1)
# native americans
race3 <- 4*(natam3==1)
# others
race3 <- 5*(other3==1)




##########   GRADE VARIABLES    ###########

# DEFINITION 1: As a continuous variable

grade <- H1GI20

# code as missing the values that are refused, skip,
# don`t know, school doesnt have grade levels
grade[grade>13]<-NA


# DEFINITION 2: As a categorical variable, 1=7th, 2=8th and so on

grade2 <- H1GI20 - 6

# code as missing the values that are refused, skip,
# don`t know, school doesnt have grade levels
grade2[grade2>13]<-NA


# DEFINITION 3: dummies?




############   GENDER VARIABLE    ################

gender <- BIO_SEX - 1
gender[gender>1]<-NA





#### create data matrix ######
# 
data<-cbind(id,SCID,COMMID,race,grade,gender)

# write file to a csv
write.csv(data, file="/hpcapp1/home/amele1/addhealth/intermediate/indivattributes.csv")

# clean memory
rm(list=ls())





##########################################
#  STEP 3: INDIVIDUAL ATTRIBUTES SATURATED SCHOOLS
##########################################

# clean memory
rm(list=ls())

# upload file with attributes data
xx<-read.csv(file="/hpcapp1/home/amele1/addhealth/intermediate/attributesraw_sat.csv")

# attach dataset
attach(xx)


##########   RACIAL VARIABLES    ###########

# DEFINITION 1 : hispanic, non-hisp whites, non-hisp blacks
#               non-hisp asians, non-hisp others (contains native americans)
#               RACE IDENTIFIED BY INTERVIEWER, hispanic identified by respondent

# hispanic
hisp <- 1*(H1GI4==1)
# non-hispanic whites
white <- 1*(H1GI9==1)*(hisp==0)
# non-hispanic blacks
black <- 1*(H1GI9==2)*(hisp==0)
# non-hispanic asians
asian <- 1*(H1GI9==4)*(hisp==0)
# non-hispanic others
other <- 1*(H1GI9==5)*(hisp==0) + 1*(H1GI9==3)*(hisp==0)


# DEFINITION 2 : whites, blacks, asians, native amer, others (as in Census 1990)
#               RACE IDENTIFIED BY INTERVIEWER

# whites
white2 <- 1*(H1GI9==1)
# blacks
black2 <- 1*(H1GI9==2)
# asians
asian2 <- 1*(H1GI9==4)
# native americans
natam2 <- 1*(H1GI9==3)
# others
other2 <- 1*(H1GI9==5)


# DEFINITION 3 : whites, blacks, asians, native amer, others (as in Census 1990)
#                individual can indicate multiple racial groups
#               RACE IDENTIFIED BY RESPONDENT

# whites
white3 <- 1*(H1GI6A==1)
# blacks
black3 <- 1*(H1GI6B==1)
# asians
asian3 <- 1*(H1GI6D==1)
# native americans
natam3 <- 1*(H1GI6C==1)
# others
other3 <- 1*(H1GI6E==1)



# RACE VARIABLE 1 : 1=white, 2=black, 3=asian, 4=hispanic, 5=other (including nat. am)
#                   using definition 1

# hispanic
race <- 4*(hisp==1)
# non-hispanic whites
race <- race + 1*(white==1)*(hisp==0)
# non-hispanic blacks
race <- race + 2*(black==1)*(hisp==0)
# non-hispanic asians
race <- race + 3*(asian==1)*(hisp==0)
# non-hispanic others
race <- race + 5*(other==1)*(hisp==0)



# RACE VARIABLE 2 : 1=whites, 2=blacks, 3=asians, 4=native amer, 5=others (as in Census 1990)
#                   using definition 2

# whites
race2 <- 1*(white2==1)
# blacks
race2 <- 2*(black2==1)
# asians
race2 <- 3*(asian2==1)
# native americans
race2 <- 4*(natam2==1)
# others
race2 <- 5*(other2==1)


# RACE VARIABLE 3 : 1=whites, 2=blacks, 3=asians, 4=native amer, 5=others (as in Census 1990)
#                   using definition 3

# whites
race3 <- 1*(white3==1)
# blacks
race3 <- 2*(black3==1)
# asians
race3 <- 3*(asian3==1)
# native americans
race3 <- 4*(natam3==1)
# others
race3 <- 5*(other3==1)




##########   GRADE VARIABLES    ###########

# DEFINITION 1: As a continuous variable

grade <- H1GI20

# code as missing the values that are refused, skip,
# don`t know, school doesnt have grade levels
grade[grade>13]<-NA


# DEFINITION 2: As a categorical variable, 1=7th, 2=8th and so on

grade2 <- H1GI20 - 6

# code as missing the values that are refused, skip,
# don`t know, school doesnt have grade levels
grade2[grade2>13]<-NA


# DEFINITION 3: dummies?




############   GENDER VARIABLE    ################

gender <- BIO_SEX - 1
gender[gender>1]<-NA





#### create data matrix ######
# 
data<-cbind(id,SCID,COMMID,race,grade,gender)

# write file to a csv
write.csv(data, file="/hpcapp1/home/amele1/addhealth/intermediate/indivattributes_sat.csv")

# clean memory
rm(list=ls())









##########################################
#  STEP 3: INDIVIDUAL ATTRIBUTES SATURATED SCHOOLS
#          NEW VARIABLES 6/16/2011
##########################################

# clean memory
rm(list=ls())

# upload file with attributes data
xx<-read.csv(file="/hpcapp1/home/amele1/addhealth/intermediate/attributesraw_sat2.csv")

# attach dataset
attach(xx)


##########   RACIAL VARIABLES    ###########

# DEFINITION 1 : hispanic, non-hisp whites, non-hisp blacks
#               non-hisp asians, non-hisp others (contains native americans)
#               RACE IDENTIFIED BY INTERVIEWER, hispanic identified by respondent

# hispanic
hisp <- 1*(H1GI4==1)
# non-hispanic whites
white <- 1*(H1GI9==1)*(hisp==0)
# non-hispanic blacks
black <- 1*(H1GI9==2)*(hisp==0)
# non-hispanic asians
asian <- 1*(H1GI9==4)*(hisp==0)
# non-hispanic others
other <- 1*(H1GI9==5)*(hisp==0) + 1*(H1GI9==3)*(hisp==0)


# DEFINITION 2 : whites, blacks, asians, native amer, others (as in Census 1990)
#               RACE IDENTIFIED BY INTERVIEWER

# whites
white2 <- 1*(H1GI9==1)
# blacks
black2 <- 1*(H1GI9==2)
# asians
asian2 <- 1*(H1GI9==4)
# native americans
natam2 <- 1*(H1GI9==3)
# others
other2 <- 1*(H1GI9==5)


# DEFINITION 3 : whites, blacks, asians, native amer, others (as in Census 1990)
#                individual can indicate multiple racial groups
#               RACE IDENTIFIED BY RESPONDENT

# whites
white3 <- 1*(H1GI6A==1)
# blacks
black3 <- 1*(H1GI6B==1)
# asians
asian3 <- 1*(H1GI6D==1)
# native americans
natam3 <- 1*(H1GI6C==1)
# others
other3 <- 1*(H1GI6E==1)



# RACE VARIABLE 1 : 1=white, 2=black, 3=asian, 4=hispanic, 5=other (including nat. am)
#                   using definition 1

# hispanic
race <- 4*(hisp==1)
# non-hispanic whites
race <- race + 1*(white==1)*(hisp==0)
# non-hispanic blacks
race <- race + 2*(black==1)*(hisp==0)
# non-hispanic asians
race <- race + 3*(asian==1)*(hisp==0)
# non-hispanic others
race <- race + 5*(other==1)*(hisp==0)



# RACE VARIABLE 2 : 1=whites, 2=blacks, 3=asians, 4=native amer, 5=others (as in Census 1990)
#                   using definition 2

# whites
race2 <- 1*(white2==1)
# blacks
race2 <- 2*(black2==1)
# asians
race2 <- 3*(asian2==1)
# native americans
race2 <- 4*(natam2==1)
# others
race2 <- 5*(other2==1)


# RACE VARIABLE 3 : 1=whites, 2=blacks, 3=asians, 4=native amer, 5=others (as in Census 1990)
#                   using definition 3

# whites
race3 <- 1*(white3==1)
# blacks
race3 <- 2*(black3==1)
# asians
race3 <- 3*(asian3==1)
# native americans
race3 <- 4*(natam3==1)
# others
race3 <- 5*(other3==1)




##########   GRADE VARIABLES    ###########

# DEFINITION 1: As a continuous variable

grade <- H1GI20

# code as missing the values that are refused, skip,
# don`t know, school doesnt have grade levels
grade[grade>13]<-NA


# DEFINITION 2: As a categorical variable, 1=7th, 2=8th and so on

grade2 <- H1GI20 - 6

# code as missing the values that are refused, skip,
# don`t know, school doesnt have grade levels
grade2[grade2>13]<-NA


# DEFINITION 3: dummies?




############   GENDER VARIABLE    ################

gender <- BIO_SEX - 1
gender[gender>1]<-NA



############   PARENTAL INCOME VARIABLE    ################

income<-log(pa55imp+.000001)


############   ATTRACTIVENESS VARIABLE    ################


beauty<-1*(H1IR1==5)
personality<-1*(H1IR2==5)



############# SCHOOL DUMMIES ##################

saturated<-c(1,2,3,7,8,28,58,77,81,88,106,115,126,175,194,369)
school1<-1*(SCID==saturated[1])
school2<-1*(SCID==saturated[2])
school3<-1*(SCID==saturated[3])
school4<-1*(SCID==saturated[4])
school5<-1*(SCID==saturated[5])
school6<-1*(SCID==saturated[6])
school7<-1*(SCID==saturated[7])
school8<-1*(SCID==saturated[8])
school9<-1*(SCID==saturated[9])
school10<-1*(SCID==saturated[10])
school11<-1*(SCID==saturated[11])
school12<-1*(SCID==saturated[12])
school13<-1*(SCID==saturated[13])
school14<-1*(SCID==saturated[14])
school15<-1*(SCID==saturated[15])
school16<-1*(SCID==saturated[16])




#### create data matrix ######
# 
data<-cbind(id,SCID,COMMID,race,grade,gender,income,beauty,personality,school1,school2,school3,school4,school5,school6,school7,school8,school9,school10,school11,school12,school13,school14,school15,school16)

# write file to a csv
write.csv(data, file="/hpcapp1/home/amele1/addhealth/intermediate/indivattributes_sat2.csv")

# clean memory
rm(list=ls())


