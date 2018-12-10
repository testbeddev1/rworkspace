#1a. Install db package
install.packages("RPostgreSQL")
#1b.Call db package
library(RPostgreSQL)
#1c. Specify DB driver in DB package
drv<-dbDriver("PostgreSQL")
#1d. Declare DB connection features; note user name and password
#(if you change 'con' take note, each select statement must start with the 'con'nection name)
con<-dbConnect(drv,dbname="aact",host="aact-db.ctti-clinicaltrials.org",port=5432,user="nwstudy1",password="Helloworld1!")


#Step 2: Call data from database as data frame to get a grasp of what is available

#2a. Collect study features where the key words used by investigators are 'hiv like'; 
#note the wild card operator in the where clause

hiv_with_keywords<-dbGetQuery(con,"select 
                           keywords.nct_id,
                              keywords.downcase_name,
                              studies.study_type,
                              studies.baseline_population,
                              studies.brief_title,
                              studies.official_title,
                              studies.phase,
                              studies.enrollment,
                              studies.number_of_arms,
                              studies.number_of_groups,
                              studies.is_fda_regulated_drug,
                              studies.is_fda_regulated_device
                              from 
                              ctgov.keywords left join ctgov.studies 
                              on 
                              ctgov.keywords.nct_id = ctgov.studies.nct_id 
                              where 
                              ctgov.keywords.downcase_name like 'hiv%'")

View(hiv_with_keywords)

#2b. Collect study features where the key words used by investigators are aids like;
#note the wild card operator in the where clause
aids_with_keywords<-dbGetQuery(con,"select 
                               keywords.nct_id,
                               keywords.downcase_name,
                               studies.study_type,
                               studies.baseline_population,
                               studies.brief_title,
                               studies.official_title,
                               studies.phase,
                               studies.enrollment,
                               studies.number_of_arms,
                               studies.number_of_groups,
                               studies.is_fda_regulated_drug,
                               studies.is_fda_regulated_device
                               from 
                               ctgov.keywords left join ctgov.studies 
                               on 
                               ctgov.keywords.nct_id = ctgov.studies.nct_id 
                               where 
                               ctgov.keywords.downcase_name like 'aids%'")
View(aids_with_keywords)

#2c. Collect study features where the disease conditions for intervention is HIV like;
#note the wild card operator in the where clause
hiv_with_condition<-dbGetQuery(con,"select
                               conditions.nct_id,
                               conditions.downcase_name,
                               studies.study_type,
                               studies.baseline_population,
                               studies.brief_title,
                               studies.official_title,
                               studies.phase, 
                               studies.enrollment, 
                               studies.number_of_arms, 
                               studies.number_of_groups,  
                               studies.is_fda_regulated_drug, 
                               studies.is_fda_regulated_device 
                               from 
                               ctgov.conditions left join ctgov.studies 
                               on 
                               ctgov.conditions.nct_id = ctgov.studies.nct_id
                               where 
                               ctgov.conditions.downcase_name like 'hiv%'")
View(hiv_with_condition)

#2d. Collect study features where the disease conditions for intervention is aids like;
#note the wild card operator in the where clause
aids_with_condition<-dbGetQuery(con,"select
                                conditions.nct_id,
                                conditions.downcase_name,
                                studies.study_type,
                                studies.baseline_population,
                                studies.brief_title,
                                studies.official_title,
                                studies.phase, 
                                studies.enrollment, 
                                studies.number_of_arms, 
                                studies.number_of_groups,  
                                studies.is_fda_regulated_drug, 
                                studies.is_fda_regulated_device 
                                from 
                                ctgov.conditions left join ctgov.studies 
                                on 
                                ctgov.conditions.nct_id = ctgov.studies.nct_id
                                where 
                                ctgov.conditions.downcase_name like 'aids%'")
View(aids_with_condition)

#Step 3: Call trial id tagged key words and disease conditions for our four study groups under step 2

#3a. Collect Key words and conditions from trial ids where trials are 'hiv like';
#note the wild card operator in the where clause
hiv_for_melt<-dbGetQuery(con,"select 
                       distinct downcase_name,
                         count(nct_id),
                         'conditions' AS studytype
                         from
                         ctgov.conditions
                         where downcase_name like 'hiv%' 
                         group by downcase_name
                         union
                         select
                         distinct downcase_name,
                         count(nct_id),
                         'keywords' AS studytype
                         from
                         ctgov.keywords
                         where downcase_name like 'hiv%' 
                         group by downcase_name
                         ")
View(hivformelt)
#3b. Collect Key words and conditions from trial ids where trials are 'aids like';
#note the wild card operator in the where clause
aids_for_melt<-dbGetQuery(con,"select 
                        distinct downcase_name,
                          count(nct_id),
                          'conditions' AS studytype
                          from
                          ctgov.conditions
                          where downcase_name like 'aids%' 
                          group by downcase_name
                          union
                          select
                          distinct downcase_name,
                          count(nct_id),
                          'keywords' AS studytype
                          from
                          ctgov.keywords
                          where downcase_name like 'aids%' 
                          group by downcase_name
                          order by count desc
                          ")
#3c. Remove not hiv terms from HIV like trial list (the aids version looked clean to me...)
# if you want to see the before cleaning version #View(hiv_for_melt)#
library(dplyr)
library(tidyr)
hiv_for_melt<-filter(hiv_for_melt,downcase_name !="hives")
hiv_for_melt<-filter(hiv_for_melt,downcase_name !="hivep2")
hiv_for_melt<-filter(hiv_for_melt,downcase_name !="hiveac")
hiv_for_melt<-filter(hiv_for_melt,downcase_name !="hive")
hiv_for_melt<-filter(hiv_for_melt,downcase_name !="hivst")
hiv_for_melt<-filter(hiv_for_melt,downcase_name !="hivac")
View(hiv_for_melt)

#3d. Melt 3a and 3b into a matrix like data frame (here melt is really called spread-)
# below spread does not mean key word condition pairs but paired sums of like terms across item response classes
#stacked bar may work better but I like seeing mutual terms across response types where available
# one use is noticing how 'hiv inventions'and 'hiv infection' are heavily declared but are different strings-
#ontology opportunity... 

HIV_Trials_spread<-spread(hiv_for_melt,studytype,count,fill=NA,convert=FALSE,drop=TRUE,sep=NULL)
AIDS_Trials_spread<-spread(aids_for_melt,studytype,count,fill=NA,convert=FALSE,drop=TRUE,sep=NULL)
View(HIV_Trials_spread)
View(AIDS_Trials_spread)


#3e. Plot 
# this plot is really a view of a plot- it populates to viewer in R studio, not plots
# single observations will not be- scatter plotted; only records where both keyword and condition had the same strings-
library(plotly)
AIDS_Trial_Terms<-plot_ly(data=AIDS_Trials_spread,x=~conditions,y=~keywords,text=~downcase_name)
print(AIDS_Trial_Terms)
HIV_Trial_Terms<-plot_ly(data=HIV_Trials_spread,x=~conditions,y=~keywords,text=~downcase_name)
print(HIV_Trial_Terms)

#Step 4. Call and join study data set features, key words and conditions for four studies.
#This method is a touch clunky due to the underlying database features. 
#Here we use an inner join to fit trial id-key word or conditions pairs to a set of hiv and 
#aids like + trial features. We also clean our feature sets to make sure they do not contain 
#bad hiv and aids names. The inner join does double duty here, 
#cleaning our trial features retrospectively if they only have dirty hiv 
#and aids terms but enrolling the features if they have clean terms.


#4a. Make hiv study group
hiv_group<-dbGetQuery(con,"
                      select
                      keywords.nct_id AS nct_id,
                      studies.study_type,
                      studies.phase,
                      studies.enrollment,
                      studies.number_of_arms,
                      studies.number_of_groups,
                      studies.is_fda_regulated_drug,
                      studies.is_fda_regulated_device
                      from ctgov.keywords
                      left join 
                      ctgov.studies
                      on
                      ctgov.keywords.nct_id = ctgov.studies.nct_id
                      where
                      ctgov.keywords.downcase_name like 'hiv%'
                      union
                      select
                      conditions.nct_id AS nct_id,
                      studies.study_type,
                      studies.phase,
                      studies.enrollment,
                      studies.number_of_arms,
                      studies.number_of_groups,
                      studies.is_fda_regulated_drug,
                      studies.is_fda_regulated_device
                      from ctgov.conditions
                      left join 
                      ctgov.studies
                      on
                      ctgov.conditions.nct_id = ctgov.studies.nct_id
                      where
                      ctgov.conditions.downcase_name like 'hiv%'
                      ")


#4b.make aids study group
aids_group<-dbGetQuery(con,"
                       select
                       keywords.nct_id AS nct_id,
                       studies.study_type,
                       studies.phase,
                       studies.enrollment,
                       studies.number_of_arms,
                       studies.number_of_groups,
                       studies.is_fda_regulated_drug,
                       studies.is_fda_regulated_device
                       from ctgov.keywords
                       left join 
                       ctgov.studies
                       on
                       ctgov.keywords.nct_id = ctgov.studies.nct_id
                       where
                       ctgov.keywords.downcase_name like 'aids%'
                       union
                       select
                       conditions.nct_id AS nct_id,
                       studies.study_type,
                       studies.phase,
                       studies.enrollment,
                       studies.number_of_arms,
                       studies.number_of_groups,
                       studies.is_fda_regulated_drug,
                       studies.is_fda_regulated_device
                       from ctgov.conditions
                       left join 
                       ctgov.studies
                       on
                       ctgov.conditions.nct_id = ctgov.studies.nct_id
                       where
                       ctgov.conditions.downcase_name like 'aids%'
                       ")

#4c.make key words and conditions features
keywords_aids<-dbGetQuery(con,"select keywords.nct_id,keywords.downcase_name AS keywords_aids
                          from ctgov.keywords
                          where keywords.downcase_name like 'aids%'")
conditions_aids<-dbGetQuery(con,"select conditions.nct_id,conditions.downcase_name AS conditions_aids
                            from ctgov.conditions
                            where conditions.downcase_name like 'aids%'")
keywords_hiv<-dbGetQuery(con,"select keywords.nct_id,keywords.downcase_name AS keywords_hiv
                         from ctgov.keywords
                         where keywords.downcase_name like 'hiv%'")
conditions_hiv<-dbGetQuery(con,"select conditions.nct_id,conditions.downcase_name AS conditions_hiv
                           from ctgov.conditions
                           where conditions.downcase_name like 'hiv%'")

#4d. clean features

#remove bad captures from conditions_hiv
conditions_hiv<-filter(conditions_hiv, conditions_hiv !="hives")
conditions_hiv<-filter(conditions_hiv, conditions_hiv !="hivep2")
conditions_aids<-filter(conditions_aids,conditions_aids !="hives")
conditions_aids<-filter(conditions_aids,conditions_aids !="hivep2")
keywords_hiv<-filter(keywords_hiv,keywords_hiv !="hives")
keywords_hiv<-filter(keywords_hiv,keywords_hiv != "hivep2")
keywords_hiv<-filter(keywords_hiv,keywords_hiv !="hiveac")
keywords_hiv<-filter(keywords_hiv,keywords_hiv !="hive")
keywords_hiv<-filter(keywords_hiv,keywords_hiv !="hivst")
keywords_aids<-filter(keywords_aids,keywords_aids !="hives")
keywords_aids<-filter(keywords_aids,keywords_aids != "hivep2")
keywords_aids<-filter(keywords_aids,keywords_aids !="hiveac")
keywords_aids<-filter(keywords_aids,keywords_aids !="hive")
keywords_aids<-filter(keywords_aids,keywords_aids !="hivst")

#4e. join features
study_1_hiv_conditions<-inner_join(hiv_group,conditions_hiv)
study_2_hiv_keywords<-inner_join(hiv_group,keywords_hiv)
study_3_aids_conditions<-inner_join(aids_group,conditions_aids)
study_4_aids_keywords<-inner_join(aids_group,keywords_aids)

#Step 5: Install, launch and connect to local analysis cluster

#this is the hard part- we are going to download some java software and connect R studio to it. The java software provides a virtual cluster that will process our
#study localy on this very machine! There are two ways to do this- the easy way and the hard way.

#5.easy way.1 load library like this:
#if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
#pkgs <- c("RCurl","jsonlite")
#for (pkg in pkgs) {
# if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
#}
#install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-wright/8/R")
library(h2o)
h2o.init(ip="localhost",port=54321)

#if you get a java version error (64 bit versions of java only with H20) update java, and restart your R console
#if you get an error saying you can not connect to H2o do “the hard way”

#5.hard way.1 download, unzip and launch the java executable file
#	a. download h2o from: http://h2o-release.s3.amazonaws.com/h2o/rel-xia/2/index.html #use the 'download and run' version#
#	b. unzip it on your desktop-or any where else where you will not lose it...
#	c. launch the java executable file called: 'h2o' ; it should be just inside the unziped folder
#	d. check if the cluster is running by going to  in your browser: 'http://localhost:54321/flow/index.html'
#	e. load h20 library in R that did not work before by calling library(h20)
#don't do this if easy way worked#library(h2o) #note the o in h2o is O as in Off, not zero#
#don't do this if easy way worked#h2o.init(ip="localhost",port= 54321)
#So that's it, you did not connect the library in R to the native instance but connected to a cluster you hand launched yourself instead.

#Step 6: Send study data sets to analysis cluster
#you can not send data from R to cluster; you have to write out csv and then call to cluster like so
#6a. write files out to working directory for R; default is my documents under windows via c/user-
# you can also search for the file name in explorer if you dont know your R default directory

#if you link to git you may write these to git and not my documents!
# if you write to git point to git, if you write to my documents... point to my documents-
write.csv(study_1_hiv_conditions,file="C:/Users/nick/Documents/study1.csv")
write.csv(study_2_hiv_keywords,file="C:/Users/nick/Documents/study2.csv")
write.csv(study_3_aids_conditions,file="C:/Users/nick/Documents/study3.csv")
write.csv(study_4_aids_keywords,file="C:/Users/nick/Documents/study4.csv")

#6b. Read written out local files to server
study1_h2o<-h2o.importFile(path="C:/Users/nick/Documents/study1.csv")
summary(study1_h2o)
study2_h2o<-h2o.importFile(path="C:/Users/nick/Documents/study2.csv")
summary(study2_h2o)
study3_h2o<-h2o.importFile(path="C:/Users/nick/Documents/study3.csv")
summary(study3_h2o)
study4_h2o<-h2o.importFile(path="C:/Users/nick/Documents/study4.csv")
summary(study4_h2o)

#6c. Name response, predictor and splits
response_s1<-"conditions_hiv"
response_s2<-"keywords_hiv"
response_s3<-"conditions_aids"
response_s4<-"keywords_aids"

predictor_s1<-setdiff(names(study1_h2o),c(response_s1))
predictor_s2<-setdiff(names(study2_h2o),c(response_s2))
predictor_s3<-setdiff(names(study3_h2o),c(response_s3))
predictor_s4<-setdiff(names(study4_h2o),c(response_s4))

splits_s1<-h2o.splitFrame(data = study1_h2o,
                          ratios = c(0.6,0.2),
                          destination_frames = c("s1_train.hex", "s1_valid.hex", "s1_test.hex"),
                          seed = 1234)
s1_train<-splits_s1 [[1]]
s1_valid<-splits_s1[[2]]
s1_test<-splits_s1[[3]]

splits_s2<-h2o.splitFrame(data = study2_h2o,
                          ratios = c(0.6,0.2),
                          destination_frames = c("s2_train.hex", "s2_valid.hex", "s2_test.hex"),
                          seed = 1234)
s2_train<-splits_s2 [[1]]
s2_valid<-splits_s2[[2]]
s2_test<-splits_s2[[3]]

splits_s3<-h2o.splitFrame(data = study3_h2o,
                          ratios = c(0.6,0.2),
                          destination_frames = c("s3_train.hex", "s3_valid.hex", "s3_test.hex"),
                          seed = 1234)
s3_train<-splits_s3 [[1]]
s3_valid<-splits_s3[[2]]
s3_test<-splits_s3[[3]]

splits_s4<-h2o.splitFrame(data = study4_h2o,
                          ratios = c(0.6,0.2),
                          destination_frames = c("s4_train.hex", "s4_valid.hex", "s4_test.hex"),
                          seed = 1234)
s4_train<-splits_s4 [[1]]
s4_valid<-splits_s4[[2]]
s4_test<-splits_s4[[3]]

#7. Run study models

#7a.study model 1
gbm_s1<-h2o.gbm(x=predictor_s1,
                y=response_s1,
                training_frame=s1_train)

#7b.study model 2
gbm_s2<-h2o.gbm(x=predictor_s2,
                y=response_s2,
                training_frame=s2_train)

#7c.study model 3
gbm_s3<-h2o.gbm(x=predictor_s3,
                y=response_s3,
                training_frame=s3_train)

#7d.study model 4
gbm_s4<-h2o.gbm(x=predictor_s4,
                y=response_s4,
                training_frame=s4_train)

#8 results(results 8b works better than 8a-)

#8a results in R- this can be done but it kind of spams the console a bit
gbm_s1 #for model 1
gbm_s2# for model 2
gbm_s3# for model 3
gbm_s4# for model 4

#8b results in flow (this works best I think)
#navigate your browser to the flow portal at: http://localhost:54321/flow/index.html
# Click Model in the menu and then click list all models-then click inspect, or click a model name-
#you will get a click sub menu with 10 or so output options- 
