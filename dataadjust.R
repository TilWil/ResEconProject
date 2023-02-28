############################################################################### 
# Data adjustments

# load "Land use"-Data
database <- data.frame(readRDS("data_sample_2_clean.rds"))


# Have a look:
# var_label(databaseClean$wald)
# val_labels(databaseClean$wald)
# var_label(database$groe_)
# val_labels(database$groe_)
# var_label(database$FoBio)
# val_labels(database$FoBio)
# var_label(database$FoTre)
# val_labels(database$FoTre)
# var_label(database$NoUse)
# val_labels(database$NoUse)
# var_label(database$prei_)
# val_labels(database$prei_)


### Recoding:

# Recode Wald: 0 - wie heute 1 - 10% weniger --\> an stelle 0 2 - bleibt
database <- database %>% mutate(wald_1 = case_when(wald_1 == 0 ~ 1, wald_1 == 1 ~ 0,
                                                   wald_1 == 2 ~ 2),
                                wald_2 = case_when(wald_2 == 0 ~ 1, wald_2 == 1 ~ 0,
                                                   wald_2 == 2 ~ 2),
                                wald_3 = 1)


# Dummys for half and double of individual field and forest size
database <- database %>% mutate(DummyHalf_1 = case_when(groe_1 ==1 ~ 1, TRUE~0),
                                DummyDouble_1 = case_when(groe_1 ==2 ~ 1, TRUE ~0),
                                DummyHalf_2 = case_when(groe_2 ==1 ~ 1, TRUE~0),
                                DummyDouble_2 = case_when(groe_2 ==2 ~ 1, TRUE ~0))
 
# FoBio stays same


# FoTre: 0 - wie heute 1 - halb so groß --> 0
database <- database %>% mutate(FoTre_1 = case_when(FoTre_1 == 0 ~ 1, FoTre_1 == 1 ~ 0,
                                                    FoTre_1 == 2 ~ 2),
                                FoTre_2 = case_when(FoTre_2 == 0 ~ 1, FoTre_2 == 1 ~ 0,
                                                    FoTre_2 == 2 ~ 2),
                                FoTre_3 = 1)


# NoUse: 1 - wie heute / 0 - 0% ungenutzte / 2 - 10% ungenutzte
#! individuell ob linear ist, da "wie heute" status quo variiert individuell
database <- database %>% mutate(NoUse_1 = case_when(NoUse_1 == 0 ~ 1, NoUse_1 == 1 ~ 0,
                                                    NoUse_1 == 2 ~ 2),
                                NoUse_2 = case_when(NoUse_2 == 0 ~ 1, NoUse_2 == 1 ~ 0,
                                                    NoUse_2 == 2 ~ 2),
                                NoUse_3 = 1)


### Mean centered variable

# 1.Remove NAs from variable 
# 2.Build mean from the variable
# 3.Subtract mean centered from the individual values
# (e.g. personal income) --> PInc - mean(PINC)

database <- database %>% drop_na(PINC) %>% mutate(IncomeMC = PINC - mean(PINC))
database <- database %>% drop_na(f006) %>% mutate(NatDayMC = f006 - mean(f006))


#### End
################################################################################