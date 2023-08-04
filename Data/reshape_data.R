if(!require(tidyverse)){install.packages('tidyverse')}

test_dates <- tibble(date=c('Pre-BCG','2m','4m','6m','8m','10m','12m','14m','post_revacc','post_P1','post_P2','post_final'),
                     Group=1,
                     calendar_date=as.Date(c(NA,'26/03/2020',
                                             '28/05/2020',
                                             '28/07/2020',
                                             '28/09/2020',
                                             '28/11/2020',
                                             '28/01/2021',
                                             '28/03/2021',
                                             NA,
                                             '28/07/2022',
                                             '03/10/2022',
                                             '10/01/2023'),format='%d/%m/%Y'),
                     t = c(-2,0,2,4,6,8,10,12,-2,10,12,12))

test_dates <- test_dates %>% mutate(deltat = c(NA,NA,calendar_date[3:8]-calendar_date[2:7],NA,NA,NA,NA))

tmp <- tibble(date=c('Pre-BCG','2m','4m','6m','8m','10m','12m','14m','post_revacc','post_P1','post_P2','post_final'),
              Group=2,
              calendar_date=as.Date(c('24/04/2020',
                                      '24/06/2020',
                                      '28/08/2020',
                                      '28/10/2020',
                                      '28/12/2020',
                                      '28/02/2021',
                                      '28/04/2021',
                                      '28/06/2021',
                                      NA,
                                      '28/07/2022',
                                      '03/10/2022',
                                      '10/01/2023'),format='%d/%m/%Y'),
              t = c(-2,0,2,4,6,8,10,12,-2,10,10,12))

tmp <- tmp %>% mutate(deltat = c(NA,NA,calendar_date[3:8]-calendar_date[2:7],NA,NA,NA,NA))

test_dates <- test_dates %>% bind_rows(tmp)

rm(tmp)



# Check dates of tests
print(test_dates)

phaseII_removals <- readxl::read_excel('PhaseII_removals.xlsx')
phaseII_removals <- phaseII_removals %>% separate(`Concat ID`,sep="/",into=c('ID1','ID2'))

phaseII_dates <- readxl::read_excel('PhaseII_dates.xlsx')
colnames(phaseII_dates) <- c('date','I','II')
phaseII_dates <- phaseII_dates %>% 
  pivot_longer(2:3,names_to='Batch',values_to='calendar_date') %>%
  filter(date!='BCG Vaccination') %>%
  mutate(t=c(-2,-2,0,0,2,2,4,4,6,6,8,8,10,10,12,12),calendar_date=as.Date(calendar_date,'%b %d, %Y')) %>%
  arrange(Batch) 
phaseII_dates <- phaseII_dates %>% mutate(deltat=c(NA,NA,phaseII_dates$calendar_date[3:8]-phaseII_dates$calendar_date[2:7],
                                                   NA,NA,phaseII_dates$calendar_date[11:16]-phaseII_dates$calendar_date[10:15]))



# Note edited version
# * Combines seeders from G1/G2 into single table with new group column
# * Removes textual references to NA for consistent coding of missing values
# * 
phaseI_calves  <- readxl::read_excel('BCG Efficacy_G1 G2_Seeder Calves with Exit TST and IGRA Andrew Format .xlsx',sheet=1)
phaseI_seeders <- readxl::read_excel('BCG Efficacy_G1 G2_Seeder Calves with Exit TST and IGRA Andrew Format .xlsx',sheet=2)
# Note value of -1000 indicates animal was dead at time point
phaseII_calves <- readxl::read_excel('Updated OT_B-I and B-II_Combined_Andrew Format.xlsx',sheet=1)


# Remove columns corresponding to post-exposure tests
phaseI_calves <- phaseI_calves %>% rename(post_revacc=PostRevac,post_P1=`Jul28,2022PreExit`, post_P2=Oct032022Exit_1,post_final=Jan102023Exit_2)
phaseI_calves <- phaseI_calves %>% pivot_longer(cols=c(`Pre-BCG`,`2m`,`4m`,`6m`,`8m`,`10m`,`12m`,`14m`,`post_revacc`,`post_P1`,`post_P2`,`post_final`),names_to = 'date')

phaseI_calves <- phaseI_calves %>% left_join(test_dates %>% select(date,Group,calendar_date,t,deltat))

phaseI_calves <- phaseI_calves %>% mutate(reactor=NA)
phaseI_calves$reactor[which(phaseI_calves$Dx=='SIT')] <- phaseI_calves$value[which(phaseI_calves$Dx=='SIT')] >= 2
phaseI_calves$reactor[which(phaseI_calves$Dx=='SICCT')] <- phaseI_calves$value[which(phaseI_calves$Dx=='SICCT')] > 4
phaseI_calves$reactor[which(phaseI_calves$Dx=='DST0.01')] <- phaseI_calves$value[which(phaseI_calves$Dx=='DST0.01')] >= 0.1
phaseI_calves$reactor[which(phaseI_calves$Dx=='DST0.1')] <- phaseI_calves$value[which(phaseI_calves$Dx=='DST0.1')] >= 0.1
phaseI_calves$reactor[which(phaseI_calves$Dx=='DST1')] <- phaseI_calves$value[which(phaseI_calves$Dx=='DST1')] >= 0.1
phaseI_calves$reactor[which(phaseI_calves$Dx=='DST10')] <- phaseI_calves$value[which(phaseI_calves$Dx=='DST10')] >= 2
phaseI_calves$reactor[which(phaseI_calves$date=='Pre-BCG')] <- FALSE

# Patch in deaths of calves 518,890,938 all of which died between 8m and 10m tests
phaseI_calves <- phaseI_calves %>% mutate(dead=(is.element(ID1,c(518,890)) & t>=4) | (ID1==938 & t>=8))
# Change reactor status to FALSE for missed tests on animals after death so timepoints
# are not dropped for missingness
phaseI_calves$reactor[which(phaseI_calves$dead)] = FALSE

# Peel off the test results from phase II after cross-over
phaseII_seeders <- phaseI_calves %>% 
  filter(startsWith(date,'post'))

phaseI_calves <- phaseI_calves %>% 
  filter(!startsWith(date,'post'))

phaseII_seeders <- phaseII_seeders %>% left_join(phaseI_calves %>% filter(is.element(Uni_ID,unique(phaseII_seeders$Uni_ID))) %>% 
                                                   group_by(Uni_ID,Dx) %>% 
                                                   summarise(status_phaseI = sum(reactor,na.rm=T)>0))

# Code in contact groups: (1,2) phase I replicate groups
#                          3 unvaccinated seeders
#                          4 vaccinated seeders

phaseII_seeders = phaseII_seeders %>% mutate(Group=3+(Treatment!='C'))
phaseII_removals = phaseII_removals %>% mutate(Group=3+(Treatment!='Control'))

phaseII_calves <- phaseII_calves %>% pivot_longer(cols=c(`Pre-BCG`,`2m`,`4m`,`6m`,`8m`,`10m`,`12m`,`14m`),names_to = 'date')

phaseII_calves <- phaseII_calves %>% mutate(Group=(Exposure_Seeder!='Control')+3,.after=ID2)
phaseII_calves <- phaseII_calves %>% left_join(phaseII_dates )

phaseII_calves <- phaseII_calves %>% mutate(reactor=NA)
phaseII_calves$reactor[which(phaseII_calves$Dx=='SIT')] <- phaseII_calves$value[which(phaseII_calves$Dx=='SIT')] > 2
phaseII_calves$reactor[which(phaseII_calves$Dx=='SICCT')] <- phaseII_calves$value[which(phaseII_calves$Dx=='SICCT')] > 4
phaseII_calves$reactor[which(phaseII_calves$Dx=='DST0.01')] <- phaseII_calves$value[which(phaseII_calves$Dx=='DST0.01')] >= 0.1
phaseII_calves$reactor[which(phaseII_calves$Dx=='DST0.1')] <- phaseII_calves$value[which(phaseII_calves$Dx=='DST0.1')] >= 0.1
phaseII_calves$reactor[which(phaseII_calves$Dx=='DST1')] <- phaseII_calves$value[which(phaseII_calves$Dx=='DST1')] >= 0.1
phaseII_calves$reactor[which(phaseII_calves$Dx=='DST10')] <- phaseII_calves$value[which(phaseII_calves$Dx=='DST10')] >= 2
phaseII_calves$reactor[which(phaseII_calves$date=='Pre-BCG')] <- FALSE

# Patch in deaths of calves 518,890,938 all of which died between 8m and 10m tests
#phaseI_calves <- phaseI_calves %>% mutate(dead=(is.element(ID1,c(518,890)) & t>=4) | (ID1==938 & t>=8))

phaseII_calves <- phaseII_calves %>% mutate(dead=(value==-1000))

# Change reactor status to FALSE for missed tests on animals after death so timepoints
# are not dropped for missingness
phaseII_calves$reactor[which(phaseII_calves$dead)] = FALSE

# Make levels for treatment effect consistent for phase II animals

phaseII_calves <- phaseII_calves %>% mutate(Treatment=str_sub(Treatment,1,1))


phaseII_seeders$reactor[which(phaseII_seeders$Dx=='SIT')] <- phaseII_seeders$value[which(phaseII_seeders$Dx=='SIT')] >= 2
phaseII_seeders$reactor[which(phaseII_seeders$Dx=='SICCT')] <- phaseII_seeders$value[which(phaseII_seeders$Dx=='SICCT')] > 4
phaseII_seeders$reactor[which(phaseII_seeders$Dx=='DST0.01')] <- phaseII_seeders$value[which(phaseII_seeders$Dx=='DST0.01')] >= 0.1
phaseII_seeders$reactor[which(phaseII_seeders$Dx=='DST0.1')] <- phaseII_seeders$value[which(phaseII_seeders$Dx=='DST0.1')] >= 0.1
phaseII_seeders$reactor[which(phaseII_seeders$Dx=='DST1')] <- phaseII_seeders$value[which(phaseII_seeders$Dx=='DST1')] >= 0.1
phaseII_seeders$reactor[which(phaseII_seeders$Dx=='DST10')] <- phaseII_seeders$value[which(phaseII_seeders$Dx=='DST10')] >= 2

mk_counts_for_glm_phaseI <- function(diagnostic_test,calves,seeders,test_dates,skin_test)
{
  
  exclude_times = NULL
  next_test = 2
  # Skin test at different frequency to blood tests
  if(skin_test){exclude_times = c(2,6,10);next_test = 4}
  
  
  # Select DST1 test results for all time points post exposures (t>0)
  # Note t=2 test is "4m" post BCG
  # Assume that all sentinels are susceptible when exposed and infectious
  # pressure in first interval is purely due to exposure to 
  DST_tot <- (calves %>% filter(Dx==diagnostic_test  & !is.element(t,exclude_times))) %>% 
    filter(t>0) %>% 
    left_join(calves %>% filter(Dx==diagnostic_test  & !is.element(t,exclude_times)) %>% 
                filter(t>0) %>% 
                arrange(t)  %>% 
                group_by(Uni_ID,Group,Treatment) %>% 
                summarise(t,reactor,reactor2=cumsum(reactor)>0))
  
  # Calculate numbers of R, N, S at each time point for each group
  DST_tot <- DST_tot %>% 
    group_by(Group,Treatment,t) %>% 
    summarise(deltat = unique(deltat), 
              calendar_date=unique(calendar_date),
              R=sum(reactor2)-sum(dead & reactor2), 
              N = length(reactor)-sum(dead), 
              S = N - R) %>%
    arrange(Group,Treatment,t) 
  
  DST_tot <- DST_tot %>% mutate(C=R)
  
  tmp <- (DST_tot %>% group_by(Group,Treatment) %>% summarise(C = c(C[1],diff(C))))
  
  DST_tot <- DST_tot %>% ungroup() %>% mutate(C = tmp$C)
  
  # Time 0 states
  DST_t0 <- calves %>% filter(t==0 & Dx==diagnostic_test  & !is.element(t,exclude_times)) %>% 
    group_by(Group,Treatment,t) %>% 
    summarise(S=n()) %>% mutate(R=0,N=S)
  
  shift_forward <- DST_t0  %>%
    bind_rows(DST_tot %>% select(Group,Treatment,t,S,R,N)) %>% mutate(t=t+next_test)
  
  DST_tot <- DST_tot %>% select(-S,-R,-N) %>% left_join(shift_forward)
  
  # Remove time points for which we still have missing test results
  
  DST_tot <- DST_tot %>% drop_na()
  
  # Remove all treatment groups for time points with missing test results
  
  #tmp <- DST_tot %>% group_by(Group,t) %>% summarise(num_t=length(Treatment)) %>% filter(num_t==1)
  
  #DST_tot <- DST_tot %>% anti_join(tmp)
  
  # Summary of maximum time point available for analysis (due to missingness)
  print(DST_tot %>% group_by(Group,Treatment) %>% summarise(max(t)))
  
  # Calculate total number of infectives (I) at each time step in each group
  # And number of infected vaccinates (IV)
  DST_tot <- DST_tot %>% 
    group_by(t,Group) %>% 
    summarise(Group,Treatment,t,deltat,calendar_date,R,N,S,C,IC = sum(R[which(Treatment=='C')]),IV = sum(R[which(Treatment=='V')]))
  
  # Consistency checks (unit tests) for constructed table
  # Unit test 1: check R+S is constant
  DST_tot %>% ungroup() %>% mutate(unit1 = (N==(S+R))) %>% summarise(sum(!unit1))
  # Unit test 2: check R==IC for control group
  DST_tot %>% ungroup() %>% filter(Treatment=='C') %>% mutate(unit2 = (IC==R)) %>% summarise(sum(!unit2))
  # Unit test 3: check R==IV for vaccinate group
  DST_tot %>% ungroup() %>% filter(Treatment=='V') %>% mutate(unit2 = (IV==R)) %>% summarise(sum(!unit2))
  # Unit test 4: check IV, IC equal for both treatment groups in each replicate for each time point
  DST_tot %>% ungroup() %>% group_by(t,Group) %>% 
    summarise(unit4=length(unique(IC))==1,unit5=length(unique(IV))==1) %>% 
    ungroup() %>% 
    summarise(sum(!unit4),sum(!unit5))
  
  # Use dates of herd tests to work out number of seeders in each period
  
  # N is group size, Control + Vaccinates + Seeders
  # First add Control and Vaccinates together
  
  tmp <- DST_tot %>% group_by(t,Group) %>% summarise(sum(N))
  
  # Note, duplicate rows and same dimensions and shape as DST_tot
  
  tmp <- tmp %>% bind_rows(tmp) %>% arrange(t,Group) 
  
  # Calculate number of seeders present in each interval
  
  DST_tot <- DST_tot %>% mutate(I_S = length(which(seeders$Group==Group & seeders$`Exit date` >= calendar_date)))
  
  DST_tot <- DST_tot %>% ungroup() %>% mutate(N=tmp$`sum(N)`+I_S)
  
  rm(tmp)
  
  DST_tot <- DST_tot %>% mutate(IV_S=0,N_S=I_S,S_S=0,V_S=0) %>% mutate(Batch='I',.after=Group)
  
  return(DST_tot)
  
}

mk_counts_for_glm_phaseII <- function(diagnostic_test,calves,seeders,test_dates,skin_test,impute)
{
  
  exclude_times = NULL
  next_test = 2
  # Skin test at different frequency to blood tests
  if(skin_test){exclude_times = c(2,6,10);next_test = 4}
  
  
  # Select  test results for all time points post exposures (t>0)
  # Note t=2 test is "4m" post BCG
  # Assume that all sentinels are susceptible when exposed and infectious
  # pressure in first interval is purely due to exposure to 
  DST_tot <- (calves %>% filter(Dx==diagnostic_test  & !is.element(t,exclude_times))) %>% 
    filter(t>0) %>% 
    left_join(calves %>% filter(Dx==diagnostic_test  & !is.element(t,exclude_times)) %>% 
                filter(t>0) %>% 
                arrange(t)  %>% 
                group_by(Uni_ID,Group,Treatment) %>% 
                summarise(t,reactor,reactor2=cumsum(reactor)>0))
  
  # Calculate numbers of R, N, S at each time point for each group
  DST_tot <- DST_tot %>% 
    group_by(Group,Treatment,Batch,t) %>% 
    summarise(deltat = unique(deltat), 
              calendar_date=unique(calendar_date),
              R=sum(reactor2)-sum(dead & reactor2), 
              N = length(reactor)-sum(dead), 
              S = N - R) %>%
    arrange(Group,Treatment,Batch,t) 
  
  
  DST_tot <- DST_tot %>% mutate(C=R)
  
  tmp <- (DST_tot %>% group_by(Group,Treatment,Batch) %>% summarise(C = c(C[1],diff(C))))
  # Test for edge case where only positive animal has been removed from group in next step
  tmp$C[tmp$C<0] = 0
  
  
  DST_tot <- DST_tot %>% ungroup() %>% mutate(C = tmp$C)
  
  # Time 0 states
  DST_t0 <- calves %>% filter(t==0 & Dx==diagnostic_test  & !is.element(t,exclude_times)) %>% 
    group_by(Group,Treatment,Batch,t) %>% 
    summarise(S=n()) %>% mutate(R=0,N=S)
  
  shift_forward <- DST_t0  %>%
    bind_rows(DST_tot %>% select(Group,Treatment,Batch,t,S,R,N)) %>% mutate(t=t+next_test)
  
  DST_tot <- DST_tot %>% select(-S,-R,-N) %>% left_join(shift_forward)
  
  # Remove time points for which we still have missing test results
  
  DST_tot <- DST_tot %>% drop_na()
  
  # Remove all treatment groups for time points with missing test results
  
  #tmp <- DST_tot %>% group_by(Group,t) %>% summarise(num_t=length(Treatment)) %>% filter(num_t==1)
  
  #DST_tot <- DST_tot %>% anti_join(tmp)
  
  # Summary of maximum time point available for analysis (due to missingness)
  print(DST_tot %>% group_by(Group,Treatment) %>% summarise(max(t)))
  
  # Calculate total number of infectives (I) at each time step in each group
  # And number of infected vaccinates (IV)
  DST_tot <- DST_tot %>% 
    group_by(t,Group) %>% 
    summarise(Group,Treatment,t,deltat,calendar_date,R,N,S,C,IC = sum(R[which(Treatment=='Control')]),IV = sum(R[which(Treatment=='Vaccinate')]))
  
  # Consistency checks (unit tests) for constructed table
  # Unit test 1: check R+S is constant
  DST_tot %>% ungroup() %>% mutate(unit1 = (N==(S+R))) %>% summarise(sum(!unit1))
  # Unit test 2: check R==IC for control group
  DST_tot %>% ungroup() %>% filter(Treatment=='C') %>% mutate(unit2 = (IC==R)) %>% summarise(sum(!unit2))
  # Unit test 3: check R==IV for vaccinate group
  DST_tot %>% ungroup() %>% filter(Treatment=='V') %>% mutate(unit2 = (IV==R)) %>% summarise(sum(!unit2))
  # Unit test 4: check IV, IC equal for both treatment groups in each replicate for each time point
  DST_tot %>% ungroup() %>% group_by(t,Group) %>% 
    summarise(unit4=length(unique(IC))==1,unit5=length(unique(IV))==1) %>% 
    ungroup() %>% 
    summarise(sum(!unit4),sum(!unit5))
  
  # Use dates of herd tests to work out number of seeders in each period
  
  # N is group size, Control + Vaccinates + Seeders
  # First add Control and Vaccinates together
  
  tmp <- DST_tot %>% group_by(t,calendar_date,Group) %>% summarise(N=sum(N))
  
  # Note, duplicate rows and same dimensions and shape as DST_tot
  
  tmp <- tmp %>% bind_rows(tmp) %>% arrange(t,Group)  %>% unique() %>% ungroup() %>% mutate(Batch = rep(c('I','II'),dim(DST_tot)[1]/4),.after=t)
  
  # Calculate "reactor2" status to simplify calculating cumulative reactor status by removing missing values
  
  seeders <- seeders %>% mutate(reactor2 = reactor)
  
  seeders$reactor2[is.na(seeders$reactor2)] = FALSE
  
  # Switch status to positive for all tests if positive by end of PhaseI
  seeders <- seeders %>% mutate(reactor2 = (reactor2 | status_phaseI))
  
  # Set date of pre-challenge test to value before all other tests in Phase II to simplify filtering below
  
  seeders$calendar_date[which(is.na(seeders$calendar_date))] = "2000-01-01"
  
  # All animal tests (of specific type) before current time for group.
  #seeders %>% filter(calendar_date <= '2021-11-28' & Group == 3 & Dx==diagnostic_test)
  
  stub <- seeders %>% filter(Dx==diagnostic_test & t==-2) %>% select(Uni_ID,ID1,ID2,Group,dead,status_phaseI)
  
  #%>% filter(!is.element(calendar_date,unique(phaseII_seeders$calendar_date)))
  
  skeleton <- tmp  %>% select(-N) %>% unique()
  
  missing_tests <- tibble()
  
  #missing_tests <- stub[1,] %>% uncount(dim(skeleton)[1]) %>% mutate(calendar_date=skeleton$calendar_date,t=skeleton$t)
  
  for(i in 1:dim(skeleton)[1])
  {
    # Select all test results for seeders less than or equal to test date
    # Imputation options
    sub <- switch(impute,
                  # Use status at end of Phase I
                  "minimum" = seeders %>% filter(Group==skeleton$Group[i] & Dx==diagnostic_test & calendar_date == '2000-01-01') %>% select(Uni_ID,ID1,ID2,reactor=status_phaseI),
                  # Use most recent status available
                  "most recent" = seeders %>% filter(Group==skeleton$Group[i] & Dx==diagnostic_test & calendar_date <= skeleton$calendar_date[i]) %>% 
                    select(Uni_ID,ID1,ID2,Group,Dx,date,t,reactor=reactor2) %>% group_by(Uni_ID,ID1,ID2) %>% summarise(reactor=max(reactor)==1),
                  # Use final status
                  "maximum" = seeders %>% filter(Group==skeleton$Group[i] & Dx==diagnostic_test) %>% select(Uni_ID,ID1,ID2,Group,Dx,date,t,reactor=reactor2) %>% group_by(Uni_ID,ID1,ID2) %>% summarise(reactor=max(reactor)==1)
    )
    
    # Check if animal is still alive at end of current contact interval
    removed = phaseII_removals %>% filter(`Kill Date` < skeleton$calendar_date[i] & Group==skeleton$Group[i]) %>% select(ID1)
    
    sub <- sub %>% filter(!is.element(ID1,removed$ID1))
    
    missing_tests <- missing_tests %>% bind_rows(cbind(skeleton[i,],sub))
    
  }
  
  #seeders_imputed <- missing_tests %>% bind_rows(seeders) %>% arrange(Uni_ID,Group, Treatment,t)
  
  
  #seeders_imputed <- seeders_imputed %>% filter(Dx==diagnostic_test) %>% group_by(Uni_ID,Group,Treatment,t,calendar_date) %>% summarise(reactor2=cumsum(na.omit((reactor | status_phaseI) & !dead))>0)
  
  
  
  # Calculate counts of reactors and number of animals at each time point (using cumulative status above) 
  
  seeders_imputed <- missing_tests %>% group_by(t,Group,Batch,calendar_date) %>% summarise(I_S=sum(reactor),IV_S=sum(reactor),N_S=length(reactor),S_S=N_S-I_S,V_S=N_S-IV_S) 
  
  # Mask out appropriate column to assign seeders correctly to either control treatment (I_S, S_S) or vaccinate treatment (IV_S, VS)
  seeders_imputed$I_S[which(seeders_imputed$Group==4)] = 0
  seeders_imputed$IV_S[which(seeders_imputed$Group==3)] = 0
  seeders_imputed$S_S[which(seeders_imputed$Group==4)] = 0
  seeders_imputed$V_S[which(seeders_imputed$Group==3)] = 0
  
  #seeders_imputed %>% filter(is.na(reactor))
  
  DST_tot <- DST_tot %>% left_join(seeders_imputed %>% select(-Batch)) %>% mutate(N_sentinel = N) %>% relocate(Batch,.after=Group)
  
  # Calculate the total number of sentinel animals alive at each test
  # Calculate based on the animals status at the last recorded test
  for(i in 1:dim(DST_tot)[1])
  {
    N = unlist(calves %>% filter(Group==DST_tot$Group[i] & calendar_date <= DST_tot$calendar_date[i] & Dx==diagnostic_test) %>% group_by(Uni_ID) %>% summarise(alive=sum(dead,na.rm=T)==0) %>% ungroup() %>% summarise(N=sum(alive)))
    DST_tot$N_sentinel[i] = N
  }
  
  DST_tot <- DST_tot %>% mutate(N = N_sentinel + N_S) %>% select(-N_sentinel)
  
  
  rm(tmp)
  
  return(DST_tot)
  
}

DST1_tot <- mk_counts_for_glm_phaseI('DST1',phaseI_calves,phaseI_seeders,test_dates,FALSE)
DST10_tot <- mk_counts_for_glm_phaseI('DST10',phaseI_calves,phaseI_seeders,test_dates,TRUE)

# Default imputation - change status of seeders in Phase II when they are tested

DST1_totII <- mk_counts_for_glm_phaseII('DST1',phaseII_calves,phaseII_seeders,test_dates,FALSE,'most recent')
DST10_totII <- mk_counts_for_glm_phaseII('DST10',phaseII_calves,phaseII_seeders,test_dates,TRUE,'most recent')

DST1_tot_def <- DST1_tot %>% bind_rows(DST1_totII)
DST10_tot_def <- DST10_tot %>% bind_rows(DST10_totII)

# Fix status of seeders in Phase II to that at beginning of Phase II

DST1_totII <- mk_counts_for_glm_phaseII('DST1',phaseII_calves,phaseII_seeders,test_dates,FALSE,'minimum')
DST10_totII <- mk_counts_for_glm_phaseII('DST10',phaseII_calves,phaseII_seeders,test_dates,TRUE,'minimum')

DST1_tot_min <- DST1_tot %>% bind_rows(DST1_totII)
DST10_tot_min <- DST10_tot %>% bind_rows(DST10_totII)

# Fix status of seeders in Phase II to that at end of Phase II

DST1_totII <- mk_counts_for_glm_phaseII('DST1',phaseII_calves,phaseII_seeders,test_dates,FALSE,'maximum')
DST10_totII <- mk_counts_for_glm_phaseII('DST10',phaseII_calves,phaseII_seeders,test_dates,TRUE,'maximum')

DST1_tot_max <- DST1_tot %>% bind_rows(DST1_totII)
DST10_tot_max <- DST10_tot %>% bind_rows(DST10_totII)

save(test_dates,
     phaseI_calves,phaseI_seeders,
     phaseII_calves,phaseII_seeders,
     DST1_tot_def,DST10_tot_def,
     DST1_tot_max,DST10_tot_max,
     DST1_tot_min,DST10_tot_min,
     file='TestingData.RData')
