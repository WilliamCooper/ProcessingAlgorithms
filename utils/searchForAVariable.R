# search for a variable or variable mask in netCDF filesD, including from recent projects:
library(Ranadu)
# VarSearch <- 'CONC_H2O_VXL'  # variable or pattern to find
VarSearch <- 'DFRSTS'  # variable or pattern to find
VarSearch <- 'GGSF_A'  # variable or pattern to find
VarSearch <- 'GLATF_A'  # variable or pattern to find
VarSearch <- 'HYB'  # variable or pattern to find
VarSearch <- 'GTKAT'  # variable or pattern to find
VarSearch <- 'GVSPD'  # variable or pattern to find
VarSearch <- 'H2O_PIC'  # variable or pattern to find
VarSearch <- 'CO_PIC'  # variable or pattern to find
VarSearch <- 'HNO3'  # variable or pattern to find
VarSearch <- 'IRB'  # variable or pattern to find
VarSearch <- 'IWC1DC'  # variable or pattern to find
VarSearch <- 'IWD'  # variable or pattern to find
VarSearch <- 'LATF_A'  # variable or pattern to find
VarSearch <- 'LI82'  # variable or pattern to find
VarSearch <- 'MHDG3'  # variable or pattern to find
VarSearch <- 'MRLH'  # variable or pattern to find
VarSearch <- 'NO_ACD'  # variable or pattern to find
# VarSearch <- 'XNOCAL'  # variable or pattern to find
VarSearch <- 'O3_ACD'  # variable or pattern to find
VarSearch <- 'O3MR'  # variable or pattern to find
VarSearch <- 'PDRY'  # variable or pattern to find
VarSearch <- 'PSTRB'  # variable or pattern to find
VarSearch <- 'PT_A'  # variable or pattern to find
VarSearch <- 'PVOL'  # variable or pattern to find
VarSearch <- 'RALT'  # variable or pattern to find
VarSearch <- 'REFFF2DC'  # variable or pattern to find
VarSearch <- 'REJAT'  # variable or pattern to find
VarSearch <- 'RHOD'  # variable or pattern to find
VarSearch <- 'SWB'  # variable or pattern to find
VarSearch <- 'AT*D'  # variable or pattern to find
VarSearch <- 'AQRATIO'  # variable or pattern to find
VarSearch <- 'AT.H'  # variable or pattern to find
VarSearch <- 'TKE'  # variable or pattern to find
VarSearch <- 'TSURF'  # variable or pattern to find
VarSearch <- 'UACC'  # variable or pattern to find
VarSearch <- 'UFLW'  # variable or pattern to find
VarSearch <- 'USFLW'  # variable or pattern to find
VarSearch <- 'UVT'  # variable or pattern to find
VarSearch <- 'VISHB'  # variable or pattern to find
VarSearch <- 'VMR_VXL'  # variable or pattern to find
VarSearch <- 'XCO_AL'  # variable or pattern to find
VarSearch <- 'Gerber'  # variable or pattern to find
VarSearch <- 'HONO'  # variable or pattern to find
PDirs <- list.dirs(path='/scr/raf/Prod_Data', recursive = FALSE)
PDirs <- list.dirs(path='/Data/Prod_Data', recursive = FALSE)
Dirs <- list.dirs(path=DataDirectory(), recursive = FALSE)
Dirs <- c(Dirs, PDirs)
SFLG <- TRUE
for (D in Dirs) {
  # if (grepl('TORERO', D)) {SFLG <- FALSE}
  # if (SFLG) {next}
  # if (!grepl('ICE-T', D)) {next}
  # if (!grepl('ICEBRIDGE2015', D)) {next}
  if (grepl('DEEPWAVE.orig', D)) {next}
  if (grepl('RICO', D)) {next}
  if (grepl('HEFT-10', D)) {next}
  print (sprintf ('processing %s', D))
  FL <- list.files(D, '.*rf...nc')
  if (length(FL) < 1) {next}
  for (F in FL[1]) {
    if (grepl('gz$', F)) {next}
    print (sprintf('directory %s file %s', D, F))
    if (grepl('CSET', D)) {
      print(sprintf('CSET flight is %s', F))
      if (grepl('rf08', F)) {next}
    }
    if (grepl('ECLIPSE2019', D)) {
      print(sprintf('E2019 flight is %s', F))
      if (grepl('rf01', F)) {next}
    }
    if (grepl('ICE-T', D)) {
      print(sprintf('ICE-T flight is %s', F))
      if (grepl('rf04', F)) {next}
      if (grepl('rf13', F)) {next}
    }
    if (grepl('ICEBRIDGE2015', D)) {
      print(sprintf('ICEBRIDGE flight is %s', F))
      if (grepl('2015rf02', F)) {next}
    }
    if (grepl('IDEAS-4-GV', D)) {
      print(sprintf('IDEAS-4-GV flight is %s', F))
      if (grepl('^rf05', F)) {next}
      if (grepl('^sid2', F)) {next}
    }
    if (grepl('IDEAS4_LAMS', D)) {
      print(sprintf('IDEAS4_LAMS flight is %s', F))
      if (grepl('rf08', F)) {next}
    }
    if (grepl('NOMADSS', D)) {
      print(sprintf('NOMADSS flight is %s', F))
      if (grepl('rf07', F)) {next}
      if (grepl('rf08', F)) {next}
      if (grepl('rf12', F)) {next}
      if (grepl('rf14', F)) {next}
      if (grepl('rf19', F)) {next}
    }
    if (grepl('PLOWS', D)) {
      print(sprintf('PLOWS flight is %s', F))
      if (grepl('rf18', F)) {next}
    }
    if (grepl('TORERO', D)) {
      print(sprintf('TORERO flight is %s', F))
      if (grepl('rf06', F)) {next}
    }
    if (grepl('PREDICT_LAMS', D)) {
      print(sprintf('PREDICT_LAMS flight is %s', F))
      if (grepl('rf04', F)) {next}
      if (grepl('rf06', F)) {next}
      if (grepl('rf07', F)) {next}
      if (grepl('rf08', F)) {next}
      if (grepl('rf09', F)) {next}
      if (grepl('rf11', F)) {next}
    }
    VS <- DataFileInfo(sprintf('%s/%s', D, F), LLrange = FALSE)
    if (any(grepl(VarSearch, VS$Variables))) {
    # if (any(VarSearch == VS$Variables)) {
      print (sprintf ('search matches in %s file %s from %s', D, F, VS$End))
      system(sprintf('ncdump -h %s/%s |grep %s', D, F, VarSearch))
    }
    if (any(grepl(VarSearch, VS$LongNames))) {
      print (sprintf ('search matches longname in %s file %s from %s', D, F, VS$End))
      system(sprintf('ncdump -h %s/%s |grep %s', D, F, VarSearch))    
    }
  }
}


