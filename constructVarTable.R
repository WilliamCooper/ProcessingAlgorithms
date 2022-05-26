# get a complete list of variables in netCDF files, including from recent projects:
library(Ranadu)
Dirs <- list.dirs(path=DataDirectory(), recursive = FALSE)
DV <- data.frame()
for (D in Dirs) {
  if (grepl('DEEPWAVE.orig', D)) {next}
  if (grepl('RICO', D)) {next}
  print (sprintf ('processing %s', D))
  FL <- list.files(D, '.*rf...nc')
  for (F in FL) {
    VS <- DataFileInfo(sprintf('%s/%s', D, F), LLrange = FALSE)$Variables
    if (length(VS) > 0) {
      DVA <- data.frame(Vars = VS, Project = rep(D, length(VS)), File = rep(F, length(VS)))
      DV <- rbind(DV, DVA)
      DV <- distinct(DV, Vars, .keep_all = TRUE)
    }
  }
}
DV$Category <- rep('undefined', nrow(DV))
DV$long_name <- rep('undefined', nrow(DV))
DV$units <- rep('undefined', nrow(DV))
DV$type <- rep('undefined', nrow(DV))
for (i in 1:nrow(DV)) {
  if (i %% 100 == 0) {print(sprintf('i = %d Project=%s', i, DV$Project[i]))}
  if (grepl('^TESTrf', DV$File[i])) {next}
  if (DV$Vars[i] == 'time_offset') {next}
  if (DV$Vars[i] == 'base_time') {next}
  DD <- getNetCDF(sprintf('%s/%s', DV$Project[i], DV$File[i]), DV$Vars[i]) %>%
          select(DV$Vars[i]) %>% sapply(attributes) %>% data.frame()
  # DVC <- DD[[1]]$Category
  ## DVC <- (getNetCDF(sprintf('%s/%s', DV$Project[i], DV$File[i]), DV$Vars[i]) %>% 
  ##                   select(DV$Vars[i]) %>% sapply(attributes) %>% data.frame())[[1]]$Category
  # DV$Category[i] <- ifelse(is.null(DVC), 'undefined', DVC)
  DVL <- DD[[1]]$long_name
  DVU <- DD[[1]]$units
  DV$long_name[i] <- ifelse(is.null(DVL), 'undefined', DVL)
  DV$units[i] <- ifelse(is.null(DVU), 'undefined', DVU)
}
# Compile set of suffixes:
sfsx <- vector()
for (i in 1:nrow(DV)) {
  sfx <- sub('^.*_', '', DV$Vars[i])
  if (nchar(sfx) < nchar(DV$Vars[i])) {
    sfsx <- c(sfsx, sfx)
  }
}
sfsx <- unique(sfsx)
ix <- c(1,12,18,19,29,37,44,45,53,63,64,75,76,93,97,100,101,102,103,104,107,108,109,110,111, 115,
        125, 126, 144, 214,215,217,219,220,223,224,225,226,227,228, 249,250,256, 265, 266) 
# Position suffix list        
posSFX <- sfsx[ix]
posSFX <- c(posSFX, '58RWI', 'new', 'old', '3H', '3V')
for (i in 1:nrow(DV)) {
  if (sub('^.*_', '', DV$Vars[i]) %in% posSFX) {
    DV$Vars[i] <- paste0(sub(sub('^.*_', '', DV$Vars[i]), '', DV$Vars[i]), 'x')
  }
}
DV <- distinct(DV, Vars, .keep_all = TRUE)
DVS <- DV[order(DV$Vars),]
rownames(DVS) <- 1:nrow(DVS)
DV <- DVS
save(DV, file='DVsave.Rdata')
# Variety suffix list (e.g., identifying an instrument)

# Classify according to "type":
#   st: status (housekeeping)
#   pa: in (or should be in) ProcessingAlgorithms
#   npa: not in, or not needed in, ProcessingAlgorithms
#   sp: special (e.g., user-supplied measurement not in PA), subset of npa

print('enter st, pa, sp, npa, or other:')
for (i in 1:nrow(DV)) {
  if (DV$type[i] == 'undefined') {
    print(DV[i, ])
    print('enter st, pa, sp, npa, v or other:')
    DV$type[i] <- readline("type? ")
    # Default is 'undefined':
    if (nchar(DV$type[i]) < 1) {
      DV$type[i] <- 'undefined'
    }
    print(sprintf('set type to %s', DV$type[i]))
  }
}
# DVS <- DV[order(DV$Vars),]
# rownames(DVS) <- 1:nrow(DVS)
# DV <- DVS
# Omit GTCIMS, PCIMS and MTP housekeeping
statusType <- function(DV, ST) {
  ix <- which(grepl(ST, DV$long_name))
  ix <- unique(c(ix, which(grepl(ST, DV$Vars))))
  # print(ix)
  if (length(ix) > 0) {
    DV$type[ix] <- 'st'
  }
  return(DV)
}
STS <- c('A2D', 'Analog level', 'INTERCEPT',
         'AC Gain', 'tion Spectrum',
         'BRDTEMP', 
         'Serial Number', 
         'Board Temperature', 
         '5 Vdc Monitor', 
         'Laser.*Temperature', 
         'Laser Current', 
         'Laser.*Monitor',
         'Laser Power',
         'Bandwidth', 
         'Baseline', 
         'Threshold', 
         'Optics.*Temperature', 
         'DSM.*Temperature', 
         'Reference Voltage', 
         'Diode Voltage',
         'bias channel',
         'Temperature of',
         'holodec temperature',
         'ENET Periodic',
         'ENET ROOT',
         'IRIG status',
         'IRIG.*diff',
         'DACQ',
         'Cell Pressure',
         'Cell Temperature',
         'Housekeeping')
for (ST in STS) {
  DV <- statusType(DV, ST)
}
# Classify special-use instruments:
specialType <- function(DV, SX) {
  ix <- which(grepl(sprintf('_%s', SX), DV$Vars))
  ix <- unique(c(ix, which(grepl(SX, DV$long_name))))
  if (length(ix) > 0) {
    DV$type[ix] <- 'sp'
  }
  return(DV)
}
SXS <- c('GTCIMS', 
         'CCN',
         '2DS',
         'AMS',
         'CMIGITS',
         'CU Total Water',
         'MTHP',
         'PIP',
         'SIDS',
         'SP2',
         'PHIPS Camera',
         'AWAS',
         'GNI',
         'UCATS',
         'NOAA',
         'QCLS',
         'MEDUSA',
         'Medusa',
         'ARI',
         'CFDC',
         'ISAF',
         'SMPS',
         'CVI',
         'DOAS',
         'Turbulence Sensor',
'PCIMS',
'MTP', 
'AVAPS', 
'PTRMS', 
'TOGA', 
'HARP', 
'TDL', 
'3VCPI', 
'3H', 
'3V', 
'GPIT', 
'CVIU')
for (SX in SXS) {
  DV <- specialType(DV, SX)
}
# Set to 'variety' types:
SVS <- c('IRS2', 'IRS3', 'GP', 'A2', 'AE', 'CR2', 'ITR', 'IE', 'NVTL', 'GMN')
for (i in 1:nrow(DV)) {
  sx <- sub('^.*_', '', DV$Vars[i])
  if (sx %in% SVS) {
    DV$type[i] <- 'v' # Variety type
  }
}
# Exclude JHWRP:
for (i in 1:nrow(DV)) {
  if (grepl('JHWRP', DV$Project[i])) {
    DV$type[i] <- 'npa'
  }
}
# Get variables that are in ProcessingAlgorithms appendix, but not is SXS:
PAV <- c(
'A200X', 'A200Y', 'A260X', 'AACT', 'ACDP',
'ACINS', 'ADIFR', 'AF300', 'AFIXx', 'AFSSP',
'A1DC', 'A1DP', ' AKFXx', 'AKRD', 'ALAT',
'ALON', ' ALPHA', 'ALT', ' ALTG', 'ALTX',
'AMS', 'APCAS', 'AS100', 'AS200', 'ATC',
'AT_ITR', 'ATKP', 'ATRF', 'ATTACK', 'ATx',
'ATX', 'ATxH', 'ATxD', 'AUHSAS', 
'AWAS', 'base_time', 'BDIFR', 'BFIXx', 'BLATA',
'BLONA', 'BNORMA', 'BPITCHR', 'BROLLR', 'BYAWR',
'C1DC', 'C1DP', 'C200X', 'C200Y', 'C260X',
'CAVP_x', 'CCDP', 'CCEP', 'CF300', 'CFSEC',
'CFSSP', 'CGS', 'CH4_PICx', 'CIMS', 'CLAT',
'CLON', 'CMODE', 'CNTEMP', 'CNTS', 'CO',
'CO2_PICx', 'COCAL', 'COCOR', 'COMR_AL', 'CONC1DC',
'CONC1DC100', 'CONC1DC150', 'CONC1DP', 'CONC3', 'CONC6',
'CONCD', 'CONCF', 'CONCH_UVH', 'CONCN', 'CONCP',
'CONCP', 'CONCU', 'CONCU', 'CONCU100', 'CONCU500',
'CONCV_VXL', 'CONCX', 'CONCY', 'CORAW_AL', 'COZRO',
'CPCAS', 'CRHP', 'CS100', 'CS200', 'CSEC',
'CSTAT', 'CUHSAS', 'DAY', 'DBAR1DC', 'DBAR1DP',
'DBAR3', 'DBAR6', 'DBARD', 'DBARF', 'DBARP',
'DBARU', 'DBARX', 'DBARY', 'DBZ1DC', 'DBZ1DP',
'DBZ6', 'DBZD', 'DBZF', 'DBZX', 'DBZY',
'DEI', 'DISP1DC', 'DISP1DP', 'DISP3', 'DISP6',
'DISPD', 'DISPF', 'DISPP', 'DISPU', 'DISPX',
'DISPY', 'DNI', 'DP_CR2C', 'DPCRC', 'DP_VXL',
'DPx', 'DPx', 'DPxC', 'DPXC', 'DP_x',
'DP_DPB,DP_DPT,DP_DPL,DP_DPR', '',
'DT1DC', 'DVALUE', 'EDPC', 'EDPC', 'EW_UVH',
'EWx', 'EWX', 'FACT', 'FBMFR', 'FCN',
'FCNC', 'FO3_ACD', 'FO3_CL', 'FO3_x', 'FP_CR2',
'FPCRC', 'FRANGE', 'FRESET', 'FRNG', 'FRST',
'FSTB', 'FSTROB', 'FXAZIM', 'FXDIST', 'GALT_A',
'GEOPHT', 'GGALT', 'GGALTC', 'GGEOIDHT', 'GGHWGS',
'GGLAT', 'GGLON', 'GGNSAT', 'GGQUAL', 'GGSPD',
'GGSTATUS', 'GGTRK', 'GGVEW', 'GGVNS', 'GGVSPD',
'GLAT', 'GLON', 'GMODE', 'GNI', ' GNSS',
'GSF', 'GSF_G', 'GSF', 'GSTAT_G', 'GSTAT',
'GVZI', 'HGM', 'HGM232', 'HGME',
'HI3', 'HOUR', 'IAS', 'IRx', 'IRxC',
'IRXC', 'IRxHT', 'IRxHTV', 'IRXV', 'LAT',
'LATC', 'LAT_G', 'LON', 'LONC', 'LON_G',
'LWC', 'LWCC', 'MACH', 'MACHx', 'MACHX,',
'MINUTE', 'MIRRORT_CR2', 'MR', 'MONTH', 'MRCR',
'MRLA', 'MRLH', 'MRVXL', 'NO', 'NOy',
'O3FS','O3MR_CL', 'OAT', 'PACT',
'PALT', 'PCAB', 'PCN', 'P_CR2', 'PFLW',
'PFLWC', 'PHDG', 'PITCH', 'PLWC', 'PLWC1',
'PLWC1DC', 'PLWC1DP', 'PLWC6', 'PLWCC', 'PLWCC1',
'PLWCD', 'PLWCF', 'PLWCG', 'PLWCX', 'PLWCY',
'PS_A', 'PSDPx', 'PSFD', 'PSFRD', 'PSTF',
'PSURF',
'PSX', 'PSxC', 'PSXC', 'PSx', 'PTIME',
'QC_A', 'QCBC', 'QCB', 'QCGC', 'QCG',
'QCLS', 'QCR', 'QCRC', 'QCTF', 'QCTFC',
'QCx', 'QCX', 'QCxC', 'QCXC',
'RAWCONC_VXL', 'REFF2DC', 'REFF2DP', 'REFFD', 'REFFF',
'RHOLA', 'RHOUV', 'RHOx', 'RHUM', 'RHUMI',
'RICE', 'ROC', 'ROLL', 'RSTx', 'RTHRx',
'RTx', 'RTX', 'RTxH', 'SECOND', 'SCLWC',
'SFC', 'SMPS', 'SOLAZ', 'SOLDE', 'SOLEL',
'SOLZE', 'SPHUM', 'SPxPitch', 'SPxRoll', 'SSFXx',
'SSLIP', 'SSRD', 'SWTC', 'SWx', 'TASx',
'TASX', 'TASHC', 'TASxD', 'TCAVB', 'TCAVT',
'TCNTP', 'TCNTU', 'TEMP1', 'TEMP2', 'TEO3',
'TEO3C', 'TEO3P', 'TEP', 'TET', 'THDG',
'THETA', 'THETAE', 'THETAP', 'THETAQ', 'THETAV',
'THF', 'THI', 'time_offset', 'TKAT_G',
'TMLAG', 'TPTIME', 'TRSTB', 'TRSTT',
'TTKP', ' TTRF', 'TTx', 'TVIR', 'UI',
'UIC', 'UPRESS', 'USFLWC', 'USMPFLW', 'UVx',
'UX', 'UXC', 'VCRH', 'VEW', 'VEWC',
'VEW', 'VEW_G', 'VI', 'VIC', 'VISxC',
'VISxHT', 'VISxHTV', 'VISxV', 'VLA', 'VNS',
'VNSC', 'VNS_G', 'VNS', 'VSPD', 'VSPD_G',
'VY', 'VYC', 'VZI', 'WD', 'WDC',
'WDRCTN', 'WI', 'WIC', 'WP3', 'WS',
'WSC', 'WSPD', 'XCOMR', 'XCOMR_AL', 'XFO3FNO',
'XFO3FS', 'XFO3P', 'XICN', 'XICNC', 'XMACH2',
'XNCLF', 'XNMBT', 'XNO', 'XNOCAL', 'XNYCAL',
'XNOCF', 'XNOSF', 'XNOY', 'XNOYP', 'XNOZA',
'XNSAF', 'XNST', 'XNZAF', 'XO3', 'XSIG_UVH',
'XUVI', 'XUVP', 'XUVT', 'XVI', 'YEAR',
'YVI'
)
for (i in 1:nrow(DV)) {
  if (DV$Vars[i] %in% PAV || sub('_.*', '', DV$Vars[i]) %in% PAV) {
    DV$type[i] <- 'pa'
  }
}
PAType <- function(DV, SX) {
  ix <- which(grepl(sprintf('_%s', SX), DV$Vars))
  if (length(ix) > 0) {
    DV$type[ix] <- 'pa'
  }
  return(DV)
}
# DV <- DV[-c(which(grepl('_G', DV$Vars))),]
# DV <- DV[-c(which(grepl('_G2', DV$Vars))),]
# # Rename position-dependent variables and eliminate duplicates:
# ix <- c(which(grepl('^CONC1DC_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CONC1DC_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CONC1DC100_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CONC1DC100_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CONC1DC150_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CONC1DC150_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CONC2DCA_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CONC2DCA_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CONC2DCR_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CONC2DCR_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CONCD_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CONCD_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CONCF_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CONCF_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CONCP_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CONCP_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CONCU_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CONCU_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CONCU100_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CONCU100_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CONCU500_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CONCU500_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^A1DC_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'A1DC_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^ACDP_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'ACDP_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^C1DC_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'C1DC_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CCDP_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CCDP_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^AREA_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'AREA_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^AS100_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'AS100_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^A1DCA_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'A1DCA_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^A1DCR_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'A1DCR_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^A2DCA_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'A2DCA_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^C2DCA_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'C2DCA_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^A2DCR_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'A2DCR_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^C2DCR_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'C2DCR_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^AS200_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'AS200_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^AUHSAS_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'AUHSAS_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CUHSAS_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CUHSAS_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^AS300_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'AS300_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CS100_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CS100_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CS200_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CS200_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CS300_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CS300_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^AVGTRNS_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'AVGTRNS_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^BALNC_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'BALNC_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CAVP_DP', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CAVP_DPx'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CD2D[0-9][0-9]', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CD2Dnn'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^CD2DL[0-9]', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'CD2DLn'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^DA2D_CH[0-9]_VTB', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'DA2D_CHn_VTB'
#   DV[ix[1],]$long_name <- 'Channel n'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^DATE_', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'DATE_x'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^DP_DP.', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'DP_DPx'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^IRIG_Status[0-9]*', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'IRIG_Statusn'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^IRIG_Tdiff[0-9]*', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'IRIG_Tdiffn'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^PD2D[0-9][0-9]', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'PD2Dn'
#   DV[ix[1],]$long_name <- 'PARTICLES IN P-PROBE SIZING BIN n'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^PD2DL[0-9]', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'PD2DLn'
#   DV[ix[1],]$long_name <- 'LARGE PARTICLE SIZING BIN n (P-PROBE)'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^THIML[0-9]', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'THIMLn'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
# ix <- c(which(grepl('^TURB[0-9][0-9]', DV$Vars)))
# if (length(ix) > 1) {
#   DV[ix[1],]$Vars <- 'TURBn'
#   DV[ix[1],]$long_name <- 'Turbulence Sensor n'
#   ix <- ix[-1]
#   DV <- DV[-c(ix), ]
# }
sumry <- function(DV, Vr) {
  tt <- sprintf('^%s', Vr)
  ix <- which(grepl(tt, DV$Vars))
  if (length(ix) > 1) {
    # If there is an _x or _X variable with specified long_name, keep it
    ixi <- which(grepl(sprintf('%sX', Vr), DV$Vars[ix]))
    if (length(ixi) <= 0) {
      ixi <- which(grepl(sprintf('%sx', Vr), DV$Vars[ix]))
      if (length(ixi) <= 0) {
        ixi <- 1
      }
    }
    DV[ix[ixi[1]],]$Vars <- sprintf('%sx', Vr)
    ix <- ix[-ixi[1]]
    DV <- DV[-c(ix), ]
    # print('sumry would eliminate these variables:')
    # print(DV$Vars[ix])
  } 
  return(DV)
}
sumryV <- c('CONC1DC_',
           'CONC1DC100_',
           'CONC1DC150_',
           'CONC2DCA_',
           'CONC2DCR_',
           'CONCD_',
           'CONCF_',
           'CONCU_',
           'CONCU100_',
           'A1DC_',
           'ACDP_',
           'C1DC_',
           'AREA_',
           'AS100_',
           'A1DCA_',
           'A1DCR_',
           'A2DCA_',
           'A2DCR_',
           'C2DCR_',
           'AS200_',
           'AUHSAS_',
           'CUHSAS_',
           'AS300_',
           'CS100_',
           'CS200_',
           'CS300_',
           'AVGTRNS_',
           'BALNC_',
           'DATE_',
            'DBAR1DC_',
           'CONC3_',
           'DBAR1DCA_',
           'DBAR1DCR_',
           'DBAR2DCA_',
           'DBAR2DCR_',
           'DBAR3_',
           'DBARD_',
           'DBARF_',
           'DBARP_',
           'DBARU_',
           'DBZ1DC_',
           'DBZ2DCA_',
           'DBZ2DCR_',
           'DBZD_',
           'DBZF_',
           'DELTA_',
           'DELTAT_',
          'DISP1DC_',
           'DISP2DCA_',
           'DISP2DCR_',
          'DISPD_',
           'DISPF_',
           'DISPP_',
           'DISPU_',
           'DT1DC_',
          'DUMMY_',
           'DVALUE_',
           'ETCN_',
          'EVENT_',
          'EW_',
           'FCNC_',
          'FRNG_',
          'FSHCN_',
           'FSTB_',
          'FTCN_',
           'GEOPTH_',
           'GGDAGE_',
           'GGALT_',
           'GGALTSD_',
           'GGEOIDHT_',
           'GGHDOP_',
           'GGHORDIL_',
           'GGLAT_',
           'GGLATSD_',
          'GGLON_',
          'GGLONSD_',
           'GGNSAT_',
           'GGQUAL_',
           'GGREFID_',
           'GGRepLag_',
           'GGSECSDAY_',
           'GGSPD_',
           'GGSTATUS_',
           'GGTRK_',
           'GGVEW_',
           'GGVNS_',
           'GGVSPD_',
           'GSTAT_',
           'GTIME_',
           'GVEW_',
           'GVNS_',
           'I2DCA_',
           'IRIG_Status_',
           'IRIG_Tdiff_',
           'IWC1DC_',
           'NACCEPT2DCA_',
           'NACCEPT2DCR_',
           'NREJECT2DCA_',
           'NREJECT2DCR_',
           'OVFLW_',
           'PALT_',
           'PFLW_',
           'PFLWC_',
           'PFLWS_',
           'PLWC1DC_',
           'PLWC2DCA_',
           'PLWC2DCR_',
           'PLWCD_',
           'PLWCF_',
           'PVOL3_',
           'PVOLP_',
           'PVOLU_',
           'REFF2DC_',
           'REFF2DCA_',
           'REFF2DCR_',
           'REFFD_',
           'REFFF_',
           'REJAT_',
           'REJDOF_',
           'RPS_',
           'SHDOR_',
           'TCNT3_',
           'TCNTD_',
           'TCNTF_',
           'TCNTP_',
           'TCNTU_',
           'UBTMP_',
           'UDIFF_',
           'UFLWC_',
           'UPRESS_',
           'UREF_',
           'USCAT_',
           'USHFLW_',
           'USMPFLW_',
           'UTEMP_')
for (S in sumryV) {
  DV <- sumry(DV, S)
}

# Omit if long_name is 'No title'
ix <- c(which(grepl('No title', DV$long_name)))
DV <- DV[-c(ix), ]
# Eliminate some housekeeping:
# ix <- c(which(grepl('A2DTEMP', DV$Vars)))
# DV <- DV[-c(ix), ]
# DV <- DV[-c(which(grepl('IRS2$', DV$Vars))),]
# DV <- DV[-c(which(grepl('IRS3$', DV$Vars))),]
# DV <- DV[-c(which(grepl('ADIFRTEMP', DV$Vars))),]
# DV <- DV[-c(which(grepl('ADIFTEMP', DV$Vars))),]
# DV <- DV[-c(which(grepl('_AMS', DV$Vars))),]
# DV <- DV[-c(which(grepl('_CUTW', DV$Vars))),]
# DV <- DV[-c(which(grepl('_LAMS', DV$Vars))),]
# DV <- DV[-c(which(grepl('.*bin_.*points.*', DV$Vars))),]
# DV <- DV[-c(which(grepl('_AWAS$', DV$Vars))),]
# DV <- DV[-c(which(grepl('^BRDTEMP', DV$Vars))),]
# DV <- DV[-c(which(grepl('Serial Number', DV$long_name))),]
# DV <- DV[-c(which(grepl('Board Temperature', DV$long_name))),]
# DV <- DV[-c(which(grepl('5 Vdc Monitor', DV$long_name))),]
# DV <- DV[-c(which(grepl('Laser.*Temperature', DV$long_name))),]
# DV <- DV[-c(which(grepl('Laser Current', DV$long_name))),]
# DV <- DV[-c(which(grepl('Laser.*Monitor', DV$long_name))),]
# DV <- DV[-c(which(grepl('Bandwidth', DV$long_name))),]
# DV <- DV[-c(which(grepl('Baseline', DV$long_name))),]
# DV <- DV[-c(which(grepl('Threshold', DV$long_name))),]
# DV <- DV[-c(which(grepl('Optics.*Temperature', DV$long_name))),]
# DV <- DV[-c(which(grepl('DSM.*Temperature', DV$long_name))),]
# DV <- DV[-c(which(grepl('Reference Voltage', DV$long_name))),]
# DV <- DV[-c(which(grepl('Housekeeping', DV$long_name))),]
# DV <- DV[-c(which(grepl('_MTHP$', DV$Vars))),]
# DV <- DV[-c(which(grepl('_PILS$', DV$Vars))),]
# # need to skip GPS Qual (IDd by Qual,)
# DV <- DV[-c(which(grepl('Qual', DV$long_name) & !grepl('Qual,', DV$long_name))),]
# DV <- DV[-c(which(grepl('BDIF.*TEMP', DV$Vars))),]
# DV <- DV[-c(which(grepl('^BIAS', DV$Vars))),]
# DV <- DV[-c(which(grepl('^BKGM', DV$Vars))),]
# DV <- DV[-c(which(grepl('_CAMS$', DV$Vars))),]
# DV <- DV[-c(which(grepl('_PAN$', DV$Vars))),]
# DV <- DV[-c(which(grepl('_CCN1$', DV$Vars))),]
# DV <- DV[-c(which(grepl('_CCN2$', DV$Vars))),]
# DV <- DV[-c(which(grepl('ITR', DV$long_name))),]
# DV <- DV[-c(which(grepl('Diode Voltage', DV$long_name))),]
# DV <- DV[-c(which(grepl('Holodec temperature', DV$long_name))),]
# DV <- DV[-c(which(grepl('^poisson', DV$Vars))),]
# DV <- DV[-c(which(grepl('SPP-200 Detector Temperature', DV$long_name))),]
# # Eliminate some more special-use variables
# DV <- DV[-c(which(grepl('_HARP', DV$Vars))),]
# DV <- DV[-c(which(grepl('3VCPI', DV$Vars))),]
# # Exclude all CVI:
# DV <- DV[-c(which(grepl('CVI', DV$long_name))),]
rownames(DV) <- 1:nrow(DV)

save(DV, file = 'DV.Rdata')
i <- 1;system(sprintf('ncdump -h %s/%s |grep %s', DV$Project[i], DV$File[i], DV$Vars[i]))

