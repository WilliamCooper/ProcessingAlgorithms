# Add to or Edit ToDo data.frame:
setwd('~/RStudio/ProcessingAlgorithms')
load('ToDo.Rdata')
file.copy('ToDo.Rdata', 'ToDo.backup', overwrite = TRUE)
# Edit or Add?
EA <- readline('enter E or A to edit or add: ')
if (grepl('^E', EA) || grepl('^e', EA)) {
  # Edit section
  ID <- readline('enter identifier (e.g., 1.1): ')
  iw <- which(ID == sub('\\[', '', sub('\\].*', '', ToDo$Ref)))
  if (length(iw) != 1) {
    print (sprintf ('%s not found; exiting.'), ID)
    exit()
  }
  print (sprintf ('"Suggestion" is currently: %s', ToDo$Suggestion[iw]))
  rpl <- readline('enter replacement, or CR to keep current: ')
  if (nchar(rpl) > 1) {ToDo$Suggestion[iw] <- rpl}
  print (sprintf ('"Who" is currently: %s', ToDo$Who[iw]))
  rpl <- readline('enter replacement, or CR to keep current: ')
  if (nchar(rpl) > 1) {ToDo$Who[iw] <- rpl}
  print (sprintf ('"Status" is currently: %s', ToDo$Status[iw]))
  rpl <- readline('enter replacement, or CR to keep current: ')
  if (nchar(rpl) > 1) {ToDo$Status[iw] <- rpl}
  print (sprintf ('"Comment" is currently: %s', ToDo$Comment[iw]))
  rpl <- readline('enter replacement, or CR to keep current: ')
  if (nchar(rpl) > 1) {ToDo$Comment[iw] <- rpl}
} else {
  # Add section
  ID <- readline('enter identifier (e.g., 1.1):')
  Ref = sub('([0-9\\.]*)', "[\\1](#punch\\1)", ID)
  while (Ref %in% ToDo$Ref) {
    print (sprintf ('ID %s is already in ToDo; choose another.', ID))
    print('in use:');print(ToDo$Ref)
    ID <- readline('enter identifier (e.g., 1.1):')
    Ref = sub('([0-9\\.]*)', "[\\1](#punch\\1)", ID)
  }
  Suggestion = readline("enter new suggestion:\r")
  Who = readline("enter 'Who' (CR for '?')")
  Status = 'pending'
  Comment = readline("enter a comment (optional):")
  TDo <- data.frame(Ref, Suggestion, Who, Status, Comment)
  if (grepl('^0', ID)) {
    ToDo <- rbind(ToDo, TDo)
  } else {
    ToDo <- rbind(TDo, ToDo)
  }
  # Sort the lists:
  iw <- which(grepl('\\[0', ToDo$Ref))
  ToDo1 <- ToDo[1:(iw[1]-1),]
  ToDo2 <- ToDo[iw[1]:nrow(ToDo),]
  ToDo1 <- ToDo1[order(as.numeric(sub('\\(.*', '', sub('\\[(.*)\\]', '\\1', ToDo1$Ref)))), ]
  ToDo2 <- ToDo2[order(as.numeric(sub('\\(.*', '', sub('\\[0.(.*)\\]', '\\1', ToDo2$Ref)))), ]
  ToDo <- rbind(ToDo1, ToDo2)
}
save(ToDo, file='ToDo.Rdata')
