
#package includes functions: get.testmap(), processing(), ctt.analysis(),
#write.winsteps(), extended.map()


#connect to DB via DBI package
#input parameters:
#   test.id - numbers of forms as vector,
#   user - user's name
#   password - wassword for DB

get.testmap <- function(test.id, user, password) {
  forms <- paste(test.id, collapse = ",")

  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "SQL1",
                        Database = "First",
                        UID = user,
                        Trusted_Connection = "True",
                        PWD = password)

  dff <- dbGetQuery(con, paste("SELECT Test_ok.Ttst_ID As Form, Test_ok.NN As Position, CASE WHEN Items.Psycho_ID = 4 THEN 'A' ELSE 'F' END AS [Function], Test_ok.IT_ID As ItemID, CASE WHEN Items.Key_Numeric IS NULL THEN
                               [Key].Num ELSE ITEMS.Key_Numeric END As [Key],
                               CASE WHEN Items.Type_ID < 3 OR Items.Type_ID = 9 THEN 'M'
                               WHEN Items.Type_ID = 5 OR Items.Type_ID = 6 OR Items.Type_ID = 13 OR Items.Type_ID = 10 THEN 'H'
                               WHEN Items.Type_ID = 7 THEN 'R'
                               WHEN Items.Type_ID = 8 THEN 'N'
                               WHEN Items.Type_ID = 14 THEN 'D'
                               WHEN Items.Type_ID = 12 THEN 'S'
                               WHEN Items.Type_ID = 4 THEN 'P'
                               END AS [Type], [Key].Num, [Key].Ncat FROM Test_ok INNER JOIN
                               Items ON Test_ok.IT_ID = Items.ID LEFT JOIN
                               [Key] ON Items.Key_ID = [Key].ID WHERE Test_ok.Ttst_ID In (", forms, ") ORDER BY Test_ok.Ttst_ID, Test_ok.NN ", sep ="" ))
  dbDisconnect(con)
  dff[is.na(dff)] <- 0
  return(dff)
}

#main function for encoding test responses into item scores and data file for WINSTEPS
#it works with different types of test items:
#   M - multiple choice with one correct answer
#   R - multiple choice with more than one correct answers (one point for each correct answer)
#   H - matching type
#   P - order type with response 4 options (1 point if firs or last is correct, 2 points if first and last are correct,
#   3 points if all are correct)
#   N, D - open-ended with short numeric answer (N - 1 point, D - 2 points)
#   X, S - open-ended with text response. Encoding is not required

#function takes two input files:
#   tstmap - data frame with information about items: Form, Position (in form), Function (A - for calibration, F - other),
#   ItemID - item ID in item bank, Key - numeric key, Type. Can be retrieved from DB with get.testmap() function
#   or read as .csv or .xls file.
#   math - data frame with person responses. First column must be person's ID. Other columns must start with "Ex" and then
#   item number.
#   returns list with nested lists (every element of nested lists belongs to current test form).

processing <- function(tstmap, math) {
  multChoice <- function (i, j, form, shift) {
    if (x[[form]][i,j + shift] == m[[form]]$Key[j]) {
      a <- 1
    } else {
      a <- 0
    }
    return(a)
  }

  matchType <- function(i, j, form, shift) {
    itemscore <- 0
    shift2 <- 0
    strkey <- as.character(m[[form]]$Key[j])
    for (key in 1:nchar(m[[form]]$Key[j])) {
      if (x[[form]][i,j + shift + shift2] == substr(strkey, key,key)) {
        itemscore <- itemscore + 1
      } else {
        itemscore <- itemscore
      }
      shift2 <- shift2 + 1
    }
    return(itemscore)
  }

  shortAnsw <- function (i, j, form, shift) {
    if (is.na(x[[form]][i,j + shift])) {
      a <- 0
    } else if (x[[form]][i,j + shift] == m[[form]]$Key[j]) {
      a <- 2
    } else {
      a <- 0
    }
    return(a)
  }

  shortAnsw2 <- function (i, j, form, shift) {
    if (is.na(x[[form]][i,j + shift])) {
      a <- 0
    } else if (x[[form]][i,j + shift] == m[[form]]$Key[j]) {
      a <- 1
    } else {
      a <- 0
    }
    return(a)
  }

  openEnded <- function (i, j, form, shift) {
    a <- x[[form]][i,j + shift]
    return(a)
  }

  multChoice2 <- function (i, j, form, shift) {
    keys <- sapply(seq(1, nchar(m[[form]]$Key[j])), function(i) substr(m[[form]]$Key[j], i, i), simplify = TRUE)
    persResponses <- c()
    for (k in 1:nchar(m[[form]]$Key[j])) {
      persResponses[k] <- x[[form]][i,j + shift + k - 1]
    }
    return(length(intersect(keys,persResponses)))
  }

  orderType <- function (i, j, form, shift) {
    keys <- sapply(seq(1, nchar(m[[form]]$Key[j])), function(i) substr(m[[form]]$Key[j], i, i), simplify = TRUE)
    persResponses <- c()
    for (k in 1:nchar(m[[form]]$Key[j])) {
      persResponses[k] <- x[[form]][i,j + shift + k - 1]
    }
    if (all(keys == persResponses)) {
      a <- 3
    } else if (first(keys) == first(persResponses) & last(keys) == last(persResponses)) {
      a <- 2
    } else if (first(keys) == first(persResponses) | last(keys) == last(persResponses)) {
      a <- 1
    } else {
      a <- 0
    }
    return(a)
  }


  forms <- as.character(unique(math$Form))
  m <- split(tstmap, f = tstmap$Form)
  x <- split(math , f = math$Form)
  x <- lapply(x, function(x) dplyr::select(x, starts_with("Ex")))
  scoredItems <- vector(mode = "list", length = length(forms))
  unscoredItems <- vector(mode = "list", length = length(forms))
  scoredList <- vector(mode = "list", length = length(forms))
  names(scoredItems) <- forms
  names(scoredList) <- forms



  for (f in forms) {
    shift <- 0
    vector <- c()
    unscored <- c()
    numofPersons <- nrow(x[[f]])
    numOfItems <- nrow(m[[f]])
    scoredItems[[f]] <- math %>% filter(Form==f) %>% dplyr::select(BlankID)
    unscoredItems[[f]] <- math %>% filter(Form==f) %>% dplyr::select(BlankID)
    for (item in 1:numOfItems) {
      itemType <- m[[f]]$Type[item]
      if (itemType == "M") {
        vector <- sapply(seq(1:numofPersons), function(i) multChoice(i, item, f, shift), simplify = TRUE)
        unscored <- x[[f]][,item + shift] %>% recode(`1` = "A", `2` = "B",`3` = "C", `4` = "D", `5` = "E",
                                                     `6` = "F", `7` = "G",`8` = "H",.default = "O")
        unscored[unscored < 0] <- 0
      } else if (itemType == "H") {
        vector <- sapply(seq(1:numofPersons), function(i) matchType(i, item, f, shift), simplify = TRUE)
      } else if (itemType == "R") {
        vector <- sapply(seq(1:numofPersons), function(i) multChoice2(i, item, f, shift), simplify = TRUE)
      } else if (itemType == "D") {
        vector <- sapply(seq(1:numofPersons), function(i) shortAnsw(i, item, f, shift), simplify = TRUE)
      } else if (itemType == "P") {
        vector <- sapply(seq(1:numofPersons), function(i) orderType(i, item, f, shift), simplify = TRUE)
      } else if (itemType == "N") {
        vector <- sapply(seq(1:numofPersons), function(i) shortAnsw2(i, item, f, shift), simplify = TRUE)
      } else if (itemType == "S" | itemType == "X") {
        vector <- sapply(seq(1:numofPersons), function(i) openEnded(i, item, f, shift), simplify = TRUE)
      }

      scoredItems[[f]] <- cbind(scoredItems[[f]], vector)
      if (itemType == "M") {
        unscoredItems[[f]] <- cbind(unscoredItems[[f]], unscored)
      } else {
        unscoredItems[[f]] <- cbind(unscoredItems[[f]], vector)
      }

      names(scoredItems[[f]])[item + 1] <- m[[f]]$Position[item]
      names(unscoredItems[[f]])[item + 1] <- m[[f]]$Position[item]
      if (itemType != 'D' & itemType != 'N') {
        shift <- shift + nchar(m[[f]]$Key[item]) - 1
      } else {
        shift <- shift
      }
    }
    names(scoredItems[[f]]) <- append(as.character(m[[f]]$ItemID), "BlankID", after = 0)
    names(unscoredItems[[f]]) <- append(as.character(m[[f]]$ItemID), "BlankID", after = 0)
    scoredList[[f]] <- scoredItems[[f]]
    row.names(scoredItems[[f]]) <- scoredItems[[f]][,1]
    scoredItems[[f]][,1] <- NULL
  }
  dat.ctt <- scoredList %>%
    reduce(join, type = "full", by = "BlankID")
  row.names(dat.ctt) <- dat.ctt[,1]
  dat.ctt[,1] <- NULL
  completed.frames <- list()
  completed.frames[['scoredItems']] <- scoredItems
  completed.frames[['dat.ctt']] <- dat.ctt
  winsteps.file <- unscoredItems
  blankId <- unscoredItems$BlankID
  completed.frames[['winsteps.dat']] <- winsteps.file
  return(completed.frames)
}

ctt.analysis <- function(x) {
  item.lables <- colnames(x)
  persons <- nrow(x)
  person.scores <- rowSums(x)
  item.scores <- colSums(x)
  item.max <- as.vector(apply(x, 2, function(x) max(x)*persons))
  pvalue <- item.scores/item.max
  Rir <- apply(x, 2, function(x) cor(x, person.scores - x, method = 'pearson'))
  flag <- sapply(seq(1:length(Rir)), FUN = function(x) {ifelse(Rir[x] < 0.20, "*", "")})
  Rir <- data.frame(Items=names(Rir), correlation=Rir, pval = pvalue, Flag = flag, row.names=NULL)
  return(Rir)
}

write.winsteps <- function(completed.frames, forms) {
  for (form in forms) {
    write.table(completed.frames$winsteps.dat[[form]], paste("winsteps_", as.character(form), ".dat", sep = ""), sep = '', na='', quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}

extended.testmap <- function(x) {
  forms <-c()
  positions <- c()
  it.functions <- c()
  lables <- c()
  keys <- c()
  types <- c()
  for (i in 1:nrow(x)){
    if (x[i,6] == 'H' | x[i,6] == 'R') {
      key <- as.numeric(unlist(strsplit(as.character(x[i,5]), "", fixed = TRUE)))
      label <- c()
      type <- c()
      it.function <- rep(x[i,3], length(key))
      position <- c()
      form <- rep(x[i,1], length(key))
      for (j in 1:length(key)) {
        label <- append(label,paste(as.character(x[i,4]), as.character(j), sep = "_"))
        type <- append(type, "M")
        position <- append(position, paste(as.character(x[i,2]), as.character(j), sep = "."))
      }
    } else {
      key <- x[i,5]
      label <- as.character(x[i,4])
      type <- x[i,6]
      it.function <- x[i,3]
      position <- as.character(x[i,2])
      form <- x[i,1]
    }
    keys <- c(keys, key)
    lables <- c(lables, label)
    types <- c(types, type)
    forms <- c(forms, form)
    positions <- c(positions, position)
    it.functions <- c(it.functions, it.function)
  }
  extended.map <- data.frame(Form = forms, Position = positions, Function = it.functions, ItemID = lables, Key = keys, Type = types)
  return (extended.map)
}
