## load libraries ----
library(boot)
library(xtable) # exporting data.frames to latex
library(bayesm)
library(mcmc)
library(coda)
library(LaplacesDemon)
library(mcmcplots)

# some parameters ----
suffix1 <- "w2r"  # file extensions, but USE ONLY w2r
suffix2 <- "w2rcsv"
suffixes <- c(suffix1, suffix2)

alpha <- 0.05
level_confidence <- 1- 2*alpha  # corresponding to an alpha 0.05 in equivalence using 1-2alpha
level_pred <- 0.25
equivalence_interval <- c()
repetitionsboot <- 9999
repetitionsMAR <- 9999
limit_obs_marp0 <- repetitionsMAR #2000  # if number of points according to Bill's work


figurewidth <- 5 #inches
heightdefault <- 5
minfigureheight <- 0.5 #

digitslatextable <- 3  #numer of digits when exporting to latex

# dir_to_read <- "~/your-dir-with-the-w2r-files"
filesprocessed <- 0
# dir_to_read <- "~/Dropbox/dhl/DataResultsHoldOut"
# dir_for_figures <- "~/DocProys/seminal/ehu2014/paper/figures"
# dir_for_tables <- "~/DocProys/seminal/ehu2014/paper/tables"

# current data 
# dir_to_read <- "~/Dropbox/dhl/DataResults3KF_3"
# dir_to_read <- "~/Dropbox/dhl/DataResults3KF_2"
# dir_to_read <- "~/Dropbox/dhl/DataResults3KF_1"
# dir_to_read <- "~/Dropbox/dhl/DataResults5KF"
dir_to_read <- "~/Dropbox/dhl/DataResults3KF"
dir_default_data <- "~/DocProys/seminal/eht2015/data"
dir_for_figures <- "~/DocProys/seminal/eht2015/article/figures"
dir_for_tables <- "~/DocProys/seminal/eht2015/ese2015"
dir_for_article <- "~/DocProys/seminal/eht2015/article"

filename_marp0 <- "marp0s.mrp"
setwd(dir_to_read)

list_of_methods <- c( "MLP", 
                     "IBK",  
                     "M5P",  
                     "LMS",  
                     "GP" 
                     , "LR" 
                     , "Ln" 
                     , "REP")
list_of_methodsnames <- c("MLP", 
                          "IBk", 
                          "M5P", 
                          "LMS",  
                          "GP" 
                          , "LR" 
                          , "LMSLn"
                          , "RTree")

list_of_datasets <- c("china", "isbsg", "kitchenham","csc", "desharnais", "maxwell")  # must be different with no substrings
list_of_datasetsnames <- c("China", "ISBSG", "CSC","CSC", "Desharnais", "Maxwell" )  # formatting purposes
filenamecismiesratios <- "AllFilesMiersCis.txt"
filenamecisSAsratios <- "AllFilesSAsCIs.txt"

mindatapoints4boot <- 3 #numer of points at the end of the process greater than or equal to this value
  
list_of_folds <- c("KF1", 
                   "KF2",
                   "KF3",
                   "KF4",
                   "KF5") # this substring appears in the file name
list_of_foldsnames <-c("(fld1)", 
                       "(fld2)", 
                       "(fld3)", 
                       "(fld4)", 
                       "(fld5)") # this is how the method's name is printed out
list_of_code_dirs <- c("3KF_1",
                       "3KF_2",
                       "3KF_3",
                       "HoldOut"
                       )
list_of_dirs_names <- c("3KF1",
                       "3KF2",
                       "3KF3",
                       "Hold1"
)

nrowsinltxtable <- 30
sink("Outputecho.txt", append=FALSE, split=TRUE) # redirect 

## FUNCTIONS ----
is_my_file <- function(file_name,suffixes){ #check if it is a file to process
  # files end in .w2r or in .w2r.***  or have w2rcsv. -- recommended .w2r
  parts <- strsplit(file_name,".",fixed=TRUE)
  nparts <- length(parts[[1]])
  n1 <- nchar(parts[[1]][nparts-1])
  prevlast3 <- substr(parts[[1]][nparts-1],n1-2,n1)
  is_file <- (parts[[1]][nparts] %in% suffixes) |
    (parts[[1]][nparts-1] %in% suffixes) |
    (prevlast3 == suffix1) #from string library
  if (grepl("~",file_name,fixed=TRUE)) {is_file <- FALSE}
  return(is_file)
#  simpler if only ont type file_list <- list.files(path=mypath,pattern = ".w2r$")
}

# get the name of the file
file_preffix <- function(file_name){
  # delete last extension after . ; case before . could be added
  parts <- strsplit(file_name,".",fixed=TRUE)
  nparts <- length(parts[[1]])
  n <- nchar(parts[[1]][nparts])
  preffix <- substr(file_name, 1, nchar(file_name)-n-1)
  return(preffix)
}

calc_error <- function(act_estim){
  # compute error of a numeric matrix or data-frame
  # The first column is the actual value and the rest of columns are different estimations
  # return a matrix of errors. 

  error <- act_estim[,-1]-act_estim[,1]

  if ((ncol(act_estim))==2){
    error <- array(t(t(error)))  # drop=false
    colnames(error) <- colnames(act_estim[,2]) #does not work
  }
  #   columnprefix <- #poner los nombres que vengan o crearlos
  #   colnames(error) <- paste(columnprefix,names(act_estim),sep="")

  return (as.matrix(error))
}


my_boot_marp <-
  function(estimation_vec, actual, marp0_computed, marp0_found, file_name) {
    # first check if the marp0 has already been computed (delete "if" for vector of marps)
    if ((!marp0_computed) & (!marp0_found)) {
      print(paste("computed or found ", marp0_computed, marp0_found, file_name))
      # estimation_vec and actual are the same por computing MARP0
      # BILL'S WORK
      # THIS CODE IS EXECUTED IF THE NUMBER OF oBSERVATIONS IS LESS THAN limit_obs_marp0
      if (length(estimation_vec) > (limit_obs_marp0*2)) {
        bootmanual <- rep(0,repetitionsMAR)
        
        print(length(estimation_vec))
        for (i in 1:repetitionsMAR) {
          # estimation_vec[j] <- sapply(actual, function(x){sample(actual[-x],1)}) #vectorised
          for (j in 1:length(estimation_vec)) {
            estimation_vec[j] <-
              sample(actual[-j],1)
          }  #resampling without replacement
          bootmanual[i] <- mean(abs(estimation_vec - actual))
          marp0 <- mean(bootmanual)
          marp0_computed <<- TRUE
          
          #   names(bootmanual) <- file_name
          oldnames <- names(marp0s_densities)
          marp0s_densities <<- cbind(marp0s_densities,
                                     as.data.frame(t(t(bootmanual))))
          names(marp0s_densities) <<- c(oldnames,file_name)
          #   names(marp0s_densities[ncol(marp0s_densities)]) <<- file_name  #bootmanual doesn't work
          
        }
      }
      else{
        #BILL's work
        n <- length(estimation_vec)
        diffs_guess <- matrix(nrow = n, ncol = n)
        colnames(diffs_guess) <- estimation_vec
        rownames(diffs_guess) <- estimation_vec
        for (i in 1:n) {
          diffs_guess[i,] <- estimation_vec[i] - estimation_vec
        }
        diffs_guess <- abs(diffs_guess)
        means_per_point <- apply(diffs_guess, 2, mean)
        marp0 <- mean(means_per_point)
        marp0_computed <<- TRUE
      }
    }
    else {
      marp0 <- MARP0_booted
    }
    return(marp0)
  }


my_boot_mean <- function(error_vec){
  samplemean <- function(x, d){return(mean(x[d]))}
    b <- boot(error_vec, samplemean, R=repetitionsboot) # with package boot
#   samples_errorvec <- lapply(1:999, function(i) sample(error_vec, replace=T)) #manually
#   b_mean <- sapply(samples_errorvec, mean)
#   b_median <- sapply(samples_errorvec, median)
#   std_err <- sqrt(var(b_median)) #calculating the standard deviation of the distribution of medians
#   return(list(std_err=std_err, resamples=samples_errorvec, medians=b_median))
  return(b)
  }

mle <-  function(dat){
  # first remove nonpositive values
  dat <- dat[dat > 0]
  dat <- dat[is.finite(dat)]
  m = mean(log(dat))
  s = mean((log(dat)-m)^2)
  return(exp(m+s/2))
}

my_boot_mle_mean <- function(error_vec){
  #http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
  # Statistic (MLE)
  b <- boot(data=error_vec, statistic=function(d, ind){mle(d[ind])}, R=repetitionsboot)
  return(b)
}

my_boot_geom_mean <- function(error_vec){
  log_error <- log(error_vec[error_vec > 0])
  log_error <-log_error[is.finite(log_error)] #remove the -Inf value before calculating the mean, just in case
  samplemean <- function(x, d){return(exp(mean(x[d])))}
  b <- boot(log_error, samplemean, R=repetitionsboot) # with package boot
  # I have changed some times the exp and the logs in the boot.ci
  return(b)
}

# Make a % confidence interval
my_ciboot <- function(b){  
  conf_int <- boot.ci(b, conf=level_confidence, type="bca")$bca #following 10.9 of Ugarte et al.'s book
  print(paste("   conf int  : ", conf_int[4], " ", conf_int[5], sep=" "))
  return (conf_int)
}

#confidence interval for the geometric mean
my_ciboot4geommean <- function(b){  
  conf_int <- boot.ci(b, conf=level_confidence, type="bca")$bca #following 10.9 of Ugarte et al.'s book
  conf_int[5] <- exp(conf_int[5]) # the boot was computed with log. Now take the measure back to its previous units
  conf_int[4] <- exp(conf_int[4])
  return (conf_int)
}

ci_length <- function(ci){ 
  length_ci <- ci[5]-ci[4]  #compute length subtracting upper limit minus lower limit
  return (length_ci)
}
calc_min_ci_width <- function(cis){
  ci_lengths <- lapply(cis, ci_length)  
  min_ci <- min(unlist(ci_lengths))  #minimum length
  return(c(cis[which(ci_lengths==min_ci)],min_ci)) #a list of two elements-shortest_ci[[2]]
}
ci_upper_limit <- function(ci){
  return(max(abs(ci[4]),abs(ci[5])))
}

calc_equiv_ci <- function(cis){
 ci_uppers <- lapply(cis, ci_upper_limit) #minimum upper limit the determines equivalence
 min_ci <- min(unlist(ci_uppers))
 indexm <- which(ci_uppers==min_ci)
 print(paste("  ", indexm, cis[indexm], sep= " "))
 return (c(cis[indexm], indexm)) # returns the CI and the index
}

plot_ci <- function(ci,i, label_ci){
#   plot confidence intervals 
    segments(ci[1],0.5*i,ci[2],lty=1,lwd=2, lend=1)  #1 butt line caps
#     text(ci[1],0.5*i,"|",offset=0.5,cex=0.8,adj=0,font=2) #offset=0.5,cex=1.1,adj=1,font=2
#     text(ci[2],0.5*i,"|",offset=0.5,cex=1.1,adj=1,font=2) #offset=0.5,cex=1.1,adj=1,font=2
    text(ci[1],0.5*i, round(ci[1],2), pos=3,offset=0.2,cex=0.3,adj=1)
    text(ci[2],0.5*i, round(ci[2],2), pos=3,offset=0.2,cex=0.3,adj=1)
    text(ci[2],0.5*i, paste(label_ci), pos=3,offset=-0.5,cex=0.3,adj=1)
}

# Final figure needs special adjustment and it plots poins
plot_ci_finalfig <- function(ci,i, label_ci, mierspoints){
  #   plot confidence intervals 
  segments(ci[1],0.35*i,ci[2],lty=1,lwd=2, lend=1)  #1 butt line caps
  #     text(ci[1],0.5*i,"|",offset=0.5,cex=0.8,adj=0,font=2) #offset=0.5,cex=1.1,adj=1,font=2
  #     text(ci[2],0.5*i,"|",offset=0.5,cex=1.1,adj=1,font=2) #offset=0.5,cex=1.1,adj=1,font=2
  text(ci[1],0.35*i, round(ci[1],2), pos=3,offset=0.2,cex=0.5,adj=1)
  text(ci[2],0.35*i, round(ci[2],2), pos=3,offset=0.2,cex=0.5,adj=1)
  midci <- ci[1]+ (ci[2]-ci[1])/2
  text(midci,0.35*i, paste(label_ci), pos=3,offset=-0.5,cex=0.6,adj=1)
  jitteredpoints <- jitter(mierspoints,0.01)
  points(jitteredpoints, rep((0.35*i +0.03), length(jitteredpoints)), pch="O", col="black", bg="white", cex=0.25)
}

# these write functions need tuning
# write_file_line <- function(element, file_name){
#   element <- as.matrix(t(element)) #for printing as rows
#   write.table(element, file=file_name, eol = "\n", 
#               sep = ",", col.names=FALSE, append=TRUE)}

write_file_errors <- function(errors, file_name){
  file_errors <- paste(file_name,"Err.txt",sep="")
  errors <- as.matrix(errors)
  write.table(errors, file=file_errors, eol = "\n", sep = ",", col.names=FALSE, append=FALSE) #, header=FALSE) # change the header variable
}

process_data_file <- function(file_name) {
  print(paste("=====================FILE:", file_name))
  file_read <- read.table(file_name, sep=",",header=FALSE, stringsAsFactors=FALSE, dec = ".") #read data
  is_header <- any(sapply(file_read[1, ], function(a) grepl("[a-z]", a, ignore.case=TRUE)))
  number_instances <- 0
  if (is_header) {
    data_mat <- data.matrix(file_read[-1,])
    colnames(data_mat) <- file_read[1,]
    column_names <- file_read[1,]
    column_names <- as.vector(sapply(column_names, function(x){x[]}))} # get only the names
    else  {
      column_names <- c()  # just a vector of names
      column_names[1] <- "Actual"
      for (i in 2:ncol(file_read)) {
        column_names[i] <- paste("Params.",i-1)
      }
      data_mat <- data.matrix(file_read)
      colnames(data_mat) <- column_names
     }
  number_instances <- nrow(data_mat)
  subDir <- files_preffixes[which(names(files_preffixes)==file_name)]
  dir.create(file.path(dir_to_read, subDir), showWarnings = FALSE)
  
  setwd(file.path(dir_to_read, subDir))

  #### 1. Errors 

  errors <- calc_error(data_mat) # compute error for each column pair 
  
  file_name <- files_preffixes[which(names(files_preffixes)==file_name)]
  
  
  ### 2. Absolute errors. Same procedures as above but using abs(errors)
  abserrors <- abs(errors) # compute error for each column pair 
  file_name_abs <- paste(file_name,"Abs", sep="")
  write_file_errors(abserrors,file_name_abs)
  bs_booted <- apply(abserrors, 2, my_boot_geom_mean)  # list of boot for each column of errors
  cis_booted <- lapply(bs_booted, my_ciboot)

  
#   file_name <- files_preffixes[which(names(files_preffixes)==file_name)] # lost name somewhere
  new_file_name <- paste(file_name_abs,"AbsCis.txt",sep="")
  lapply(cis_booted, write, new_file_name, append=TRUE)  # write conf ints to a file
  # writeLines(unlist(lapply(cis_booted, paste, collapse=" ")))
  
  shortest_ci_abs <- calc_min_ci_width(cis_booted) # minimum coef interval in WIDTH of all booted samples. best ci
  min_equiv_ci_index <- calc_equiv_ci(cis_booted) # Index of the best MIE found
  
  min_equiv_ci_abserr <- min_equiv_ci_index[1]
  index_min_equiv_ci <- min_equiv_ci_index[2]  # the index of the best CI MIE
  
  index_of_min_ci[which(names(index_of_min_ci)==names(file_name))] <<- unlist(index_min_equiv_ci)
  print(paste("       index_of_min_equivci[]",  index_min_equiv_ci, sep = " ")) 
  index_min_equiv_ci <- unlist(index_min_equiv_ci)

  #   2.1 MARP
  # compute MARP0  --vectors --
  # although MARP0s do not vary much, only take the first computation and propagate
  
  marp0_computed <- FALSE  # check if it has been computed within a file
  marp0_found <- FALSE
  need_marp0 <- TRUE  #variable for asking computation of boot marp0
  create_mrp0s <- FALSE  # variable for creating the file mrp0s
  
  find_name_marp0 <- function(datasetcode, file_name){grepl(datasetcode, file_name, ignore.case=TRUE)} # tries to find the code of the file within the file_name
  in_dataset <- sapply(list_of_datasets, find_name_marp0, file_name)
  
  if (sum(in_dataset)==0){  
    print("name of dataset is not in the list")
    # add it to the list of names
    list_of_datasets <<- c(list_of_datasets, file_name)
    list_of_datasetsnames <<- c(list_of_datasetsnames, file_name)
    the_name <- file_name  # this is the filename==datasetname
    marp0s <- c(marp0s, NA)
    names(marp0s)[length(marp0s)] <- the_name
  }
  else {
    find_kfold <- function(foldscode, file_name){grepl(foldscode, file_name, ignore.case=TRUE)}
    thekfold <- sapply(list_of_folds, find_kfold, file_name)
    namekfold <- ""
    namekfold <- list_of_folds[which(thekfold==TRUE)]
    the_name <- list_of_datasetsnames[which(in_dataset==TRUE)]
    # now join the string of the kfold to the name
    the_name <- paste(the_name, namekfold, sep="", collapse=NULL)}

 # now we have in the_name the name of the dataset we are dealing with for the MARP0
## check if the marp0 has been computed. it should return false if the file_name is new
## the search key is the "datasetsname" 
  
  find_the_marp0 <- function(){
    # first check if it is in the marp0s list
    if (!is.na(marp0s[the_name])){
      # marp0 already computed. no need to look in the file
      marp0_found <<- TRUE
      MARP0_booted <<- marp0s[the_name]
    }
    else {
      # marp0 wasn't in the list
      #check if marp0 is in the file
      filename_marp0 <- paste(dir_to_read,"/", filename_marp0, sep="") #add path to read
      if(file.exists(filename_marp0)) {  #file marp0s has already been created and populated
        marp0s_read <- read.table(filename_marp0, sep=",", header=TRUE, stringsAsFactors=FALSE, dec = ".")
        thereis_thename <- marp0s_read[[1]]==the_name
        if (sum(thereis_thename)>=1){ #at least one entry matches the file_name
          #now check if the nreps match. nreps is the second column of the file
          reps_found <- marp0s_read[[2]]==repetitionsMAR
          result <- thereis_thename & reps_found
#           browser(text="let's see the result")
          if (sum(result) >= 1) { #we have found one result
            # marp0s are in the 3rd column of the file
            MARP0_booted <<- marp0s_read[[3]][result]
            marp0s[the_name] <<- MARP0_booted
            marp0_found <<- TRUE 
            print("Found the MARP0 in the file")
          }
          else{ #reps do not match, so compute marp0
            print("nreps do not match --> compute")
            marp0_found <<- FALSE
          }  
        }
        else{
          #nothing found in the file, therefore compute as usual
          print("name of dataset not found in the file -- > compute")
          marp0_found <<- FALSE
        }    
      }
      else { #file marp0s not yet created, therefore compute and create
        create_mrp0s <<- TRUE  
        marp0_found <<- FALSE
      }  
    }
    return(marp0_found)
  }
  
 if (!(find_the_marp0())){
   #compute the marp0 with bootstrap
   print("Computing the MARP0 with bootstrap")
   MARP0_booted <<- my_boot_marp(data_mat[,1], actual=data_mat[,1], 
                                marp0_computed, marp0_found,the_name)
   # and also add to the list of marp0s
   marp0s[length(marp0s)] <<- MARP0_booted 
   names(marp0s) <<- c(names(marp0s[1:(length(marp0s)-1)]),the_name)
   marp0_computed <<- TRUE
   if (create_mrp0s){
     file_marp0 <- paste(dir_to_read,"/", filename_marp0, sep="")
     linemarp0 <- t(c(the_name, repetitionsMAR, 
                      MARP0_booted, format(Sys.time(), "%b %d %X")))
     colnames_marp0 <- c("dataset", "nreps", "marp0","timedate")
     write.table(linemarp0, file=file_marp0,  eol = "\n", 
                 sep = ",", col.names=colnames_marp0, row.names=FALSE, append=FALSE) #, header=FALSE) # change the header variable
     create_mrp0s <<- FALSE
   } 
   else{ #   add a line to the file
     print("Adding a line to the file MARP0s")
     file_marp0 <- paste(dir_to_read,"/", filename_marp0, sep="")
     linemarp0 <- t(c(the_name, repetitionsMAR, MARP0_booted, format(Sys.time(), "%b %d %X")))
     write.table(linemarp0, file=file_marp0,  eol = "\n", 
                 sep = ",", col.names=FALSE,  row.names=FALSE, append=TRUE) #, header=FALSE) # change the header variable

   }
 }
 
#http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))}

  print(paste("MARP0", MARP0_booted))
  MAR <- apply(abserrors,2,mean)
#   print(paste("MAR -->",MAR))
  gMAR <- apply(abserrors, 2, gm_mean)
  print(paste("gMAR -->", gMAR))
  mleMAR <- apply(abserrors, 2, mle)
#   print(paste("mleMAR -->", mleMAR))
  SA <- 1 - (MAR/MARP0_booted)
  print(paste("-- SA --> ",SA))
  MIE <- min_equiv_ci_abserr[[1]][5]
  MIER <- MIE/(MARP0_booted - MIE)
  print(paste("----MIER --> ", MIER))
  MIEmar <- (MIE-MAR)/MAR
  
  # classic MMRE, MdMRE  and Pred, just for curiosity
  mre <- abs(errors)/data_mat[,1] # each error column divided by the actual values
  
  MMRE <- apply(mre, 2, sum)/number_instances  # mean of the mre (or simply use mean)
  
  MdMRE <- apply(mre, 2, median) #Median of the mre
  

  lowpred <- data_mat[ , 1]*(1-level_pred)
  uppred <-  data_mat[ , 1]*(1+level_pred)
  pred  <-  (data_mat[,-1] <= uppred) & (data_mat[,-1] >= lowpred)  #pred is a matrix with logical values 
  LPRED <- apply(t(t(pred)), 2, sum)/number_instances  # the vector of lpred for all columns of the estimations
 
  file_mars <- paste(file_name,"MARs.txt",sep="")
  linemars <- c(MARP0_booted, MAR, SA)
  write.table(linemars, file=file_mars,  eol = "\n", sep = ",", col.names=FALSE, append=TRUE) #, header=FALSE) # change the header variable
  
  
  ## now plot the cis of the file  ----
  previous_dir <- getwd()
  setwd(dir_for_figures)
  namethisfile <- paste(getwd(),"/",file_name_abs,alpha,"CIs",".eps", sep="")
  num_cis <- ncol(errors)
  heightpage <- minfigureheight + num_cis*0.5
  setEPS()
  postscript(file=namethisfile, height=heightpage, width=4.5)     
  par(mar=c(3,0,0,0))
  maxx <- 1.20 * max(ci_upper_limit(unlist(cis_booted))) #
  minx <- 0 # -0.1*maxx  # not needed now
  maxx <- max(maxx, MARP0_booted)
  ymax <- heightpage # for test
  plot(0,0,type="n", xlim=c(minx, maxx*1.10), ylim=c(0, ymax),axes=FALSE, ann=FALSE)
  intervals <- c(0,MIE, maxx)
  labels <- c(0, round(MIE, digits=2),round(maxx, digits=0))
  axis(1, at=intervals, labels = labels, pos=0, cex=0.4)
  mtext(expression(paste("|",mu[act.]-mu[est.],"|", sep=""),at=0,side=1,line=2))
#   mtext("",at=maxx,side=1,line=2)
    label_ci <- column_names[2:ncol(file_read)]  # here put the initial Column's name
  
  segments(MIE,0, MIE, ymax, lty=3,lwd=0.8) # plot the MIE
  mtext(c("MIE", "MARP0"), at=c(MIE, MARP0_booted), side=1,line=2, cex=0.8)
        
  for (i in 1:num_cis) {
    a_ci <- unlist(cis_booted[i])
    plot_ci(c(a_ci[4],a_ci[5]),i, label_ci[i]) #send the ci to plotting
    points(gMAR[i], 0.5*i-0.08, pch=2, cex=0.6) # offset not valid pos=1 below
    text(gMAR[i], 0.5*i+0.1, "gMAR", cex=0.5 )
  }
  points(MARP0_booted[1], 0.5, pch="O", cex=0.9)
  text(MARP0_booted[1], 0.3, "MARP0", cex=0.5 )
  dev.off()  # for pdf et al
  
  setwd(previous_dir)  #after plotting

  # ----
  filesprocessed <<- filesprocessed + 1 # write to the global environment or assign
  allfiles_df[filesprocessed, ] <<- c(as.character(Sys.Date()),
                                               file_name, 
                                               as.numeric(number_instances),
                                               as.numeric(alpha),
                                               level_confidence, 
                                               level_pred,
                                               repetitionsboot, 
                                               repetitionsMAR,
#                                                cis_booted,
                                      as.numeric(shortest_ci_abs[[2]]), 
                                      as.numeric(min_equiv_ci_abserr[[1]][5]), 
                                               as.numeric(MARP0_booted),
                                               as.numeric(MAR[index_min_equiv_ci]), 
                                              as.numeric(gMAR[index_min_equiv_ci]),
                                      as.numeric(mleMAR[index_min_equiv_ci]),
                                      as.numeric(MMRE[index_min_equiv_ci]),
                                      as.numeric(MdMRE[index_min_equiv_ci]),
                                      as.numeric(LPRED[index_min_equiv_ci]),
                                      as.numeric(SA[index_min_equiv_ci]), 
                                      as.numeric(MIER), 
                                      as.numeric(MIEmar[index_min_equiv_ci]))
  
  setwd(dir_to_read)
  #select the minimum interval to return  (Error, AbsError, etc. )
  return(min_equiv_ci_abserr)  #currently, return the minimum confident interval obtained for the Error
  # TO DO
  # set a tolerance limit  equiv_interval_error <- (min_error, max_error) and verify
  
  # now check level of prediction 
  }

plot_marp0_density <- function(bootmarp0, thename){
  setwd(dir_for_figures)
  setEPS()
  #now clean thename -the name of the file
  name_selected <- sapply(list_of_datasets, 
                          function(x){grepl(x, thename, ignore.case=TRUE)}) #find the dataste
  newname <- list_of_datasetsnames[name_selected]  #set the formatted name
  namethisdensity <- paste(newname,"Marp0Hist.eps", sep="")
  postscript(file=namethisdensity, height=heightdefault, width=figurewidth)
  textmain <- paste("MARP0 Histogram of dataset ", newname,"\n", " " ,sep="")
  ylabtext <- paste("Frequency in ", repetitionsMAR, " runs", sep="")
  marp0hist <- hist(bootmarp0)
  plot(marp0hist, main=textmain, xlab="Effort", ylab=ylabtext) #prob=TRUE for probabilities not counts
  lines(density(bootmarp0))
  dev.off()  
}
## end functions
#
### MAIN PROCEDURE -----

## processing files in a directory
a_list_of_files <- list.files()  #all files in the directory
data_files <- sapply(a_list_of_files,is_my_file,suffixes,simplify=TRUE) # select those to process

data_to_read <- a_list_of_files[which(data_files==TRUE)]  # actual data files to process
files_preffixes <- sapply(data_to_read ,file_preffix)  # create the prefixes to file names 

## create the final data.frame ----
N <- length(data_to_read)  # number of rows of the data.frame
# we create a new df for each run. currently no need for updating an old one

index_of_min_ci <- rep(NA, N)  # vector of the index -for each file- of the order of the minimum ci equiv
names(index_of_min_ci) <- data_to_read  # or files_preffixes to index the vector too

allfiles_df  <- data.frame(Date=as.Date(character()),
                           FileName=character(), 
                           NValidationpoints=integer(),
                           Alphalevel=numeric(),
                           ConfidenceLevel=numeric(),
                           Predictionlevel=numeric(),
                           Repetitionsboot=numeric(),
                           RepetitionsMAR=numeric(),
#                            ConfidenceIntervals=list(),
                           LengthShortest=numeric(),
                           MIE=numeric(),
                           MARP0=numeric(),
                           MAR=numeric(),
                            gMAR=numeric(),
                           mleMAR=numeric(),
                           MMRE=numeric(),
                           MdMRE=numeric(),
                           LPRED = numeric(), 
                           SA=numeric(),
                           MIERatio=numeric(),
                           MIEmar=numeric(),
                           stringsAsFactors=FALSE)
allfiles_df[N, ] <- NA  # initial fill 

# now process each file ----

## marp0s for the datasets  (only one (the first booted. ok.) marp0 for each dataset)) ----
marp0s <- rep(NA, length(list_of_datasets)) #initialize all marp0s to NA
names(marp0s) <- unlist(list_of_datasetsnames) #use actual printed name
# list of vector boots for plotting densities of marp0s
marp0s_densities <- data.frame(matrix(NA, nrow=repetitionsMAR, ncol=0))  # the marp0s data for histograms 

##--- PROCESS EACH FILE FOUND IN THE DIR ----
minimum_cis <- lapply(data_to_read, process_data_file) # process each file and get a list of minimum ci_s for each file

## --- plot marp0s densities ----
mapply(plot_marp0_density, marp0s_densities, names(marp0s_densities))

# plot the MINIMUM cis in the final Figure ----
print(paste("--> proceed to compute all MINIMUM CIs of all files"))
min_equiv_ci_err <- min(ci_upper_limit(unlist(minimum_cis))) #

print(paste("   min_equiv_ci_err ", min_equiv_ci_err, sep=" "))

num_cis <- length(minimum_cis)

maxx <-  max(sapply(minimum_cis, function(x) ci_upper_limit(unlist(x)))) #for plotting purposes
maxp0 <- max(as.numeric(allfiles_df$MARP0))
maxx <- max(maxx, maxp0 )
minx <- 0  # simmetric plots

file_name <- "AllFiles"
setwd(dir_for_figures)
namethisfile <- paste(getwd(),"/",file_name,alpha,"MinCis.eps", sep="")
heightpage <-  num_cis*0.36 #minfigureheight +
setEPS()
postscript(file=namethisfile, height=heightpage, width=figurewidth)     
par(mar=c(3,0,0,0))
# ymax <- 4+ 1.2 * num_cis
ymax <- heightpage # for test
plot(0,0,type="n", xlim=c(0, maxp0*1.10), ylim=c(0, ymax),axes=FALSE, ann=FALSE)
intervals <- c(0, maxx, maxp0)
labels <- c(0, round(maxx,digits=0), round(maxp0, digits=0))
axis(1, at=intervals, labels = labels, pos=0)
  mtext("Magnitude of the Absolute Error",at=2000,side=1,line=2, cex=0.8)
  mtext("MAR",at=maxx-1000,side=1,line=2)
label_ci <- files_preffixes
for (i in 1:length(minimum_cis)) {
  a_ci <- unlist(minimum_cis[i])
  plot_ci_finalfig(c(a_ci[4],a_ci[5]),i, label_ci[i], c()) #send the ci to plotting
  points(allfiles_df$MARP0[i], 0.35*i, pch="O", cex=0.5)
#   text(allfiles_df$MARP0[i], 0.35*i-0.08, "MARP0", cex=0.5 )
}
dev.off()
setwd(dir_to_read)
save(allfiles_df, file="allfiles.Rda")  #something wrong since it does nothing

# remove all rows that have NA in any column
allfiles_df <- allfiles_df[complete.cases(allfiles_df),]

previous_dir <- getwd()
setwd(dir_for_tables)
## ---- subset the data.frame and export to latex ----
keeps <- c("FileName",  "MARP0", "MAR", "gMAR", "MMRE", 
           "MdMRE", 
           "LPRED", "MIE", "SA", "MIERatio")
data2latex <- allfiles_df[,keeps]  # or subset(allfiles_df, select=c(MAR,,,))
data2latex[, 2:length(keeps)] <- sapply(data2latex[, 2:length(keeps)], as.numeric)

namelpred <- paste("Prd(", level_pred,")", sep="")
colnames(data2latex) <- c("File Name",  #for latex printing
                          "$\\overline{MAR}_{P_0}$", #MARP0
                          "MAR", 
                          "gMAR",
                          "MMRE",
                          "MdMRE", 
                          namelpred, 
                          "MIE", "SA",  
                          "\\textbf{MIEratio}")#  MIERatio

# now add two character columns with method and dataset
filenames <- as.vector(data2latex[,1])
col_method <- filenames  # initially store the file name
col_dataset <- filenames
# if the file name has the corresponding identifier i.e, china, isbsg, nn, m5p and so on
#list of datasets and methods at the beginning of this file

setcolumnsnames <- function(anacronym, aname){
  the_index <- grepl(anacronym, filenames , ignore.case=TRUE)
  thecolumn[the_index] <<- aname
}

thecolumn <- col_method
mapply(setcolumnsnames, list_of_methods, list_of_methodsnames)
col_method <- as.data.frame(thecolumn)
names(col_method) <- "Method"

setcolumnsnames4folds <- function(anacronym, aname){
  the_index <- grepl(anacronym, filenames , ignore.case=TRUE)
  # here add the string aname
  thecolumn[the_index] <<- paste (thecolumn[the_index], aname,sep = "", collapse = NULL) 
}

thecolumn <- col_dataset
mapply(setcolumnsnames, list_of_datasets, list_of_datasetsnames)
mapply(setcolumnsnames4folds, list_of_folds, list_of_foldsnames)
col_dataset <- as.data.frame(thecolumn)
names(col_dataset) <- "Dataset"

# add the columns to the data.frame (deleting previous column 1)
keepthecolumnnames <- c(colnames(col_method), 
                        colnames(col_dataset), 
                        colnames(data2latex[ ,2:length(keeps)]))
data2latex <- data.frame(c(col_method, 
                           col_dataset, 
                           data2latex[ ,2:length(keeps)])) 
colnames(data2latex) <- keepthecolumnnames  #latex formatting not lost

# sort the dataset
data2latex <- data2latex[with(data2latex, 
                              order(data2latex[ncol(data2latex)])), ] # sort by last column MIERatio

negsdata2latex <- data2latex[data2latex[[ncol(data2latex)]]<0,]  #select negative values on the MIEratio

# decreasing ordering for negative values, just in case it is needed
# negsdata2latex <- negsdata2latex[with(data2latex,
#                                       order(negsdata2latex[ncol(data2latex)], decreasing=TRUE)),]

data2latex <- data2latex[data2latex[[ncol(data2latex)]]>=0,]

data2latex <- rbind(data2latex,negsdata2latex)

# select the number of rows to be printed IF it is less than the actual number of rows
nrowsinltxtable <- min(nrowsinltxtable, nrow(data2latex))
data2latex <- data2latex[1:nrowsinltxtable, ]

# before converting to latex, transform the SA column to string 
# make boldface those cells in SA column that are not in descending order
# use digitslatextable 
SA <- data2latex[ncol(data2latex)-1]
SAformatted <- ""
names(SAformatted) <- colnames(data2latex)[ncol(data2latex)-1] 

SAformatted[1] <- formatC(SA[1,], dig=digitslatextable, format="f")
maxvalue <- SA[1,]
for (i in 2:(nrow(data2latex))) {
  if (maxvalue < SA[i,]) {
    SAformatted[i] <- paste("\\textit{", formatC(SA[i,], 
                                                 dig=digitslatextable, format="f"), "}",
                            sep="")
  }
  else {
    SAformatted[i] <- formatC(SA[i,], dig=digitslatextable, format="f")
    maxvalue <- SA[i,]
  }
}

### put the last column (MIEratio) in boldface
MIEformatted <- sapply(data2latex[ncol(data2latex)],
                       function(x){
                         paste("\\textbf{",
                               formatC(x, 
                                       dig=digitslatextable, format="f"),
                               "}", sep="")})

### rebuild the dataframe
data2latex <- cbind(data2latex[ ,1:(ncol(data2latex)-2)], 
                    SAformatted, 
                    MIEformatted)
#                     data2latex[ ,ncol(data2latex)])
colnames(data2latex) <- keepthecolumnnames

my_latex_table1 <- xtable(data2latex)  # convert R object to Latex
digits(my_latex_table1)[] <- digitslatextable
# align(my_latex_table1) <- paste("lll", 
#                                 paste(rep("c", ncol(my_latex_table1)-2, sep=""),
#                                       collapse=""), sep="")  # align  ncol(x) + 1

# set labels and captions to the table
labeltable <- paste("table:summary",alpha,sep="")
label(my_latex_table1) <- labeltable
caption(my_latex_table1) <- paste("This table shows the summary results for the best ", nrowsinltxtable, " values
                                  in ascending order of MIEratio ($\\alpha=", alpha,"$).", sep="")

alltable.tex <- print.xtable(my_latex_table1, 
                             type = "latex",
                             print.results=FALSE, 
                      booktabs=TRUE, include.rownames=FALSE,
                      floating.environment='sidewaystable',
#                      floating.environment='table',
                      size = 'small', 
                      scalebox = 0.8,
                      sanitize.text.function=function(x){x},
                      sanitize.colnames.function=function(x){x})
nametable <- paste("AllTable",alpha,".tex", sep="")
write(alltable.tex, file=nametable, append=FALSE)

# return to original dir
# setwd(previous_dir) #

setwd(dir_to_read)
 
# ## --- Plot Histograms of the absolute errors of best MIEs
# plotHistBestMIE <- function(x, thename){  
#   file_read <- read.table(thename, sep=",",header=FALSE, stringsAsFactors=FALSE, dec = ".") #read data
#   is_header <- any(sapply(file_read[1, ], function(a) grepl("[a-z]", a, ignore.case=TRUE)))
#   if (is_header) {
#     data_mat <- data.matrix(file_read[-1,])
#     colnames(data_mat) <- file_read[1,]
#     column_names <- file_read[1,]
#     column_names <- as.vector(sapply(column_names, function(x){x[]}))} # get only the names
#   else  {
#     column_names <- c()  # just a vector of names
#     column_names[1] <- "Actual"
#     for (i in 2:ncol(file_read)) {
#       column_names[i] <- paste("Params.",i-1)
#     }
#     data_mat <- data.matrix(file_read)
#     colnames(data_mat) <- column_names
#   }
#   
#   setwd(dir_for_figures)
#   setEPS()
#   namethisfile <- sub(".w2r", "", thename)
#   namethisfile <- paste(namethisfile,alpha,"Hist.eps",sep="")
#   postscript(file=namethisfile, height=5, width=7)     
#   #   par(mar=c(2,1,0,0))
#   actual <- data_mat[,1]
#   estimated <- data_mat[, x+1]
#   AR <- abs(actual-estimated)
#   hist(AR, xlab="Absolute Residuals (Errors)", main="")
#   dev.off()
#   setwd(dir_to_read)
# }
# mapply(plotHistBestMIE, index_of_min_ci, names(index_of_min_ci))  

setwd(dir_to_read)
#####  Start GENERATE THE CONFIDENCE INTERVALS FOR THE MIERATIO ##############
#subset the dataframe allfiles_df by method and apply the ci
# call my_ciboot
# first create 
print("--- Start generating the Confidence Intervals for the MIERatio")

mieratios_df <- as.data.frame(c(col_method, as.data.frame(as.numeric(allfiles_df$MIERatio))))
colnames(mieratios_df)[2] <- "MIERatio"
miersbymethodfull <- split(mieratios_df, mieratios_df$Method) # list of data.frames for each of the methods found. 
#these data.frames may have different number of observations -> so we remove data.frames with few observations 
min_number_of_points <- mindatapoints4boot
n_observspermethod <- lapply(miersbymethodfull, function(x) {nrow(x[1])}) # list of number of observations per method
n_observspermethod <- unlist(n_observspermethod)

#### remove all miers with negative values. remove NAs and check the number of data points
leaveonlyvalids <- function(x) {
  tmp <- as.vector(x[2]) #take the values of each method
  tmp <- tmp[!is.na(tmp)]
  tmp <- tmp [tmp > 0]
  return (tmp)
}
miersbymethod <- lapply(miersbymethodfull, leaveonlyvalids)

# select for the bootstrap only those methods with a minimum number of observations. 
leavewithpoints <- function(x, points) { 
  if (length(x) >= points) {return (x)}
  else {return (NULL)}
}
miersbymethod <- lapply(miersbymethod, leavewithpoints, min_number_of_points)
miersbymethod <- miersbymethod[!sapply(miersbymethod, is.null)]  # Remove method with null values

#miersbymethod is a list of methods: take the names of the list
namesmethods <- names(miersbymethod)

listofboots <- lapply(miersbymethod,  function(x){my_boot_geom_mean (x)})  # list of boot for each column of miers-method

#set the level of confidence to alpha, and then 

level_confidence <- 1 - alpha
mierscis_booted <- lapply(listofboots, my_ciboot) # this creates the confs ints.  

level_confidence <- 1- 2*alpha  # back to the initial value

setwd(dir_default_data)
filename_cis_miesratios <- filenamecismiesratios
lapply(mierscis_booted, write, filename_cis_miesratios, append=TRUE)  # write conf ints to a file
# writeLines(unlist(lapply(cis_booted, paste, collapse=" ")))
shortest_ci_miers <- calc_min_ci_width(mierscis_booted) # minimum coef interval in WIDTH of all booted samples. best ci
#min_equiv_ci_index <- calc_equiv_ci(mierscis_booted)

##### Plot the Cis for the MIEratios ##### COPY FROM plot the MINIMUM cis in the final Figure ----
# plot the MIERATIOS cis in the final Figure ----
print(paste("--> plot MIERatios CIs of all files"))
# remove elements that are null (mier-ci not computed)
# null_values <- unlist(lapply (mierscis_booted, function(x){is.null(x[[1]])}))
# mierscis_booted <- mierscis_booted[!null_values]

num_cis <- length(mierscis_booted)

maxx <-  max(sapply(mierscis_booted, function(x) ci_upper_limit(unlist(x)))) #for plotting purposes
minx <- 0  # all positive values

file_name <- "AllFiles"
setwd(dir_for_figures)
alphachar <- gsub ('\\.', '', alpha) # take care of . but not as a regular expression

#### find string for directories  in   newcode
find_dir_name <- function(anacronym, aname){
  found <- grepl(anacronym, aname , ignore.case=TRUE)
  return (found) 
}
indexresult <- which(sapply(list_of_code_dirs, find_dir_name,  dir_to_read) == TRUE)
newcode <- list_of_dirs_names[indexresult]

namethisfile <- paste(getwd(),"/",file_name,newcode,alphachar,"MIERsCis.eps", sep="")
heightpage <-  minfigureheight + num_cis*0.36 #
setEPS()
postscript(file=namethisfile, height=heightpage, width=figurewidth)     
par(mar=c(2,0,0,0)) # par(mar=c(3,0,0,0)) 
#par(oma=c(5,7,1,1))
# ymax <- 4+ 1.2 * num_cis
ymax <- heightpage # for test
plot(0,0,type="n", xlim=c(0, maxx*1.10), ylim=c(0, ymax),axes=FALSE, ann=FALSE)
intervals <- c(0, maxx)
labels <- c(0, round(maxx,digits=0))
axis(1, at=intervals, labels = labels, pos=0)
# mtext("Bootstraped Confidence intervals for the mean of the MIER",at=(maxx/3),side=1,line=2, cex=0.8)
#mtext("M--",at=maxx-1000,side=1,line=2)
label_ci <- names(mierscis_booted) ## 
for (i in 1:num_cis) {
  a_ci <- unlist(mierscis_booted[i])
  plot_ci_finalfig(c(a_ci[4],a_ci[5]),i, label_ci[i], miersbymethod[[i]]) #send the ci to plfotting
}
dev.off()

#### Example of t-test for for two methods  -- check methods if adding more of them
# t.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
# mlp <- as.vector(unlist(miersbymethod_lessfirstcol[6]))
# rtree <- as.vector(unlist(miersbymethod_lessfirstcol[7]))
# t.test(mlp, rtree, alternative = "two.sided", mu=0, paired = FALSE, conf.level = 0.95)


#####   BAYESIAN METROPOLIS-HASTINGS 
# http://stats.stackexchange.com/questions/33382/how-do-i-calculate-a-confidence-interval-for-the-mean-of-a-log-normal-data-set
bayesCImethastings <- function(x) {
    set.seed(1)
  
  # Simulated data
  data0 = x
  
  # Log posterior
  lp = function(par){
    if(par[2]>0) return( sum(log(dlnorm(data0,par[1],par[2]))) - 2*log(par[2]))
    else return(-Inf)
  }
  
  # Metropolis-Hastings
  NMH = 260000
  out = metrop(lp, scale = 0.175, initial = c(0.1,0.8), nbatch = NMH)
  
  #Acceptance rate
#   out$acc
  
  deltap = exp(  out$batch[,1][seq(10000,NMH,25)] + 0.5*(out$batch[,2][seq(10000,NMH,25)])^2  )
  
  #plot(density(deltap))
  
  # 95% credibility interval
  twovalues <- c(quantile(deltap,0.025),quantile(deltap,0.975))
  names(twovalues) <- c("MH2.5\\%", "MH97.5\\%")
  return (twovalues)
}


#####   BAYESIAN WITH JEFFREYS  PRIOR  #########
bayesCIjeffreys <- function(x){  
  # required package library(bayesm)
  # simulated data
  mu <- 0
  sdv <- 1
  y <- x
  
  # model matrix
  X <- model.matrix(log(y)~1)
  # prior parameters
  Theta0 <- c(0)
  A0 <- 0.0001*diag(1)
  nu0 <- 1 # Jeffreys prior for the normal model; set nu0 to 1 for the lognormal model
  sigam0sq <- 0
  # number of simulations
  n.sims <- 5000
  
  # run posterior simulations
  Data <- list(y=log(y),X=X)
  Prior <- list(betabar=Theta0, A=A0, nu=nu0, ssq=sigam0sq)
  Mcmc <- list(R=n.sims)
  bayesian.reg <- runireg(Data, Prior, Mcmc)
  mu.sims <- t(bayesian.reg$betadraw) # transpose of bayesian.reg$betadraw
  sigmasq.sims <- bayesian.reg$sigmasqdraw
  
  # posterior simulations of the mean of y: exp(mu+sigma²/2)
  lmean.sims <- exp(mu.sims+sigmasq.sims/2)
  density(lmean.sims)
  
  # credibility interval about lmean:
  twovalues <- quantile(lmean.sims, probs = c(0.025, 0.975))
  names(twovalues) <- c("Jfr 2.5\\%", "Jfr 97.5\\%")
  return (twovalues)
}

bayesCIquantile <- function(x){
  #from LaplacesDemon
  p_int_quantile <- p.interval(x, HPD=FALSE, MM=FALSE, plot=FALSE, prob=1-alpha)
  twovalues <- c(p_int_quantile[1], p_int_quantile[2])
  names(twovalues) <- c("Q 2.5\\%", "Q 97.5\\%")
  return(twovalues)
}

##### Changed from LaplacesDemon ####
p.intervalv2 <- function (obj, HPD = TRUE, MM = TRUE, prob = 0.95, plot = FALSE, 
            PDF = FALSE, nombre) 
  {
    #nombre <- toString(names(obj))
    if (missing(obj)) 
      stop("The obj argument is required.")
    if (length(prob) > 1) 
      stop("The prob argument must be a scalar.")
    if ((prob <= 0) | (prob >= 1)) 
      stop("The prob argument must be in the interval (0,1).")
    if (identical(class(obj), "demonoid")) {
      thin <- obj$Rec.BurnIn.Thinned
      n <- nrow(obj$Posterior1)
      if (thin < nrow(obj$Posterior1)) 
        x <- cbind(obj$Posterior1[thin:n, ], obj$Monitor[thin:n, 
                                                         ])
      if (thin >= nrow(obj$Posterior1)) 
        x <- cbind(obj$Posterior1, obj$Monitor)
      colnames(x) <- c(colnames(obj$Posterior1), colnames(obj$Monitor))
      obj <- x
    }
    else if (identical(class(obj), "iterquad")) {
      if (any(is.na(obj$Posterior))) 
        stop("Posterior samples do not exist in obj.")
      obj <- obj$Posterior
    }
    else if (identical(class(obj), "laplace")) {
      if (any(is.na(obj$Posterior))) 
        stop("Posterior samples do not exist in obj.")
      obj <- obj$Posterior
    }
    else if (identical(class(obj), "pmc")) {
      x <- cbind(obj$Posterior2, obj$Monitor)
      colnames(x) <- c(colnames(obj$Posterior2), colnames(obj$Monitor))
      obj <- x
    }
    else {
      x <- as.matrix(obj)
      oname <- deparse(substitute(obj))
      colnames(x) <- colnames(obj)
      obj <- x
    }
    if (any(!is.finite(obj))) 
      stop("The obj argument must contain finite values.")
    if (any(apply(obj, 2, is.constant))) 
      stop("The obj argument has insufficient length.")
    if (HPD == TRUE) {
      vals <- apply(obj, 2, sort)
      if (!is.matrix(vals)) 
        stop("obj must have nsamp > 1.")
      nsamp <- nrow(vals)
      npar <- ncol(vals)
      gap <- max(1, min(nsamp - 1, round(nsamp * prob)))
      init <- 1:(nsamp - gap)
      inds <- apply(vals[init + gap, , drop = FALSE] - vals[init, 
                                                            , drop = FALSE], 2, which.min)
      ans <- cbind(vals[cbind(inds, 1:npar)], vals[cbind(inds + 
                                                           gap, 1:npar)])
      dimnames(ans) <- list(colnames(obj), c("Lower", "Upper"))
      attr(ans, "Probability.Interval") <- gap/nsamp
    }
    if (HPD == FALSE) {
      ans <- apply(obj, 2, quantile, probs = c((1 - prob)/2, 
                                               1 - ((1 - prob)/2)))
      ans <- as.matrix(t(ans))
      colnames(ans) <- c("Lower", "Upper")
      attr(ans, "Probability.Interval") <- prob
    }
    if (MM == TRUE) {
      ansmm <- ans
      mm <- apply(obj, 2, is.multimodal)
      if (any(mm)) {
        cat("\n\nPotentially multimodal column vectors:\n", 
            which(mm), "\n")
        vals <- apply(obj, 2, sort)
        if (!is.matrix(vals)) 
          stop("obj must have nsamp > 1.")
        for (m in which(mm)) {
          kde <- density(vals[, m])
          dens <- approx(kde$x, kde$y, vals[, m])$y
          dens.ind <- dens >= as.vector(quantile(dens, 
                                                 probs = 1 - prob)) * 1
          ints <- ""
          count <- 1
          for (i in 1:nrow(vals)) {
            if ((i == 1) & (dens.ind[i] == 1)) {
              ints <- paste("(", round(vals[i, m], 3), 
                            ",", sep = "")
              if (count > ncol(ansmm)) 
                ansmm <- cbind(ansmm, NA)
              ansmm[m, count] <- vals[i, m]
              count <- count + 1
            }
            if (i > 1) {
              if ((dens.ind[i] == 0) & (dens.ind[i - 1] == 
                                          1)) {
                ints <- paste(ints, round(vals[i - 1, m], 
                                          3), ")", sep = "")
                if (count > ncol(ansmm)) 
                  ansmm <- cbind(ansmm, NA)
                ansmm[m, count] <- vals[i - 1, m]
                count <- count + 1
              }
              if ((dens.ind[i] == 1) & (dens.ind[i - 1] == 
                                          0)) {
                ints <- paste(ints, " (", round(vals[i, 
                                                     m], 3), ",", sep = "")
                if (count > ncol(ansmm)) 
                  ansmm <- cbind(ansmm, NA)
                ansmm[m, count] <- vals[i, m]
                count <- count + 1
              }
            }
          }
          if ((dens.ind[i] == 1) & (dens.ind[i - 1] == 
                                      1)) {
            ints <- paste(ints, round(vals[i, m], 3), ")", 
                          sep = "")
            if (count > ncol(ansmm)) 
              ansmm <- cbind(ansmm, NA)
            ansmm[m, count] <- vals[i, m]
            count <- count + 1
          }
          cat("\nColumn", m, "multimodal intervals:", ints, 
              "\n")
        }
      }
    }
    if (plot == TRUE) {
      if (PDF == TRUE) {
        setEPS()
        namethisfile <- paste("aFig", nombre, ".eps", sep = "")
        postscript(file=namethisfile, height=heightpage, width=figurewidth)   
        #pdf("P.Interval.Plots.pdf")
        #par(mfrow = c(1, 1))
        par(mar=c(2,2,1,0))
      }
      else par(mar=c(2,2,1,0), ask=FALSE)#par(mfrow = c(1, 1), ask = FALSE) #TRUE)   CHANGED   
      if (MM == FALSE) {
        for (i in 1:nrow(ans)) {
          kde <- kde.low <- kde.high <- density(obj[, i])
          kde.low$x <- kde$x[kde$x < ans[i, 1]]
          kde.low$y <- kde$y[which(kde$x < ans[i, 1])]
          kde.high$x <- kde$x[kde$x > ans[i, 2]]
          kde.high$y <- kde$y[which(kde$x > ans[i, 2])]
          plot(kde, xlab = "", main = nombre) #colnames(obj)[i])  #deleted "Value"  CHANGED
          polygon(kde, col = "black", border = "black")
          polygon(c(min(kde.low$x), kde.low$x, max(kde.low$x)), 
                  c(min(kde.low$y), kde.low$y, min(kde.low$y)), 
                  col = "gray", border = "gray")
          polygon(c(min(kde.high$x), kde.high$x, max(kde.high$x)), 
                  c(min(kde.high$y), kde.high$y, min(kde.high$y)), 
                  col = "gray", border = "gray")
          abline(v = 0, col = "red", lty = 2)
        }
      }
      if (MM == TRUE) {
        for (i in 1:nrow(ansmm)) {
          kde <- density(obj[, i])
          x1 <- min(which(kde$x >= ansmm[i, 1]))
          x2 <- max(which(kde$x <= ansmm[i, 2]))
          plot(kde, xlab = "Value", main = "") #colnames(obj)[i])
          polygon(kde, col = "gray", border = "gray")
          with(kde, polygon(x = c(x[c(x1, x1:x2, x2)]), 
                            y = c(0, y[x1:x2], 0), col = "black"))
          if ((ncol(ansmm) > 2) && !is.na(ansmm[i, 3])) {
            x1 <- min(which(kde$x >= ansmm[i, 3]))
            x2 <- max(which(kde$x <= ansmm[i, 4]))
            with(kde, polygon(x = c(x[c(x1, x1:x2, x2)]), 
                              y = c(0, y[x1:x2], 0), col = "black"))
          }
          if ((ncol(ansmm) > 4) && !is.na(ansmm[i, 5])) {
            x1 <- min(which(kde$x >= ansmm[i, 5]))
            x2 <- max(which(kde$x <= ansmm[i, 6]))
            with(kde, polygon(x = c(x[c(x1, x1:x2, x2)]), 
                              y = c(0, y[x1:x2], 0), col = "black"))
          }
          abline(v = 0, col = "red", lty = 2)
        }
      }
      if (PDF == TRUE) 
        dev.off()
    }
    return(ans)
  }



bayesCIhpdnoprior <- function(x, method){
  #from LaplacesDemon
  p_int_hpd <- p.intervalv2(x, HPD=TRUE, MM=FALSE, plot=TRUE, prob=1-alpha, PDF=TRUE, method)
  twovalues <- c(p_int_hpd[1], p_int_hpd[2])
  names(twovalues) <- c("HPDl", "HPDu")
  return(twovalues)
}

###### COMPUTE BAYESIAN INTERVALS (2 types) and plot them
plotseveralcis <- function(cis, name_file, points_in_cis) {
  setwd(dir_for_figures)
  setEPS()
  num_cis <- length(cis)
  maxx <-  max(sapply(cis, function(x) ci_upper_limit_v2(x))) #for plotting purposes
  minx <- 0  # all positive values
  heightpage <-  minfigureheight + num_cis*0.36 #
  
  postscript(file=name_file, height=heightpage, width=figurewidth)     
  par(mar=c(2,0,0,0)) # par(mar=c(3,0,0,0)) 
  #par(oma=c(5,7,1,1))
  # ymax <- 4+ 1.2 * num_cis
  ymax <- heightpage # for test
  plot(0,0,type="n", xlim=c(0, maxx*1.10), ylim=c(0, ymax),axes=FALSE, ann=FALSE)
  intervals <- c(0, maxx)
  labels <- c(0, round(maxx,digits=0))
  axis(1, at=intervals, labels = labels, pos=0)
  # mtext("Bootstraped Confidence intervals for the mean of the MIER",at=(maxx/3),side=1,line=2, cex=0.8)
  #mtext("M--",at=maxx-1000,side=1,line=2)
  label_ci <- names(cis) ## 
  for (i in 1:num_cis) {
    a_ci <- unlist(cis[i])
    plot_ci_finalfig(c(a_ci[1],a_ci[2]),i, label_ci[i], points_in_cis[[i]]) #send the ci to plfotting
  }
  dev.off()
}

miersbymethodvector <- miersbymethod # TO DO , currently no changes needed
names_methods <- names(miersbymethodvector)
names_methods <- paste(names_methods,"_MIEratio",sep="")

setwd(dir_for_article)
cisbayeshastings <- lapply (miersbymethodvector, bayesCImethastings)
cisbayesjeffreys <- lapply (miersbymethodvector, bayesCIjeffreys)
cisbayesQuantileinterval <- lapply(miersbymethodvector, bayesCIquantile)
cisbayesHPDintervalsNoprior <- mapply(bayesCIhpdnoprior, miersbymethodvector, names_methods)
setwd(dir_to_read)

AllcisBayes <- cbind(t(as.data.frame(cisbayesQuantileinterval)), t(as.data.frame(cisbayesHPDintervalsNoprior)), 
                     t(as.data.frame(cisbayeshastings)), t(as.data.frame(cisbayesjeffreys)))

ci_upper_limit_v2 <- function(aci){
  ci <- unlist(aci)
  return(max(abs(ci[1]),abs(ci[2])))
}


### plot CIs
alphachar <- gsub ('\\.', '', alpha) # take care of . but not as a regular expression
namethisfile <- paste("AllFiles",newcode,"BayesHAST",alphachar,"mierCis.eps", sep="")
plotseveralcis(cisbayeshastings, namethisfile, miersbymethod)  

namethisfile <- paste("AllFiles",newcode,"BayesJEFF",alphachar,"mierCis.eps", sep="")
plotseveralcis(cisbayesjeffreys, namethisfile, miersbymethod)  


writelineMiers2file <- function(file_name){
  file_miers <- paste(file_name,"Miers.txt",sep="")
  #   write.table(errors, file=file_miers, 
  #               eol = "\n", sep = ",", col.names=FALSE, append=FALSE) #, header=FALSE) # change the header variable
}

setwd(dir_default_data)
file_namemiers <- "Miers_Methods.RData"
save(miersbymethodfull, file =file_namemiers)

load(file =file_namemiers, .GlobalEnv)
## This however annihilates all objects in .GlobalEnv with the same names !

##### PRINT OUT THE LATEX TABLE OF THE CONFIDENCE INTERVALS   AllcisBayes
# bayeslatex <- xtable(AllcisBayes)  

# join columns
bbayeslatex <- data.frame(matrix(NA, nrow = nrow(AllcisBayes), ncol = ncol(AllcisBayes)/2))
for (i in 1:(ncol(AllcisBayes)/2)){
    newcol <- rep(NA, nrow(AllcisBayes))
     for (j in 1:nrow(AllcisBayes)){
       newcol[j] <- paste(toString(round(AllcisBayes[j,2*i-1], digits = digitslatextable)), "-", toString(round(AllcisBayes[j,2*i], digits = digitslatextable)), sep="")      
     }
     bbayeslatex[,i] <- newcol
}

colnames(bbayeslatex) <- c("Qtle. 2.5\\%-97.5\\%", "HPD low-upper", "M-Hast. 2.5\\%-97.5\\%", "Jeffr. 2.5\\%-97.5\\%")
rownames(bbayeslatex) <- rownames(AllcisBayes)
labeltable <- paste("table:bayescis", alpha, sep="")
#digits(bayeslatex)[] <- digitslatextable

# remove last column
bbayeslatex <- bbayeslatex[,-ncol(bbayeslatex)]

bayeslatex <- xtable(bbayeslatex)
align(bayeslatex) <- "r|ccc"
label(bayeslatex) <- paste("table:bayescis", alpha, sep="")
caption(bayeslatex) <- paste("This table shows different probabilistic intervals for each one of the ", nrow(AllcisBayes),
                              " methods ($\\alpha=", alpha, "$). Scale is 0-$\\infty$. Lower values are better.", sep="")

bayestable.tex <- print.xtable(bayeslatex,
                               type = "latex",
                               print.results=FALSE,
                               booktabs=TRUE, 
                               include.rownames=TRUE,
#                                floating.environment='sidewaystable',
                               floating.environment='table',
                                align = "r|ccc",
#                                size = 'small',
                               scalebox = 1.0,
                               sanitize.text.function=function(x){x},
                               sanitize.colnames.function=function(x){x})
nametablebayes <- paste("AllIntervals", alpha, ".tex", sep="")

setwd(dir_for_tables)
write(bayestable.tex, file = nametablebayes, append=FALSE) 
setwd(dir_to_read)


#####  REPEATING THE SAME PROCESS OF THE MIERATIOS FOR THE STANDARD ACCURACY
#####  Start GENERATE THE CONFIDENCE INTERVALS FOR THE MIERATIO ##############
#subset the dataframe allfiles_df by method and apply the ci
# call my_ciboot
# first create 
print("--- Start generating the Confidence-CREDIBLE Intervals for the SA")

# SA_df will contain the Data of SA grouped by method -in the end-
SA_df <- as.data.frame(c(col_method, as.data.frame(as.numeric(allfiles_df$SA))))
colnames(SA_df)[2] <- "SA"
SAbymethodfull <- split(SA_df, SA_df$Method) # list of data.frames for each of the methods found. 
#these data.frames may have different number of observations -> so we remove data.frames with few observations 
n_observspermethod_inSA <- lapply(SAbymethodfull, function(x) {nrow(x[1])}) # list of number of observations per method
n_observspermethod_inSA <- unlist(n_observspermethod_inSA)
#### remove all ((miers))-->SA  with negative values. remove NAs and check the number of data points

SAvaluesbymethod <- lapply(SAbymethodfull, leaveonlyvalids)
# select for the bootstrap only those methods with a minimum number of observations. 

SAvaluesbymethod <- lapply(SAvaluesbymethod, leavewithpoints, min_number_of_points)
SAvaluesbymethod <- SAvaluesbymethod[!sapply(SAvaluesbymethod, is.null)]  # Remove method with null values

#SAvaluesbymethod is a list of methods: take the names of the list
namesmethods_SA <- names(SAvaluesbymethod)

listofboots_SA <- lapply(SAvaluesbymethod,  function(x){my_boot_geom_mean (x)})  # list of boot for each column of miers-method

#set the level of confidence to alpha, and then 

level_confidence <- 1 - alpha
SAscis_booted <- lapply(listofboots_SA, my_ciboot) # this creates the confs ints.  

level_confidence <- 1- 2*alpha  # back to the initial value

setwd(dir_default_data)
filename_cis_SAsratios <- filenamecisSAsratios
lapply(SAscis_booted, write, filename_cis_SAsratios, append=TRUE)  # write conf ints to a file
# writeLines(unlist(lapply(cis_booted, paste, collapse=" ")))
shortest_ci_SAs <- calc_min_ci_width(SAscis_booted) # minimum coef interval in WIDTH of all booted samples. best ci
#min_equiv_ci_index <- calc_equiv_ci(mierscis_booted)

##### Plot the Cis for the SAsratios ##### COPY FROM plot the MINIMUM cis in the final Figure ----
# plot the MIERATIOS cis in the final Figure ----
print(paste("--> plot SAsatios CIs of all files"))
# remove elements that are null (mier-ci not computed)
# null_values <- unlist(lapply (mierscis_booted, function(x){is.null(x[[1]])}))
# mierscis_booted <- mierscis_booted[!null_values]

num_cis_SA <- length(SAscis_booted)

maxx_SA <-  max(sapply(SAscis_booted, function(x) ci_upper_limit(unlist(x)))) #for plotting purposes
minx <- 0  # all positive values

file_name <- "AllFiles_SA"
setwd(dir_for_figures)
alphachar <- gsub ('\\.', '', alpha) # take care of . but not as a regular expression
#### find string for directories  in   newcode

## function already defined find_dir_name
indexresult_4SA <- which(sapply(list_of_code_dirs, find_dir_name,  dir_to_read) == TRUE)
newcode_4SA <- list_of_dirs_names[indexresult_4SA]

namethisfile_SA <- paste(getwd(),"/",file_name,newcode_4SA,alphachar,"SAs_Cis.eps", sep="")
heightpage <-  minfigureheight + num_cis_SA*0.36 #
setEPS()
postscript(file=namethisfile_SA, height=heightpage, width=figurewidth)     
par(mar=c(2,0,0,0)) # par(mar=c(3,0,0,0)) 
#par(oma=c(5,7,1,1))
# ymax <- 4+ 1.2 * num_cis
ymax <- heightpage # for test
plot(0,0,type="n", xlim=c(0, maxx_SA*1.10), ylim=c(0, ymax),axes=FALSE, ann=FALSE)
intervals <- c(0, maxx)
labels <- c(0, round(maxx_SA,digits=0))
axis(1, at=intervals, labels = labels, pos=0)
# mtext("Bootstraped Confidence intervals for the mean of the SA",at=(maxx/3),side=1,line=2, cex=0.8)
#mtext("M--",at=maxx-1000,side=1,line=2)
label_ci <- names(SAscis_booted) ## 
for (i in 1:num_cis_SA) {
  a_ci <- unlist(SAscis_booted[i])
  plot_ci_finalfig(c(a_ci[4],a_ci[5]),i, label_ci[i], SAscis_booted[[i]]) #send the ci to plfotting
}
dev.off()

SAs_bymethodvector <- SAvaluesbymethod # TO DO 
names_methods_SA <- names(SAvaluesbymethod)
names_methods_SA <- paste(names_methods_SA,"_SA",sep="")

setwd(dir_for_article)
cisbayeshastings_SA <- lapply (SAs_bymethodvector, bayesCImethastings)
cisbayesjeffreys_SA <- lapply (SAs_bymethodvector, bayesCIjeffreys)
cisbayesQuantileinterval_SA <- lapply(SAs_bymethodvector, bayesCIquantile)
cisbayesHPDintervalsNoprior_SA <- mapply(bayesCIhpdnoprior, SAs_bymethodvector, names_methods_SA)
setwd(dir_to_read)

AllcisBayes_SA <- cbind(t(as.data.frame(cisbayesQuantileinterval_SA)), t(as.data.frame(cisbayesHPDintervalsNoprior_SA)), 
                        t(as.data.frame(cisbayeshastings_SA)), t(as.data.frame(cisbayesjeffreys_SA)))


### plot CIs
alphachar <- gsub ('\\.', '', alpha) # take care of . but not as a regular expression
namethisfile_4SA <- paste("AllFiles_SA",newcode,"BayesHAST",alphachar,"SA_Cis.eps", sep="")
plotseveralcis(cisbayeshastings, namethisfile_4SA, SAvaluesbymethod)  

namethisfile_4SA <- paste("AllFiles_SA",newcode,"BayesJEFF",alphachar,"SA_Cis.eps", sep="")
plotseveralcis(cisbayesjeffreys, namethisfile_4SA, SAvaluesbymethod)  


writelineSAs2file <- function(file_name){
  file_SAs <- paste(file_name,"SAs.txt",sep="")
  #   write.table(errors, file=file_miers, 
  #               eol = "\n", sep = ",", col.names=FALSE, append=FALSE) #, header=FALSE) # change the header variable
}

setwd(dir_default_data)
file_nameSAs <- "SAs_Methods.RData"
save(SAbymethodfull, file =file_nameSAs)

load(file =file_nameSAs, .GlobalEnv)
## This however annihilates all objects in .GlobalEnv with the same names !

##### PRINT OUT THE LATEX TABLE OF THE CREDIBLE INTERVALS FOR SA  AllcisBayes_SA
#bayeslatex_SA <- xtable(AllcisBayes_SA)

bbayeslatexSA <- data.frame(matrix(NA, nrow = nrow(AllcisBayes_SA), ncol = ncol(AllcisBayes_SA)/2))
for (i in 1:(ncol(AllcisBayes_SA)/2)){
    newcol <- rep(NA, nrow(AllcisBayes_SA))
     for (j in 1:nrow(AllcisBayes_SA)){
       newcol[j] <- paste(toString(round(AllcisBayes_SA[j,2*i-1], digits = digitslatextable)), "-", toString(round(AllcisBayes_SA[j,2*i], digits = digitslatextable)), sep="")      
     }
     bbayeslatexSA[,i] <- newcol
}

colnames(bbayeslatexSA) <- c("Qtle. 2.5\\%-97.5\\%", "HPD low-upper", "M-Hast. 2.5\\%-97.5\\%", "Jeffr. 2.5\\%-97.5\\%")
rownames(bbayeslatexSA) <- rownames(AllcisBayes_SA)
labeltable_SA <- paste("table:bayescisSA", alpha, sep="")

# digits(bayeslatex_SA)[] <- digitslatextable

# remove last column
bbayeslatexSA <- bbayeslatexSA[,-ncol(bbayeslatexSA)]

bayeslatex_SA <- xtable(bbayeslatexSA)
align(bayeslatex_SA) <- "r|ccc"
label(bayeslatex_SA) <- paste("table:bayescisSA", alpha, sep="")
caption(bayeslatex_SA) <- paste("This table shows different credible intervals for each one of the ", 
                                nrow(AllcisBayes_SA),
                                " methods ($\\alpha=", alpha, "$). Scale is 0-1. Greater values are better.", sep="")

bayestableSA.tex <- print.xtable(bayeslatex_SA,
                                 type = "latex",
                                 print.results=FALSE,
                                 booktabs=TRUE, 
                                 include.rownames=TRUE,
                                 floating.environment='table',
                                align = "r|ccc",
 #                                size = 'small',
                                 scalebox = 1.0,
                                 sanitize.text.function=function(x){x},
                                 sanitize.colnames.function=function(x){x})
nametablebayes_SA <- paste("AllIntervalsSA", alpha, ".tex", sep="")
setwd(dir_for_tables)
write(bayestableSA.tex, file = nametablebayes_SA, append=FALSE) 
setwd(dir_to_read)

# setwd("/home/javier/Downloads/")
# install.packages("LaplacesDemon_13.03.04.tar.gz", lib="/home/javier/R/i686-pc-linux-gnu-library/3.2", repos=NULL)

