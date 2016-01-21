library(boot)
library(xtable) # exporting data.frames to latex
library(bayesm)
library(mcmc)
library(coda)
library(LaplacesDemon)
library(mcmcplots)
# read the list of all dataframes (tables)

# dir_to_read <- "~/Dropbox/dhl/DataResults3KF"
# dir_default_data <- "~/DocProys/seminal/eht2015/data"
# dir_for_figures <- "~/DocProys/seminal/eht2015/article/figures"
# dir_for_tables <- "~/DocProys/seminal/eht2015/ese2015"
# dir_for_article <- "~/DocProys/seminal/eht2015/article"
dir_for_averages <- "~/DocProys/seminal/eht2015/article/averages" #dir for the averages of runs
setwd(dir_for_averages)

# 
file_name_alldata_rds <- "Tables4avergs.rds"
file_name_alldata_cvs <- "Tables4avergs.cvs"
file_name_run <- "TableDataResults3KF.txt"
file_name <- file_name_run
nrowsinltxtable <- 30
digitstablemeans <- 3
digitslatextable <- 3
mindatapoints4boot <- 5
alpha <- 0.05

figurewidth <- 5 #inches
heightdefault <- 5
minfigureheight <- 0.5 #
num_cis <- 7 # is the number of methods
heightpage <- minfigureheight + num_cis*0.5

setwd(dir_for_averages)

file_read <- read.table(file_name, sep=" ",header=TRUE, stringsAsFactors=FALSE, dec = ".") #read data

list_of_all_tables <- readRDS(file_name_alldata_rds)  

df <- do.call("rbind", list_of_all_tables)
# or df <- ldply(list_of_all_tables, data.frame)

#last column df[,ncol(df)]>=0
#next to last column  df[,ncol(df)-1]>=0

# remove rows with negative values
newdf <- subset(df, df[,ncol(df)]>=0 & df[,ncol(df)-1]>=0 )

results <- by(newdf[, 3:ncol(newdf)], newdf[,1:2], summary)
#results

fby1 <- factor( newdf[,1], exclude = "")
fby2 <- factor( newdf[,2], exclude = "")

resultsmean <- aggregate(x = newdf[, 3:ncol(newdf)], by = list(fby1, fby2), FUN = "mean")

# means ordered by last column (Mieratio)
resultsmean <- resultsmean[with(resultsmean, 
                              order(resultsmean[ncol(resultsmean)])), ] # sort by last column MIERatio

nrowsinltxtable <- min(nrowsinltxtable, nrow(resultsmean))
resultsmean4latex <- resultsmean[1:nrowsinltxtable, ]


# before converting the df  to latex, transform the SA column to string 
# make italics those cells in SA column that are not in descending order
# use digitslatextable 
SA <- resultsmean4latex[ncol(resultsmean)-1]
SAformatted <- ""
names(SAformatted) <- colnames(resultsmean)[ncol(resultsmean)-1] 

SAformatted[1] <- formatC(SA[1,], dig=digitstablemeans, format="f")
maxvalue <- SA[1,]
for (i in 2:(nrow(resultsmean4latex))) {
  if (maxvalue < SA[i,]) {
    SAformatted[i] <- paste("\\textit{", formatC(SA[i,], 
                                                 dig=digitstablemeans, format="f"), "}",
                            sep="")
  }
  else {
    SAformatted[i] <- formatC(SA[i,], dig=digitstablemeans, format="f")
    maxvalue <- SA[i,]
  }
}

### put the last column (MIEratio) in boldface
MIEmeansformatted <- sapply(resultsmean4latex[ncol(resultsmean4latex)],
                       function(x){
                         paste("\\textbf{",
                               formatC(x, 
                                       dig=digitstablemeans, format="f"),
                               "}", sep="")})

### rebuild the dataframe
means2latex <- cbind(resultsmean4latex[ ,1:2], resultsmean4latex[ ,5:8], 
                     SAformatted, 
                    MIEmeansformatted)
#                     data2latex[ ,ncol(data2latex)])

colnames(means2latex)[1] <- "Method"
colnames(means2latex)[2] <- "Fold"
colnames(means2latex)[7] <- "SA"

latex_table_means <- xtable(means2latex)  # convert R object to Latex
digits(latex_table_means)[] <- digitstablemeans

# set labels and captions to the table
labeltable <- paste("table:means",alpha,sep="")
label(latex_table_means) <- labeltable
caption(latex_table_means) <- paste("This table shows the means for the best ", nrowsinltxtable, " values
                                  in ascending order of MIEratio ($\\alpha=", alpha,"$).", sep="")

allmeans.tex <- print.xtable(latex_table_means, 
                             type = "latex",
                             print.results=FALSE, 
                             booktabs=TRUE, include.rownames=FALSE,
#                              floating.environment='sidewaystable',
                             floating.environment='table',
                             size = 'small', 
                             scalebox = 0.8,
                             sanitize.text.function=function(x){x},
                             sanitize.colnames.function=function(x){x})
nametable <- paste("AllMeans",alpha,".tex", sep="")
write(allmeans.tex, file=nametable, append=FALSE)

##### COMPUTE CREDIBLE INTERVALS
print("--- Start generating the Intervals for the MIERatio")

#select only two columns
lastcol <- ncol(resultsmean)
miermeans_df <- resultsmean[,c(1,lastcol)]
colnames(miermeans_df) <- c("Method", "MIERatio")

miersmeansbymethodfull <- split(miermeans_df, miermeans_df$Method) # list of data.frames for each of the methods found. 
#these data.frames may have different number of observations -> so we remove data.frames with few observations 
min_number_of_points <- mindatapoints4boot
n_observspermethod <- lapply(miersmeansbymethodfull, function(x) {nrow(x[1])}) # list of number of observations per method
n_observspermethod <- unlist(n_observspermethod)

####   remove all miers with negative values. remove NAs and check the number of data points
leaveonlyvalids <- function(x) {
  tmp <- as.vector(x[2]) #take the values of each method
  tmp <- tmp[!is.na(tmp)]
  tmp <- tmp [tmp > 0]
  return (tmp)
}

# NOT NEEDED HERE, but kept for keeping the transformation to vector
miersmeansbymethod <- lapply(miersmeansbymethodfull, leaveonlyvalids)


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

miersbymethodvector <- miersmeansbymethod # TO DO , currently no changes needed
names_methods <- names(miersbymethodvector)
names_methods <- paste(names_methods,"_MIEratioMean",sep="")

cisbayeshastingsmeans <- lapply (miersbymethodvector, bayesCImethastings)
cisbayesjeffreysmeans <- lapply (miersbymethodvector, bayesCIjeffreys)
cisbayesQuantileintervalmeans <- lapply(miersbymethodvector, bayesCIquantile)
cisbayesHPDintervalsNopriormeans <- mapply(bayesCIhpdnoprior, miersbymethodvector, names_methods)

AllcisBayesmeans <- cbind(t(as.data.frame(cisbayesQuantileintervalmeans)), t(as.data.frame(cisbayesHPDintervalsNopriormeans)), 
                     t(as.data.frame(cisbayeshastingsmeans)), t(as.data.frame(cisbayesjeffreysmeans)))

ci_upper_limit_v2 <- function(aci){
  ci <- unlist(aci)
  return(max(abs(ci[1]),abs(ci[2])))
}


##### PRINT OUT THE LATEX TABLE OF THE CONFIDENCE INTERVALS   AllcisBayesMeans
# bayeslatex <- xtable(AllcisBayes)  

# join columns
bbayeslatexmeans <- data.frame(matrix(NA, nrow = nrow(AllcisBayesmeans), ncol = ncol(AllcisBayesmeans)/2))
for (i in 1:(ncol(AllcisBayesmeans)/2)){
  newcol <- rep(NA, nrow(AllcisBayesmeans))
  for (j in 1:nrow(AllcisBayesmeans)){
    newcol[j] <- paste(toString(round(AllcisBayesmeans[j,2*i-1], digits = digitslatextable)), "-", toString(round(AllcisBayesmeans[j,2*i], digits = digitslatextable)), sep="")      
  }
  bbayeslatexmeans[,i] <- newcol
}

colnames(bbayeslatexmeans) <- c("Qtle. 2.5\\%-97.5\\%", "HPD low-upper", "M-Hast. 2.5\\%-97.5\\%", "Jeffr. 2.5\\%-97.5\\%")
rownames(bbayeslatexmeans) <- rownames(AllcisBayesmeans)
labeltable <- paste("table:bayescismeans", alpha, sep="")
#digits(bayeslatex)[] <- digitslatextable

# remove last column
bbayeslatexmeans <- bbayeslatexmeans[,-ncol(bbayeslatexmeans)]

bayeslatexm <- xtable(bbayeslatexmeans)
align(bayeslatexm) <- "r|ccc"
label(bayeslatexm) <- paste("table:bayescismeans", alpha, sep="")
caption(bayeslatexm) <- paste("This table shows different probabilistic intervals for each one of the ", nrow(AllcisBayesmeans),
                             " methods ($\\alpha=", alpha, "$) for the means of the MIEratios in 10 runs. Scale is 0-$\\infty$. Lower values are better.", sep="")

bayestablemn.tex <- print.xtable(bayeslatexm,
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
nametablebayes <- paste("AllIntervalsMeans", alpha, ".tex", sep="")


write(bayestablemn.tex, file = nametablebayes, append=FALSE) 
