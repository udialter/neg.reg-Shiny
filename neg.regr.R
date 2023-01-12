
regr <- function(data=NULL, formula=NULL, predictor=NULL, #input for full dataset
                    b = NULL, se=NULL, nop=NULL, n=NULL,     #input for no dataset
                    eil, eiu, alpha=.05, test="AH", std=FALSE,
                    bootstrap=TRUE, nboot=1000, 
                    plots = TRUE, saveplots = FALSE, seed=NA,...) # optional input for both
  {


  ################################  delta / MMES / Equivalence Interval ##########################
  if (is.null(eiu)) {
    stop("please enter the upper bound of the equivalence interval using the 'eiu' argument")}

  if (is.null(eil)) {
    stop("please enter the lower bound of the equivalence interval using the 'eil' argument")}

  if (sign(eiu) == sign(eil)) {
    stop("The equivalence interval must include zero")
  }

  if (eiu < 0 & eil > 0) {
    temp.eiu <- eil
    temp.eil <- eiu

    eiu <- temp.eiu
    eil <- temp.eil

  }

  l.delta <-  eil
  delta <- u.delta <- eiu


  ############################################ FULL DATA SECTION ####################
  if(!is.null(data) & is.null(formula)){
    stop("please enter the regression formula using the 'formula' argument")}
  if(is.null(data) & !is.null(formula)){
    stop("please enter the dataset using the 'data' argument")}

  if(!is.null(data) & !is.null(formula)){ # when dataset is entered
    withdata <- TRUE

    extract.formula.vars <- function(formula, data){
      var.dat <- match.call(expand.dots = FALSE)
      match <- match(c("formula", "data"),
                 names(var.dat), 0L)
      var.dat <- var.dat[c(1L, match)]
      var.dat$drop.unused.levels <- TRUE
      var.dat[[1L]] <- quote(stats::model.frame)
      var.dat <- eval(var.dat, parent.frame())
      var.dat <- data.frame(stats::na.omit(var.dat))
      return(var.dat)
    }

    data <- extract.formula.vars(formula,data)
    n  <- nrow(data) # determining the sample size
    model <- stats::lm(formula, data)
    model.results <- summary(model)

    # reg. coefficient point estimate and ci's
    #predictor <- deparse(substitute(predictor))
    if (predictor=="NULL") {
      stop("please enter the name of the predictor you are interested in testing using the 'predictor' argument")
    }
    if(!(predictor %in% colnames(data))){
      stop(predictor, " variable was not found in your data")
    }
    b.num <- grep(predictor, attr(model$terms , "term.labels"))+1 # this will indicate the place/number of the predictor in the model (e.g., 1st pred.) accounts for the intercept as well
    b <- model$coefficients[b.num][[1]] # extract raw coefficient value
    l.ci <- stats::confint(model, level = 1-alpha)[predictor,][[1]] # extract raw coefficient value, lower bound at 1-alpha
    u.ci <- stats::confint(model,level = 1-alpha)[predictor,][[2]] # extract raw coefficient value, upper bound at 1-alpha
    l.ci.2a <- stats::confint(model, level = 1-2*alpha)[predictor,][[1]] # extract raw coefficient value, lower bound 1 - 2*alpha for NHST decision
    u.ci.2a <- stats::confint(model, level = 1-2*alpha)[predictor,][[2]] # extract raw coefficient value, upper bound 1 - 2*alpha for NHST decision

    # std.error, degrees of freedom, and prerequisites
    se <- model.results$coefficients[b.num,2] # extract standard error for predictor
    df <- model$df.residual # extract degrees of freedom for model
    depname <- attr(model$terms, "variables")[[2]] # extract name of outcome variable

    # standardized forms (beta, variables, delta, ci's)
    std.data <- data.frame(scale(data))
    std.model <- stats::lm(formula, std.data)
    std.model.results <- summary(std.model)

    beta <- std.model$coefficients[b.num][[1]] # extract std. coefficient value
    beta.se <- std.model.results$coefficients[b.num,2] # extract standard error for predictor
    l.std.ci <- stats::confint(std.model, level = 1-alpha)[predictor,][[1]] # extract std. coefficient value, lower bound at 1-alpha
    u.std.ci <- stats::confint(std.model,level = 1-alpha)[predictor,][[2]] # extract std. coefficient value, upper bound at 1-alpha
    l.std.ci.2a <- stats::confint(std.model, level = 1-2*alpha)[predictor,][[1]] # extract std. coefficient value, lower bound 1 - 2*alpha for NHST decision
    u.std.ci.2a <- stats::confint(std.model, level = 1-2*alpha)[predictor,][[2]] # extract std. coefficient value, upper bound 1 - 2*alpha for NHST decision

    # BOOTSTRAPPING SECTION
    # creating an empty matrix to soon hold our results
      b.coef<-numeric(nboot)
      propdis.b <-numeric(nboot)

      beta.coef<-numeric(nboot)
      propdis.beta<-numeric(nboot)

      
      if(is.na(seed)){
        if (!exists(".Random.seed")) runif(1)
        seed <- sample(.Random.seed[1], size = 1)
      } else {
        seed <- seed
        }
      set.seed(seed)
      for (i in 1:nboot) {
        temp.data <- dplyr::sample_n(data,n , replace = TRUE)
        temp.std.data <- dplyr::sample_n(std.data,n , replace = TRUE)
        temp.model <- stats::lm(formula, data = temp.data)
        temp.std.model <- stats::lm(formula, data = temp.std.data)
        # regression coefficients
        b.coef[i]<-temp.model$coefficients[b.num][[1]]
        beta.coef[i]<- temp.std.model$coefficients[b.num][[1]]
        # Proportional Distance
        ifelse(sign(b.coef[i])==sign(eiu), temp.EIc<-eiu, temp.EIc<-eil)
        propdis.b[i] <- b.coef[i]/abs(temp.EIc)
        propdis.beta[i] <- beta.coef[i]/abs(temp.EIc)
      } # end of bootstrapping For loop

      # Proportional Distance and confidence intervals for PD
      ifelse(sign(b)==sign(eiu), EIc<-eiu, EIc<-eil)
      # unstandardized
      pd <- b/abs(EIc)
      pd.se <- stats::sd(propdis.b)
      pd.u.ci <- stats::quantile(propdis.b,1-alpha/2)
      pd.l.ci <- stats::quantile(propdis.b,alpha/2)
      pd.u.ci.2a <- stats::quantile(propdis.b,1-alpha)
      pd.l.ci.2a <- stats::quantile(propdis.b,alpha)
      # standardized
      PD <- beta/abs(EIc)
      PD.se <- stats::sd(propdis.beta)
      PD.u.ci <- stats::quantile(propdis.beta,1-alpha/2)
      PD.l.ci <- stats::quantile(propdis.beta,alpha/2)
      PD.u.ci.2a <- stats::quantile(propdis.beta,1-alpha)
      PD.l.ci.2a <- stats::quantile(propdis.beta,alpha)

      if (bootstrap==TRUE){
      u.ci <- stats::quantile(b.coef,1-alpha/2)
      l.ci <- stats::quantile(b.coef,alpha/2)
      u.ci.2a <- stats::quantile(b.coef,1-alpha)
      l.ci.2a <- stats::quantile(b.coef,alpha)


      u.std.ci <- stats::quantile(beta.coef,1-alpha/2)
      l.std.ci <- stats::quantile(beta.coef,alpha/2)
      u.std.ci.2a <- stats::quantile(beta.coef,1-alpha)
      l.std.ci.2a <- stats::quantile(beta.coef,alpha)

    } # end of bootstrapping section

    if (std==TRUE) {
      b   <- beta
      se <- beta.se
      u.ci <- u.std.ci
      l.ci <- l.std.ci
      u.ci.2a <- u.std.ci.2a
      l.ci.2a <- l.std.ci.2a
      pd <- PD
      pd.u.ci <- PD.u.ci
      pd.l.ci <- PD.l.ci
      pd.u.ci.2a <-  PD.u.ci.2a
      pd.l.ci.2a <- PD.l.ci.2a
    } # end of std = T selection

      } # end of full data sections

 ############################################ NO DATA SECTION ####################

  if(is.null(data) & is.null(formula)){
   if(is.null(b)){
     stop("please specifiy the regression coefficient effect size using the 'b' argument")}
   if(is.null(se)){
     stop("please specifiy the standard error associated with regression coefficient using the 'se' argument")}
   if(is.null(nop)){
     stop("please specifiy the number of predictors in the model using the 'nop' argument")}
   if (is.null(n)){
     stop("please specifiy the sample size using the 'n' argument")}


  if(!is.null(b) & !is.null(se)& !is.null(nop)& !is.null(n)){ # NO FULL DATA
    withdata <- FALSE
    df <- n-1-nop
    l.ci <- b+stats::qt(alpha/2,df, lower.tail = T)*se
    u.ci <- b+stats::qt(alpha/2,df, lower.tail = F)*se
    l.ci.2a <- b+stats::qt(alpha,df, lower.tail = T)*se
    u.ci.2a <- b+stats::qt(alpha,df, lower.tail = F)*se
    predictor <- "predictor of interest"

    # PD
    ifelse(sign(b)==sign(eiu), EIc<-eiu, EIc<-eil)
    pd <- b/abs(EIc)
    pd.u.ci <- u.ci/abs(eiu) # because u.ci will be positive and l.ci will be negative
    pd.l.ci <- l.ci/abs(eil)
    pd.u.ci.2a <-  u.ci.2a/abs(eiu)
    pd.l.ci.2a <- l.ci.2a/abs(eil)

  }

 }# end of no data sections

############################## EQUIVALENCE TESTING SECTION #####################

  title <- "Evaluating Negligible Effects Between Predictor and Outcome in Multiple Regression"
  ifelse(std==F,effect <-  "Unstandardized", effect <-  "Standardized")
  ifelse(std==F,  symb <- "b", symb <- "\u03B2")


  # Anderson-Hauck (AH)
  if (test=="AH") {
    subtitle  <- "Anderson-Hauck (AH) procedure:"
    t.value <- (b - (l.delta+u.delta)/2)/se
    H.A.del <- ((u.delta-l.delta)/2)/se #this is the delta as defined in Hauck and Anderson (1986)
    p.value <- stats::pt(abs(t.value)-H.A.del,df) - stats::pt(-abs(t.value)-H.A.del, df)

    ifelse(p.value < alpha, decision <- 'The null hypothesis that the regression coefficient is non-negligible can be rejected. A negligible effect is concluded. Be sure to interpret the magnitude (and precision) of the effect size.',
           decision <-'The null hypothesis that the regression coefficient is non-negligible cannot be rejected. There is insufficient evidence to conclude a negligible effect. Be sure to interpret the magnitude (and precision) of the effect size.')
  }

  # Two One-Sided Tests (TOST)
  if (test=="TOST") {
    subtitle  <- "Two One-Sided Tests (TOST) Procedure:"
    t.value.1 <- (b - l.delta)/se
    t.value.2 <- (b-u.delta)/se
    p.value.1 <-stats::pt(t.value.1, df, lower.tail=FALSE)
    p.value.2 <-stats::pt(t.value.2, df, lower.tail=TRUE)

    ifelse(abs(t.value.1) <= abs(t.value.2), t.value <- t.value.1, t.value <- t.value.2) # finding the smaller t to present
    ifelse(p.value.1 >= p.value.2, p.value <- p.value.1, p.value <- p.value.2) # finding the larger p to present

    ifelse(p.value.1 < alpha & p.value.2 < alpha, decision <- 'The null hypothesis that the regression coefficient is non-negligible can be rejected. A negligible effect is concluded. The null hypothesis that the direct effect (difference between the total and indirect effect) is non-negligible can be rejected. Substantial Mediation CAN be concluded. Be sure to interpret the magnitude (and precision) of the effect size.',
           decision <-'The null hypothesis that the regression coefficient is non-negligible cannot be rejected. There is insufficient evidence to conclude a negligible effect. The null hypothesis that the direct effect (difference between the total and indirect effect) is non-negligible can be rejected. Substantial Mediation CAN be concluded. Be sure to interpret the magnitude (and precision) of the effect size.')
  }


########## RESULTS ############

  ret <- data.frame(  title = title,
                      subtitle = subtitle,
                      alpha = alpha,
                      formula = deparse(formula, width.cutoff = 500),
                      effect = effect,
                      symb = symb,
                      bootstrap = bootstrap,
                      nboot = nboot,
                      test = test,
                      t.value = t.value,
                      df = df,
                      n = n,
                      p.value = p.value,
                      decision = decision,
                      eiu = round(eiu,3),
                      eil = round(eil,3),
                      b = b,
                      se = se,
                      u.ci = u.ci,
                      l.ci = l.ci,
                      u.ci.2a = u.ci.2a,
                      l.ci.2a = l.ci.2a,
                      pd = pd,
                      EIc = EIc,
                      pd.u.ci = pd.u.ci,
                      pd.l.ci = pd.l.ci,
                      pd.u.ci.2a = pd.u.ci.2a,
                      pd.l.ci.2a = pd.l.ci.2a,
                      std = std,
                      predictor = predictor,
                      perc.a = (1-alpha)*100,
                      perc.2a = (1-2*alpha)*100,
                      plots = plots,
                      saveplots=saveplots,
                      seed = seed,
                      oe="Regression Coefficient",
                      withdata = withdata,
                      outcome = deparse(substitute(depname)))
  class(ret) <- "regr"
return(ret)
} # end of entire function

#' @rdname regr
#' @param x object of class \code{regr}
#' @param ... extra arguments
#' @return
#' @export
#'
print.regr <- function(x,...) {
  ifelse(round(x$p.value,3) == 0, p.val <- " < 0.001", p.val <- paste(" = ", round(x$p.value,3), sep = ""))
  cat("\n\n")
  cat("***",x$title, "***\n\n")
  
  if (x$bootstrap == TRUE & x$withdata == TRUE) {
    cat(x$effect, " regression coefficient for ", x$predictor," and confidence interval using ", x$nboot," bootstrap iterations (random seed = ",x$seed,"):","\n",
        x$symb, " = ", round(x$b,3), ", ",x$perc.a, "% CI [",round(x$l.ci,3),", ",round(x$u.ci,3),"]" ,"\n",
        "std. error = ", round(x$se,3),  sep = "")

  } else {
  cat(x$effect, " regression coefficient for ", x$predictor,":", "\n",
      x$symb, " = ", round(x$b,3), ", ",x$perc.a, "% CI [",round(x$l.ci,3),", ",round(x$u.ci,3),"]" ,"\n",
      "std. error = ", round(x$se,3), sep = "")
  }
  
  cat("\n\n**********************\n\n")
  cat(x$subtitle, "\n\n")
  cat("Equivalence interval: ","lower= ", x$eil, ", ", "upper= ", x$eiu, "\n", sep = "")

  if(x$test == "TOST"){
    
  cat("t(",x$df,") = ",round(x$t.value,3)," (smallest magnitude t value out of t1/t2)","\n","p",p.val,"\n", sep = "")
  cat("NHST decision: ", x$decision,"\n", sep = "")
  } 
  if (x$test=="AH") {
    cat("Anderson-Hauck T statistic = ",round(x$t.value,3),"\n","p",p.val,"\n", sep = "")
    cat("NHST decision: ", x$decision,"\n", sep = "")
  }
  
  if (x$test == "AH" & x$plots == TRUE){
    cat("\n*Note that NHST decisions using the AH procedure may not match TOST NHST results or the Symmetric CI Approach at 100*(1-2\u03B1)% illustrated in the plots. \n")
  }
  cat("\n**********************\n\n")
  cat("Proportional Distance","\n\n")
  cat("Proportional distance:", round(x$pd,3),"\n")
  cat(x$perc.a, "% confidence interval for the proportional distance: (",round(x$pd.l.ci,3), ", ",round(x$pd.u.ci,3),")","\n\n",sep="")
  cat("*Note that the confidence interval for the proportional distance may not be precise with small sample sizes","\n")
  cat("*******************", "\n\n")


if (x$plots == TRUE) {
  plot(NA, axes=F,
       xlim = c(min(x$l.ci,x$eil)-max(x$u.ci-x$l.ci, x$eiu-x$eil)/5, max(x$u.ci,x$eiu)+max(x$u.ci-x$l.ci, x$eiu-x$eil)/5),
       ylim = c(0,1),
       yaxt='n',
       ylab="",
       xlab = "Regression Coefficient Estimate",
       main = "")
  graphics::title(main = expression(paste("Symmetric CI Approach at 100*(1-2"*alpha, ")%")))
  graphics::abline(v =0 , lty = 2, col= "light grey") # vertical line in the middle of the eq. interval, right now it's always 0, can be changed
  graphics::points(x=x$b, y=.3, pch=8, cex=2) # point at the estimated predictor value
  graphics::abline(v=x$eiu, lty=2, col = "red") # mark the upper eq. bound
  graphics::abline(v=x$eil, lty=2, col = "red") # mark the lower eq. bound
  graphics::segments(x$l.ci.2a,0.3,x$u.ci.2a,0.3, lwd=3) # plotting the 90% CI for the predictor estimate
  graphics::text(x$eiu+0.01,.7,labels=paste("upper negligible effect bound", sep = ""),srt=270,pos=3, offset = 0, col = "red", cex=0.7) # text for eq. bound line (upper)
  graphics::text(x$eil-0.01,.7,labels=paste("lower negligible effect bound", sep = "") ,srt=90,pos=3, offset = 0, col = "red", cex=0.7) # text for eq. bound line (lower)
  graphics::text(x$eiu+0.02,0,round(x$eiu,2),srt=0,pos=3, offset = .15, col = "red") # writing the eq. interval bound value (upper)
  graphics::text(x$eil-0.02,0,round(x$eil,2),srt=0,pos=3, offset = .15, col = "red") # writing the eq. interval bound value (lower)
  graphics::text(x$b,.37,round(x$b, digits = 2),srt=0) # writing the predictor point estimate value
  graphics::text(x= x$b,y=.36, labels= x$symb, srt=1, pos = 2, offset = 2.3)# adding text to indicate above line
  graphics::text(x= x$b,y=.36, labels="  =", srt=1, pos = 2, offset = 1.5) # adding text to indicate above line
  graphics::text(x$u.ci.2a,.27,round(x$u.ci.2a, digits = 2),pos =4, offset = .01, col = "black") # writing the 90% CI upper limit value for the predictor estimate
  graphics::text(x$l.ci.2a,.27,round(x$l.ci.2a, digits = 2), pos =2, offset = .01,col = "black") # writing the 90% CI lower limit value for the predictor estimate
  graphics::axis(side=1, pos=0, lwd.ticks=0)
  if (!(x$saveplots == FALSE)) {
    if (x$saveplots == "png"){
      grDevices::dev.copy(grDevices::png,paste("regr_plot_seed_",x$seed,".png")) }
    if (x$saveplots == "jpeg"){
      grDevices::dev.copy(grDevices::jpeg,paste("regr_plot_seed_",x$seed,".jpeg")) }
  }

  #grDevices::dev.off()
  
 }
}

