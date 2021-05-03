################################################################################
##################        Portfolio Functions
################################################################################

library('Sim.DiffProc')
library('dplyr')
library('tidyr')
library('quadprog')
library('ggplot2')
library('ggforce')

#*******************************************************************************
#  I .- Function to estimate returns

getReturns<-function(x){
  names<-c(names(x)[-1]);
  time<-x[-1,1];
  x<-as.matrix(x[,-1],ncol=ncol(x[,-1]),byrow=FALSE) 
  B<- matrix(ncol=ncol(x),nrow=nrow(x)-1);
  for(i in 1:ncol(x)){
    B[,i]<-diff(log(x[,i]),lag=1);    
  }
  B<-data.frame(B)     
  colnames(B)<-names
  C<-data.frame(Fecha=time,B)
  return(C)
}

#**************************************************************************
# II.- Portfolio optimization

getMinPortfolio<-function(r.e,mat.cov){
  Dmat <- 2*mat.cov   # matriz cuadrática
  dvec <- rep.int(0, length(r.e))     #rep.int(0, length(r.e)) # 
  Amat <- cbind(rep(1,length(r.e)),          # res1:  sum w_i = 1
                diag(1,length(r.e)),          # res2:  w_i > 0              
                -diag(1,length(r.e)))   # res2: w_i <= 0.35          - w_i >= 0.35
  bvec <- c(1,                      # res1                                 
            rep(0,length(r.e)),     # res2
            -rep(1,length(r.e)))  # res2
  
  resultado <- solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
  w.min <- round(resultado$solution, 4)
  
  names(w.min) <- names(r.e)
  port.min <- list("pesos" = w.min)
  return(port.min) 
}


#*************************************************************************
# III. Portfolio returns

getPortfolioReturns <-function(returns,weights){
  #w<- rep(weights,nrow(returns))
  #w.mat <- matrix(a,ncol=ncol(returns[,-1]),byrow=TRUE) 
  #r.port <- w.mat*base_roll[,-1]
  r.port <- NULL
  for(i in 1:nrow(returns)){
    r.port[i] <- sum(weights*returns[i,-1]) 
  }
  return(r.port)
}


#*******************************************************************************
#  IV. Selecting the top_n stocks

top_n <- function(returns,top=top.k){
  df_stat <<- returns %>% 
    gather(key='asset',value='valor',-Fecha) %>%
    group_by(asset) %>%
    summarise(n = n(), 
              mean = mean(valor), 
              sd = sd(valor),
              min = min(valor),
              max = max(valor),
              skeness = skewness(valor),
              kurtosis = kurtosis(valor)) %>%
    ungroup()%>%data.frame()%>%arrange(desc(sd))
  
  topn <- df_stat$asset[1:top]
  df  <- returns[,c('Fecha',topn)]
  return(df)
  
}

#*******************************************************************************
# V. Random weights

random_weights <- function(n_assets){
  weights <- c()
  for(i in 1:(n_assets-1)){
    if(i == 1){
      weights[i] <- sample(1000,1,replace = TRUE)
    } else{
      weights[i] <- sample((1000-sum(weights)),1,replace = TRUE)
    }
  }
  weights[n_assets] <- 1000 - sum(weights)
  return(sample(weights,n_assets,replace=FALSE)/1000)
} 

#*******************************************************************************
# VI.  parameter estimation

param_estimation <- function(X){
  X <-  as.ts(X) 
  ## drift 
  fx <- expression( theta[1]*x) 
  ## diffusion 
  gx <- expression( theta[2]*x)
  pmle <- "kessler"
  fitmodel <- fitsde(X,drift=fx,diffusion=gx,pmle=pmle,start = list(theta1=1,theta2=1))
  Coef <- data.frame(coef(fitmodel))
  Info <- rbind(logLik(fitmodel),
                AIC(fitmodel),
                BIC(fitmodel))
  colnames(Coef) <- c(pmle)
  rownames(Info) <- c("logLik","AIC","BIC")
  return(Coef$kessler)
}

#*******************************************************************************
#  VII. Variance and covariance matrix     

mat_cov <- function(base_roll,type='mv'){
  if (type == 'mv'){
    mat.cov <- cov(as.matrix(base_roll[,-1]))
  } else {
    t <- base_roll%>%nrow()
    names <- colnames(base_roll[,-1])
    e.r <- NULL
    sd.r <- NULL
    for(i in 1:length(names)){
      X <- base_roll[,i+1]
      est <- param_estimation(X)
      e.r[i] <- est[1]
      sd.r[i] <- est[2]
    }
    # expected value and risk 
    r.e_roll <- ((e.r - sd.r^2)/2)*t
    mat.cov_roll <- cov(base_roll[,-1])
    mat.cov_roll2 <- matrix(0,ncol=ncol(mat.cov_roll),nrow=nrow(mat.cov_roll))
    
    for(i in 1:ncol(mat.cov_roll)){
      for(j in 1:nrow(mat.cov_roll)){
        if(i != j){
          mat.cov_roll2[j,i] <- sd.r[j]*sd.r[i]*cor(base_roll[,j+1],base_roll[,i+1])
        } else{
          mat.cov_roll2[j,i] <- sd.r[i]^2
        }
        
      }
    }
    mat.cov <- mat.cov_roll2*t
  }
  return(mat.cov)
}

#*******************************************************************************
# VII. Function to match weights

all_weights <- function(base,pesos){
  mat.weights <- data.frame(matrix(0,ncol=length(base[,-1]),nrow=1))
  colnames(mat.weights) <- colnames(base[,-1])
  mat.weights[,names(pesos)] <- pesos
  return(mat.weights)
}


#*******************************************************************************
# VIII. Portfolio weights

getPortfolio <- function(base,year_to_start, rebalance_period=24,mod='mv',top.k=10){
  df_rend <- getReturns(base)
  # filter assets
  assets <- colnames(df_rend[,-1]) 
  df_rend2 <- df_rend%>%
    gather(key='asset',value='valor',-Fecha)%>%
    filter(asset %in% assets)%>%
    spread(asset,valor)%>%
    mutate(Fecha = as.Date(Fecha,"%d/%m/%Y"),
           year = format(Fecha, '%Y'),
           month = format(Fecha, '%m'),
           year_month_index=paste(year,month,sep='-'))
  # create sequence of periods
  initial.periods <- unique(df_rend2$year_month_index)[1:(length(unique(df_rend2$year_month_index))-which(unique(df_rend2$year_month_index)==paste(year_to_start,'01',sep='-'))+1)]
  final.periods <- unique(df_rend2$year_month_index)[which(unique(df_rend2$year_month_index)==paste(year_to_start,'01',sep='-')):length(unique(df_rend2$year_month_index))]
  start.period <-  initial.periods[seq(1,length(initial.periods),by=rebalance_period)]
  final.period <- final.periods[seq(1,length(final.periods),by=rebalance_period)]
  # initial values
  df_re_all_years <- NULL
  df_sd_all_years <- NULL
  df_min.ret.weights <- NULL
  df_eqw.ret.weights <- NULL
  df_ran.ret.weights <- NULL
  df_cum_return <- NULL
  
  for (i in 1:length(final.period)){
    date_min <- start.period[i]
    date_max <- final.period[i]
    # filter by max_date
    base_roll <- df_rend2 %>%
      filter(year_month_index >= date_min & year_month_index < date_max) %>%
      dplyr::select(-year,-month,-year_month_index)
    # selecting the best k stocks
    base_roll_top <- top_n(base_roll,top=top.k)
    # parameters
    mat.cov_roll2 <- mat_cov(base_roll_top,type=mod)
    sd_roll <- sqrt(diag(mat.cov_roll2))
    r.e_roll <- colMeans(base_roll_top[,-1])
    # get min porfolio
    port.min<-getMinPortfolio(r.e_roll,mat.cov_roll2) 
    # weights
    w_min <- port.min$pesos
    w_eqw <- rep(1/length(base_roll_top[,-1]),length(base_roll_top[,-1]))
    names(w_eqw) <- names(w_min)
    w_rand <- random_weights(ncol(base_roll_top[,-1]))
    names(w_rand) <- names(w_min)
    #  weights matched
    min.ret.weights <- all_weights(base=df_rend, w_min)
    eqw.ret.weights <- all_weights(base=df_rend, w_eqw)
    ran.ret.weights <- all_weights(base=df_rend, w_rand)
    # cummulative weights
    df_min.ret.weights <- rbind(df_min.ret.weights,min.ret.weights)
    df_eqw.ret.weights <- rbind(df_eqw.ret.weights,eqw.ret.weights)
    df_ran.ret.weights <- rbind(df_ran.ret.weights,ran.ret.weights)
    # total portfolio base
    if(i < length(final.period)){
      base_total_port <- df_rend2 %>%
        filter(year_month_index >= final.period[i] & year_month_index < final.period[i+1]) %>%
        dplyr::select(-year,-month,-year_month_index) 
    } else{
      base_total_port <- df_rend2 %>%
        filter(year_month_index >= final.period[i] & Fecha < max(Fecha)) %>%
        dplyr::select(-year,-month,-year_month_index) 
    }
    # df of total portfolio
    df_cum <- 
      data.frame('date' = base_total_port[,1],
                 'min.ret' = getPortfolioReturns(base_total_port,w_min),
                 'eqw.ret' = getPortfolioReturns(base_total_port,w_eqw),
                 'ran.ret' = getPortfolioReturns(base_total_port,w_rand))
    # cummulative total portfolio
    df_cum_return <- rbind(df_cum_return, df_cum)
    
    cat(paste('Estimated period : ', date_max,sep=''),"\n")
    
  }
  # df weights
  df_min_weights <- data.frame('year' = final.period, df_min.ret.weights)
  df_eqw_weights <- data.frame('year' = final.period, df_eqw.ret.weights)
  df_ran_weights <- data.frame('year' = final.period, df_ran.ret.weights)
  all <- list('df_min_weights' = df_min_weights,
              'df_eqw_weights' = df_eqw_weights,
              'df_ran_weights' = df_ran_weights,
              'df.port.ret' = df_cum_return)
  return(all)
  
}
################################################################################

