source("http://www.haptonstahl.org/R/Decruft/Decruft.R")


irt.model <- "
// IRT 1PL (RASCH) MODEL

data {
int<lower=1> J; // number of students
int<lower=1> K; // number of questions
int<lower=1> N; // number of observations
int<lower=1,upper=J> jj[N]; // student for observation n
int<lower=1,upper=K> kk[N]; // question for observation n
int<lower=0,upper=1> y[N]; // correctness for observation n
}
parameters {
real delta; // mean student ability
real alpha[J]; // ability of student j - mean ability
real beta[K]; // difficulty of question k
}
model {
alpha ~ normal(0,1); // informative true prior
beta ~ normal(0,1); // informative true prior
delta ~ normal(.75,1); // informative true prior
for (n in 1:N)
y[n] ~ bernoulli_logit(alpha[jj[n]] - beta[kk[n]] + delta);
}
"

library('rstan');
NMax <- 1000000;
fit <- 0;
Nchains <- 1;
Niter <- 200;
Js <- c(10,100,1000,10000);
Ks <- c(10,100,1000);
times <- array(NA,c(length(Js),length(Ks)));
for (jidx in 1:length(Js)) {
  J <- Js[jidx];
  for (kidx in 1:length(Ks)) {
    K <- Ks[kidx];
    if (Js[jidx] * Ks[kidx] <= NMax) {
      
      
      inv_logit = function(u) { 1.0/(1.0 + exp(-u)); }
      
      J <- 400;
      K <- 100;
      alpha <- rnorm(J,0,1);
      beta <- rnorm(K,0,1);
      delta <- 0.75;
      
      y_all <- matrix(0,nrow=J,ncol=K);
      for (j in 1:J)
        for (k in 1:K)
          y_all[j,k] <- rbinom(1,1,inv_logit(alpha[j] - beta[k] + delta));
      
      p_observed = 0.75;
      observed <- matrix(rbinom(J*K,1,p_observed),nrow=J,ncol=K);
      N <- sum(observed);
      y <- rep(-1,N);
      jj <- rep(-1,N);
      kk <- rep(-1,N);
      n <- 1;
      for (j in 1:J) {
        for (k in 1:K) {
          if (observed[j,k]) {
            y[n] <- y_all[j,k];
            jj[n] = j;
            kk[n] = k;
            n <- n + 1;
          }
        }
      }
      
      
      dump(c("J","K","N","jj","kk","y"), "irt.data.R");
      
      if (jidx == 1 && kidx == 1) {
        fit <- stan(model_code=irt.model,
                    data=list(J=J,K=K,N=N,jj=jj,kk=kk,y=y),
                    init=0, seed = 23,
                    iter=2,
                    chains=1)
      }
      t_start <- proc.time()[3];
      print(paste("J=",J,", K=",K," N=",J*K,sep=""),quote=F);
      fit <- stan(model_code=irt.model,
                  data=list(J=J,K=K,N=N,jj=jj,kk=kk,y=y),
                  fit=fit,
                  # init=0,
                  iter=Niter,
                  chains=Nchains,
                  seed=23);
      t_end <- proc.time()[3];
      t_elapsed <- t_end - t_start;
      times[jidx,kidx] <- t_elapsed / Nchains / (Niter/2);
    }
  }
}

print("raters,items,N,sec/iter",quote=F);
for (jidx in 1:length(Js)) {
  for (kidx in 1:length(Ks)) {
    if (Js[jidx] * Ks[kidx] <= NMax) {
      print(paste(Js[jidx],Ks[kidx],Js[jidx]*Ks[kidx],times[jidx,kidx],
                  sep=","),
            quote=F);
    }
  }
}