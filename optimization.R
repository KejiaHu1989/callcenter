optimize.func=function(par.est){
  mu.w=exp(par.est[1]);sigma.w=exp(par.est[2]);rate.bb=exp(par.est[3]);rate.ta=exp(par.est[4]);
  theta.w=exp(par.est[5]);mu.o=exp(par.est[6]);sigma.o=exp(par.est[7]);theta.o=exp(par.est[8]);
  pnew.cc=exp(-par.est[9]^2);pnew.bb=exp(-par.est[10]^2)

  #select seed
  source("offline.stage.func.R")
  loglik.offline=offline.stage.func(par.offline =c(mu.o,sigma.o,theta.o),index)

  #select seed
  source("online.stage.func.R")
  loglik.online=online.stage.func(par.online =c(mu.w,sigma.w,rate.bb,rate.ta,theta.w,pnew.cc,pnew.bb))
  
  loglik=sum(loglik.online,loglik.offline)
  loglik
}

