offline.df.compute=function(zi,par.offline){
  mu.o=par.offline[1];sigma.o=par.offline[2];theta.o=par.offline[3]
  co.i=abs(mu.o+sigma.o*zi)
  chi=phi=xo.hi=rep(0,length=(gap.thresh/of.interval))
  phi[1]=-co.i+0;xo.hi[1]=exp(phi[1]/theta.o)
  
  for(i in 2:(gap.thresh/of.interval)){
    chi[i]=theta.o*log(xo.hi[i-1]+1)
    phi[i]=-co.i+chi[i]
    xo.hi[i]=exp(phi[i]/theta.o)
  }
  offline.df=data.frame(chi=rev(chi),phi=rev(phi),xo.hi=rev(xo.hi),zi)
  offline.df=offline.df%>%mutate(p.wait=xo.hi/(1+xo.hi),p.call=1-p.wait,off.periods=1:length(chi))
  return(c(offline.df$chi[1],offline.df$p.wait))
}