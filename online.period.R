online.df.compute=function(comb.set,par.online,max.ow.periods,pw.estimate){
  chi0=comb.set[1];zwi=comb.set[2];Tik=comb.set[3]
  mu.w=par.online[1];sigma.w=par.online[2];rate.bb=par.online[3];rate.ta=par.online[4];theta.w=par.online[5]
  pnew.cc=par.online[6];pnew.bb=par.online[7];
  cw.i=abs(mu.w+sigma.w*zwi)
  vhi=pshi=xw.hi=rep(chi0,length=max.ow.periods)
  Reward=c(1,rate.bb,rate.ta)*Tik+c(pnew.cc*chi0,pnew.bb*chi0,0)
  vhi[1]=theta.w*log(1)
  pshi[1]=-cw.i+sum(pw.estimate[max.ow.periods,-1]*Reward)+(1-(sum(pw.estimate[max.ow.periods,-1])))*vhi[1]
  xw.hi[1]=exp(pshi[1]/theta.w)

  for(i in 2:max.ow.periods){
    vhi[i]=theta.w*log(1+xw.hi[i-1])
    pshi[i]=-cw.i+sum(pw.estimate[max.ow.periods-i+1,-1]*Reward)+(1-(sum(pw.estimate[max.ow.periods-i+1,-1])))*vhi[i]
    xw.hi[i]=exp(pshi[i]/theta.w)
  }
  
  online.df=data.frame(rev(vhi),rev(pshi),xw.hi=rev(xw.hi))
  online.df=online.df%>%mutate(p.wait=xw.hi/(1+xw.hi),online.periods=1:max.ow.periods)
  return(online.df$p.wait)
}
