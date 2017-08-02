source("offline.period.R")
load("selectdata.rda")
offline.stage.func=function(par.offline,index){
  select.data.off=select.data%>%select(call_id,customer_id,gap)%>%mutate(gap=gap*(gap<=index)+(index+1)*(gap>index))
  
  set.seed(random.seed)
  zi.list=c(0,1,-1,2,-2)
  offline.df=c(zi.list[1],offline.df.compute(zi.list[1],par.offline))
  for(zi in zi.list[-1]){
    temp=c(zi,offline.df.compute(zi,par.offline))
    offline.df=rbind(offline.df,temp)
  }
  offline.df=data.frame(offline.df,row.names=1:length(zi.list))
  names(offline.df)=c("zi","chi0",1:index)
  
  customer.random=data.frame(customer_id=unique(select.data.off$customer_id))
  customer.random$zi=rep(zi.list,times=ceiling(dim(customer.random)[1]/length(zi.list)))[1:dim(customer.random)[1]]
  customer.random$zi.prob=dnorm(customer.random$zi,0,1)
  
  select.data.off=merge(merge(select.data.off,customer.random,by="customer_id"),offline.df,by="zi")
  chi0.df=unique(select.data.off[,c("customer_id","chi0")])
  save(chi0.df,file="chi0.rda")
  
  select.data.count=select.data.off%>%group_by(zi,gap)%>%summarize(count=n())
  select.data.off=merge(unique(select.data.off[,-c(2,3)]),select.data.count,by=c("zi","gap"))
  select.data.off=melt(select.data.off%>%select(-chi0),id.var=c("zi","zi.prob","gap","count"))
  names(select.data.off)[5:6]=c("off.periods","p.wait")
  
  select.data.off=select.data.off%>%group_by(zi,gap)%>%arrange(zi,gap,off.periods)
  
  part1=select.data.off%>%filter(gap>1&gap<=index)%>%summarize(Prob.offline=count[1]*log(prod(zi.prob[1],p.wait[1:(gap[1]-1)],(1-p.wait[gap[1]]))))
  part2=select.data.off%>%filter(gap==1)%>%summarize(Prob.offline=count[1]*log(prod(zi.prob[1],1-p.wait[gap[1]])))                                  
  part3=select.data.off%>%filter(gap>index)%>%summarize(Prob.offline=count[1]*log(prod(zi.prob[1],p.wait[1:(gap[1]-1)])))
  loglik.offline=sum(part1$Prob.offline,part2$Prob.offline,part3$Prob.offline)
  return(loglik.offline)
}
