load("pwestimate.rda")
load("selectdata.rda")
load("chi0.rda")
source("online.period.R")

online.stage.func=function(par.online){
  max.ow.periods=max(select.data$wait_time)
  pw.estimate=(pw.estimate%>%dcast(wait_time~answered_agent_type))[1:max.ow.periods,]
  
  select.data.online=select.data%>%select(call_id,customer_id,wait_time,service_quantile)
  select.data.online=merge(select.data.online,chi0.df,by="customer_id")
  
  set.seed(random.seed2)
  zwi.list=  zi.list=c(0,1,-1,2,-2)
  
  customer.random=data.frame(customer_id=unique(select.data.online$customer_id))
  customer.random$zwi=rep(zwi.list,times=ceiling(dim(customer.random)[1]/length(zwi.list)))[1:dim(customer.random)[1]]
  customer.random$zwi.prob=dnorm(customer.random$zwi,0,1)
  
  select.data.online=merge(select.data.online,customer.random,by="customer_id")
  
  online.comb=unique(select.data.online%>%select(chi0,zwi,zwi.prob,service_quantile))
  online.df=c(as.numeric(online.comb[1,]),online.df.compute(as.numeric(online.comb[1,-3]),par.online,max.ow.periods,pw.estimate))
  
  for(i in 2:dim(online.comb)[1]){
    temp=c(as.numeric(online.comb[i,]),online.df.compute(as.numeric(online.comb[i,-3]),par.online,max.ow.periods,pw.estimate))
    online.df=rbind(online.df,temp)
  }
  
  online.df=data.frame(online.df,row.names=1:dim(online.comb)[1])
  names(online.df)=c(names(online.comb),1:max.ow.periods)
  
  select.data.online=select.data.online%>%group_by(wait_time,chi0,zwi,service_quantile)%>%summarize(count=n())
  select.data.online=merge(select.data.online,online.df,by=c("chi0","zwi","service_quantile"))
  select.data.online=melt(select.data.online,id.var=names(select.data.online)[1:6])
  names(select.data.online)[7:8]=c("on.periods","p.wait")
  select.data.online=select.data.online%>%group_by(chi0,zwi,service_quantile,wait_time,count,zwi.prob)%>%arrange(chi0,zwi,service_quantile,wait_time,count,zwi.prob)
  
  select.data.online$wait_time[select.data.online$wait_time==0]=1
  part1=select.data.online%>%filter(wait_time>1)%>%summarize(Prob.online=count[1]*log(prod(zwi.prob[1],p.wait[1:(wait_time[1]-1)],(1-p.wait[wait_time[1]]))))
  part2=select.data.online%>%filter(wait_time==1)%>%summarize(Prob.online=count[1]*log(prod(zwi.prob[1],1-p.wait[wait_time[1]])))                                  
  
  loglik.online=sum(part1$Prob.online,part2$Prob.online)
  loglik.online
}
