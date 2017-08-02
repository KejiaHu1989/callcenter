load("calldattreated.rda")
cust.dat$service_quantile=0
for(agent in 2:4){
  for(cust in c("private","business")){
    select.row=which(cust.dat$answered_agent_type==agent&cust.dat$cust.type==cust&cust.dat$service_time>0)
    quant.store=quantile(cust.dat$service_time[select.row],seq(0,1,0.1))
    cust.dat$service_quantile[select.row]=findInterval(cust.dat$service_time[select.row],quant.store)
    select.row=which(cust.dat$answered_agent_type==agent&cust.dat$cust.type==cust&cust.dat$service_time==0)
    cust.dat$service_quantile[select.row]=5
  }
}
cust.dat$service_quantile[cust.dat$answered_agent_type==1]=5

cust.dat$wait_time=ceiling(cust.dat$waiting/ow.interval)
cust.dat$gap=ceiling(cust.dat$gap/of.interval)
cust.dat=cust.dat%>%select(call_id,customer_id,start,answered_agent_type,wait_time,service_quantile,gap,cust.type)
if(data.select=="business"){cust.dat=cust.dat%>%filter(cust.type=="business")}
if(data.select=="private"){cust.dat=cust.dat%>%filter(cust.type=="private")}
cut=quantile(cust.dat$wait_time,0.95)
select.data=cust.dat%>%filter(wait_time<cut)
save(select.data,file=paste0(data.select,"selectdata.rda"))
