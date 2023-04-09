library(aakmisc)

set.seed(2015734481)

png(filename="misc-%01d.png",res=100)

x <- c(-13994,49994,0.00003459,1499595e12,0,1,2,-0.5,NA)

scinot(x,digits=3)
scinot(x,digits=1,simplify=TRUE)
scinot(x,digits=0)
scinot(x,digits=0,simplify=TRUE)
scinot(x,digits=3,format="latex")
scinot(x,digits=1,simplify=TRUE,format="latex")
scinot(x,digits=0,format="latex")
scinot(x,digits=0,simplify=TRUE,format="latex")
scinot(x,digits=3,format="math")
scinot(x,digits=1,simplify=TRUE,format="math")
scinot(x,digits=0,format="math")
scinot(x,digits=0,simplify=TRUE,format="math")

trnc(x)
trnc(x,range=c(-1,1))
trnc(x,range=c(0,2),only.finite=TRUE)
trnc(x,only.finite=TRUE)

library(ggplot2)

data.frame(
  x=1:1000,
  y=exp(cumsum(rnorm(1000,0,1)))
) |>
  ggplot(aes(x=x,y=y))+
  geom_line()+
  scale_y_log10(limits=c(2,20),oob=trnc)

data.frame(
  x=1:1000,
  y=exp(cumsum(rnorm(1000,0,1)))
) |>
  ggplot(aes(x=x,y=y))+
  geom_line()+
  scale_y_log10(labels=scinot)

data.frame(
  x=1:1000,
  y=exp(cumsum(rnorm(1000,0,1)))
) |>
  ggplot(aes(x=x,y=y))+
  geom_line()+
  scale_y_log10(labels=function(x)scinot(x,digits=0,simplify=T))

dev.off()
