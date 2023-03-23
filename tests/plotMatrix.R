library(dplyr)
library(grid)
library(aakmisc)

png(filename="plotmatrix-%01d.png",res=100)

x <- data.frame(a=rexp(n=1000,rate=1/3),b=rnorm(1000))
mutate(x,c=a+b^2,d=a-b^3) -> x

plotMatrix(x,alpha=0.2)

plotMatrix(
           x[-2],
           labels=c(
             expression(alpha),
             expression(beta),
             expression(phi)
             ),
           alpha=0.3
           )

(g <- plotMatrix(x[1]))

g <- plotMatrix(as.list(x),alpha=0.2,breaks='scott')
print(g)

try(plotMatrix(numeric(10)))

print(g,vp=plotViewport(c(1,1,10,10)),newpage=T)
print(g,vp=viewport(x=0.8,y=0.8,width=0.4,height=0.4))

dev.off()
