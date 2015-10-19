# Test size
#
N <- matrix( c(4,5,6,7,8,9,10,11,20,21,500,520) , 6,2)
wt.size <- mc.test.size( wald.test , 0.1 , 20000 , N )
mwt.size <- mc.test.size( mann.whitney.test , 0.1 , 20000 , N )
plot( wt.size , t='l', col='darkred' , lwd=2 , ylim=c(0,0.20))
grid()
lines( mwt.size , t='l', col='darkblue' , lwd=2 )
lines( rep( 0.1 , nrow(N) ) , col='black' , lwd=1 )
legend( "topright", c('wald','mann-whitney') , col=c('darkred','darkblue') , lty=1 )

# Test power
