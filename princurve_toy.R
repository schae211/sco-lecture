## Tries for principle curve

# Let's first make a nice curve

library( splines )

knots <- cbind(
  x = c( 0.1, 1.0, 1.7, 2.5, 2.8, 2.4, 2.2, 3.2, 4.0, 5.2 ),
  y = c( 1.0, 1.2, 1.7, 1.9, 1.5, 1.1, 0.2, 0.6, 1.1, 1.2 ) )

plot( knots, type="b" )

spline_basis <- bs( seq( 0, 1, l=1000), df=nrow(knots), intercept=TRUE )

plot( spline_basis %*% knots, type="l" )
points( knots, col="red" )

x <- bs( runif(1000), df=nrow(knots), intercept=TRUE ) %*% knots +
  matrix( rnorm( 2000, 0 , .1), ncol=2 )
plot( x, col="gray" )

prc <- princurve::principal_curve( x, df=10 )
lines( prc$s[ prc$ord, ], col="magenta" )

points( x[1:5,], col="blue" )
xx <- princurve::project_to_curve( x[1:5,], prc$s, stretch=0 )
points( xx$s, col="orange" )
