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
lines( spline_basis %*% knots, col="blue" )

# Get a principal curve

prc <- princurve::principal_curve( x )
lines( prc$s[ prc$ord, ], col="magenta" )

# Try again with more DoF

prc <- princurve::principal_curve( x, df=10 )
lines( prc$s[ prc$ord, ], col="purple" )


# Now use Slingshot

library( Slingshot )

sl <- slingshot( x )
lines( sl@metadata$curves$Lineage1$s, col="green" )


sl <- slingshot( x, df=10 )
lines( sl@metadata$curves$Lineage1$s, col="green" )



## ------- Branched data ----

# Make some branched data

set.seed( 13245768 )

knots1 <- cbind(
  c( 0.1, 1.0, 1.7, 2.5, 2.8, 2.4, 2.2, 3.2, 4.0, 5.2 ),
  c( 1.0, 1.2, 1.7, 1.9, 1.5, 1.1, 0.2, 0.6, 1.1, 1.2 ) )

knots2 <- cbind(
  c( 0.1, 1.0, 1.7, 2.5, 2.8, 2.4, 2.1, 2.5, 2.4, 1.9 ),
  c( 1.0, 1.2, 1.7, 1.9, 1.5, 1.1, 0.2, 0.0, -.3, -.9 ) )

spline_basis <- splines::bs( seq( 0, 1, l=1000), df=nrow(knots1), intercept=TRUE )

t <- runif( 1000 )
br <- as.integer( runif(1000) < .4 ) + 1

x0 <- abind::abind(
  splines::bs( t, df=nrow(knots1), intercept=TRUE ) %*% knots1 +
    matrix( rnorm( 2000, 0 , .1), ncol=2 ),
  splines::bs( t, df=nrow(knots2), intercept=TRUE ) %*% knots2 +
    matrix( rnorm( 2000, 0 , .1), ncol=2 ),
  along=3 )

x <- t( sapply( 1:length(t), function(i) x0[i,,br[i]] ) )
colnames(x) <- c( "V1", "V2" )
rm(x0)

plot( x, asp=1 )


# Do clustering

nn <- FNN::get.knn( x, 15 )
edge_table <-
  map_dfr( 2:ncol(nn$nn.index), function(i)
    tibble( 
      from = 1:nrow(nn$nn.index), 
      to = nn$nn.index[,i] ) )
nn_graph <- igraph::graph_from_edgelist( as.matrix(edge_table), directed=FALSE )
set.seed( 13245768 )
leiden <- igraph::cluster_leiden( nn_graph, objective_function="modularity" )

plot( x, col=leiden$membership, asp=1 )

# Slingshot with branches

sl <- slingshot( x, leiden$membership, df=10 )

# The MST
plot( sl@metadata$mst )

plot( x, col="gray", asp=1 )

for( n in names(sl@metadata$curves) )
  lines( sl@metadata$curves[[n]]$s, col="red" )

as_tibble(x) %>%
mutate( pt = sl@assays@data$pseudotime[,1] ) %>%
ggplot + geom_point( aes( x=V1, y=V2, col=pt ) ) +
  coord_equal() + scale_color_viridis_c()

as_tibble(x) %>%
mutate( pt = sl@assays@data$pseudotime[,2] ) %>%
ggplot + geom_point( aes( x=V1, y=V2, col=pt ) ) +
  coord_equal() + scale_color_viridis_c()
