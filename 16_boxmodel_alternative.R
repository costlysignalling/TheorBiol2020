## R code 16.4
library(rethinking)
data(Boxes)
precis(Boxes)

# prep data
dat_list <- list(
  N = nrow(Boxes),
  y = Boxes$y,
  majority_first = Boxes$majority_first )

boxcode<-"data{
    int N;
    int y[N];
    int majority_first[N];
}
parameters{
    simplex[5] p;
}
model{
    vector[5] phi;

    p ~ dirichlet( rep_vector(4,5) );
    
    // probability of data if given strategy (Pr(y_i|phi[i] ))
    // follow majority, follow minority, meverick, random, follow first, 
    for ( i in 1:N ) {
            if ( majority_first[i]==1 ){
                if ( y[i]==1 ) {phi[1]=0; phi[2]=0; phi[3]=1; phi[4]=1.0/3.0; phi[5]=0;}
                if ( y[i]==2 ) {phi[1]=1; phi[2]=0; phi[3]=0; phi[4]=1.0/3.0; phi[5]=1;}
                if ( y[i]==3 ) {phi[1]=0; phi[2]=1; phi[3]=0; phi[4]=1.0/3.0; phi[5]=0;}
            }
            if ( majority_first[i]==0 ){
                if ( y[i]==1 ) {phi[1]=0; phi[2]=0; phi[3]=1; phi[4]=1.0/3.0; phi[5]=0;}
                if ( y[i]==2 ) {phi[1]=1; phi[2]=0; phi[3]=0; phi[4]=1.0/3.0; phi[5]=0;}
                if ( y[i]==3 ) {phi[1]=0; phi[2]=1; phi[3]=0; phi[4]=1.0/3.0; phi[5]=1;}
            }

        for ( j in 1:5 ) phi[j] = p[j]*phi[j];
        target += log(sum(phi));
    }
}"

m16.my <- stan( model_code=boxcode , data=dat_list , chains=3 , cores=3 )


# show marginal posterior for p
p_labels <- c("1 Majority","2 Minority","3 Maverick","4 Random",
              "5 Follow First")

plot( precis(m16.my,2) , labels=p_labels )

