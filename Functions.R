
# this function returns n preference profiles
# for the moment, we assume those preference profiles are strict
non_proxy_random <- function( n, m ){
    profiles_voters <- matrix(0, nrow = m, ncol = n);
    for( i in 1:n ){
        
        profiles_voters[,i] <- sample(m);
        
    }
    return(profiles_voters);
}


# this function generates m candidates profiles
# where the candidate ranks themselves top of list
proxy_random <- function( m )
{
    profiles <- matrix(0,nrow=m,ncol=m)
    for( i in 1:m )
    {
        profiles[1,i] = i;
        profiles[2:m,i] = sample(m-1);
        if( i<m )
        {
            for( j in 2:m )
            {
                if( profiles[j,i] >= i)
                {
                    profiles[j,i] <- profiles[j,i] + 1;
                }
            }
        }
    }
    return(profiles);
}


# for a given voters profile, count for each
# candidate how many times they were top choice
count_votes_top <- function( profiles ){
    cntr <- matrix(0,ncol=nrow(profiles),nrow=1);
    for( i in 1:ncol(profiles) )
    {
        cntr[1,profiles[1,i]] = cntr[1,profiles[1,i]] + 1;
    }
    return(cntr);
}


# for a given voters profile,
# proxify the profiles by replacing the orders
# by the order of the top choice
proxified_profiles <- function( p_voters, p_cand){
    p <- matrix(0,nrow = nrow(p_voters), ncol = ncol(p_voters));
    for( i in 1:ncol(p_voters) ){
        p[,i] <- p_cand[,p_voters[1,i]];
    }
    return(p);
}

# this function takes as input a set of preference orders
# and a matrix that contains the count of the occurence of each order
# that way, I can input a "normal" preference profile where each column
# is only counted once, and I can input a "weighted" profile

condorcet_winner_2 <- function( profiles, weights ){
    m <- nrow(profiles);
    n <- ncol(profiles);
    ranking <- m:1;
    count_votes <- matrix(0,m,m);
    boolean_votes <- matrix(0,m,m);
    total_votes <- sum(weights);
    condorcet_w <- 0;
    
#    print('from new');
    for( j in 1:(m-1) ){
        for( i in (j+1):m){
            indic_i <- profiles == i;
            indic_j <- profiles == j;
            rank_i <- t(ranking) %*% indic_i;
            rank_j <- t(ranking) %*% indic_j;
            count_votes[i,j] <- ((rank_j > rank_i)*1) %*% t(weights);
            count_votes[j,i] <- total_votes - count_votes[i,j];
#            boolean_votes[j,i] <- (count_votes[j,i]>total_votes/2)*1;
#            boolean_votes[i,j] <- (count_votes[i,j]>total_votes/2)*1;
        }
        boolean_votes[,j] <- (count_votes[,j] > total_votes/2)*1;
#        print(paste('candidate',j));
#        print(count_votes);
      
        temp <-sum(boolean_votes[,j]);
        if( temp == m-1 ){
          condorcet_w <- j;
          break;
        }
    }
    
    if( condorcet_w == 0 ){
      boolean_votes[,m] <- (count_votes[,m] > total_votes/2)*1;
      if( sum(boolean_votes[,m]) == (m-1) ){
      condorcet_w <- m;
      }
    }
#    print( "from NEW " );
#    print( boolean_votes );
#    print( count_votes );
    
    return(condorcet_w);
}


condorcet_winner <- function( profiles, weights ){
  m <- nrow(profiles);
  n <- ncol(profiles);
  ranking <- m:1;
  count_votes <- matrix(0,m,m);
  boolean_votes <- matrix(0,m,m);
  total_votes <- sum(weights);
  condorcet_w <- 0;
  
  
  
  for( i in 1:(m-1) ){
      for( j in (i+1):m ){
      indic_i <- profiles == i;
      indic_j <- profiles == j;
      rank_i <- t(ranking) %*% indic_i;
      rank_j <- t(ranking) %*% indic_j;
      count_votes[j,i] <- ((rank_i > rank_j)*1) %*% t(weights);
      count_votes[i,j] <- total_votes - count_votes[j,i];
      boolean_votes[j,i] <- (count_votes[j,i]>total_votes/2)*1;
      boolean_votes[i,j] <- (count_votes[i,j]>total_votes/2)*1;
    }
  }
    temp <- colSums(boolean_votes);
    for( i in 1:m ){
        if( temp[i] == (m-1) ){
            condorcet_w <- i;
        } 
    }
#    print( "from OLD");
#    print( boolean_votes );
#    print(count_votes);
  
  return(condorcet_w);
}

plurality_winner <- function(weights){
    maxim <- max(weights);
    cntr <- 0;
    m <- ncol(weights);
    for( i in 1:m){
        if( weights[i] == maxim ){
            cntr <- cntr+1;
        }
    }
    if( cntr > 1 ){
        return(0);
    }

    for( i in 1:m ){
        if( weights[i] == maxim ){
            return(i);
        }
    }

}

#stv_winner <- function( profiles, weights){
#    print("WEIGHTS");
#    print(weights);
#   m <- ncol(weights);
#    new_weights <-  matrix(0,ncol=m,nrow=1);
#    new_weights <- -weights;
#    for( i in 1:(m-1)){
#        minim <- min(new_weights);
#        for( j in 1:m ){
#            if(  new_weights[j]==minim ){
#                for( k in 2:m ){
#                   if( new_weights[profiles[k,j]]<0 ){
#                        new_weights[profiles[k,j]] <- new_weights[profiles[k,j]] + minim;
#                       new_weights[j] <- 0;
#                       break;
#                    }
#                    
#                    
#                }
#            }
#        }
#    }
#    print(-new_weights);
#    winner <- (-new_weights / sum(weights)) %*% 1:m;
#    return(winner);
#}


# this function generates a sample-size -sample of U([0,1]^power)
# correlation 0

unif_power <- function( sample_size, power){
    sample <- matrix( 0, nrow = power, ncol = sample_size );
    
    for( i in 1:power ){
      sample[i,] <- runif(sample_size);
    }
    return(sample);
}

# this function generate a sample-size -sample of N(mu,sigma)^power
# correlation 0

normal_power <- function( sample_size, power, mu, sigma ){
    sample <- matrix( 0, nrow = power, ncol = sample_size);
    for( i in 1:power ){
      sample[i,] <- rnorm(sample_size,mu,sigma);
    }
    return(sample);
}


#this function measures the distance between two 2-dimensional vectors

eucl_distance <- function( x1, x2 ){
    x_squared <- ( x1 - x2 ) * ( x1 - x2 );
    h <- sqrt(sum(x_squared));
    return(h);
}

#this function takes as input a matrix 2*m were each column is a 2-dim vector
#it returns an m*m matrix where matrix(i,j) is the distance between the points 
#i and j, which are stored in input in columns i and j.
#distance of a vector to itself is 0
eucl_dist_vector <- function( samples ){
    m <- ncol(samples);
    h <- matrix(0,m,m);
    for( j in 1:(m-1) ){
        for( k in (j+1):m ){
            h[j,k] <- eucl_distance(samples[,j], samples[,k]);
            h[k,j] <- h[j,k];
        }
    }
    return( h );
}



#this function takes as input a vector
#it outputs the minimum of this vector,
#excluding all zeros that are in v
#so if we have [1,2,0], the function will return 1
min_wo_zero <- function( v ){
    #print(v);
    miny <- max(v);
    m<- length(v);
    for( i in 1:m ){
        if(v[i]!=0 && v[i] < miny){
            miny = v[i];
        }
    }
    return(miny);
}

#this function will return the list of candidates that are closest
#to a candidate, in increasing order of distance
#it takes as input an m*1 matrix where the distances are listed
#it returns an m*1 matrix, where the numbers of the candidates
#are listed in increasing order of distance
#nb_cand is the number of the candidate whose list we are sorting

sort_dist <- function( dist){

    m <- length(dist);
    rank <- 1:m;
    listy <- matrix(0,m,1);
    list_un <- dist;
    
    #find the first element of the list
    #this is done separately, so candidates'and voters' lists can be handled
    #in one function
    
    alpha <- min(list_un);
    z <- rank %*% (list_un == alpha)*1 ;
    listy[1] <- z;
    list_un[z] <- 0;
    
    for( i in 2:m){
        
        alpha <- min_wo_zero(list_un);
        
        z <- rank %*% (list_un == alpha)*1 ;
        
        listy[i] <- z;
        list_un[z] <- 0;
    }
    return(listy);
}


dist_1_voter <- function( voter, cands ){
    m <- ncol(cands);
    dist <- matrix(0,m,1);
    for( i in 1:m ){
        dist[i,1] <- eucl_distance(voter, cands[,i]);
    }
    return(dist);
}



#input: the location matrix of the voters
# and the location matrix of the candidates
# returns an m * n matrix listing the distances
# of each voters to the candidates column-wise
dist_voters <- function( voters, cands ){
    n <- ncol(voters);
    m <- ncol(cands);
    dist <- matrix(0,m,n);
    for( i in 1:n ){
        dist[,i] <- dist_1_voter( voters[,i], cands );
    }
    return(dist);
}


find_lists <- function( dist ){
    n <- ncol( dist );
    m <- nrow( dist );
    listy <- matrix(0,m,n);
    for( i in 1:n ){
        if( m < 10 ){
          listy[,i] <- sort_dist(dist[,i]);
        }
        else{
          #update: improve the sorting algorithm
          listy[,i] <- sort(dist[,i],method='quick',decreasing = FALSE, index.return=TRUE)$ix;
        }
    }
    return( listy );
}