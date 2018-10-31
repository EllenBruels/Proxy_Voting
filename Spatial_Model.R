
run_simulations_unif <- function( seed_0, NB_SIM, DIM, filename, folder, 
                                  MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
                                  MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS)
{  
  set.seed( seed_0 );
  cntr <- 0;
  file_name <- paste( folder, filename, '.txt');
  
  col_n <- c('time','iteration','sub_iteration','nb_voters', 'nb_cands', 
             'plurality', 'cond_wo_proxy', 'cond_proxy');
  winners <- data.frame( matrix( ncol = 9, nrow = 0 ));
  colnames(winners) <- c('index',col_n);
  write.table( winners, file_name, sep='\t');
  
  
  for( m in seq(MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS)){
    for( n in seq(MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS)) {  
      
      print(Sys.time());          
      print('n == ');
      print(n);
      print('m == ');
      print(m);
      
      winners <- data.frame( matrix( ncol = 8, nrow = NB_SIM ));
      sum_time <- Sys.time();
      colnames(winners) <- col_n;
      
      for( i in 1:NB_SIM ){
        cntr <- cntr + 1;
        
        sample_voters <- unif_power(n, DIM);
        sample_cands <- unif_power(m, DIM);
        
        dist_cands <- eucl_dist_vector( sample_cands );
        ranks_cand <- find_lists( dist_cands );
        
        
        dist_voter <- dist_voters( sample_voters, sample_cands );
        ranks_voters <- find_lists( dist_voter );
        
        
        plurality <- count_votes_top( ranks_voters );
        normal_weights <- matrix(1,1,n);
        
        plurality_w <- plurality_winner( plurality );
        cond <- condorcet_winner_2( ranks_voters , normal_weights );
        cond_proxy <- condorcet_winner_2( ranks_cand, plurality );
        
        winners[i,'time'] <- format(Sys.time(), '%F %H:%M:%OS3');
        winners[i,'plurality'] <- plurality_w;
        winners[i,'cond_wo_proxy'] <- cond;
        winners[i,'cond_proxy'] <- cond_proxy;
        
        winners[i,'iteration'] <- i;
        winners[i,'sub_iteration'] <- cntr;
        winners[i,'nb_voters'] <- n;
        winners[i,'nb_cands'] <- m;
      }
      write.table( winners, file_name, sep='\t',col.names = FALSE, append = TRUE);
      
      print(paste('Took ', format(Sys.time()-sum_time) ));
    }
  }
  
}



run_simulations_normal <- function( seed_0, NB_SIM, DIM, filename, folder, 
                                  MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
                                  MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS)
{  
  set.seed( seed_0 );
  cntr <- 0;
  file_name <- paste( folder, filename, '.txt');
  
  col_n <- c('time','iteration','sub_iteration','dim','distr','nb_voters', 
             'nb_cands', 'plurality', 'cond_wo_proxy', 'cond_proxy');
  winners <- data.frame( matrix( ncol = 11, nrow = 0 ));
  colnames(winners) <- c('index',col_n);
  write.table( winners, file_name, sep='\t');
  
  
  for( m in seq(MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS)){
    for( n in seq(MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS)) {  
      sum_time <- Sys.time();
      print(Sys.time());          
      print('n == ');
      print(n);
      print('m == ');
      print(m);
      
      winners <- data.frame( matrix( ncol = 10, nrow = NB_SIM ));
      
      colnames(winners) <- col_n;
      
      for( i in 1:NB_SIM ){
        cntr <- cntr + 1;
        
        if( i %% 1000 == 0 ){
          print("Simulation nb");
          print(i);
        }
        
        sample_voters <- normal_power(n, DIM,0,1);
        sample_cands <- normal_power(m, DIM,0,1);
        
        dist_cands <- eucl_dist_vector( sample_cands );
        ranks_cand <- find_lists( dist_cands );
        
        
        dist_voter <- dist_voters( sample_voters, sample_cands );
        ranks_voters <- find_lists( dist_voter );
        
        
        plurality <- count_votes_top( ranks_voters );
        normal_weights <- matrix(1,1,n);
        
        plurality_w <- plurality_winner( plurality );
        cond <- condorcet_winner_2( ranks_voters , normal_weights );
        cond_proxy <- condorcet_winner_2( ranks_cand, plurality );
        
        winners[i,'time'] <- format(Sys.time(), '%F %H:%M:%OS3');
        winners[i,'plurality'] <- plurality_w;
        winners[i,'cond_wo_proxy'] <- cond;
        winners[i,'cond_proxy'] <- cond_proxy;
        
        winners[i,'iteration'] <- i;
        winners[i,'sub_iteration'] <- cntr;
        winners[i,'nb_voters'] <- n;
        winners[i,'nb_cands'] <- m;
        winners[i,'dim'] <- DIM;
        winners[i,'distr'] <- 'Gaussian';
      }
      write.table( winners, file_name, sep='\t', 
                   col.names = FALSE, append = TRUE);
      
      print(paste('Took ', format(Sys.time()-sum_time) ));
    }
  }
  
}


setwd('/home/ellen/Documents/Computer tests/');


SEED_0 <- 5;

MIN_NB_CANDS <- 3;
MAX_NB_CANDS <- 15;
STEP_CANDS <- 1;

MIN_NB_VOTERS <- 1001;
MAX_NB_VOTERS <- 10001; 
STEP_VOTERS <- 3000;
NB_SIM <- 10000;
DIMENSION <- 2;

folder <- '/home/ellen/Documents/Computer tests/';

filename <- paste(format(Sys.time(), '%F %H-%M'),
                 '    m=',MIN_NB_CANDS,'-',MAX_NB_CANDS,'(',STEP_CANDS,
                 ')   n=',MIN_NB_VOTERS,'-',MAX_NB_VOTERS,'(',STEP_VOTERS,
                 ')   NB_SIM=',NB_SIM,
                 '   DIM=', DIMENSION);

run_simulations_normal( SEED_0, NB_SIM, DIMENSION, filename, folder, 
                  MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
                  MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS);



# 
# MIN_NB_VOTERS <- 1001;
# MAX_NB_VOTERS <- 1001;
# STEP_VOTERS <- 400;
# 
# NB_SIM <- 10000;
# 
# DIMENSION <- 3;
# 
# folder <- '/home/ellen/Documents/Computer tests/';
# 
# filename <- paste(format(Sys.time(), '%F %H-%M'),
#                   '    m=',MIN_NB_CANDS,'-',MAX_NB_CANDS,'(',STEP_CANDS,
#                   ')   n=',MIN_NB_VOTERS,'-',MAX_NB_VOTERS,'(',STEP_VOTERS,
#                   ')   NB_SIM=',NB_SIM,
#                   '   DIM=', DIMENSION);
# 
# 
# 
# run_simulations_normal( SEED_0, NB_SIM, DIMENSION, filename, folder, 
#                  MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
#                  MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS);
# 
# filename <- paste('Unif ', format(Sys.time(), '%F %H-%M'),
#                   '    m=',MIN_NB_CANDS,'-',MAX_NB_CANDS,'(',STEP_CANDS,
#                   ')   n=',MIN_NB_VOTERS,'-',MAX_NB_VOTERS,'(',STEP_VOTERS,
#                   ')   NB_SIM=',NB_SIM,
#                   '   DIM=', DIMENSION
# );
# 
# run_simulations_unif( SEED_0, NB_SIM, DIMENSION, filename, folder, 
#                       MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
#                       MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS);
# 
# 
# DIMENSION <- 4;
# 
# filename <- paste(format(Sys.time(), '%F %H-%M'),
#                   '    m=',MIN_NB_CANDS,'-',MAX_NB_CANDS,'(',STEP_CANDS,
#                   ')   n=',MIN_NB_VOTERS,'-',MAX_NB_VOTERS,'(',STEP_VOTERS,
#                   ')   NB_SIM=',NB_SIM,
#                   '   DIM=', DIMENSION);
# 
# 
# run_simulations_normal( SEED_0, NB_SIM, DIMENSION, filename, folder, 
#                         MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
#                         MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS);
# 
# 
# 
# filename <- paste('Unif ', format(Sys.time(), '%F %H-%M'),
#                   '    m=',MIN_NB_CANDS,'-',MAX_NB_CANDS,'(',STEP_CANDS,
#                   ')   n=',MIN_NB_VOTERS,'-',MAX_NB_VOTERS,'(',STEP_VOTERS,
#                   ')   NB_SIM=',NB_SIM,
#                   '   DIM=', DIMENSION
# );
# 
# run_simulations_unif( SEED_0, NB_SIM, DIMENSION, filename, folder, 
#                       MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
#                       MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS);
# 
# 
# DIMENSION <- 2;
# 
# MIN_NB_CANDS <- 3;
# MAX_NB_CANDS <- 15;
# STEP_CANDS <- 1;
# 
# MIN_NB_VOTERS <- 201;
# MAX_NB_VOTERS <- 1001;
# STEP_VOTERS <- 400;
# 
# filename <- paste(format(Sys.time(), '%F %H-%M'),
#                   '    m=',MIN_NB_CANDS,'-',MAX_NB_CANDS,'(',STEP_CANDS,
#                   ')   n=',MIN_NB_VOTERS,'-',MAX_NB_VOTERS,'(',STEP_VOTERS,
#                   ')   NB_SIM=',NB_SIM,
#                   '   DIM=', DIMENSION);
# 
# 
# run_simulations_normal( SEED_0, NB_SIM, DIMENSION, filename, folder, 
#                         MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
#                         MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS);
# 
# filename <- paste('Unif ', format(Sys.time(), '%F %H-%M'),
#                   '    m=',MIN_NB_CANDS,'-',MAX_NB_CANDS,'(',STEP_CANDS,
#                   ')   n=',MIN_NB_VOTERS,'-',MAX_NB_VOTERS,'(',STEP_VOTERS,
#                   ')   NB_SIM=',NB_SIM,
#                   '   DIM=', DIMENSION
# );
# 
# run_simulations_unif( SEED_0, NB_SIM, DIMENSION, filename, folder, 
#                       MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
#                       MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS);
# 
# 
# DIMENSION <- 2;
# 
# MIN_NB_CANDS <- 18;
# MAX_NB_CANDS <- 51;
# STEP_CANDS <- 3;
# 
# MIN_NB_VOTERS <- 201;
# MAX_NB_VOTERS <- 1001;
# STEP_VOTERS <- 400;
# 
# filename <- paste(format(Sys.time(), '%F %H-%M'),
#                   '    m=',MIN_NB_CANDS,'-',MAX_NB_CANDS,'(',STEP_CANDS,
#                   ')   n=',MIN_NB_VOTERS,'-',MAX_NB_VOTERS,'(',STEP_VOTERS,
#                   ')   NB_SIM=',NB_SIM,
#                   '   DIM=', DIMENSION);
# 
# 
# run_simulations_normal( SEED_0, NB_SIM, DIMENSION, filename, folder, 
#                         MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
#                         MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS);
# 
# filename <- paste('Unif ', format(Sys.time(), '%F %H-%M'),
#                   '    m=',MIN_NB_CANDS,'-',MAX_NB_CANDS,'(',STEP_CANDS,
#                   ')   n=',MIN_NB_VOTERS,'-',MAX_NB_VOTERS,'(',STEP_VOTERS,
#                   ')   NB_SIM=',NB_SIM,
#                   '   DIM=', DIMENSION
# );
# 
# run_simulations_unif( SEED_0, NB_SIM, DIMENSION, filename, folder, 
#                       MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
#                       MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS);
# 
# 
# 
# 
