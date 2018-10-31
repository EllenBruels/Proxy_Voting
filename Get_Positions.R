
get_positions_dim_2_normal <- function( seed_0, NB_SIM, DIM, filename, folder, 
                                    MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
                                    MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS)
{  
  set.seed( seed_0 );
  cntr <- 0;
  file_name <- paste( folder, filename, '.txt');
  
  col_n <- c('time','iteration','sub_iteration','dim','distr','nb_voters', 
             'nb_cands', 'candidate', 'x', 'y');
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
      
      winners <- data.frame( matrix( ncol = 10, nrow = 1000*m ));
      
      colnames(winners) <- col_n;
      
      for( i in 1:NB_SIM ){
        cntr <- cntr + 1;
        
        if( i %% 1000 == 0 ){
          print("Simulation nb");
          print(i);
        }
        
        sample_voters <- normal_power(n, DIM,0,1);
        sample_cands <- normal_power(m, DIM,0,1);
        if( i <= 1000 ){
          for( j in 1:m )
          {
            winners[(i-1)*m+j,'time'] <- format(Sys.time(), '%F %H:%M:%OS3');
  
          
            winners[(i-1)*m+j,'iteration'] <- i;
            winners[(i-1)*m+j,'sub_iteration'] <- cntr;
            winners[(i-1)*m+j,'nb_voters'] <- n;
            winners[(i-1)*m+j,'nb_cands'] <- m;
            winners[(i-1)*m+j,'dim'] <- DIM;
            winners[(i-1)*m+j,'distr'] <- 'Gaussian';
            winners[(i-1)*m+j, 'x'] <- sample_cands[1,j];
            winners[(i-1)*m+j, 'y'] <- sample_cands[2,j];
            winners[(i-1)*m+j, 'candidate'] <- j;
          }
        } 
      }
      write.table( winners, file_name, sep='\t', 
                   col.names = FALSE, append = TRUE);
      
      print(paste('Took ', format(Sys.time()-sum_time) ));
    }
  }
  
}


setwd('/home/ellen/Documents/Computer tests/');


SEED_0 <- 5;

MIN_NB_CANDS <- 18;
MAX_NB_CANDS <- 51;
STEP_CANDS <- 3;

MIN_NB_VOTERS <- 1001;
MAX_NB_VOTERS <- 1001; 
STEP_VOTERS <- 1000;
NB_SIM <- 10000;
DIMENSION <- 2;

folder <- '/home/ellen/Documents/Computer tests/';

filename <- paste('Positions ',format(Sys.time(), '%F %H-%M'),
                  '    m=',MIN_NB_CANDS,'-',MAX_NB_CANDS,'(',STEP_CANDS,
                  ')   n=',MIN_NB_VOTERS,'-',MAX_NB_VOTERS,'(',STEP_VOTERS,
                  ')   NB_SIM=',NB_SIM,
                  '   DIM=', DIMENSION);

get_positions_dim_2_normal( SEED_0, NB_SIM, DIMENSION, filename, folder, 
                        MIN_NB_CANDS, MAX_NB_CANDS, STEP_CANDS,
                        MIN_NB_VOTERS, MAX_NB_VOTERS, STEP_VOTERS);




