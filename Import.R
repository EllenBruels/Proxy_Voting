file <- rep(0,8);

setwd('C:/Users/ellen/Desktop/Master_paper/Computer_Tests/Spatial_Model/')
col_n <- c('index','time','iteration','sub_iteration','nb_voters', 'nb_cands', 
           'plurality', 'cond_wo_proxy', 'cond_proxy','dim','distr');
winners <- data.frame( matrix( ncol = 12, nrow = 0 ));
colnames(winners) <- c('super_index',col_n);
file_name <- 'C:/Users/ellen/Desktop/Master_paper/Computer_Tests/Spatial_Model/Data.txt';
write.table( winners, file_name, sep='\t');




file[1] <- '2018-10-19 01-25     m= 3 - 15 ( 1 )   n= 201 - 1001 ( 400 )   NB_SIM= 10000    DIM= 2 .txt';
file[2] <- '2018-10-18 14-39     m= 3 - 15 ( 1 )   n= 1001 - 1001 ( 400 )   NB_SIM= 10000    DIM= 3 .txt';
file[3] <- '2018-10-18 21-49     m= 3 - 15 ( 1 )   n= 1001 - 1001 ( 400 )   NB_SIM= 10000    DIM= 4 .txt';
file[4] <- '2018-10-19 07-25     m= 18 - 51 ( 3 )   n= 201 - 1001 ( 400 )   NB_SIM= 10000    DIM= 2 .txt';
file[5] <- 'Unif  2018-10-19 04-26     m= 3 - 15 ( 1 )   n= 201 - 1001 ( 400 )   NB_SIM= 10000    DIM= 2 .txt';
file[6] <- 'Unif  2018-10-18 19-49     m= 3 - 15 ( 1 )   n= 1001 - 1001 ( 400 )   NB_SIM= 10000    DIM= 3 .txt';
file[7] <- 'Unif  2018-10-18 23-45     m= 3 - 15 ( 1 )   n= 1001 - 1001 ( 400 )   NB_SIM= 10000    DIM= 4 .txt';
file[8] <- 'Unif  2018-10-20 14-56     m= 18 - 51 ( 3 )   n= 201 - 1001 ( 400 )   NB_SIM= 10000    DIM= 2 .txt';



data_1 <- read.delim(file[1], header = TRUE, sep = "\t", dec = ".");
col <- rep(2,nrow(data_1));
col_2 <- rep('Gaussian',nrow(data_1));

data_1$dim <- col;
data_1$distr <- col_2;

data_2 <- read.delim(file[2], header = TRUE, sep = "\t", dec = ".");

col <- rep(3,nrow(data_2));
col_2<- rep('Gaussian',nrow(data_2));

data_2$dim <- col;
data_2$distr <- col_2;

data_3 <- read.delim(file[3], header = TRUE, sep = "\t", dec = ".");

col <- rep(4,nrow(data_3));
col_2<- rep('Gaussian',nrow(data_3));

data_3$dim <- col;
data_3$distr <- col_2;

data_4 <- read.delim(file[4], header = TRUE, sep = "\t", dec = ".");

col <- rep(2,nrow(data_4));
col_2<- rep('Gaussian',nrow(data_4));

data_4$dim <- col;
data_4$distr <- col_2;

data_5 <- read.delim(file[5], header = TRUE, sep = "\t", dec = ".");
col <- rep(2,nrow(data_5));
col_2<- rep('unif',nrow(data_5));

data_5$dim <- col;
data_5$distr <- col_2;

data_6 <- read.delim(file[6], header = TRUE, sep = "\t", dec = ".");

col <- rep(3,nrow(data_6));
col_2<- rep('unif',nrow(data_6));

data_6$dim <- col;
data_6$distr <- col_2;

data_7 <- read.delim(file[7], header = TRUE, sep = "\t", dec = ".");

col <- rep(4,nrow(data_7));
col_2<- rep('unif',nrow(data_7));

data_7$dim <- col;
data_7$distr <- col_2;

data_8 <- read.delim(file[8], header = TRUE, sep = "\t", dec = ".");

col <- rep(2,nrow(data_8));
col_2<- rep('unif',nrow(data_8));

data_8$dim <- col;
data_8$distr <- col_2;

data_new <- rbind(data_1,data_2,data_3,data_4,data_5,data_6,data_7,data_8);



write.table( data_new, file_name, sep='\t',col.names = FALSE, append = TRUE);