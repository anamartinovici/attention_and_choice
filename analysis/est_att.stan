
functions {
  matrix calc_kron_AB(int m, int n, int p, int q, matrix A, matrix B) {
      matrix[m*p, n*q] kron_AB;
      int row_start;
      int row_end;
      int col_start;
      int col_end;
      
      kron_AB = rep_matrix(0.0, m*p, n*q);
      for(i in 1:m) {
        for(j in 1:n) {
          row_start = (i-1)*p+1;
          row_end   = i*p;
          col_start = (j-1)*q+1;
          col_end   = j*q;
          
          kron_AB[row_start:row_end, col_start:col_end] = A[i,j]*B;
        }
      }
      
      return kron_AB;
  }
}   

data {
  // these are the same for fullsample and Kfold
  int<lower=0> B;                        // number of brands
  int<lower=0> G;                        // number of measures
  int<lower=0> Q;                        // number of observations per consumer, brand, and measure
  int att_K;                             // number of growth factors
  int att_K_0j;                          // number of consumer specific effects
  int att_K_ij;                          // number of brand specific effects
  matrix[Q, att_K] X_QK;                 // time scores
  
  // these differ between fullsample and Kfold, due to different participants being used
  int<lower=0> est_N;                    // number of participants used in the estimation
  vector[att_K_0j] est_X_0j[est_N];      // characteristics of participants included in estimation
  vector[att_K_ij] est_X_ij[est_N, B];   // brand-characteristics for participants included in estimation
  vector[Q] est_obs_y[est_N, B, G];      // log transformed eye movementsfor participants included in estimation
}

transformed data {
  vector[Q] obs_y_fix_b1[est_N];
  vector[Q] obs_y_fix_b2[est_N];
  vector[Q] obs_y_fix_b3[est_N];
  vector[Q] obs_y_fix_b4[est_N];
  vector[Q] obs_y_fix_b5[est_N];

  vector[Q*(G-1)] obs_y_sac_b1[est_N];        
  vector[Q*(G-1)] obs_y_sac_b2[est_N];        
  vector[Q*(G-1)] obs_y_sac_b3[est_N];        
  vector[Q*(G-1)] obs_y_sac_b4[est_N];        
  vector[Q*(G-1)] obs_y_sac_b5[est_N];        
  // the order of the dimensions matches how data is arranged
  // so first rows are all for q1, then bsac, asac, nasac
  // quarter -> type of saccade

  matrix[Q, att_K]                 X_fix_Q_K;              // time scores
  matrix[Q*(G-1), att_K*(G-1)]     X_sac_QG1_KG1;          // time scores

  matrix[att_K_0j, est_N]   X_0j_transf; 
  matrix[att_K_ij, est_N]   X_ij_transf[B]; 

  matrix[att_K, att_K] I_K_K;
  matrix[B, B] I_B_B;
  matrix[att_K*(G-1), att_K*(G-1)] I_KG1_KG1;
  matrix[G-1, G-1] I_G1_G1;
  matrix[Q*(G-1), Q*(G-1)] O_QG1_QG1;
  
  for(j in 1:est_N) {
      // vector[Q] obs_y[N, B, G]; 
      obs_y_fix_b1[j, ] = est_obs_y[j, 1, 1];
      obs_y_fix_b2[j, ] = est_obs_y[j, 2, 1];
      obs_y_fix_b3[j, ] = est_obs_y[j, 3, 1];
      obs_y_fix_b4[j, ] = est_obs_y[j, 4, 1];
      obs_y_fix_b5[j, ] = est_obs_y[j, 5, 1];
      
      for(p in 1:(G-1)) {
          for(q_now in 1:Q) {
             int index;
             index = (q_now-1)*(G-1) + p;
             obs_y_sac_b1[j, index] = est_obs_y[j, 1, 1+p, q_now];    
             obs_y_sac_b2[j, index] = est_obs_y[j, 2, 1+p, q_now];    
             obs_y_sac_b3[j, index] = est_obs_y[j, 3, 1+p, q_now];    
             obs_y_sac_b4[j, index] = est_obs_y[j, 4, 1+p, q_now];    
             obs_y_sac_b5[j, index] = est_obs_y[j, 5, 1+p, q_now];    
          }
      }
  }
  
  X_fix_Q_K   = X_QK;
  X_sac_QG1_KG1   = calc_kron_AB(Q, att_K, G-1, G-1, X_QK, diag_matrix(rep_vector(1, G-1)));

  for(j in 1:est_N) {
      X_0j_transf[, j] = est_X_0j[j];
      for(i in 1:B) {
          X_ij_transf[i, , j] = est_X_ij[j, i];
      }
  }
  
  I_K_K = diag_matrix(rep_vector(1, att_K));
  I_B_B = diag_matrix(rep_vector(1, B));
  I_KG1_KG1 = diag_matrix(rep_vector(1, att_K*(G-1)));
  I_G1_G1 = diag_matrix(rep_vector(1, G-1));
  O_QG1_QG1 = rep_matrix(0.0, Q*(G-1), Q*(G-1));
}

parameters {
  // fixations
  matrix[att_K, att_K_0j] beta_fix_0j;
  matrix[att_K, att_K_ij] beta_fix_ij;

  vector<lower=0>[Q] sd_y_fix_0j;
  vector<lower=0>[Q] sd_y_fix_ij;

  cov_matrix[att_K] var_theta_fix_0j;
  cov_matrix[att_K] var_theta_fix_ij;

  matrix[Q, est_N]     z_y_fix_0j;    
  matrix[att_K, est_N] z_theta_fix_0j;   
  matrix[att_K, est_N] z_theta_fix_ij[B]; 
  
  
  // saccades
  matrix[att_K*(G-1), att_K_0j] beta_sac_0j;
  matrix[att_K*(G-1), att_K_ij] beta_sac_ij;

  cov_matrix[G-1] var_y_sac_Q1_0j;
  cov_matrix[G-1] var_y_sac_Q2_0j;
  cov_matrix[G-1] var_y_sac_Q3_0j;
  cov_matrix[G-1] var_y_sac_Q4_0j;
  
  cov_matrix[G-1] var_y_sac_Q1_ij;
  cov_matrix[G-1] var_y_sac_Q2_ij;
  cov_matrix[G-1] var_y_sac_Q3_ij;
  cov_matrix[G-1] var_y_sac_Q4_ij;

  cov_matrix[att_K*(G-1)] var_theta_sac_0j;
  cov_matrix[att_K*(G-1)] var_theta_sac_ij;

  matrix[Q*(G-1), est_N]     z_y_sac_0j;    
  matrix[att_K*(G-1), est_N] z_theta_sac_0j;   
  matrix[att_K*(G-1), est_N] z_theta_sac_ij[B]; 
}

transformed parameters {
  matrix[att_K, est_N] theta_fix_0j;
  matrix[att_K*(G-1), est_N] theta_sac_0j;

  matrix[att_K, est_N] theta_fix_ij[B]; 
  matrix[att_K*(G-1), est_N] theta_sac_ij[B]; 

  theta_fix_0j = beta_fix_0j * X_0j_transf + cholesky_decompose(var_theta_fix_0j)*z_theta_fix_0j;
  theta_sac_0j = beta_sac_0j * X_0j_transf + cholesky_decompose(var_theta_sac_0j)*z_theta_sac_0j;

  for(i in 1:B) {
    theta_fix_ij[i] = beta_fix_ij*X_ij_transf[i] + cholesky_decompose(var_theta_fix_ij)*z_theta_fix_ij[i];
    theta_sac_ij[i] = beta_sac_ij*X_ij_transf[i] + cholesky_decompose(var_theta_sac_ij)*z_theta_sac_ij[i]; 
  }
}

model {
  // fixations
  to_vector(beta_fix_0j) ~ normal(0, 5);
  to_vector(beta_fix_ij) ~ normal(0, 5);

  sd_y_fix_0j ~ gamma(2.0, 3.0);
  sd_y_fix_ij ~ gamma(2.0, 3.0);

  var_theta_fix_0j ~ wishart(att_K+2, I_K_K);
  var_theta_fix_ij ~ wishart(att_K+2, I_K_K);
  
  to_vector(beta_sac_0j) ~ normal(0, 5);
  to_vector(beta_sac_ij) ~ normal(0, 5);
  
  var_y_sac_Q1_0j ~ wishart(G+1, I_G1_G1);
  var_y_sac_Q2_0j ~ wishart(G+1, I_G1_G1);
  var_y_sac_Q3_0j ~ wishart(G+1, I_G1_G1);
  var_y_sac_Q4_0j ~ wishart(G+1, I_G1_G1);
  
  var_y_sac_Q1_ij ~ wishart(G+1, I_G1_G1);
  var_y_sac_Q2_ij ~ wishart(G+1, I_G1_G1);
  var_y_sac_Q3_ij ~ wishart(G+1, I_G1_G1);
  var_y_sac_Q4_ij ~ wishart(G+1, I_G1_G1);
  
  var_theta_sac_0j ~ wishart(att_K*(G-1)+2, I_KG1_KG1);
  var_theta_sac_ij ~ wishart(att_K*(G-1)+2, I_KG1_KG1);
  
  to_vector(z_theta_fix_0j) ~ normal(0, 1); 
  to_vector(z_y_fix_0j) ~ normal(0, 1); 
  for(i in 1:B) {
    to_vector(z_theta_fix_ij[i]) ~ normal(0, 1); 
  }
  
  to_vector(z_theta_sac_0j) ~ normal(0, 1); 
  to_vector(z_y_sac_0j) ~ normal(0, 1); 
  for(i in 1:B) {
    to_vector(z_theta_sac_ij[i]) ~ normal(0, 1);  
  }
  
  {
   matrix[Q, est_N]   y_fix_0j; 
   matrix[Q, Q]   chol_var_y_fix;
   
   matrix[Q, est_N]   mat_y_fix_b1_mean;
   matrix[Q, est_N]   mat_y_fix_b2_mean;
   matrix[Q, est_N]   mat_y_fix_b3_mean;
   matrix[Q, est_N]   mat_y_fix_b4_mean;
   matrix[Q, est_N]   mat_y_fix_b5_mean;
   
   vector[Q]      y_fix_b1_mean[est_N];
   vector[Q]      y_fix_b2_mean[est_N];
   vector[Q]      y_fix_b3_mean[est_N];
   vector[Q]      y_fix_b4_mean[est_N];
   vector[Q]      y_fix_b5_mean[est_N];

   matrix[Q*(G-1), Q*(G-1)]     var_y_sac_0j;
   matrix[Q*(G-1), est_N]       y_sac_0j; 
   vector[Q*(G-1)]              y_sac_b1_mean[est_N];
   matrix[Q*(G-1), est_N]       mat_y_sac_b1_mean;
   
   vector[Q*(G-1)]              y_sac_b2_mean[est_N];
   matrix[Q*(G-1), est_N]       mat_y_sac_b2_mean;
   
   vector[Q*(G-1)]              y_sac_b3_mean[est_N];
   matrix[Q*(G-1), est_N]       mat_y_sac_b3_mean;
   
   vector[Q*(G-1)]              y_sac_b4_mean[est_N];
   matrix[Q*(G-1), est_N]       mat_y_sac_b4_mean;
   
   vector[Q*(G-1)]              y_sac_b5_mean[est_N];
   matrix[Q*(G-1), est_N]       mat_y_sac_b5_mean;
   
   matrix[Q*(G-1), Q*(G-1)]     aux_var_y_sac_ij;
   matrix[Q*(G-1), Q*(G-1)]     chol_var_y_sac_ij;
   
   y_fix_0j = X_fix_Q_K * theta_fix_0j + diag_matrix(sd_y_fix_0j)*z_y_fix_0j;
   mat_y_fix_b1_mean = y_fix_0j + X_fix_Q_K*theta_fix_ij[1];
   mat_y_fix_b2_mean = y_fix_0j + X_fix_Q_K*theta_fix_ij[2];
   mat_y_fix_b3_mean = y_fix_0j + X_fix_Q_K*theta_fix_ij[3];
   mat_y_fix_b4_mean = y_fix_0j + X_fix_Q_K*theta_fix_ij[4];
   mat_y_fix_b5_mean = y_fix_0j + X_fix_Q_K*theta_fix_ij[5];
  
  for(j in 1:est_N) {
     y_fix_b1_mean[j] = mat_y_fix_b1_mean[, j];
     y_fix_b2_mean[j] = mat_y_fix_b2_mean[, j];
     y_fix_b3_mean[j] = mat_y_fix_b3_mean[, j];
     y_fix_b4_mean[j] = mat_y_fix_b4_mean[, j];
     y_fix_b5_mean[j] = mat_y_fix_b5_mean[, j];
   }
  
   chol_var_y_fix = diag_matrix(sd_y_fix_ij);
   
   var_y_sac_0j = O_QG1_QG1;
   var_y_sac_0j[1:(G-1), 1:(G-1)] = var_y_sac_Q1_0j;
   var_y_sac_0j[G:(2*(G-1)), G:(2*(G-1))] = var_y_sac_Q2_0j;
   var_y_sac_0j[(2*(G-1)+1):(3*(G-1)), (2*(G-1)+1):(3*(G-1))] = var_y_sac_Q3_0j;
   var_y_sac_0j[(3*(G-1)+1):(4*(G-1)), (3*(G-1)+1):(4*(G-1))] = var_y_sac_Q4_0j;

   y_sac_0j = X_sac_QG1_KG1 * theta_sac_0j + cholesky_decompose(var_y_sac_0j)*z_y_sac_0j;
   mat_y_sac_b1_mean = y_sac_0j + X_sac_QG1_KG1*theta_sac_ij[1];
   mat_y_sac_b2_mean = y_sac_0j + X_sac_QG1_KG1*theta_sac_ij[2];
   mat_y_sac_b3_mean = y_sac_0j + X_sac_QG1_KG1*theta_sac_ij[3];
   mat_y_sac_b4_mean = y_sac_0j + X_sac_QG1_KG1*theta_sac_ij[4];
   mat_y_sac_b5_mean = y_sac_0j + X_sac_QG1_KG1*theta_sac_ij[5];
  
   for(j in 1:est_N) {
     y_sac_b1_mean[j] = mat_y_sac_b1_mean[, j];
     y_sac_b2_mean[j] = mat_y_sac_b2_mean[, j];
     y_sac_b3_mean[j] = mat_y_sac_b3_mean[, j];
     y_sac_b4_mean[j] = mat_y_sac_b4_mean[, j];
     y_sac_b5_mean[j] = mat_y_sac_b5_mean[, j];
   }
  
   aux_var_y_sac_ij = O_QG1_QG1;
   aux_var_y_sac_ij[1:(G-1), 1:(G-1)] = var_y_sac_Q1_ij;
   aux_var_y_sac_ij[G:(2*(G-1)), G:(2*(G-1))] = var_y_sac_Q2_ij;
   aux_var_y_sac_ij[(2*(G-1)+1):(3*(G-1)), (2*(G-1)+1):(3*(G-1))] = var_y_sac_Q3_ij;
   aux_var_y_sac_ij[(3*(G-1)+1):(4*(G-1)), (3*(G-1)+1):(4*(G-1))] = var_y_sac_Q4_ij;
   chol_var_y_sac_ij = cholesky_decompose(aux_var_y_sac_ij);
   
   obs_y_fix_b1 ~ multi_normal_cholesky(y_fix_b1_mean, chol_var_y_fix);  
   obs_y_fix_b2 ~ multi_normal_cholesky(y_fix_b2_mean, chol_var_y_fix);  
   obs_y_fix_b3 ~ multi_normal_cholesky(y_fix_b3_mean, chol_var_y_fix);  
   obs_y_fix_b4 ~ multi_normal_cholesky(y_fix_b4_mean, chol_var_y_fix);  
   obs_y_fix_b5 ~ multi_normal_cholesky(y_fix_b5_mean, chol_var_y_fix);  
   
   obs_y_sac_b1 ~ multi_normal_cholesky(y_sac_b1_mean, chol_var_y_sac_ij);  
   obs_y_sac_b2 ~ multi_normal_cholesky(y_sac_b2_mean, chol_var_y_sac_ij);  
   obs_y_sac_b3 ~ multi_normal_cholesky(y_sac_b3_mean, chol_var_y_sac_ij);  
   obs_y_sac_b4 ~ multi_normal_cholesky(y_sac_b4_mean, chol_var_y_sac_ij);  
   obs_y_sac_b5 ~ multi_normal_cholesky(y_sac_b5_mean, chol_var_y_sac_ij);  
  }
}
