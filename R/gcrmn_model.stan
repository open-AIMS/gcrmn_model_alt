data {
  int<lower=1> N;              // total number of observations
  vector[N] Y;                 // response variable
  int<lower=1> K;              // number of population-level Year effects
  matrix[N, K] X;              // population-level Year design matrix
  int<lower=1> N_0;  // number of grouping levels
  int<lower=1> M_0;  // number of coefficients per level
  array[N] int<lower=1> J_0;   // number of population-level datasetID effects 
  matrix[N, N_0] Z;            // population-level sum-to-zero datasetID design matrix
  int<lower=1> N_1;            // number of levels of grid_id
  int<lower=1> M_1;            // number of coefficients per level
  array[N] int<lower=1> J_1;   // grouping indicator per observation
  vector[N_1] wt_1;            // weights per grid_id
  int<lower=1> N_2;            // number of levels of site
  int<lower=1> M_2;            // number of coefficients per level
  array[N] int<lower=1> J_2;   // grouping indicator per observation
  int<lower=1> N_3;            // number of levels of replicate
  int<lower=1> M_3;            // number of coefficients per level
  array[N] int<lower=1> J_3;   // grouping indicator per observation

  int<lower=1> n_all_years;    // total number of years (whole GCRMN)
  array[n_all_years] int all_years; // all the GCRMN years
  array[n_all_years] int data_lookup; // data lookup
  array[n_all_years] int gap_lookup; // gap lookup
  int<lower=0> n_prior_years;  // number of years prior to observed data
  int<lower=0> n_post_years;   // number of years post observed data
  int<lower=1> init_year;      // first year of observed data
  real init_cover;             // approximate cover of first year of observed data
  array[n_prior_years] int prior_years;  // years prior to observed data
  array[n_post_years] int post_years;    // years post observed data
  int<lower=0> n_gap_years;              // number of gap years in observed data
  array [n_gap_years]int<lower=1> gap_years;  // gap years in observed data
  int<lower=0> n_after_years;                 // number of years after observed data
  array [n_after_years] int<lower=0> after_years; // years after observed data
  int<lower=0> n_data_years;             // number of years of observed data
  array [n_data_years] int data_years;   // years of observed data
  array [n_data_years] int year_conversions;  // indicies to year conversions
}
transformed data {
}
parameters {
  vector[K] beta;  // regression coefficients
  real<lower=0> phi;  // precision parameter
  // vector<lower=0>[M_0] sd_0;  // group-level standard deviations
  // array[M_0] vector[N_0] z_0;  // standardized group-level effects
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  array[M_1] vector[N_1] z_1;  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  array[M_2] vector[N_2] z_2;  // standardized group-level effects
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  array[M_3] vector[N_3] z_3;  // standardized group-level effects
  vector[n_all_years] Years; 
  real<lower=1e-6> sigma_year;
  // vector[N_0] gamma_raw;
  vector[N_0] gamma;
}
transformed parameters {
  // vector[N_0] gamma;
  vector[N] mu= rep_vector(0.0, N);
  // vector[N_0] r_0_1;  // actual group-level effects
  vector[N_1] r_1_1;  // actual group-level effects
  vector[N_2] r_2_1;  // actual group-level effects
  vector[N_3] r_3_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  // r_0_1 = (sd_0[1] * (z_0[1]));
  r_1_1 = (sd_1[1] * (z_1[1]));
  r_2_1 = (sd_2[1] * (z_2[1]));
  r_3_1 = (sd_3[1] * (z_3[1]));

  // gamma = gamma_raw - mean(gamma_raw);

  lprior += gamma_lpdf(phi | 0.01, 0.01);

  for (i in 1:K) {
   lprior += normal_lpdf(beta[i]|Years[year_conversions[i]], sigma_year);
  }

  lprior += student_t_lpdf(sigma_year | 3, 0, 0.5)
    - 1 * student_t_lccdf(0 | 3, 0, 0.5);

  lprior += normal_lpdf(Years[init_year]| init_cover, sigma_year);

  for (i in prior_years) {
    if (data_lookup[i+1] > 0) { // end of the tail
      lprior += normal_lpdf(Years[i]| beta[data_lookup[i+1]], 0.1);
    } else {
      lprior += normal_lpdf(Years[i]| Years[i+1], 0.1);
    }
  }
  
  for (i in data_years) {
    if (i != init_year) {
      lprior += normal_lpdf(Years[i]| Years[i-1], sigma_year);
    }
  }
  for (i in gap_years) {
    if (data_lookup[i-1] > 0) {  // start of a gap
      lprior += normal_lpdf(Years[i]| (beta[data_lookup[i-1]] + Years[i+1])/2, 0.1);
    } else if (data_lookup[i+1] > 0) { // end of the gap
      lprior += normal_lpdf(Years[i]| (Years[i-1] + beta[data_lookup[i+1]])/2, 0.1);
    } else {  // middle of the gap
      lprior += normal_lpdf(Years[i]| (Years[i-1] + Years[i+1])/2, 0.1);
    }
  }
  for (i in after_years) {
    if (data_lookup[i-1] > 0) {  // start of tail
      lprior += normal_lpdf(Years[i]| beta[data_lookup[i-1]], 0.1); 
    } else {  // rest of tail
      lprior += normal_lpdf(Years[i]| Years[i-1], 0.1); 
    }
  }
  
  // lprior += student_t_lpdf(sd_0 | 3, 0, 1)
  //   - 1 * student_t_lccdf(0 | 3, 0, 1);
  lprior += student_t_lpdf(sd_1 | 3, 0, 1)
    - 1 * student_t_lccdf(0 | 3, 0, 1);
  lprior += student_t_lpdf(sd_2 | 3, 0, 1)
    - 1 * student_t_lccdf(0 | 3, 0, 1);
  lprior += student_t_lpdf(sd_3 | 3, 0, 1)
    - 1 * student_t_lccdf(0 | 3, 0, 1);

  // lprior += normal_lpdf(gamma_raw | 0, 1);
  lprior += normal_lpdf(gamma | 0, 0.1);

  mu += X * beta + Z * gamma;
  
  for (n in 1:N) {
    // add more terms to the linear predictor
    // mu[n] += r_0_1[J_0[n]] + r_1_1[J_1[n]] + r_2_1[J_2[n]] + r_3_1[J_3[n]];
    mu[n] += (wt_1[J_1[n]] * r_1_1[J_1[n]]) + r_2_1[J_2[n]] + r_3_1[J_3[n]];
    // mu[n] += r_2_1[J_2[n]] + r_3_1[J_3[n]];
  }
  mu = inv_logit(mu);
}
model {
  // likelihood including constants
  // initialize linear predictor term
  target += beta_lpdf(Y | mu * phi + 1.0e-9, (1 - mu) * phi + 1.0e-9);
  // priors including constants
  target += lprior;
  // target += std_normal_lpdf(z_0[1]);
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
  target += std_normal_lpdf(z_3[1]);
}
generated quantities {
  array [N] real ypred = beta_rng(mu * phi + 1.0e-9, (1 - mu) * phi + 1.0e-9);
  vector[K] cellmeans = inv_logit(beta);
  vector[n_all_years] cellmeans_years = inv_logit(Years);
  int j = 0;
  for (i in data_years) {
    j += 1;
    cellmeans_years[i] = cellmeans[j];
  }
}
