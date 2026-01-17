// Hierarchical dynamic topic-IRT (with topic dispersion, respondent noise, and time deviations)
//
// Notes:
// - Full generative model for binary responses y[n] with item i[n], person j[n],
//   topic k[n], and wave t[n].
// - Identification:
//   (1) theta_j ~ Normal(0, 0.3) anchors global scale/location.
//   (2) Topic deviations d_jk are centered per person via mean-centering: sum_k d_jk = 0.
//   (3) sigma_topic,j controls across-topic dispersion of d_jk (person-specific).
//   (4) u_jkt are topic-time deviations with person-specific scale sigma_u_j;
//       and are centered across time for each (j,k) via mean-centering: sum_t u_jkt = 0.
// - tau_{jk} > 0 scales the linear predictor and is pooled toward tau_j on log scale.
// - Mean-centering makes topics/waves exchangeable.
// 

data {
  int<lower=1> N;                       // number of observations
  array[N] int<lower=0,upper=1> y;      // binary responses

  int<lower=1> J;                       // respondents
  int<lower=1> I;                       // items
  int<lower=1> K;                       // topics
  int<lower=1> T;                       // waves

  array[N] int<lower=1,upper=J> jj;     // respondent index for each obs
  array[N] int<lower=1,upper=I> ii;     // item index
  array[N] int<lower=1,upper=K> kk;     // topic index
  array[N] int<lower=1,upper=T> tt;     // wave index

  // Person–item pair indexing for stable item-specific deviations nu_ij
  int<lower=1> P;                        // number of unique (j,i) pairs observed
  array[N] int<lower=1,upper=P> pair_id; // pair index for each observation
  array[P] int<lower=1,upper=J> pair_j;  // respondent for each pair
  array[P] int<lower=1,upper=I> pair_i;  // item for each pair
  array[P] int<lower=1,upper=K> pair_k;  // topic for each pair
}

parameters {
  // Item parameters
  vector<lower=1e-6>[I] alpha;          // discrimination
  vector[I] delta;                      // item cutpoints (difficulty)

  // Global respondent ideology
  vector[J] theta;                      // theta_j

  // Across-topic dispersion
  vector<lower=1e-6>[J] sigma_topic;

  // Topic deviations: unconstrained, then mean-centered per person
  matrix[J, K] d_tilde;                 // unconstrained topic deviations
  
  // Topic-time deviations: unconstrained, then mean-centered across time per (j,k)
  array[J, K] vector[T] u_tilde;        // unconstrained time deviations

  // Person-specific scale for time deviations
  vector<lower=1e-6>[J] sigma_u;        // controls magnitude of u_jkt

  // Hyperparameters for item priors
  real<lower=1e-6> sigma_alpha;
  real<lower=1e-6> sigma_delta;
  
  // Hyperparameters for volatility and dispersion
  real mu_sigma_topic;
  real<lower=1e-6> sigma_sigma_topic;
  
  real<lower=1e-6> sigma_sigma_u;

  // Stable item-specific deviations nu_ij
  vector[P] nu_raw;                            // non-centered (j,i) deviations
  vector<lower=1e-6>[J] sigma_nu_person;       // respondent-level SD for nu
  real mu_sigma_nu;                            // global mean on log SD scale
  real<lower=1e-6> sigma_sigma_nu;             // global SD

  // Mixture with a “random answering” component
  vector[J] pi_raw;
}

transformed parameters {
  // Construct d_jk with per-person sum-to-zero via mean-centering
  matrix[J, K] d;                       // d_jk
  for (j in 1:J) {
    vector[K] row = to_vector(d_tilde[j]);
    real m = mean(row);
    for (k in 1:K) {
      d[j, k] = row[k] - m;             // ensures sum_k d[j,k] = 0
    }
  }
  
  // Construct u_jkt with per-(j,k) sum-to-zero across time via mean-centering
  array[J, K] vector[T] u;              // u_jkt
  for (j in 1:J) {
    for (k in 1:K) {
      vector[T] tmp = sigma_u[j] * u_tilde[j, k];
      real mt = mean(tmp);
      for (t in 1:T) {
        u[j, k][t] = tmp[t] - mt;       // ensures sum_t u[j][k,t] = 0
      }
    }
  }

  // Realized nu_ij for observed (j,i) pairs using nested respondent-topic SD
  vector[P] nu;
  for (p in 1:P) {
    int j = pair_j[p];
    nu[p] = sigma_nu_person[j] * nu_raw[p];
  }

  // Mixture with a “random answering” component
  vector[J] pi;
  for (j in 1:J)
    pi[j] = inv_logit(pi_raw[j]);
}

model {
  // Identification anchor: global ideology scale/location
  theta ~ normal(0, 0.3); # Change this back to normal(0, 1)

  // Item priors
  sigma_alpha ~ normal(0, 0.3);
  alpha ~ normal(0, sigma_alpha);

  sigma_delta ~ normal(0, 0.3);
  delta ~ normal(0, sigma_delta);

  // Across-topic dispersion on d_tilde
  mu_sigma_topic ~ normal(0, 0.1);
  sigma_sigma_topic ~ normal(0, 0.1);

  sigma_topic ~ normal(mu_sigma_topic, sigma_sigma_topic);
  for (j in 1:J)
    for (k in 1:K)
      d_tilde[j, k] ~ normal(0, sigma_topic[j]);
      
  // Time volatility priors for u_tilde
  sigma_sigma_u ~ normal(0, 0.05);

  sigma_u ~ normal(0, sigma_sigma_u);
  for (j in 1:J)
    for (k in 1:K)
      u_tilde[j][k] ~ normal(0, 1);

  // Priors for stable item-specific deviations nu_ij with nested variance
  mu_sigma_nu ~ normal(0, 0.1); // 0.2
  sigma_sigma_nu ~ normal(0, 0.1); // 0.3

  for (j in 1:J) {
    sigma_nu_person[j] ~ normal(mu_sigma_nu, sigma_sigma_nu);
  }

  // non-centered nu
  nu_raw ~ normal(0, 1);

  // Mixture with a “random answering” component
  pi_raw ~ normal(logit(0.05), 1);

  // Likelihood (vectorized)
  {
    vector[N] psi;
    vector[N] x;
    vector[N] linpred;

    for (n in 1:N) {
      psi[n] = theta[jj[n]] + d[jj[n], kk[n]] + u[jj[n], kk[n]][tt[n]];
    }

    x = psi - delta[ii];

    vector[N] alpha_obs = alpha[ii];
    vector[N] nu_obs = nu[pair_id];

    linpred = alpha_obs .* x + nu_obs;

    {
      vector[N] p_irt = inv_logit(linpred);
      vector[N] p;

      for (n in 1:N) {
        real pij = pi[jj[n]];
        p[n] = (1 - pij) * p_irt[n] + pij * 0.5;
      }

      target += bernoulli_lpmf(y | p);
    }
  }
}

generated quantities {
  // Generate sigma_topic^2
  vector[J] sigma_topic_sq;
  for (j in 1:J) sigma_topic_sq[j] = square(sigma_topic[j]);

  // Generate respondent-level nu variance summaries
  vector[J] sigma_nu_person_sq;
  for (j in 1:J) sigma_nu_person_sq[j] = square(sigma_nu_person[j]);

  // Posterior predictive replication of the observed responses
  array[N] int y_rep;
  for (n in 1:N) {
    int j = jj[n];
    int i = ii[n];
    int k = kk[n];
    int t = tt[n];

    real psi_jkt = theta[j] + d[j, k] + u[j, k][t];
    real x = psi_jkt - delta[i];

    {
      real linpred = alpha[i] * x + nu[pair_id[n]];
      real p_irt = inv_logit(linpred);
      real p = (1 - pi[j]) * p_irt + pi[j] * 0.5;
      y_rep[n] = bernoulli_rng(p);
    }
  }
}
