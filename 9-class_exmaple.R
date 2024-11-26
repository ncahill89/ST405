running_dat <- cherry_blossom_sample
running_dat

ggplot(running_dat, aes(age, net)) +
  geom_point() +
  facet_wrap(~ runner, scales = "free")



bhm = 
"
model{

for(i in 1:n)
{
y.i[i] ~ dnorm(mu.i[i], sigma^-2)

mu.i[i] <- alpha.j[runner_index[i]] + beta.j[runner_index[i]]*(x.i[i]-50)
}

for(j in 1:n_runner)
{
alpha.j[j] ~ dnorm(mu_alpha, sigma_alpha^-2)
beta.j[j] ~ dnorm(mu_beta, sigma_beta^-2)
}

mu_alpha ~ dnorm(100, 30^-2)
mu_beta ~ dnorm(0, 10^-2)

sigma_alpha ~ dt(0,20^-2,1)T(0,)
sigma_beta ~ dt(0,5^-2,1)T(0,)

sigma ~ dunif(0,50)

for(k in 1:n_pred)
{
mupred.k[k] <- alpha.j[runner_index_pred[k]] + beta.j[runner_index_pred[k]]*(xpred.k[k] -50)
muoverall.k[k] <- mu_alpha + mu_beta*(xpred.k[k]-50)
}
}
"

parnames <- c("alpha.j","beta.j", "mu_alpha","mu_beta", "mupred.k", "muoverall.k")

jags.data <- list(y.i = running_dat$net,
                  x.i = running_dat$age,
                  runner_index = running_dat$runner,
                  n = nrow(running_dat),
                  n_runner = running_dat$runner %>% unique() %>% length(),
                  xpred.k = rep(50:60,36),
                  runner_index_pred = rep(1:36, each = 11),
                  n_pred = 11*36)


library(rjags)
library(R2jags)

mod <- jags(data = jags.data, 
     parameters.to.save =  parnames,
     model.file = textConnection(bhm),
     n.iter = 20000,
     n.burnin = 2000,
     n.thin = 10)

plot(mod)

m <- mod$BUGSoutput$sims.matrix

pred_summary <- m %>% 
  gather_rvars(mupred.k[pred_ind]) %>% 
  median_qi(.value) %>% 
  mutate(x = jags.data$xpred.k,
         runner = jags.data$runner_index_pred)


ggplot(pred_summary, aes(x = x, y = .value)) +
  geom_line() +
  geom_point(data = running_dat, aes(x = age, y = net)) +
  facet_wrap(~ runner)


par_summary <- m %>% 
  gather_rvars(alpha.j[1:36],beta.j[1:36],mu_alpha,mu_beta) %>% 
  median_qi(.value)
par_summary %>% print(n = 74)



