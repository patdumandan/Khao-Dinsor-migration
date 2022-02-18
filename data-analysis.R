#Code for manuscript: Among-species differences in seasonal timing and weather correlates of 
#autumn raptor migration at Khao Dinsor, Thailand, 2015-2016####


#MODELS####
bb=kdata%>%filter(Species=="Black Baza")
cs=kdata%>%filter(Species=="Chinese Sparrowhawk")
ohb=kdata%>%filter(Species=="Oriental Honey-buzzard")

#compare model performance when weather variables are lagged or not####

#Black Baza

mod_bb_lag=stan_glm(Count~yearsd+
                      lag_temp+lag_uwind+lag_vwind+lag_baro,
                    data=bb, family = neg_binomial_2, offset=obs_effort)
mod_bb=stan_glm(Count~yearsd+tempsd+uwindsd+vwindsd+barosd,
                data=bb, family = neg_binomial_2, offset=obs_effort)
loo(mod_bb)
loo(mod_bb_lag)

#Chinese sparrowhawk

mod_cs_lag=stan_glm(Count~yearsd+
                      lag_temp+lag_uwind+lag_vwind+lag_baro,
                    data=cs, family = neg_binomial_2, offset=obs_effort)
mod_cs=stan_glm(Count~yearsd+tempsd+uwindsd+vwindsd+barosd,
                data=cs, family = neg_binomial_2, offset=obs_effort)
loo(mod_cs)
loo(mod_cs_lag)

#oriental honey-buzzard
mod_ohb_lag=stan_glm(Count~yearsd+
                      lag_temp+lag_uwind+lag_vwind+lag_baro,
                    data=ohb, family = neg_binomial_2, offset=obs_effort)
mod_ohb=stan_glm(Count~yearsd+tempsd+uwindsd+vwindsd+barosd,
                 data=ohb, family = neg_binomial_2, offset=obs_effort)
loo(mod_ohb)
loo(mod_ohb_lag)