# index_PPR

A suite of tools used to calculate indices based on Trophic level.

1. Primary Production Required (PPR)

$$PPR_t = \sum_{i=1}^n \left( \left(\frac{Catch_{t,i}}{9}\right) 10^{TL_i-1}\right)$$

where n = number of species, $TL_i$ is the trophic level of species i

2. Mean Trophic Level 

$$ \hat{TL}_t = \frac{\Sigma_{i} (landings_{t,i}  TL_{t,i})}{\Sigma_{i} landings_{t,i}}$$

where n = number of species, $TL_{t,i}$ is the trophic level of species i in time t
