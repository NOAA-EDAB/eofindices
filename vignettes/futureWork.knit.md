---
title: "Future Research"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Future Research}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



### Primary production required (PPR)

$$PPR_t = \sum_{i=1}^{n_t}  \left(\frac{landings_{t,i}}{9}\right) \left(\frac{1}{TE}\right)^{TL_i-1}$$
where $n_t$ = number of species in time $t$, $landings_{t,i}$ = landings of species $i$ in time $t$, $TL_i$ is the trophic level of species $i$, $TE$ = Trophic efficiency. The PPR estimate assumes a 9:1 ratio for the conversion of wet weight to carbon and a constant transfer efficiency per trophic level.


We have explored the index in the following ways. Using: 

* *A global transfer efficiency of 15% for all species.*
    
    This probably needs some refinement and can/should be adapted based on previous studies. One adaptation would be to use a different transfer efficienct for the first level. eg. $\left( \frac{1}{TE_1}\right)  \left(\frac{1}{TE_2}\right)^{TL_i-2}$. Whatever choices are made, the sensitivity of the index to such changes should be examined.

* *Primary production not lagged with landings.*

    This is probably not realistic. You wouldn't expect to see changes in the landing the same year as changes in primary production. This needs to be explored, either using specific lags in time (which may prove problematic since species lower on the food chain will be effected by shorter lags in time versus species higher up the chain) or by adopting some weighted scheme.

* *A threshold of 80% for landings.*

    It would be a good idea to explore the sensitivity of the index for other threshold levels. Of course the higher the threshold used would imply that less common species will then contribute to the index.

* *Combined vertibrates and invertibrates.*

    The landings in some of the EPUs are dominated by invertibrates (LOBSTER, CLAMS) which may play a significant part in driving this index. Creating two additional indices, one for vertibrates and one for invertibrates may be an interesting avenue. This will of course imply the inclusion of many other lesser caught species into the index. It will also involve partioning the landings into vertibrates and invertibrates.
    
* *Uncertainty Estimates*

    No uncertainty is presented. This could be included in several ways. And the sensitivity of the index to each of the following examined.
    * Trophic level uncertainty - fishbase
    * landings uncertainty
    * primary production uncertainty

* *Survey Comparison*

    Compare mean trophic level from survey data with commercial landings. (Just for vertibrates). There are some pretty big spikes in MTL, maybe due to invertibrates

* *Extend time series*

    Reacalculate the index going back to the 60's. Primary production info is not available for that time period however we can make some assumptions (mean, median, max of PP data we do have). The idea is to how we are now relative to the 60's when we had very large harvests
    
* *Examine composition of species at trophic level*

    The species composition within a trophic level could change throughout time. This may not be picked up in the index but may be of interest to managers. Need to examine data
    

### Other comments (+ Councils)

* Some classifications in the commercial fisheries database are not at the species level. Some are Genus, Family or even higher orders, some are just general unclassified. eg. (DOGFISH, UNC, FLATFISH, Argentinidae). Most of these cases are associated with lower landings. However if we increase the threshold and/or split landings into vertibrates and invertibrates we will encounter more of these classifications. They will need to be assigned a trophic level which may cause complications and/ or subjective decision making.

* It is possible for species to drop out of the top x% of the landings and be replaced by other species with similar trophic level and the index will somewhat insensitve to this. The mean trophic level would also be insensitive to such changes. This may or maynot be of concern but it my be worth looking into how often this happens.

* Related to yield: Can we link this indicator to fisheries yield and/or revenue?

* Primary productivity to support landings is a difficult phrasing. Alternatives: Landings in terms of primary productivity units; Fraction of primary productivity that is accounted for by landings.

* Guidance: What to do with this information. Should we fish harder/less? Is the system less efficient restricting catch? Use for rebuilding targets?

* Investigate size distribution in index

* Would benefit having improved phytoplankton size class data to look at how PP moves through the web

* Interpretation of Index: A decline in PPR could be explained by constant landings and increase in PP or constant PP but increase in landings. How best to interpret the index. Maybe tease out species specific contributions to indicator
