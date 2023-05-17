# ZooMSS_FishMIP
 
These code were used to create the output for ZooMSS within the ensemble simulations published in Tittensor et al. (2021) and used within "Chapter 3: Oceans and Coastal Ecosystems and their Services" of the IPCC WGII Sixth Assessment Report (https://www.ipcc.ch/report/ar6/wg2/chapter/chapter-3/).
 
## Overview of ZooMSS

The Zooplankton Model of Size Spectra (ZooMSS) is a functional size-spectrum model of the marine ecosystem (following Heneghan et al. 2016, ZooMSSv1) to resolve phytoplankton, nine zooplankton functional groups (heterotrophic flagellates and ciliates, omnivorous and carnivorous copepods, larvaceans, euphausiids, salps, chaetognaths and jellyfish) and three size-based fish groups. Zooplankton functional groups are resolved using their body-size ranges, size- based feeding characteristics and carbon content, and the zooplankton community emerges from the model across global environmental gradients, depending on the functional traits of the different groups. 

We developed the Zooplankton Model of Size Spectra version 2 (ZooMSSv2) based on the prototype of Heneghan et al. (2016). ZooMSSv2 uses the functional size-spectrum framework (Blanchard et al., 2017) to resolve the body size ranges, size-based feeding characteristics and carbon content of nine zooplankton groups and three fish groups. The model is run on 5° grid squares across the global ocean. For each region, the model is forced with the long- term mean satellite sea surface temperature and chlorophyll a from MODIS-Aqua.

ZooMSSv2 represents the marine ecosystem as three communities: phytoplankton, zooplankton and fish. The zooplankton community consists of nine of the most abundant zooplankton groups, and the fish community was made up of a small, medium and large group. Dynamics of the phytoplankton are not explicitly resolved in the model, rather the mean size structure of the phytoplankton community is estimated directly from satellite chlorophyll a observations (Brewin et al., 2010; Barnes et al., 2011; Hirata et al., 2011). Abundances of the zooplankton and fish communities are driven by size-dependent processes of growth and mortality, with the temporal dynamics of each functional group governed by separate second- order McKendrick-von Foerster equations.

## References

Blanchard, Julia L., Ryan F. Heneghan, Jason D. Everett, Rowan Trebilco, and Anthony J. Richardson. “From Bacteria to Whales: Using Functional Size Spectra to Model Marine Ecosystems.” Trends in Ecology & Evolution 32, no. 3 (March 2017): 174–86. https://doi.org/10.1016/j.tree.2016.12.003.

Heneghan, Ryan F., Jason D. Everett, Julia L. Blanchard, and Anthony J. Richardson. “Zooplankton Are Not Fish: Improving Zooplankton Realism in Size-Spectrum Models Mediates Energy Transfer in Food Webs.” Frontiers in Marine Science 3 (October 19, 2016). https://doi.org/10.3389/fmars.2016.00201.

Heneghan, Ryan F., Eric Galbraith, Julia L. Blanchard, Cheryl Harrison, Nicolas Barrier, Catherine Bulman, William Cheung, et al. “Disentangling Diverse Responses to Climate Change among Global Marine Ecosystem Models.” Progress in Oceanography 198 (November 2021): 102659. https://doi.org/10.1016/j.pocean.2021.102659.
