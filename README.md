# ZooMSS 2300

Use ZooMSS to assess the impacts of climate change on the structure of the global zooplankton community, and its role in mediating energy from phytoplankton to fish for long term simulations out to 2300.

## Datasets ##
Annual average surface chlorophyll (chlo: mg m-3), converted from phytoplankton carbon concentration (phyc: mol m-3) and surface temperature (tos: oC) netcdfs for 
* historical (1850 - 2014)
* picontrol (1601 - 2014): Preindustrial climate as simulated by the ESM, minimum 50 years, ending on last year of picontrol
* ssp126 (2015-2300), 
* ssp534-over (2015-2300) and 
* ssp585 (2015-2300) climate simulations

* nat: No fishing (naturalised run)

Files too large for github tracking, so Inputs/ is in .gitignore
Inputs found on RDM in MME1Data/ZooMSS_2300/Inputs

## Earth System Models Used
* CESM2, 
* IPSL-CM6A-LR, 
* UKESM1-0-LL earth system models. 

Required runs
For each dataset, we need to complete these runs:
1)	The standard model run, with all zooplankton groups included

## Enviro matrices
* `novel_sst_chl_combinations_2300_wPhyto.rds` in `Enviro_Matrix` folder contains all combinations of sst-chlo from the 2300 ESM inputs not already run in `ZooMSS_Climate_Change`
* `all_sst_chl_combinations_2300_wPhyto.rds` in `Enviro_Matrix` folder contains all combinations of sst-chlo from the 2300 ESM inputs
* `ClimateChange_Compiled_Distinct.rds` in `Enviro_Matrix` folder is from ZooMSS_Climate_Change
* `enviro_CMIP_Matrix_wPhyto.RDS` in `Enviro_Matrix` folder is from ZooMSS_Climate_Change

## Mandatory outputs from ZooMSS for protocol
Mandatory output from global models.

- Total Consumer Biomass Density: tcb (g m-2)
    All consumers (trophic level >1, vertebrates and invertebrates)

- Total Consumer Biomass Density in log10 Weight Bins: tcblog10 (g m-2)
    Level dimensions: (time, bins, lat, lon).
    If the model is size-structured, please provide biomass in equal log 10 g weight bins (1-10g, 10-100g, 100g-1kg, 1-10kg, 10-100kg, >100kg)

- Total Pelagic Biomass Density: tpb (g m-2)
    All pelagic consumers (trophic level >1, vertebrates and invertebrates)
  
- Total Demersal Biomass Density: tdb (g m-2)
    All demersal consumers (trophic level >1, vertebrates and invertebrates)


