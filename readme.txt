This repository contains the code used to run the simulation design described in 'design.dat' and analyse the associated results.
In particular:
    + settings.dat:     It contains input information for the SLURM-based HPC used to run the simulation study
    + simdata.R:        It contains the code used to simulated data under the LDA model (CTM model is also available but not used in this version). The otput is stored into data/.
    + estimModel.R:     It contains the code used to estimate P_DT and P_WT for all the algorithms being considered in this study. The output is stored into results/.

    + analyse_Results.R, analyse_Results_exec.R:
                        It contains the code used to compute the PCP-based indices for the data contained into results/.
                        The output is still stored into results/.
                        Note: Unlike for the previous parallel computations, here the 'GNU parallel' library has been used instead of the R implementation of parallel computing.
                              This is done to speed-up the entire computations (the R library doParall seems to work quite slowly).

    + aggregate_Results.R:
                        It contains the code used to compute averaged PCP-based mesaures. The output is stored into elaborated_results/.
        
        
        
