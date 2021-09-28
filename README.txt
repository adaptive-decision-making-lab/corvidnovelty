ReadMe file
Created on 2021-09-28 by Jeffrey R. Stevens (jeffrey.r.stevens@gmail.com)

**********************************************************
If you use the data, please cite the following:
Miller, R., Lambert, M. L., Frohnwieser, A., Brecht, K. F., Bugnyar, T., Crampton, I., Garcia-Pelegrin, E., Gould, K., Greggor, A. L., Izawa, E.-I., Kelly, D. M., Li, Z., Luo, Y., Luong, L. B., Massen, J. J. M., Neider, A., Reber, S. A., Schiestl, M., Seguchi, A., Stevens, J.R., Taylor, A.H., Wang, L., Wolff, L.M., Zhang, Y., Clayton, N. S. (2021). Individual repeatability, species differences, and the influence of socio-ecological factors on neophobia in 10 corvid species. BioRxiv, 2021.07.27.453788. 
**********************************************************

Summary: The data set consists of behavioral trials from 241 corvid subjects across 10 species and 10 lab teams: common ravens (n=15), carrion crows and carrion/hooded hybrid crows (n=18), large-billed crows (n=13), New Caledonian crows (n=9), ‘Alalā (n=108), Eurasian jays (n=24), pinyon jays (n=21), blue jays (n=9), Clark’s nutcrackers (n=10) and azure-winged magpies (n=14). Each row represents a single trial for a single subject.

License:
All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0). You are free to:
    Share — copy and redistribute the material in any medium or format
    Adapt — remix, transform, and build upon the material for any purpose, even commercially.
Under the following terms:
    Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
    No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.

Data files:
 individual - Individual subject name
 species - Species common name
 lab - Experimental lab
 sex - Subject sex (m = male, f = female)
 age - Subject age (Adult or Juvenile)
 condition - Experimental condition
 round - Round number (1-3)
 range - Species range (0 = mainland, 1 = island)
 urban_habitat - Usage of urban habitats (0 = no, 1 = yes)
 adult_sociality - Level of sociality for adults (0 = territorial pairs, 1 = family groups)
 caching - Level of caching (0 = moderate, 1 = specialized)
 source - Source of subject (0 = captivity, 1 = wild)
 live_hunting - Exhibits live hunting (0 = no, 1 = yes)
 flock_size - Average flock size (0 = less than 100, 1 = greater than 100)
 touch - Whether subject touched familar food in 10 minutes (0 = no, 1 = yes)
 latency - Latency to touch familiar food (s)

R code:
 miller_etal_2021_rcode.R - code for running computations and generating figures

R Markdown documents:
 miller_etal_2021_SM.Rmd - R Markdown document with R code embedded for supplementary materials

Instructions to reproduce results:
 To reproduce these results, first unzip the files into a folder. Then, ensure that a subfolder named "figures" is in the folder. Next, open miller_etal_2021_rcode.R and ensure that all packages mentioned at the top of the script are installed.  Once all packages are installed, run the script in R using 'source("miller_etal_2021_rcode.R")'.

 Once the script runs without errors, you can compile the R Markdown document miller_etal_2021_SM.Rmd.  Open this file in RStudio and ensure that you have packages {knitr} and {rmarkdown} installed.  Once installed, use {knitr} to render the document (control-shift-K).

