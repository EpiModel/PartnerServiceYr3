# PartnerServiceYr3

This repository holds the code for our network modeling study of HIV partner services for gay, bisexual, and other men who have sex with men previously diagnosed with HIV in Atlanta.

## Paper and engine

> Onwubiko UN, Le Guillou A, Lyles C, Maloney KM, Delaney KP, Jenness SM. Optimizing HIV Partner Services for Gay, Bisexual, and Other Men Who Have Sex With Men Previously Diagnosed With HIV: A Modeling Study. _Sex Transm Dis._ 2025; 52(8): 495-502. [doi:10.1097/OLQ.0000000000002167](https://doi.org/10.1097/OLQ.0000000000002167)

The EpiModelHIV engine for this paper is release [`PartServDx-STD`](https://github.com/EpiModel/EpiModelHIV/releases/tag/PartServDx-STD) of EpiModel/EpiModelHIV. Install it with:

```r
remotes::install_github("EpiModel/EpiModelHIV@PartServDx-STD")
```

`install_packages.R` installs EpiModelHIV-p, which is private. Use the install line above instead.

ARTnetData is a private package. It is needed only to re-estimate the networks. The paper's networks were estimated with ARTnet 2.7.0, which is no longer public, so `renv.lock` installs ARTnet 2.9.3, the first version in the public [ARTnet](https://github.com/EpiModel/ARTnet) repository. Networks re-estimated with it may not match the paper's exactly.
