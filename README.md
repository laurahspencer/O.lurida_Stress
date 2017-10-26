# Climate adaptation in _Ostrea lurida_ via transgenerational epigenetic inheritance

#### This repo houses data for the 2016-onwards _Ostrea lurida_ experiment on gonad maturation, larval production, and larval recruitment under varying temperature and pH overwintering treatments.  This project is being conducted at the Kenneth K. Chew Center for Shellfish Research & Restoration, at the NOAA Manchester Laboratory.  The following is a narrative of the project including details on repository structure.

In December 2016 four groups of _Ostrea lurida_ were moved from pearl nets hanging off the NOAA dock in Clam Bay, Manchester WA into the the Ken Chew Center's hatcery.  Populations include: 
  1. Three groups of first-generation hatchery-produce (F1) oysters, all hatched at the Port Gamble hatchery in 2013 by Puget Sound Restoration Fund staff and Jake Heare, UW SAFS graduate student. The broodstock used to produce these F1 oysters were wild, harvested from Fidalgo Bay in North Puget Sound (NF, for "North-Fidalgo"), Dabob Bay in Hood Canal (HC, for "Hood Canal"), and Oyster Bay in South Puget Sound (SS, for "South South"). 
  2. One group of second-generation hatchery-produced (F2) oysters (K, for "Katherine"), hatched at the Ken Chew Center in 2015 by Katherine Silliman, University of Chicago graduate student. The broodstock used to produce these F2 oysters were the aforementioned F1 oysters from Oyser Bay in South Sound.
  
_**Figure**: Sites where Oly populations' progenitors were collected (F, D, O) and where Olys were housed prior to experiment (C). Source: [Heare JE, White SJ, Vadopalas B, Roberts SB. (2017) Differential response to stress in Ostrea lurida as measured by gene expression. PeerJ Preprints 5:e1595v3](https://doi.org/10.7287/peerj.preprints.1595v3)_
![Oly population sites](Images/Oly-population-sites-Heare.png?raw=true)

---
### Temperature Treatment
Each of the 4 Oly populations were divided into 2 treatments and "overwintered" From December 6th to February 4th in two temperature treatments: 6degC, 10degC which represents historic and projected winter temperatures, respectively. While each population was divided into two bags per treatment (4 bags total per population), two overwinterint tanks were used, one per temperature.  

_**Table 1:** Number of oysters during the temperature treatments for each population, with -1 and -2 denoting "replicates":_

|      | 6 degC | 10 degC |
|------|--------|---------|
| K-1  | 168    | 170     |
| K-2  | 148    | 167     |
| SS-1 | 52     | 51      |
| SS-2 | 50     | 48      |
| HC-1 | 28     | 29      |
| HC-2 | 27     | 27      |
| NF-1 | 46     | 51      |
| NF-2 | 53     | 47      |

Temperature treatments were terminated on February 4th.  A subset of animals from each treatment from SN, HC & NF animals were sampled on February 4th, and K on February 11th, for histology and tissues.  Check out [Steven's video](https://www.youtube.com/watch?v=y1bOt7gC9U4&feature=youtu.be) for a time-lapse of the sampling, and my [Lab notebook posts from [2/4](https://laurahspencer.github.io/LabNotebook/OysterStress_Samping,System-setup/) & [2/11](https://laurahspencer.github.io/LabNotebook/OA-prep-final-touches/] for images and details. Breifly, all sampled animals were weighed (g), measured (mm), shucked, and tissue was isolated. Tissue samples included: mantle (M), ctenidia (C) & adductor (A) into 2mL centrifuge snap-top tubes, immediately flash-frozen in an ethanol/dry ice bath, and kept on dry ice until transported to the -80 in Rick’s lab. The remaining visceral mass sampled for gonad was put into histology cassettes. Any residual tissue (gill, mantle or adductor) was sampled for DNA by placing into tubes with 1mL ethanol, and stored at room temperature.

_**Table 2:** Number of each population sampled post-temperature treatment; see [2017-02-04_SamplingData.xlsx](../../Data/2017-02-04_SamplingData.xlsx) for details.

|    | 6 degC | 10 degC | Date Sampled |
|----|--------|---------|--------------|
| K  | 15     | 15      | 2/11/2017    |
| SS | 15     | 15      | 2/04/2017    |
| HC | 9      | 9       | 2/04/2017    |
| NF | 15     | 15      | 2/04/2017    |

--- 
### pH Treatment
On [February 15th](https://laurahspencer.github.io/LabNotebook/OA-Exp-Day1/) oyster groups were again divided in half for a 2-month pH treatment, at pH ~7.8 and ~7.2 constituting ambient and low pH, respectively. Animals were housed in six 50L flow-through tanks, 3 replicates per pH treatment, were fed Reeds shellfish Diet via dosing pump at >XX thousand cells/mL, and co-habiatated with ~20 large _Crassostrea gigas_ per tank.  Temperature was measured continuously via [HOBO Data Loggers]() and discrete water samples were collected 3x/week with simultaneous T, S & pH parameters measured. 

pH treatments were terminated on April 8, and a subset of SN, HC, NF were again sampled on [2/8](https://laurahspencer.github.io/LabNotebook/OA-Day52/) and K on [2/13](https://laurahspencer.github.io/LabNotebook/Sampling-Ks/) using the same protocol as the February sampling dates. All samples were again labeled and housed in Rick's -80 freezer. 

_**Table 3:** Number of each population sampled post-pH treatment; see [20170408_Sampling-Data.xlsx](../../Data/20170408_Sampling-Data.xlsx)_

|                          |            |                       |           |
|--------------------------|------------|-----------------------|-----------|
| **Sample Label**         | **Population** | **Treatment**             | **# Sampled** |
| SN-6-16 to SN-6-24       | SN         | chilled T, low pH     | 9 (3/rep) |
| SN-6-26 to SN-6-33       | SN         | chilled T, ambient pH | 9 (3/rep) |
| SN-10-16 to SN-10-24     | SN         | ambient T, low pH     | 9 (3/rep) |
| SN-10-25 to SN-10-33     | SN         | ambient T, ambient pH | 9 (3/rep) |
| NF-6-16 to NF-6-24       | NF         | chilled T, low pH     | 9 (3/rep) |
| NF-6-25 to NF-6-33       | NF         | chilled T, ambient pH | 9 (3/rep) |
| NF-10-16 to NF-10-24     | NF         | ambient T, low pH     | 9 (3/rep) |
| NF-10-25 to NF-10-33     | NF         | ambient T, ambient pH | 9 (3/rep) |
| HL-6-10 to HL-6-15       | HL         | chilled T, low pH     | 6 (2/rep) |
| HL-6-16 to HL-6-21       | HL         | chilled T, ambient pH | 6 (2/rep) |
| HL-10-10 to HL-10-15     | HL         | ambient T, low pH     | 6 (2/rep) |
| HL-10-15 to HL-10-21     | HL         | ambient T, ambient pH | 6 (2/rep) |
| K-6-16 to K-6-30   | K          | chilled T, low pH     | 15 (5/rep) |
| K-6-31 to K-6-45   | K          | chilled T, ambient pH | 15 (5/rep) |
| K-10-16 to K-10-30 | K          | ambient T, low pH     | 15 (5/rep) |
| K-10-31 to K-10-45 | K          | ambient T, ambient pH | 15 (5/rep) |

---
### Conditioning for Spawn
On [April 11th Olys were moved](https://laurahspencer.github.io/LabNotebook/Conditioning-Olys/) into a 50L flow-through tank equipped with a recirculating heater to condition them for the spring spawn. Temperature was raised ~1degC/day to ~18degC, the target temperature for broodstock conditioning.   

### Larval Collection

### Larval Rearing 

### Juvenile Care 
