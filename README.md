# Evaluation of Estimation Models using the Minimum Interval of Equivalence #

Replication package for the paper "Evaluation of Estimation Models using the Minimum Interval of Equivalence"
by Javier Dolado, Daniel Rodriguez, Mark Harman, Bill Langdon and Federica Sarro



## Data ##
Datasets are all publicly available form the [PROMISE respository] (http://openscience.us/repo/ "PROMISE repository") or the literature with the exception of the except the ISBSG dataset (http://www.isbsg.org/ "ISBSG"). In particular, we used:

1. [CSC dataset] (./datasets/kitchenham.arff)
2. [China dataset] (./datasets/china.arff)
3. [Maxwell dataset] (./datasets/maxwell.arff)
4. [Desharnais] (./datasets/desharnais.arff)
5. ISBSG
6. Atkinson and Telecom1 datasets


## Weka and R scripts ##

Weka was used to generate the data and R for the rest. The following scripts in both Weka and R need to be modified adjusting the directories were data is retrieved or generated. 

### Weka scripts ###

[Weka] (http://www.cs.waikato.ac.nz/ml/weka/) was used to create folds for training and testing files. A set of files (one file per machine learning technique and fold) with multiple estimates was obtained through varying the algorithm parameters. The generated structed is as the following table:

|                |             |     |              |
| --------------:|------------:| ----|-------------:|
| ActualValue1   | Estimate1,2 | ... | Estimate1,N  |
| ActualValue2   | Estimate1,2 | ... | Estimate1,N  |
| ...            | ...         | ... | ...          |
| ActualValueM   | EstimateM,2 | ... | EstimateM,N  |

These files use the extension w2r (as they will be used by the R script)

Bash scripts that make use of Weka:

1. [divideData.sh] (./bashScripts/divideData.sh) for creating 3 training and test sets using stratified folds.
2. [bashScripts.zip] (./bashScripts/divideData.sh) to generate the files fir the aforementioned format and w2r extension.
3. [runAll3KF.sh] (./bashScripts/divideData.sh) to run all generateData scripts
4. [moveToDir.sh] (./bashScripts/divideData.sh) to move all the w2r estimate files to the input folder for the R script.
 

Some techniques are needed that can be instaled through Weka's package manager. In particular we used the following algorithms covering different types of machine learning techniques: 

1. OLR 
2. LSM
3. REPTree
4. M5P
5. MLP

We also used a Genetic Programming package. We created a zip file that can be loaded into Weka or use an old [Weka GP project] (http://www.leyan.org/Genetic+Programming) (by Yan Levasseur)

### R Scripts ###

[R script] (./RScripts/computeMierCi.R) 

R is used for for everything else. It automatically generates Latex tables and figures shown in the paper. In order to replicate the results, it requires to update the directories and have installed the following R packages:
 
1. boot for bootstrapping
2. xtable for exporting data.frames to latex
3. bayesm
4. mcmc
5. mcmcplots
6. coda
7. LaplacesDemon
