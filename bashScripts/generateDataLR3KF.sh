#!/bin/bash

export CLASSPATH=/home/drg/weka/weka/weka.jar
alg=LR
echo $alg

EXPECTED_ARGS=1
E_BADARGS=65

if [ $# -ne $EXPECTED_ARGS ]
then
	echo "Usage: `basename $0`  inputFile (without .arff)"
  exit $E_BADARGS
fi

#infile=$1
#echo $infile
#echo 

infileTrain=$1"_train"
infileTest=$1"_test"
#infile=./data/
echo "File training: "$infileTrain
echo "File testing: "$infileTest 

outDir=$1$alg      # before as argument $2
#"out"$alg${infile%.*} Now we input this value too (we dont just remove the .arff extension)
echo "Directory: "$outDir

#firstTime=1 

if [ ! -d $outDir ]; then
    echo "Creating Directory"
    mkdir $outDir
fi

for i in `seq 1 3`; do
  echo "Starting KFold: "$i
  firstTime=1

for S in 0 1 2
do
  for R in 1.0E-8 0.1 
    do
     echo "Output: "$outfileTemp
     echo "Train: "$infileTrain$i".arff"
     echo "Test: "$infileTest$i".arff"
     outfileTemp=./$outDir/temp"S"$S
     echo "Temp: "$outfileTemp
     java -cp $CLASSPATH weka.classifiers.functions.LinearRegression \
	  -S $S  \
          -R $R \
	  -t $infileTrain$i".arff" \
	  -T $infileTest$i".arff" \
	  -classifications weka.classifiers.evaluation.output.prediction.CSV > $outfileTemp
     sed '1,5d' $outfileTemp > $outfileTemp".csv"	
     #
     # Add actual and expected if it is the first one, otherwise we only need to add the expected values column
     if [ $firstTime -eq 1 ]; then
        echo "Fist time solution file"
	cut -d, -f2,3 -s $outfileTemp".csv" > ./$outDir/resultsAll.csv
        let firstTime=0
     else
	cut -d, -f3 -s $outfileTemp".csv" > $outfileTemp"merge.csv"
 	paste -d, ./$outDir/resultsAll.csv  $outfileTemp"merge.csv" > ./$outDir/temp.csv
	rm ./$outDir/resultsAll.csv
	mv ./$outDir/temp.csv  ./$outDir/resultsAll.csv
    	rm $outfileTemp"merge.csv"
     fi
     # 
     #java -cp $CLASSPATH weka.classifiers.functions.LinearRegression \
	#  -S $S  \
        #  -R $R \
	#  -t $infileTrain \
	#  -T $infileTest \
        #  -o > $outfileTemp".stats"
     rm ${outfileTemp}*
  done
done

  echo "Finished kFold: "$i
  mv ./$outDir/resultsAll.csv ./$outDir/$outDir"KF"${i}".w2r"
done

