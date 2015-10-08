#!/bin/bash

export CLASSPATH=/home/drg/WekaGP/wekaGP.jar

alg=GP
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


#

for i in `seq 1 3`; do
  echo "Starting KFold: "$i
  firstTime=1


#####################################
#java -cp /home/drg/wekaGP/weka.jar weka.classifiers.functions.GeneticProgramming -pop 1000 -no -func +,-,/,*,Pow,Exp,Log -sa 1000 -t china3AttSelectedAFPTrain.arff -T china3AttSelectedAFPTest.arff

for com in 100 200 400
do
  for pop in 75 100 
  do
     # Next line was to be used with 
     # outfileTemp=./$outDir/${infile%.*}"com"$com
     #
     outfileTemp=./$outDir/temp"Com"$com"Pop"$pop
     echo "OutputTemp: "$outfileTemp
     echo "Train: "$infileTrain$i".arff"
     echo "Test: "$infileTest$i".arff"
     java -cp $CLASSPATH weka.classifiers.functions.GeneticProgramming \
	  -pop $pop -no -func +,-,/,*,Pow,Exp,Log -sa $pop \
          -com 0.9 $com 0.33 \
	  -t $infileTrain$i".arff" \
	  -T $infileTest$i".arff" \
	  -p 0 > $outfileTemp".csv"
     # There is no classification option with the original wekaGP (very old version)
     #   sed '1,5d' $outfileTemp > $outfileTemp".csv"	
     #
     # Add actual and expected values if it's the first cycle, otherwise only expected values
     if [ $firstTime -eq 1 ]; then
        echo "Fist time solution file"
        # actual
	cut -d" " -f3 -s $outfileTemp".csv" > ./$outDir/resultsAll.csv
        # 1st estimate        
        cut -d" " -f2 -s $outfileTemp".csv" > $outfileTemp"merge.csv"
 	paste -d, ./$outDir/resultsAll.csv  $outfileTemp"merge.csv" > ./$outDir/temp.csv
	rm ./$outDir/resultsAll.csv
	mv ./$outDir/temp.csv  ./$outDir/resultsAll.csv
    	rm $outfileTemp"merge.csv"
        let firstTime=0
     else
	cut -d" " -f2 -s $outfileTemp".csv" > $outfileTemp"merge.csv"
 	paste -d, ./$outDir/resultsAll.csv  $outfileTemp"merge.csv" > ./$outDir/temp.csv
	rm ./$outDir/resultsAll.csv
	mv ./$outDir/temp.csv  ./$outDir/resultsAll.csv
    	rm $outfileTemp"merge.csv"
     fi
     # 
     #java -cp $CLASSPATH weka.classifiers.functions.GeneticProgramming \
     #	  -pop $pop -no -func +,-,/,*,Pow,Exp,Log -sa $pop \
     #    -com 0.9 $com 0.33 \
     # 	  -t $infileTrain \
     #	  -T $infileTest \
     #    -o > $outfileTemp".stats"
     #
     rm ${outfileTemp}*
  done
done

  echo "Finished kFold: "$i
  mv ./$outDir/resultsAll.csv ./$outDir/$outDir"KF"${i}".w2r"
done

