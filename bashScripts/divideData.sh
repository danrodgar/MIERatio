#!/bin/bash

export CLASSPATH=$CLASSPATH:/home/drg/weka/weka/weka.jar

# command-line args
if [ -z "$1" ]; then
	echo usage: $0 dataset_infile
	exit
fi
infile=$1
echo "Infile: " $infile

# target directory
# rm -rf $infile
# mkdir $infile
# echo "\nAll output will be saved in directory ./"$infile

# partition dataset in 2 (train 90% + test 10%) * 10 folds (10CV)
# folds_dir=$infile"/folds"
# mkdir $folds_dir

# test_fold_dir=$folds_dir"/test"
# mkdir $test_fold_dir
# echo $test_fold_dir

# train_fold_dir=$folds_dir"/train"
# mkdir $train_fold_dir
# echo $train_fold_dir

# base_output_dir=$infile"/output"
# mkdir $base_output_dir
# echo $base_output_dir

for i in `seq 1 3`; do
	test_outfile=$infile"_test"$i".arff"
	echo $test_outfile

	java -cp $CLASSPATH weka.filters.supervised.instance.StratifiedRemoveFolds -i $infile".arff"  -c "last" -N 3 -F $i -S 0 -o $test_outfile
	# echo "\nSaved test folds: "$test_outfile
	
	train_outfile=$infile"_train"$i".arff"
	echo $train_outfile
	java -cp $CLASSPATH weka.filters.supervised.instance.StratifiedRemoveFolds -i $infile".arff"  -c "last" -V -N 3 -F $i -S 0 -o $train_outfile
	# echo "Saved train folds: "$train_outfile
done

# For each individual fold
# for i in `seq 1 10`; do
	# echo "\nFold "$i
	# fold_output_dir=$base_output_dir"/fold_"$i
	# mkdir $fold_output_dir
	# train_outfile=$train_fold_dir"/"$infile"_train"$i".arff"
	# test_outfile=$test_fold_dir"/"$infile"_test"$i".arff"
	# attr_sel_output_file=$fold_output_dir"/"$infile"_attr_sel_output"$i".txt"
	# classification_output_file=$fold_output_dir"/"$infile"_classification_output"$i".txt"
	# echo "Performing attribute selection on: "$train_outfile
	# java -cp $CLASSPATH weka.attributeSelection.SVMAttributeEval -s "weka.attributeSelection.Ranker -T -1.7976931348623157E308 -N -1" -i $train_outfile -X 1 -Y 0 -Z 0 -P 1.0E-25 -T 1.0E-10 -C 1.0 -N 0 > $attr_sel_output_file
	# for num_attrs in `seq 16 -1 1`; do # Leave One Feature Out (LOFO) process
	# 	echo "Performing classification on: "$test_outfile" with "$num_attrs" features and model learned from "$train_outfile
	# 	echo "Performing classification on: "$test_outfile" with "$num_attrs" features and model learned from "$train_outfile >> $classification_output_file
	# 	java -cp $CLASSPATH weka.classifiers.meta.AttributeSelectedClassifier -t $train_outfile -T $test_outfile -o -i -E "weka.attributeSelection.SVMAttributeEval -X 1 -Y 0 -Z 0 -P 1.0E-25 -T 1.0E-10 -C 1.0 -N 0" -S "weka.attributeSelection.Ranker -T -1.7976931348623157E308 -N "$num_attrs -W weka.classifiers.bayes.NaiveBayes -- >> $classification_output_file
	# done
# done

