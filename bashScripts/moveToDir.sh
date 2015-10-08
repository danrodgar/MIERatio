#!/bin/bash

to=~/Dropbox/dhl/DataResults3KF

for i in */; 
do 
  cp "$i"*.w2r $to
done

