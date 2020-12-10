#!/usr/bin/env bash


# files='*.TXT'
files='/Users/Rich_Yaxley/Desktop/E*.TXT'

for i in $files; do 

	echo $i; 
	# Remove Windows line return
	tr '\r' '\n' < $i > temp.txt 
	# Remove lines containing "Light Blocked"
	grep -v '^Light Blocked$' temp.txt > temp2.txt
	# Remove all blank spaces
	tr -d ' ' < temp2.txt > temp3.txt
	
	mv temp3.txt $i

done

rm temp.txt temp2.txt