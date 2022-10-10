#!/bin/bash

#num_clients=(1 2 3 4)
#num_clients=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23)
#num_clients=(2 50 100 150)
num_clients=(1 4 16 64 256 1024)
num_entries=(1 11 21 31 41 51 61 71 81 91 101)
num_reads=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
num_writes=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)

read_ratios=(0 0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90 1)
sub_percentage=(0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90 1.0)

total=10



# perl -e 'print int(x*y)'
#read_ratio=5; total=100; perl -le 'print '"$read_ratio"'*'"$total"''
# reads =$(perl -le 'print '"$read_ratio"' * '"$total"' ')
# writes=$(expr $total - $reads)

sleeptime=5
maxtime=4
#sleeptime=10
#maxtime=8

d_entries=10
d_clients=2
d_writes=1
d_reads=1
#rm exp1/*
erl -make

#for clients in "${num_clients[@]}"; do
    #echo $clients, $j, $k, $d;
    #filename=opty"$clients".out;
    #erl -noshell -pa ebin -eval "opty:start($clients, $d_entries, $d_reads, $d_writes, $maxtime)" > numclients/$filename & pid=$!; sleep $sleeptime; kill $pid
    #geomean=$(grep "Mean" numclients/$filename | awk -F '[:]' '{print $2}')
    #stddev=$(grep "Stddev" numclients/$filename | awk -F '[:]' '{print $2}')
    #echo $clients, $geomean, $stddev >> numclients/clean
#done

entries=(1 2 3 4 5 6)
for entries in "${entries[@]}"; do
    for clients in "${num_clients[@]}"; do
	echo $clients, $j, $k, $d;
	filename=opty"$clients".out;
	erl -noshell -pa ebin -eval "opty:start($clients, $entries, $d_reads, $d_writes, $maxtime)" > numclients3/$filename & pid=$!; sleep $sleeptime; kill $pid
	geomean=$(grep "Mean" numclients3/$filename | awk -F '[:]' '{print $2}')
	stddev=$(grep "Stddev" numclients3/$filename | awk -F '[:]' '{print $2}')
	echo $clients, $geomean, $stddev >> numclients3/clean_$entries
    done
done

#for entries in "${num_entries[@]}"; do
    #echo $entries, $j, $k, $d;
    #filename=opty"$entries".out;
    #erl -noshell -pa ebin -eval "opty:start($d_clients, $entries, 10, 10, $maxtime)" > numentries3/$filename & pid=$!; sleep $sleeptime; kill $pid
    #geomean=$(grep "Mean" numentries3/$filename | awk -F '[:]' '{print $2}')
    #stddev=$(grep "Stddev" numentries3/$filename | awk -F '[:]' '{print $2}')
    #echo $entries, $geomean, $stddev >> numentries3/clean
#done

#for reads in "${num_reads[@]}"; do
    #echo $reads, $j, $k, $d;
    #filename=opty"$reads".out;
    #erl -noshell -pa ebin -eval "opty:start($d_clients, $d_entries, $reads, $d_writes, $maxtime)" > numreads/$filename & pid=$!; sleep $sleeptime; kill $pid
    #geomean=$(grep "Mean" numreads/$filename | awk -F '[:]' '{print $2}')
    #stddev=$(grep "Stddev" numreads/$filename | awk -F '[:]' '{print $2}')
    #echo $reads, $geomean, $stddev >> numreads/clean
#done

#for writes in "${num_writes[@]}"; do
    #echo $writes, $j, $k, $d;
    #filename=opty"$writes".out;
    #erl -noshell -pa ebin -eval "opty:start($d_clients, $d_entries, $d_reads, $writes, $maxtime)" > numwrites/$filename & pid=$!; sleep $sleeptime; kill $pid
    #geomean=$(grep "Mean" numwrites/$filename | awk -F '[:]' '{print $2}')
    #stddev=$(grep "Stddev" numwrites/$filename | awk -F '[:]' '{print $2}')
    #echo $writes, $geomean, $stddev >> numwrites/clean
#done

# reads =$(perl -le 'print '"$read_ratio"' * '"$total"' ')
# writes=$(expr $total - $reads)
#for ratio in "${read_ratios[@]}"; do
    #filename=opty"$ratio".out;
    #reads=$(perl -le 'print int('"$ratio"' * '"$total"') ')
    #writes=$(expr $total - $reads)

    #echo $ratio, $writes, $reads, $total;

    #erl -noshell -pa ebin -eval "opty:start($d_clients, $d_entries, $reads, $writes, $maxtime)" > readratio/$filename & pid=$!; sleep $sleeptime; kill $pid
    #geomean=$(grep "Mean" readratio/$filename | awk -F '[:]' '{print $2}')
    #stddev=$(grep "Stddev" readratio/$filename | awk -F '[:]' '{print $2}')
    #echo $ratio, $geomean, $stddev >> readratio/clean
#done

#for percentage in "${sub_percentage[@]}"; do
    #echo $percentage;
    #filename=opty"$percentage".out;
    #export subset_percentage=$percentage; erl -noshell -pa ebin -eval "opty:start(20, $d_entries, $d_reads, $d_writes, $maxtime)" > subset/$filename & pid=$!; sleep $sleeptime; kill $pid
    #geomean=$(grep "Mean" subset/$filename | awk -F '[:]' '{print $2}')
    #stddev=$(grep "Stddev" subset/$filename | awk -F '[:]' '{print $2}')
    #echo $percentage, $geomean, $stddev >> subset/clean
#done

#sort -V -t ',' -k1 oldrounds > rounds
# Ordenar csv por valor numérico por la primera columna
