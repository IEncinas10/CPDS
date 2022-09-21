#!/bin/bash

sleep_p1=(200)
sleep_p2=(200)
sleep_p3=(200)

#acceptor_delays=(50)
#acceptor_delays=(50 100 150 200 250 300 350 400 450 500 550 600 650 750)
#acceptor_delays=(800 850 900 950 1000 1050 1100 1150 1200 1250 1300 1350 1400)

#acceptor_delays=(50 100 150 200 250 300 350 400 450 500 550 600 650 750 800 850 900 950 1000 1050 1100 1150 1200 1250 1300 1350 1400)
#acceptor_delays=(1600 1800 2000 2200 2400 2600 2800 3000)
acceptor_delays=(2600 2800 3000 3500 4000 4500 5000 5500)

#rm exp1/*

for i in "${sleep_p1[@]}"; do
    for j in "${sleep_p2[@]}"; do
	for k in "${sleep_p3[@]}"; do
	    for d in "${acceptor_delays[@]}"; do
		#echo $i, $j, $k, $d;
		#echo "export delay=$d; erl -pa ebin -eval paxy:start["$i", "$j", "$k"] > exp1/paxy"$i"_"$j"_"$k"_"$d".out & pid=$!; sleep 20; kill $pid"

		filename=paxy"$i"_"$j"_"$k"_"$d".out;
		export delay=$d; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > exp1/$filename & pid=$!; sleep 35; kill $pid

		result=$(grep "Total" exp1/$filename | awk -F '[:]' '{print $2}')
		if [ -n "$result" ]; then
		    result=35000
		fi
		echo $d, $result >> exp1/clean
		#awk -F'[:]' '{print $2}' paxy200_200_200_50.out

		#export delay=$d; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > exp1/$filename;
	    done

	done
    done
done
