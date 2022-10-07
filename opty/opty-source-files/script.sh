#!/bin/bash

sleep_p1=(200)
sleep_p2=(200)
sleep_p3=(200)

#acceptor_delays=(50)
#acceptor_delays=(50 100 150 200 250 300 350 400 450 500 550 600 650 750)
#acceptor_delays=(800 850 900 950 1000 1050 1100 1150 1200 1250 1300 1350 1400)

#acceptor_delays=(50 100 150 200 250 300 350 400 450 500 550 600 650 750 800 850 900 950 1000 1050 1100 1150 1200 1250 1300 1350 1400)
#acceptor_delays=(1600 1800 2000 2200 2400 2600 2800 3000)
#acceptor_delays=(2600 2800 3000 3500 4000 4500 5000 5500)

#acceptor_delays=(1000 100 1050 1100 1150 1200 1250 1300 1350 1400 150 1600 1800 2000 200 2200 2400 250 2600 2800 3000 300 3500 350 4000 400 4500 450 5000 500 50  5500 550 600 650 750 800 850 900 950)
#acceptor_delays=(50 100 150 200 250 300 350 400 450 500 550 600 650 750 800 850 900 950 1000 1050 1100 1150 1200 1250 1300 1350 1400 1600 1800 2000 2200 2400 2600 2800 3000 3500 4000 4500 5000 5500)

acceptor_delays=(1)

drop_probabilities=(5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95)

num_proposers=(1 2 3 4 5 6 7 8)
num_acceptors=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)


#rm exp1/*
erl -make

for i in "${sleep_p1[@]}"; do
    for j in "${sleep_p2[@]}"; do
	for k in "${sleep_p3[@]}"; do
	    for d in "${acceptor_delays[@]}"; do
		#echo $i, $j, $k, $d;
		#echo "export delay=$d; erl -pa ebin -eval paxy:start["$i", "$j", "$k"] > exp1/paxy"$i"_"$j"_"$k"_"$d".out & pid=$!; sleep 20; kill $pid"

		#filename=paxy"$i"_"$j"_"$k"_"$d".out;

		# RUN, GET TIME TO REACH CONSENSUS
		#export delay=$d; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > exp1/$filename & pid=$!; sleep 35; kill $pid
		#result=$(grep "Total" exp1/$filename | awk -F '[:]' '{print $2}')
		#echo $d, $result >> exp1/clean

		# NUMBER OF ROUNDS TO REACH AGREEMENT
		#result=$(grep "DECIDED" exp1/$filename | tail -1)
		#echo $d, $result >> exp1/rounds

		 #NUMBER OF TIMEOUTS
		#filename=paxy"$i"_"$j"_"$k"_"$d".out;
		#result=$(grep "Timed out" nosorry/$filename | wc -l)
		#echo $d, $result >> nosorry/timeouts


		## RUN, GET TIME TO REACH CONSENSUS
		#filename=paxy"$i"_"$j"_"$k"_"$d".out;
		#export delay=$d; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > nosorry2/$filename & pid=$!; sleep 35; kill $pid
		#result=$(grep "Total" nosorry2/$filename | awk -F '[:]' '{print $2}')
		#echo $d, $result >> nosorry2/clean

		for p in "${drop_probabilities[@]}"; do
		    #echo $i, $j, $k, $d, $p;


		    # Drop promises
		    #filename=paxy"$i"_"$j"_"$k"_"$d"_"$p".out;
		    #export delay=$d; export drop=$p; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > droppromise/$filename & pid=$!; sleep 35; kill $pid
		    #result=$(grep "Total" droppromise/$filename | awk -F '[:]' '{print $2}')
		    #echo $p, $result >> droppromise/clean

		    # Drop vote
		    #filename=paxy"$i"_"$j"_"$k"_"$d"_"$p".out;
		    #export delay=$d; export drop=$p; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > dropvote/$filename & pid=$!; sleep 35; kill $pid
		    #result=$(grep "Total" dropvote/$filename | awk -F '[:]' '{print $2}')
		    #echo $p, $result >> dropvote/clean

		    # Drop both
		    #filename=paxy"$i"_"$j"_"$k"_"$d"_"$p".out;
		    #export delay=$d; export drop=$p; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > dropboth/$filename & pid=$!; sleep 35; kill $pid
		    #result=$(grep "Total" dropboth/$filename | awk -F '[:]' '{print $2}')
		    #echo $p, $result >> dropboth/clean

		    # NUMBER OF ROUNDS TO REACH AGREEMENT
		    filename=paxy"$i"_"$j"_"$k"_"$d"_"$p".out;
		    result=$(grep "DECIDED" dropboth/$filename | tail -1)
		    echo $p, $result >> dropboth/rounds

		    result=$(grep "DECIDED" dropvote/$filename | tail -1)
		    echo $p, $result >> dropvote/rounds

		    result=$(grep "DECIDED" droppromise/$filename | tail -1)
		    echo $p, $result >> droppromise/rounds
		done

		#for p in "${num_proposers[@]}"; do
		    #for a in "${num_acceptors[@]}"; do

			#echo $i, $j, $k, $d, $p, $a;
			#filename=paxy"$i"_"$j"_"$k"_"$d"_"$p"_"$a".out;
			#export delay=$d; export proposers=$p; export acceptors=$a; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k, $k, $k, $k, $k, $k])" > exp4/$filename & pid=$!; sleep 15; kill $pid
			#result=$(grep "Total" exp4/$filename | awk -F '[:]' '{print $2}')
			#echo $p, $a, $result >> exp4/clean
		    #done
		#done

		## RUN, GET TIME TO REACH CONSENSUS
		#filename=paxy"$i"_"$j"_"$k"_"$d".out;
		#export delay=$d; erl -noshell -pa ebin -eval "paxy:start([$i, $j, $k])" > improvement/$filename & pid=$!; sleep 35; kill $pid
		#result=$(grep "Total" improvement/$filename | awk -F '[:]' '{print $2}')
		#echo $d, $result >> improvement/clean
	    done

	done
    done
done

#sort -V -t ',' -k1 oldrounds > rounds
# Ordenar csv por valor numérico por la primera columna
