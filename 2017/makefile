.SILENT:
run:
	DAY=$(shell printf "%.2d" $(day)) ;\
	cd day$$DAY && mkdir -p bin && g++ -std=c++17 day$$DAY.cpp -o bin/day$$DAY && bin/day$$DAY ;\
