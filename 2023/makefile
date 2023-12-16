.SILENT:
run:
	DAY=$(shell printf "%.2d" $(day)) ;\
	 cargo build -q --release --bin day$$DAY && ./target/release/day$$DAY
