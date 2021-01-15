import { Day01 } from "./day01";
import { Day02 } from "./day02";
import { Day03 } from "./day03";
import { Day04 } from "./day04";
import { Task } from "./Task";

type NullableTask = Task | null;

if (process.argv.length < 3) {
    console.error("Invalid amount of arguments");
    process.exit(1);
}

const dayInt = parseInt(process.argv[2]);
let day: NullableTask = null;

switch (dayInt) {
    case 1: day = new Day01(); break;
    case 2: day = new Day02(); break;
    case 3: day = new Day03(); break;
    case 4: day = new Day04(); break;
}

if (day == null) {
    console.error("Day not found");
    process.exit(1);
}

console.log(`Part One: ${day.partOne()}`);
console.log(`Part Two: ${day.partTwo()}`);
