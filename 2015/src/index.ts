import { Day01 } from "./day01";
import { Task } from "./Task";

type NullableTask = Task | null;

if (process.argv.length < 3) {
    console.error("Invalid amount of arguments");
    process.exit(1);
}

const dayInt = parseInt(process.argv[2]);
let day: NullableTask = null;

switch (dayInt) {
    case 1:
        day = new Day01();
        break;
}

if (day == null) {
    console.error("Day not found");
    process.exit(1);
}

console.log(`Part One: ${day.partOne()}`);
console.log(`Part Two: ${day.partTwo()}`);
