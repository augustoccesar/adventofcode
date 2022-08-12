import { Day01 } from "./day01";
import { Day02 } from "./day02";
import { Day03 } from "./day03";
import { Day04 } from "./day04";
import { Day05 } from "./day05";
import { Day06 } from "./day06";
import { Day07 } from "./day07";
import { Day08 } from "./day08";
import { Day09 } from "./day09";
import { Day10 } from "./day10";
//SETUP:target_import
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
    case 5: day = new Day05(); break;
    case 6: day = new Day06(); break;
    case 7: day = new Day07(); break;
    case 8: day = new Day08(); break;
	case 9: day = new Day09(); break;
	case 10: day = new Day10(); break;
//SETUP:target_switch
}

if (day == null) {
    console.error("Day not found");
    process.exit(1);
}

new Map([
    ["One", () => {
        return day!.partOne()
    }],
    ["Two", () => {
        return day!.partTwo()
    }],
]).forEach((value, key) => {
    const startTime = Date.now();
    const result = value();
    const endTime = Date.now();

    console.log(`Part ${key}: ${result} (took: ${formatMs(endTime.valueOf() - startTime.valueOf())})`);
})

function formatMs(ms: number): string {
    const minutes = Math.floor(ms / 60000);
    const seconds = ms / 1000.0;

    const secondsStr = seconds < 10
        ? "0" + String(seconds.toPrecision(7))
        : String(seconds.toPrecision(7));
    const minutesStr = minutes < 10 ? "0" + String(minutes) : String(minutes);

    return `${minutesStr}:${secondsStr}`;
}
