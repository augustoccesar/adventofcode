import { Command } from "@cliffy/command";

import { getDay } from "./Day.ts";

import "./y2015/d01.ts";
import "./y2015/d02.ts";
import "./y2015/d03.ts";
import "./y2015/d04.ts";
import "./y2015/d05.ts";
import "./y2015/d06.ts";
import "./y2015/d07.ts";
import "./y2015/d08.ts";
import "./y2015/d09.ts";
import "./y2015/d10.ts";
// CODEGEN:import_day

const runCommand = new Command()
  .name("run")
  .description("run a specific day of a specific year")
  .arguments("<year:number> <day:number>")
  // deno-lint-ignore no-explicit-any
  .action((_options: any, year: number, day: number) => {
    const dayInstance = getDay(year, day);
    if (dayInstance == undefined) {
      console.error(`Day ${day} not found for year ${year}`);
      Deno.exit(1);
    }

    console.log(dayInstance.partOne());
    console.log(dayInstance.partTwo());
  });

const cli = new Command()
  .name("aoc-ts")
  .version("0.1.0")
  .command("run", runCommand);

if (import.meta.main) {
  cli.parse(Deno.args);
}
