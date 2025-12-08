import { Command } from "@cliffy/command";

import { getDay, getDaysRegistry } from "./Day.ts";

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

    const partOneStart = Temporal.Now.instant();
    const partOneResult = dayInstance.partOne();
    const partOneEnd = Temporal.Now.instant();
    console.log(`${partOneResult};${partOneEnd.epochNanoseconds - partOneStart.epochNanoseconds}`);

    const partTwoStart = Temporal.Now.instant();
    const partTwoResult = dayInstance.partTwo();
    const partTwoEnd = Temporal.Now.instant();
    console.log(`${partTwoResult};${partTwoEnd.epochNanoseconds - partTwoStart.epochNanoseconds}`);
  });

const daysCommand = new Command()
  .name("days")
  .description("list available years and its days")
  // deno-lint-ignore no-explicit-any
  .action((_options: any) => {
    const daysRegistry = getDaysRegistry();

    const yearDays = Array.from(daysRegistry.keys()).map((key) =>
      key.split("-")
    ).reduce((acc, [year, day]) => {
      if (!acc[year]) {
        acc[year] = [];
      }
      acc[year].push(day);
      return acc;
    }, {} as Record<string, string[]>);

    for (const [year, days] of Object.entries(yearDays)) {
      console.log(`${year};${days.join(";")}`)
    }
  });

const cli = new Command()
  .name("aoc-ts")
  .version("0.1.0")
  .command("run", runCommand)
  .command("days", daysCommand);

if (import.meta.main) {
  cli.parse(Deno.args);
}
