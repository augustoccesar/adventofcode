export abstract class Day {
  year: number;
  day: number;

  abstract partOne(): string;
  abstract partTwo(): string;

  constructor(year: number, day: number) {
    this.year = year;
    this.day = day;
  }

  protected readInput(suffix?: string): string {
    const path = `../inputs/${this.year}_${
      this.day.toString().padStart(2, "0")
    }${suffix ? `_${suffix}` : ""}.txt`;

    return Deno.readTextFileSync(path);
  }
}

const DAY_REGISTRY: Map<string, new (year: number, day: number) => Day> =
  new Map();

export function getDay(year: number, day: number): Day | undefined {
  const DayConstructor = DAY_REGISTRY.get(`${year}-${day}`);
  if (DayConstructor == undefined) return undefined;

  return new DayConstructor(year, day);
}

export function RegisterDay(year: number, day: number) {
  return function <T extends new (year: number, day: number) => Day>(
    constructor: T,
  ) {
    DAY_REGISTRY.set(`${year}-${day}`, constructor);

    return constructor;
  };
}
