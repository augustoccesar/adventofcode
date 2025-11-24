import { Day, RegisterDay } from "../Day.ts";

@RegisterDay(2015, 1)
export class Day01 extends Day {
  override partOne(): string {
    const input = this.readInput();
    let floor: number = 0;

    input.split("").forEach(item => item == '(' ? floor++ : floor--);

    return floor.toString();
  }

  override partTwo(): string {
    const input = this.readInput();
    let floor: number = 0;
    let pos: number = 1

    for (const item of input.split("")) {
      item == '(' ? floor++ : floor--
      if (floor == -1) {
        break;
      }

      pos++;
    }

    return pos.toString();
  }
}
