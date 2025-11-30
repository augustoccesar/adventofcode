import { Day, RegisterDay } from "../Day.ts";

@RegisterDay(2015, 2)
export class Day02 extends Day {
  override partOne(): string {
    const input: String = this.readInput();
    const requests: number[][] = input.split("\n").map((x) =>
      x.split("x").map((y) => parseInt(y))
    );
    let total: number = 0;

    for (let request of requests) {
      let [l, w, h] = request;
      const surface = (2 * l * w) + (2 * w * h) + (2 * h * l);

      let slack = request.sort((a, b) => a - b).slice(0, 2).reduce((a, b) =>
        a * b
      );

      const requestTotal = surface + slack;

      total += requestTotal;
    }

    return total.toString();
  }

  override partTwo(): string {
    const input: String = this.readInput();
    const requests: number[][] = input.split("\n").map((x) =>
      x.split("x").map((y) => parseInt(y))
    );
    let total: number = 0;

    for (let request of requests) {
      let ribbonWrap = request.sort((a, b) =>
        a - b
      ).slice(0, 2).reduce((a, b) => a + b) * 2;
      let ribbonBow = request.reduce((a, b) => a * b);

      total += ribbonWrap + ribbonBow;
    }

    return total.toString();
  }
}
