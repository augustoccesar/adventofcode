import md5 from "md5";

import { Day, RegisterDay } from "../Day.ts";

@RegisterDay(2015, 4)
export class Day04 extends Day {
  override partOne(): string {
    const secret = this.readInput();
    let result: number = lookup(secret, 5);

    return result.toString();
  }

  override partTwo(): string {
    const secret = this.readInput();
    let result: number = lookup(secret, 6);

    return result.toString();
  }
}

function lookup(secret: String, leading_zeros: number) {
  let result: number = -1;

  for (let i = 1; result == -1; i++) {
    const md = md5(`${secret}${i}`);

    if (md.slice(0, leading_zeros).split("").every(item => item == "0")) {
      result = i;
    }
  }

  return result;
}

function toHex(str: String): String {
  var hex, i;

  var result = "";
  for (i = 0; i < str.length; i++) {
    hex = str.charCodeAt(i).toString(16);
    result += ("000" + hex).slice(-4);
  }

  return result
}
