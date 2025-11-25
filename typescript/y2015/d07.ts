import { Day, RegisterDay } from "../Day.ts";

@RegisterDay(2015, 7)
export class Day07 extends Day {
  override partOne(): string {
    const input = this.readInput();
    const wiresMap = new Map<string, number>();

    const operations: Map<string, boolean> = new Map();
    input.split('\n').forEach(line => operations.set(line, false));

    const result = execute(operations, wiresMap, 'a');

    return String(result);
  }

  override partTwo(): string {
    const input = this.readInput();
    const wiresMap = new Map<string, number>();

    const operations: Map<string, boolean> = new Map();
    input.split('\n').forEach(line => operations.set(line, false));

    let result = execute(operations, wiresMap, 'a');

    // Rewire 'b' and reset all other wires
    wiresMap.clear();
    wiresMap.set('b', result);

    for (const key of operations.keys()) {
      operations.set(key, false);
    }

    result = execute(operations, wiresMap, 'a');

    return String(result);
  }
}

const REG = {
  INPUT: /^(\d+|\w+) -> (\w+)$/,
  AND: /^(.+) AND (.+) -> (\w+)$/,
  OR: /^(.+) OR (.+) -> (\w+)$/,
  LSHIFT: /^(.+) LSHIFT (.+) -> (\w+)$/,
  RSHIFT: /^(.+) RSHIFT (.+) -> (\w+)$/,
  NOT: /^NOT (.+) -> (\w+)$/,
}

function execute(operations: Map<string, boolean>, wiresMap: Map<string, number>, output: string): number {
  let result = undefined;
  while (!result) {
    for (const [op, executed] of operations.entries()) {
      if (wiresMap.get(output)) {
        result = wiresMap.get(output);
        break;
      }

      if (executed) {
        continue;
      }

      let matches;
      if (matches = op.match(REG.INPUT)) {
        if (wiresMap.get(matches[2])) {
          // Value is already set, so skip (important to not "override an override" like on day 2)
          operations.set(op, true);
          continue;
        }

        const in1 = evaluateInput(matches[1], wiresMap)
        if (in1 == undefined) {
          continue;
        }

        wiresMap.set(matches[2], conv16Bit(in1));
        operations.set(op, true);
        continue;
      }

      if (matches = op.match(REG.AND)) {
        const in1 = evaluateInput(matches[1], wiresMap);
        const in2 = evaluateInput(matches[2], wiresMap);
        const out = matches[3];

        if (!in1 || !in2) {
          continue;
        }

        wiresMap.set(out, conv16Bit(in1 & in2));
        operations.set(op, true);
        continue;
      }

      if (matches = op.match(REG.OR)) {
        const in1 = evaluateInput(matches[1], wiresMap);
        const in2 = evaluateInput(matches[2], wiresMap);
        const out = matches[3];

        if (in1 == undefined || in2 == undefined) {
          continue;
        }

        wiresMap.set(out, conv16Bit(in1 | in2));
        operations.set(op, true);
        continue;
      }

      if (matches = op.match(REG.LSHIFT)) {
        const in1 = evaluateInput(matches[1], wiresMap);
        const in2 = evaluateInput(matches[2], wiresMap);
        const out = matches[3];

        if (in1 == undefined || in2 == undefined) {
          continue;
        }

        wiresMap.set(out, conv16Bit(in1 << in2));
        operations.set(op, true);
        continue;
      }

      if (matches = op.match(REG.RSHIFT)) {
        const in1 = evaluateInput(matches[1], wiresMap);
        const in2 = evaluateInput(matches[2], wiresMap);
        const out = matches[3];

        if (in1 == undefined || in2 == undefined) {
          ;
          continue;
        }

        wiresMap.set(out, conv16Bit(in1 >>> in2));
        operations.set(op, true);
        continue;
      }

      if (matches = op.match(REG.NOT)) {
        const in1 = evaluateInput(matches[1], wiresMap);
        const out = matches[2];

        if (in1 == undefined) {
          continue;
        }

        wiresMap.set(out, conv16Bit(~in1));
        operations.set(op, true);
        continue;
      }
    }
  }

  return result;
}

function evaluateInput(input: string, wiresMap: Map<string, number>): number | undefined {
  const numeric = Number(input);

  if (!isNaN(numeric)) {
    return numeric;
  }

  return wiresMap.get(input);
}

function conv16Bit(num: number): number {
  return (num << 16) >>> 16;
}

function dec2bin(dec: number) {
  return (dec >>> 0).toString(2);
}
