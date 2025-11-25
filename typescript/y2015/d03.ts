import { Day, RegisterDay } from "../Day.ts";

@RegisterDay(2015, 3)
export class Day03 extends Day {
  override partOne(): string {
    const input: String = this.readInput();
    let pos: [number, number] = [0, 0];
    let log: Map<String, number> = new Map();
    log.set(toKey(pos), 1);

    for (let item of input.split("")) {
      const modifier = modifierMap.get(item);
      if (modifier == undefined) {
        throw new Error("Encountered an invalid item on input");
      }

      const newPos: [number, number] = [
        pos[0] + modifier[0],
        pos[1] + modifier[1]
      ];

      pos = moveAndLog(pos, modifier, log);
    }

    return log.size.toString();
  }

  override partTwo(): string {
    const input: String = this.readInput();
    let pos: [number, number] = [0, 0];
    let roboPos: [number, number] = [0, 0];

    let log: Map<String, number> = new Map();
    log.set(toKey(pos), 2);

    const inputItems = input.split("");
    for (let i = 0; i < inputItems.length; i++) {
      const modifier = modifierMap.get(inputItems[i]);
      if (modifier == undefined) {
        throw new Error("Encountered an invalid item on input");
      }

      if (i % 2 == 0) {
        pos = moveAndLog(pos, modifier, log);
      } else {
        roboPos = moveAndLog(roboPos, modifier, log);
      }
    }

    return log.size.toString();
  }
}

const modifierMap: Map<String, [number, number]> = new Map([
  ["^", [0, 1]],
  [">", [1, 0]],
  ["v", [0, -1]],
  ["<", [-1, 0]],
]);

function toKey(pos: [number, number]): String {
  return `${pos[0]},${pos[1]}`;
}

function moveAndLog(
  pos: [number, number],
  modifier: [number, number],
  log: Map<String, number>
): [number, number] {
  const newPos: [number, number] = [
    pos[0] + modifier[0],
    pos[1] + modifier[1]
  ];

  const currentValue = log.get(toKey(newPos));
  if (currentValue == undefined) {
    log.set(toKey(newPos), 1);
  } else {
    log.set(toKey(newPos), currentValue + 1);
  }

  return newPos;
}
