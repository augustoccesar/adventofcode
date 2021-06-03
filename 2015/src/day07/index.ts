import { Task } from '../Task';

export class Day07 extends Task {
    partOne(): string {
        const input = this.readInput();
        const wiresMap = new Map<string, number>();

        const operations: Map<string, boolean> = new Map();
        input.split('\n').forEach(line => operations.set(line, false));

        const REG = {
            INPUT: /^(\d+|\w+) -> (\w+)$/,
            AND: /^(.+) AND (.+) -> (\w+)$/,
            OR: /^(.+) OR (.+) -> (\w+)$/,
            LSHIFT: /^(.+) LSHIFT (.+) -> (\w+)$/,
            RSHIFT: /^(.+) RSHIFT (.+) -> (\w+)$/,
            NOT: /^NOT (.+) -> (\w+)$/,
        }

        let resultLookup = 'a';
        let result = undefined;
        while (!result) {
            for (const [op, executed] of operations.entries()) {
                if (wiresMap.get(resultLookup)) {
                    result = wiresMap.get(resultLookup);
                    break;
                }

                if (executed) {
                    continue;
                }

                let matches;
                if (matches = op.match(REG.INPUT)) {
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

                    if (in1 == undefined || in2 == undefined) {;
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

        return String(result);
    }

    partTwo(): string {
        return String("-");
    }
}

// ---------------------------------------------------------------------------------------------------------------

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