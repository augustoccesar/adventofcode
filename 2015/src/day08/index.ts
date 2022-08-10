import { Task } from '../Task';

export class Day08 extends Task {
    partOne(): string {
        const data = this.readInput().split("\n");
        const result = calculate(data);

        return result.toString();
    }

    partTwo(): string {
        const data = this.readInput().split("\n").map(item => encode(item));
        const result = calculate(data);

        return result.toString();
    }
}

function encode(str: string): string {
    return "\"" + str.replace(/\\/g, "\\\\").replace(/\"/g, "\\\"") + "\""
}

function calculate(data: string[]): number {
    let codeChars = 0;
    let memoryChars = 0;

    data.forEach(item => {
        codeChars += item.length;
        memoryChars += eval(item).length;
    });

    return (codeChars - memoryChars);
}
