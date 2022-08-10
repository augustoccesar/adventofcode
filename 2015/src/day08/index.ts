import { Task } from '../Task';

export class Day08 extends Task {
    partOne(): string {
        const data = this.readInput().split("\n");
        let codeChars = 0;
        let memoryChars = 0;

        data.forEach(item => {
            codeChars += item.length;
            memoryChars += eval(item).length;
        });

        return (codeChars - memoryChars).toString();
    }

    partTwo(): string {
        return "-";
    }
}
