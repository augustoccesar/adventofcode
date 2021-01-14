import { Task } from "../Task";

export class Day01 extends Task {
    partOne(): String {
        const input = this.readInput();
        let floor: number = 0;

        input.split("").forEach(item => item == '(' ? floor++ : floor--);

        return floor.toString();
    }
    partTwo(): String {
        const input = this.readInput();
        let floor: number = 0;
        let pos: number = 0

        for (let item of input.split("")) {
            item == '(' ? floor++ : floor--
            if (floor == -1) {
                break;
            }

            pos++;
        }

        return pos.toString();
    }
}
