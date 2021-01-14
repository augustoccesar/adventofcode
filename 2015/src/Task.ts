import { readFileSync } from "fs";

export abstract class Task {
    abstract partOne(): String;
    abstract partTwo(): String;

    protected readInput(suffix: String = "input"): String {
        const path = `./inputs/${this.constructor.name.toLowerCase()}_${suffix}.txt`
        return readFileSync(path, 'utf-8');
    }
}
