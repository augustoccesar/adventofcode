import { Task } from "../Task";

export class Day05 extends Task {
    partOne(): String {
        let naughty_str: Array<string> = ["ab", "cd", "pq", "xy"];
        let vowels: Array<string> = ["a", "e", "i", "o", "u"];
        let niceWords: number = 0;

        for (let str of this.readInput().split("\n")) {
            if (naughty_str.filter(item => str.indexOf(item) != -1).length > 0) {
                continue;
            }

            let vowelsCount = 0;
            let containsConsecutive = false;
            for (let i = 0; i < str.split("").length; i++) {
                if (vowels.includes(str[i])) { vowelsCount++; }

                if (i != 0 && !containsConsecutive) {
                    if (str[i] == str[i - 1]) { containsConsecutive = true; }
                }
            }

            if (vowelsCount >= 3 && containsConsecutive) {
                niceWords++;
            }
        }

        return niceWords.toString();
    }

    partTwo(): String {
        let niceWords: number = 0;

        for (let str of this.readInput().split("\n")) {
            let check_1 = false;
            let check_2 = false;
            for (let i = 0; i < str.split("").length; i++) {
                if (i == 0) { continue; }

                const repeat = str.match(new RegExp(str.substr(i - 1, 2), "g"));
                if (repeat != null && repeat.length > 1) {
                    check_1 = true;
                }

                if (i > 1 && str[i] == str[i - 2]) {
                    check_2 = true;
                }
            }

            if (check_1 && check_2) {
                niceWords++;
            }
        }

        return niceWords.toString();
    }
}
