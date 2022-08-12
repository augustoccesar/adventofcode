import { Task } from '../Task';

export class Day10 extends Task {
    partOne(): string {
        const input = this.readInput().split("");
        const result = runGame(input, 40);

        return result.length.toString();
    }

    partTwo(): string {
        const input = this.readInput().split("");
        const result = runGame(input, 50);

        return result.length.toString();
    }
}

function runGame(chars: string[], times: number): string[] {
    let latestChars = chars;
    for(let n = 0; n < times; n++) {
        const newChars = [];
        for(let i = 0; i < latestChars.length;) {
            const value = latestChars[i];
    
            if(i == latestChars.length - 1) {
                newChars.push("1");
                newChars.push(value);
                i++;
                continue;
            }
    
            let j = 1;
            while(latestChars[i + j] == value) {
                j += 1;
            }
    
            newChars.push(j.toString());
            newChars.push(value);
            i += j;
        }
        
        latestChars = newChars;
    }

    return latestChars;
}
