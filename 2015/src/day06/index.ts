import { Task } from '../Task';

export class Day06 extends Task {
    partOne(): string {
        const lights: Map<string, boolean> = new Map();
        const commands = parseInput(this.readInput());

        for (const command of commands) {
            const points = pointsInRange(command.range);
            for (const point of points) {
                if (!lights.has(point.id)) {
                    lights.set(point.id, false);
                }

                const currentVal = lights.get(point.id);
                if (command.type == CommandType.TOGGLE) {
                    lights.set(point.id, !currentVal);
                } else if (command.type == CommandType.TURN_ON) {
                    lights.set(point.id, true);
                } else if (command.type == CommandType.TURN_OFF) {
                    lights.set(point.id, false);
                }
            }
        }

        let lightsOn = 0;
        lights.forEach((value: boolean) => {
            if (value) {
                lightsOn++;
            }
        });

        return String(lightsOn);
    }

    partTwo(): string {
        const lights: Map<string, number> = new Map();
        const commands = parseInput(this.readInput());

        for (const command of commands) {
            const points = pointsInRange(command.range);
            for (const point of points) {
                const currentVal = lights.get(point.id) || 0;
                if (command.type == CommandType.TOGGLE) {
                    lights.set(point.id, currentVal + 2);
                } else if (command.type == CommandType.TURN_ON) {
                    lights.set(point.id, currentVal + 1);
                } else if (command.type == CommandType.TURN_OFF && currentVal > 0) {
                    lights.set(point.id, currentVal - 1);
                }
            }
        }

        let totalBrightness = 0;
        lights.forEach((value: number) => {
            if (value) {
                totalBrightness += value;
            }
        });

        return String(totalBrightness);
    }
}

// ---------------------------------------------------------------------------------------------------------------

const commandRe = /(toggle|turn on|turn off)\s(\d+,\d+)\sthrough\s(\d+,\d+)/gi;

function pointsInRange(range: [Point, Point]): Array<Point> {
    const points: Array<Point> = [];

    const [a, b] = range;
    const [startX, endX] = a.x > b.x ? [b.x, a.x] : [a.x, b.x];
    const [startY, endY] = a.y > b.y ? [b.y, a.y] : [a.y, b.y];

    for (let x = startX; x <= endX; x++) {
        for (let y = startY; y <= endY; y++) {
            points.push(new Point(x, y));
        }
    }

    return points;
}

function parseInput(input: string): Array<Command> {
    const commands: Array<Command> = [];
    const matches = input.matchAll(commandRe);

    for (const res of matches) {
        commands.push(
            new Command(
                res[1] as CommandType,
                [Point.fromString(res[2]), Point.fromString(res[3])]
            )
        );
    }

    return commands;
}

class Point {
    id: string;
    x: number;
    y: number;

    constructor(x: number, y: number) {
        this.x = x;
        this.y = y;
        this.id = `${this.x},${this.y}`;
    }

    // str = x,y
    static fromString(str: string): Point {
        const values = str.split(',');
        return new Point(Number(values[0]), Number(values[1]));
    }
}

enum CommandType {
    TOGGLE = 'toggle',
    TURN_ON = 'turn on',
    TURN_OFF = 'turn off'
}

class Command {
    type: CommandType;
    range: [Point, Point];

    constructor(type: CommandType, range: [Point, Point]) {
        this.type = type;
        this.range = range;
    }
}
