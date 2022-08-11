import {Task} from '../Task';

export class Day09 extends Task {
    partOne(): string {
        const graph = Graph.fromInput(this.readInput());
        const result = graph.traverseAll(TraverseLookup.MIN);

        return result.toString();
    }

    partTwo(): string {
        const graph = Graph.fromInput(this.readInput());
        const result = graph.traverseAll(TraverseLookup.MAX);

        return result.toString();
    }
}

class Vertex {
    name: string
    neighbors: Map<Vertex, number>

    constructor(name: string) {
        this.name = name;
        this.neighbors = new Map();
    }

    addNeighbor(vertex: Vertex, weight: number) {
        this.neighbors.set(vertex, weight);
    }
}

class Graph {
    verticesMap: Map<string, Vertex>

    constructor() {
        this.verticesMap = new Map();
    }

    addEdge(from: string, to: string, weight: number) {
        if(!this.verticesMap.has(from)) {
            this.verticesMap.set(from, new Vertex(from));
        }

        if(!this.verticesMap.has(to)) {
            this.verticesMap.set(to, new Vertex(to));
        }

        const fromVertex = this.verticesMap.get(from)!;
        const toVertex = this.verticesMap.get(to)!;

        fromVertex.addNeighbor(toVertex, weight);
        toVertex.addNeighbor(fromVertex, weight);
    }

    traverseAll(traverseLookup: TraverseLookup): number {
        let result = traverseLookup === TraverseLookup.MAX ? Number.NEGATIVE_INFINITY : Number.POSITIVE_INFINITY;
        for(const vertex of this.verticesMap.values()) {
            const total = this.traverseFromVertex(vertex, traverseLookup);
            if(traverseLookupCmp(result, total, traverseLookup)) {
                result = total;
            }
        }

        return result;
    }

    traverseFromVertex(start: Vertex, traverseLookup: TraverseLookup, visited: Map<Vertex, boolean> = new Map()): number {
        visited.set(start, true);

        const visitedKeys = Array.from(visited.keys()).map(vertex => vertex.name);
        const neighborKeys = Array.from(start.neighbors.keys()).map(vertex => vertex.name);

        const nonVisitedKeys = neighborKeys.filter(key => {
            return !visitedKeys.includes(key)
        });

        if(nonVisitedKeys.length === 0) {
            return 0;
        }

        let result = traverseLookup === TraverseLookup.MAX ? Number.NEGATIVE_INFINITY : Number.POSITIVE_INFINITY;
        for(const key of nonVisitedKeys) {
            let total = 0;
            const neighbor = this.verticesMap.get(key)!
            total += start.neighbors.get(neighbor)!;

            total += this.traverseFromVertex(neighbor, traverseLookup, cloneVisited(visited));

            if(traverseLookupCmp(result, total, traverseLookup)) {
                result = total;
            }
        }


        return result;
    }

    static fromInput(input: string): Graph {
        const graph = new Graph();
        for(const line of input.split("\n")) {
            const [path, distance] = line.split("=");
            const [from, to] = path.split("to").map(item => item.trim());

            graph.addEdge(from, to, Number(distance.trim()));
        }

        return graph;
    }
}

enum TraverseLookup {
    MIN,
    MAX
}

function traverseLookupCmp(baseNumber: number, newNumber: number, traverseLookup: TraverseLookup) {
    switch (traverseLookup) {
        case TraverseLookup.MAX:
            return baseNumber < newNumber;
        case TraverseLookup.MIN:
            return baseNumber > newNumber;
    }
}

function cloneVisited<T, E>(map: Map<T, E>): Map<T, E> {
    const cloned = new Map();
    for(const [key, val] of map.entries()) {
        cloned.set(key, val);
    }

    return cloned;
}
