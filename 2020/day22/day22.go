package main

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

var playerPattern = regexp.MustCompile(`Player\s(\d+):`)

type player struct {
	id   int
	deck []string
}

func (p *player) pop() string {
	item, newDeck := utils.SlicePopFront(p.deck)
	p.deck = newDeck
	return item
}

func (p *player) add(cards ...string) {
	p.deck = append(p.deck, cards...)
}

func partOne() {
	players := parseInput()

	p1 := players[1]
	p2 := players[2]

	var winner player
	for len(p1.deck) > 0 && len(p2.deck) > 0 {
		c1 := p1.pop()
		c2 := p2.pop()

		if utils.Atoi(c1) > utils.Atoi(c2) {
			p1.add(c1, c2)
		} else {
			p2.add(c2, c1)
		}

		if len(p1.deck) == 0 {
			winner = p2
			break
		}

		if len(p2.deck) == 0 {
			winner = p1
			break
		}
	}

	result := 0
	for i, card := range winner.deck {
		result += utils.Atoi(card) * (len(winner.deck) - i)
	}

	fmt.Printf("Part One: %d\n", result)
}

func partTwo() {

}

func parseInput() map[int]player {
	input := utils.ReadFile("./input.txt")
	players := map[int]player{}

	for _, playerData := range strings.Split(input, "\n\n") {
		lines := strings.Split(playerData, "\n")
		id := playerPattern.FindStringSubmatch(lines[0])[1]

		p := player{id: utils.Atoi(id), deck: make([]string, len(lines)-1)}

		for i, line := range lines[1:] {
			p.deck[i] = line
		}

		players[p.id] = p
	}

	return players
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}
