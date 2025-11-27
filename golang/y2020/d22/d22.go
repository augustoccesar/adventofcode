package d22

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"com.github/augustoccesar/adventofcode/golang/structure"
	"com.github/augustoccesar/adventofcode/golang/y2020/utils"
)

type Day22 struct{}

func (d *Day22) Year() int { return 2020 }
func (d *Day22) Day() int  { return 22 }

func (d *Day22) PartOne() string {
	players := parseInput(structure.ReadDefaultInput(d))

	p1 := players[0]
	p2 := players[1]

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

	return strconv.Itoa(winner.deckScore())
}

func (d *Day22) PartTwo() string {
	players := parseInput(structure.ReadDefaultInput(d))
	winner := playRecursive(players)

	return strconv.Itoa(winner.deckScore())
}

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

func (p *player) hashDeck() string {
	return strings.Join(p.deck, ":")
}

func (p *player) deckScore() int {
	result := 0
	for i, card := range p.deck {
		result += utils.Atoi(card) * (len(p.deck) - i)
	}

	return result
}

func (p *player) String() string {
	return fmt.Sprintf("Player %d's deck: %s", p.id, strings.Join(p.deck, ", "))
}

func hasDecksHappened(history [][]string, deckHash1, deckHash2 string) bool {
	for _, round := range history {
		if utils.SliceContains(round, deckHash1) && utils.SliceContains(round, deckHash2) {
			return true
		}
	}

	return false
}

func playRecursive(players []player) (gameWinner player) {
	p1 := players[0]
	p2 := players[1]

	deckRoundHistory := [][]string{}

	round := 1
	for len(p1.deck) > 0 && len(p2.deck) > 0 {
		var roundWinner *player

		// Before either player deals a card, if there was a previous round in this game that had
		// exactly the same cards in the same order in the same players' decks, the game instantly
		// ends in a win for player 1. Previous rounds from other games are not considered.
		// (This prevents infinite games of Recursive Combat, which everyone agrees is a bad idea.)
		if hasDecksHappened(deckRoundHistory, p1.hashDeck(), p2.hashDeck()) {
			gameWinner = p1
			break
		}

		deckRoundHistory = append(deckRoundHistory, []string{p1.hashDeck(), p2.hashDeck()})

		// Otherwise, this round's cards must be in a new configuration;
		// the players begin the round by each drawing the top card of their deck as normal.
		c1 := p1.pop()
		c2 := p2.pop()

		// If both players have at least as many cards remaining in their deck as the value of the card they
		// just drew, the winner of the round is determined by playing a new
		// game of Recursive Combat .
		if len(p1.deck) >= utils.Atoi(c1) && len(p2.deck) >= utils.Atoi(c2) {
			// Make copies to not modify original game
			newP1 := player{id: p1.id, deck: utils.SliceCopy(p1.deck)[:utils.Atoi(c1)]}
			newP2 := player{id: p2.id, deck: utils.SliceCopy(p2.deck)[:utils.Atoi(c2)]}

			recursiveWinner := playRecursive([]player{newP1, newP2})
			if recursiveWinner.id == p1.id {
				roundWinner = &p1
			} else {
				roundWinner = &p2
			}
		} else {
			// Otherwise, at least one player must not have enough cards left in their deck to recurse;
			// the winner of the round is the player with the higher-value card.
			if utils.Atoi(c1) > utils.Atoi(c2) {
				roundWinner = &p1
			} else {
				roundWinner = &p2
			}
		}

		round++

		// As in regular Combat, the winner of the round (even if they won the round by winning a sub-game)
		// takes the two cards dealt at the beginning of the round and places them on the bottom of their
		// own deck (again so that the winner's card is above the other card). Note that the winner's
		// card might be the lower-valued of the two cards if they won the round due to winning a sub-game
		var pot []string
		if roundWinner.id == p1.id {
			pot = []string{c1, c2}
		} else {
			pot = []string{c2, c1}
		}
		roundWinner.add(pot...)

		// If collecting cards by winning the round causes a player to have all of the cards,
		// they win, and the game ends.
		if len(p1.deck) == 0 {
			gameWinner = p2
			break
		}

		if len(p2.deck) == 0 {
			gameWinner = p1
			break
		}
	}

	return
}

func parseInput(input string) []player {
	players := []player{}

	for _, playerData := range strings.Split(input, "\n\n") {
		lines := strings.Split(playerData, "\n")
		id := playerPattern.FindStringSubmatch(lines[0])[1]

		p := player{id: utils.Atoi(id), deck: make([]string, len(lines)-1)}

		for i, line := range lines[1:] {
			p.deck[i] = line
		}

		players = append(players, p)
	}

	return players
}
