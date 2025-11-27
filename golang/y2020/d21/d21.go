package d21

import (
	"regexp"
	"sort"
	"strconv"
	"strings"

	"com.github/augustoccesar/adventofcode/golang/structure"
	"com.github/augustoccesar/adventofcode/golang/y2020/utils"
)

type Day21 struct{}

func (d *Day21) Year() int { return 2020 }
func (d *Day21) Day() int  { return 21 }

func (d *Day21) PartOne() string {
	recipes := parseInput(structure.ReadDefaultInput(d))
	ingredients, _, ingredientAllergen := processRecipes(recipes)

	sum := 0
	for _, ingredient := range ingredients {
		if _, ok := ingredientAllergen[ingredient]; ok {
			continue
		}

		sum++
	}

	return strconv.Itoa(sum)
}

func (d *Day21) PartTwo() string {
	recipes := parseInput(structure.ReadDefaultInput(d))
	_, _, ingredientAllergen := processRecipes(recipes)

	allergenIngredient := map[string]string{}
	for k, v := range ingredientAllergen {
		allergenIngredient[v] = k
	}

	sortedAllergens := utils.MapKeys(allergenIngredient)
	sort.Strings(sortedAllergens)

	result := []string{}
	for _, key := range sortedAllergens {
		result = append(result, allergenIngredient[key])
	}

	return strings.Join(result, ",")
}

type recipe struct {
	ingredients []string
	allergens   []string
}

var lineRegex = regexp.MustCompile(`(.*)\s\(contains\s(.*)\)`)

func processRecipes(recipes []recipe) (ingredients []string, allergens []string, ingredientAllergen map[string]string) {
	// Allergen -> []Ingredient
	allergenPossibleIngredients := map[string][]string{}
	ingredientAllergen = map[string]string{}

	allergens = []string{}
	ingredients = []string{}

	for _, recipe := range recipes {
		for _, allergen := range recipe.allergens {
			if _, ok := allergenPossibleIngredients[allergen]; !ok {
				allergens = append(allergens, allergen)
				allergenPossibleIngredients[allergen] = recipe.ingredients
				continue
			}

			allergenPossibleIngredients[allergen] = utils.SliceIntersect(allergenPossibleIngredients[allergen], recipe.ingredients)
		}

		ingredients = append(ingredients, recipe.ingredients...)
	}

	for len(allergenPossibleIngredients) > 0 {
		for allergen, aIngredients := range allergenPossibleIngredients {
			if len(aIngredients) == 1 {
				ingredientAllergen[aIngredients[0]] = allergen

				utils.MapRemoveFromAll(&allergenPossibleIngredients, aIngredients[0])
				delete(allergenPossibleIngredients, allergen)
			}
		}
	}

	return
}

func parseInput(input string) []recipe {
	lines := strings.Split(input, "\n")
	recipes := make([]recipe, len(lines))

	for i, line := range lines {
		match := lineRegex.FindStringSubmatch(line)
		ingredients := strings.Split(match[1], " ")
		allergens := strings.Split(match[2], ", ")
		recipes[i] = recipe{ingredients: ingredients, allergens: allergens}
	}

	return recipes
}
