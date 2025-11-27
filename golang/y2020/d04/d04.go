package d04

import (
	"encoding/json"
	"regexp"
	"strconv"
	"strings"

	"com.github/augustoccesar/adventofcode/golang/structure"
)

type Day04 struct{}

func (d *Day04) Year() int { return 2020 }
func (d *Day04) Day() int  { return 4 }

func (d *Day04) PartOne() string {
	passports := parsePassports(structure.ReadDefaultInput(d))
	valid := 0

	for _, pass := range passports {
		if validatePassportV1(pass) {
			valid++
		}
	}

	return strconv.Itoa(valid)
}

func (d *Day04) PartTwo() string {
	passports := parsePassports(structure.ReadDefaultInput(d))
	valid := 0

	for _, pass := range passports {
		if validatePassportV2(pass) {
			valid++
		}
	}

	return strconv.Itoa(valid)
}

func isValid(s *string) bool {
	return s != nil && *s != ""
}

type passport struct {
	BYR *string `json:"byr"`
	IYR *string `json:"iyr"`
	EYR *string `json:"eyr"`
	HGT *string `json:"hgt"`
	HCL *string `json:"hcl"`
	ECL *string `json:"ecl"`
	PID *string `json:"pid"`
	CID *string `json:"cid"`
}

func buildPassport(rawData string) passport {
	dataMap := map[string]string{}
	for _, fieldPair := range strings.Split(rawData, " ") {
		fieldPairArr := strings.Split(fieldPair, ":")
		fieldName, fieldValue := fieldPairArr[0], fieldPairArr[1]

		dataMap[fieldName] = fieldValue
	}

	jsonData, err := json.Marshal(dataMap)
	if err != nil {
		panic(err)
	}

	var passport passport
	err = json.Unmarshal(jsonData, &passport)
	if err != nil {
		panic(err)
	}

	return passport
}

func parsePassports(input string) []passport {
	passportsRaw := strings.Split(input, "\n\n")
	passports := make([]passport, len(passportsRaw))

	for i, passportRaw := range passportsRaw {
		sanitizedPassportRaw := strings.ReplaceAll(passportRaw, "\n", " ")
		passport := buildPassport(sanitizedPassportRaw)

		passports[i] = passport
	}

	return passports
}
func validatePassportV1(p passport) bool {
	return isValid(p.BYR) && isValid(p.IYR) && isValid(p.EYR) && isValid(p.HGT) && isValid(p.HCL) && isValid(p.ECL) && isValid(p.PID)
}

func validatePassportV2(p passport) bool {
	if !validatePassportV1(p) {
		return false
	}

	// Validate BYR
	intByr, err := strconv.Atoi(*p.BYR)
	if err != nil {
		return false
	}
	if intByr < 1920 || intByr > 2002 {
		return false
	}

	// Validate IYR
	intIyr, err := strconv.Atoi(*p.IYR)
	if err != nil {
		return false
	}
	if intIyr < 2010 || intIyr > 2020 {
		return false
	}

	// Validate EYR
	intEyr, err := strconv.Atoi(*p.EYR)
	if err != nil {
		return false
	}
	if intEyr < 2020 || intEyr > 2030 {
		return false
	}

	// Validate HGT
	heightPattern := regexp.MustCompile(`(\d+)(cm|in)`)
	matches := heightPattern.FindAllStringSubmatch(*p.HGT, -1)
	if matches == nil {
		return false
	}

	groups := matches[0]
	heightValue, _ := strconv.Atoi(groups[1]) // Ignore err, shouldn't happen since we filtered by the regex
	heightMetric := groups[2]
	if heightMetric == "cm" {
		if heightValue < 150 || heightValue > 193 {
			return false
		}
	} else if heightMetric == "in" {
		if heightValue < 59 || heightValue > 76 {
			return false
		}
	}

	// Validate HCL
	hairColorPattern := regexp.MustCompile(`^#[a-f0-9]{6}$`)
	matches = hairColorPattern.FindAllStringSubmatch(*p.HCL, -1)
	if matches == nil {
		return false
	}

	// Validate ECL
	isValidECL := false
	validECLs := []string{"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}
	for _, validECL := range validECLs {
		if *p.ECL == validECL {
			isValidECL = true
			break
		}
	}
	if !isValidECL {
		return false
	}

	// Validate PID
	pidPattern := regexp.MustCompile(`^[0-9]{9}$`)
	matches = pidPattern.FindAllStringSubmatch(*p.PID, -1)
	if matches == nil {
		return false
	}

	return true
}
