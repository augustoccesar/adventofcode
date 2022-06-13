# frozen_string_literal: true

require_relative "../task"

#   | A B C D E F G | Size | Unique
# --|---------------|------|--------
# 0 | A B C   E F G |  6   |
# 1 |     C     F   |  2   |   X
# 2 | A   C D E   G |  5   |
# 3 | A   C D   F G |  5   |
# 4 |   B C D   F   |  4   |   X
# 5 | A B   D   F G |  5   |
# 6 | A B   D E F G |  6   |
# 7 | A   C     F   |  3   |   X
# 8 | A B C D E F G |  7   |   X
# 9 | A B C D   F G |  6   |
class Day08
  include Task

  def part_one
    count = { 2 => 0, 3 => 0, 4 => 0, 7 => 0 }

    parse_input(read_input).each do |(_, output_values)|
      output_values.each do |value|
        next unless count.key?(value.size)

        count[value.size] += 1
      end
    end

    count.values.reduce(&:+).to_s
  end

  def part_two
    sum = 0
    parse_input(read_input).each do |(signal_patterns, output_values)|
      sum += untangle(signal_patterns, output_values)
    end

    sum.to_s
  end

  private

  NUMBERS = {
    "abcefg" => 0,
    "cf" => 1,
    "acdeg" => 2,
    "acdfg" => 3,
    "bcdf" => 4,
    "abdfg" => 5,
    "abdefg" => 6,
    "acf" => 7,
    "abcdefg" => 8,
    "abcdfg" => 9
  }.freeze

  def untangle(signals, output)
    signals = signals.sort_by(&:size).map(&:chars)
    output = output.map { |s| s.chars.sort.join }
    signals_by_size = signals.group_by(&:size)

    found_letters = {}
    found_numbers = {
      1 => signals.find { |el| el.size == 2 }, # size 2
      4 => signals.find { |el| el.size == 4 }, # size 4
      7 => signals.find { |el| el.size == 3 }, # size 3
      8 => signals.find { |el| el.size == 7 }  # size 7
    }

    found_letters[:a] = (found_numbers[7] - found_numbers[1]).first

    # Finding: 2 and F
    # Logic: F is only not present on the number 2, which means that we can find it pretty easy
    found_letters[:f] = signals.flatten.tally.filter { |_, v| v == 9 }.map { |k, _| k }.first
    found_numbers[2] = signals.filter { |s| !s.include?(found_letters[:f]) }.first

    # Finding: C
    # Logic: Since we have F, C is the other one that makes the 1
    found_letters[:c] = (found_numbers[1] - [found_letters[:f]]).first

    # Finding: 9 and G
    # Logic: 9 is the only one with size 6 that if we remove all the groups that we know, will have only one
    #        letter left (which is the G)
    #
    # 9 contains: A, B, C, D, F, G
    # We know:    A, B, C, D, F    (of which B and D are grouped)
    bd_group = found_numbers[4] - found_numbers[1]
    known_group = bd_group + [found_letters[:a], found_letters[:c], found_letters[:f]]
    found_numbers[9] = signals_by_size[6].filter { |s| (s & known_group).size == 5 }.first
    found_letters[:g] = (found_numbers[9] - known_group).first

    # Finding: E
    found_letters[:e] = (found_numbers[8] - found_numbers[9]).first

    # Finding: D
    found_letters[:d] = (found_numbers[2] - found_letters.values).first

    # Finding: B
    found_letters[:b] = (found_numbers[8] - found_letters.values).first

    lookup = found_letters.map { |k, v| [v, k] }.to_h

    output.map { |s| NUMBERS[s.chars.map { |c| lookup[c] }.sort.join].to_s }.join.to_i
  end

  def parse_input(input)
    result = []
    input.split("\n").each do |line|
      tokens = line.split(" | ")
      signal_patterns = tokens[0].split.map { |item| item.chars.sort.join }
      output_values = tokens[1].split.map { |item| item.chars.sort.join }

      result << [signal_patterns, output_values]
    end

    result
  end
end
