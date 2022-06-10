# frozen_string_literal: true

require_relative "../task"

class Day08
  include Task

  def part_one
    count = {
      NUMBERS[1].size => 0,
      NUMBERS[4].size => 0,
      NUMBERS[7].size => 0,
      NUMBERS[8].size => 0
    }

    parse_input(read_input).each do |(_, output_values)|
      output_values.each do |value|
        next unless count.key?(value.size)

        count[value.size] += 1
      end
    end

    count.values.reduce(&:+).to_s
  end

  def part_two
    "-"
  end

  private

  NUMBERS = {
    0 => "abcefg",
    1 => "cf",
    2 => "acdeg",
    3 => "acdfg",
    4 => "bcdf",
    5 => "abdfg",
    6 => "abdefg",
    7 => "acf",
    8 => "abcdefg",
    9 => "abcdfg"
  }.freeze

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
