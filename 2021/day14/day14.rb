# frozen_string_literal: true

# TODO(augustoccesar)[2021-12-14]: This can very probably be written better. Take a look at it again.
# TODO(augustoccesar)[2021-12-14]: Explain what am I doing with some comments (unless TODO above makes it simpler)

require_relative "../task"

class Day14
  include Task

  def part_one
    template, pair_rules = parse_input(read_input)

    run(template, pair_rules, 10).to_s
  end

  def part_two
    template, pair_rules = parse_input(read_input)

    run(template, pair_rules, 40).to_s
  end

  private

  def run(template, pair_rules, num)
    chars = template.chars
    pairs = {}
    (0..chars.size - 2).each do |i|
      pair = chars[i] + chars[i + 1]
      pairs[pair] = 0 unless pairs.key?(pair)

      pairs[pair] += 1
    end

    num.times do
      new_pairs = {}
      pairs.each do |pair, count|
        res = pair_rules[pair]

        [pair[0] + res, res + pair[1]].each do |new_pair|
          new_pairs[new_pair] = 0 unless new_pairs.key?(new_pair)

          new_pairs[new_pair] += count
        end
      end

      pairs = new_pairs
    end

    occurrences = {}
    pairs.each do |k, v|
      occurrences[k[0]] = occurrences.key?(k[0]) ? occurrences[k[0]] + v : v
      occurrences[k[1]] = occurrences.key?(k[1]) ? occurrences[k[1]] + v : v
    end
    occurrences.each { |k, v| occurrences[k] = (v / 2.0).ceil.to_i }

    occurrences.values.max - occurrences.values.min
  end

  def parse_input(input)
    template, pairs_data = input.split("\n\n")
    pair_rules = pairs_data.lines(chomp: true).map { |it| it.split(" -> ") }.to_h

    [template, pair_rules]
  end
end
