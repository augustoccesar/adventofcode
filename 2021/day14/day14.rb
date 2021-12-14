# frozen_string_literal: true

require_relative "../task"

class Day14
  include Task

  def part_one
    template, pair_rules = parse_input(read_input)

    10.times do
      template = iter(template, pair_rules)
    end

    registry = {}
    template.chars.each do |c|
      registry[c] = 0 unless registry.key?(c)

      registry[c] += 1
    end

    (registry.values.max - registry.values.min).to_s
  end

  def part_two
    "-"
  end

  private

  def iter(template, pair_rules)
    template_chars = template.chars

    insertions = []
    (0..template_chars.size - 2).each do |i|
      pair = [template_chars[i], template_chars[i + 1]].reduce(&:+)
      insertions << [i + 1 + insertions.size, pair_rules[pair]]
    end

    new_template = template.chars
    insertions.each do |ins|
      new_template.insert(ins[0], ins[1])
    end

    new_template.reduce(&:+)
  end

  def parse_input(input)
    template, pairs_data = input.split("\n\n")
    pair_rules = pairs_data.lines(chomp: true).map { |it| it.split(" -> ") }.to_h

    [template, pair_rules]
  end
end
