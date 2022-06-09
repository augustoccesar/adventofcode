# frozen_string_literal: true

require_relative "../task"

class Day07
  include Task

  def part_one
    positions = parse_input(read_input)

    min_cost = (1 << 64)

    positions.each do |base|
      cost = 0
      positions.each do |pos|
        cost += (pos - base).abs
      end

      min_cost = cost if cost < min_cost
    end

    min_cost.to_s
  end

  def part_two
    "-"
  end

  private

  def parse_input(input)
    input.split(",").map(&:to_i)
  end
end
