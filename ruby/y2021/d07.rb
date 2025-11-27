# frozen_string_literal: true

require_relative '../day'

class Day07 < Day
  def part_one
    run { |steps| steps }
  end

  def part_two
    # https://en.wikipedia.org/wiki/Triangular_number
    run { |steps| (steps * (steps + 1)) / 2 }
  end

  private

  def run(&fuel_calc)
    positions = parse_input(read_input)

    min_cost = (1 << 64)

    # TODO: There is probably a better way to avoid doing these many loops
    (0..positions.max).each do |base|
      cost = 0
      positions.each do |pos|
        cost += fuel_calc.call((pos - base).abs)
      end

      min_cost = cost if cost < min_cost
    end

    min_cost.to_s
  end

  def parse_input(input)
    input.split(',').map(&:to_i)
  end
end
