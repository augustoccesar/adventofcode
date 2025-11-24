# frozen_string_literal: true

require_relative '../day'

class Day01 < Day
  def part_one
    data = read_input.lines.map(&:to_i)

    increased = 0
    (1..(data.length - 1)).each do |i|
      increased += 1 if data[i] > data[i - 1]
    end

    increased.to_s
  end

  def part_two
    data = read_input.lines.map(&:to_i)

    increased = 0
    (3..(data.length - 1)).each do |i|
      current_sum = data[(i - 2)..i].reduce(&:+)
      previous_sum = data[(i - 3)..(i - 1)].reduce(&:+)

      increased += 1 if current_sum > previous_sum
    end

    increased.to_s
  end
end
