# frozen_string_literal: true

require_relative "../task"

class Day01
  include Task

  def part_one
    data = read_input.lines.map(&:to_i)

    increased = 0
    (1..data.length - 1).each do |i|
      increased += 1 if data[i] > data[i - 1]
    end

    increased.to_s
  end

  def part_two
    "-"
  end
end
