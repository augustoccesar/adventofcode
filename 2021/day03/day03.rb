# frozen_string_literal: true

require_relative "../task"

class Day03
  include Task

  def part_one
    diagnostics = parse_input(read_input)
    size = diagnostics[0].size

    zeros_ones = Array.new(size) { Array.new(2, 0) }
    diagnostics.each do |d|
      d.each_with_index do |bit, i|
        zeros_ones[i][0] += 1 if bit.zero?
        zeros_ones[i][1] += 1 if bit == 1
      end
    end

    gama_rate = Array.new(size, -1)
    epsilon_rate = Array.new(size, -1)
    zeros_ones.each_with_index do |zo, i|
      gama_rate[i] = 0 if zo[0] > zo[1]
      gama_rate[i] = 1 if zo[1] > zo[0]

      epsilon_rate[i] = gama_rate[i].zero? ? 1 : 0
    end

    (bit_arr_to_int(gama_rate) * bit_arr_to_int(epsilon_rate)).to_s
  end

  def part_two
    "-"
  end

  private

  def parse_input(input)
    input.lines(chomp: true).map(&:chars).map { |it| it.map(&:to_i) }
  end

  def bit_arr_to_int(arr)
    arr.map(&:to_s).reduce(&:concat).to_i(2)
  end
end
