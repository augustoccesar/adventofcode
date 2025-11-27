# frozen_string_literal: true

require_relative '../day'

class Day03 < Day
  def part_one
    diagnostics = parse_input(read_input)
    zeros_ones = most_common_bits(diagnostics)
    epsilon_rate, gama_rate = min_max_bin(zeros_ones)

    (bit_arr_to_int(gama_rate) * bit_arr_to_int(epsilon_rate)).to_s
  end

  def part_two
    diagnostics = parse_input(read_input)

    funcs = %i[max min]
    results = [-1, -1]

    funcs.each_with_index do |func, i|
      curr_bit = 0
      res = diagnostics
      while res.size != 1
        res = f(res, func, curr_bit)
        curr_bit += 1
      end

      results[i] = bit_arr_to_int(res[0])
    end

    results.reduce(&:*).to_s
  end

  private

  def f(items, min_max, curr_bit)
    zeros_ones = most_common_bits(items)
    min_bin, max_bin = min_max_bin(zeros_ones)

    arr = min_max == :min ? min_bin : max_bin

    result = []
    items.each do |item|
      item[curr_bit] == arr[curr_bit] ? result << item : next
    end

    result
  end

  def min_max_bin(common_bits)
    size = common_bits.size

    max_bin = Array.new(size, -1)
    min_bin = Array.new(size, -1)
    common_bits.each_with_index do |zo, i|
      max_bin[i] = 0 if zo[0] > zo[1]
      max_bin[i] = 1 if zo[1] > zo[0]
      max_bin[i] = 1 if zo[1] == zo[0]

      min_bin[i] = max_bin[i].zero? ? 1 : 0
    end

    [min_bin, max_bin]
  end

  def most_common_bits(arr)
    size = arr[0].size

    zeros_ones = Array.new(size) { Array.new(2, 0) }
    arr.each do |item|
      item.each_with_index do |bit, i|
        zeros_ones[i][0] += 1 if bit.zero?
        zeros_ones[i][1] += 1 if bit == 1
      end
    end

    zeros_ones
  end

  def parse_input(input)
    input.lines(chomp: true).map(&:chars).map { |char| char.map(&:to_i) }
  end

  def bit_arr_to_int(arr)
    arr.map(&:to_s).reduce(&:concat).to_i(2)
  end
end
