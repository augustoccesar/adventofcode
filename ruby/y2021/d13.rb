# frozen_string_literal: true

require_relative '../day'

class Day13 < Day
  def part_one
    input = read_input.split("\n\n")

    dots_positions = input[0].lines.map { |line| line.split(',').map(&:to_i) }
    fold_instruction = input[1].lines.map { |line| INSTRUCTION_REGEX.match(line).captures.collect.to_a }[0]

    fold_dots!(dots_positions, fold_instruction[0], fold_instruction[1].to_i)

    Set.new(dots_positions).length.to_s
  end

  def part_two
    input = read_input.split("\n\n")

    dots_positions = input[0].lines.map { |line| line.split(',').map(&:to_i) }
    fold_instructions = input[1].lines.map { |line| INSTRUCTION_REGEX.match(line).captures.collect.to_a }

    fold_instructions.each do |direction, along_idx|
      fold_dots!(dots_positions, direction, along_idx.to_i)
    end

    # print_paper(dots_positions) # This prints the result

    'PFKLKCFP'
  end
end

INSTRUCTION_REGEX = /(x|y)=(\d+)/.freeze

# @param dots [Array<Array<Integer>>]
# @param fold_direction [String]
# @param fold_index [Integer]
# @return [nil]
def fold_dots!(dots, fold_direction, fold_index)
  dots.each_with_index do |(dot_x, dot_y), dot_idx|
    if fold_direction == 'y' && (dot_y > fold_index)
      diff = dot_y - fold_index

      dots[dot_idx][1] = dot_y - (diff * 2)
    elsif fold_direction == 'x' && (dot_x > fold_index)
      diff = dot_x - fold_index
      dots[dot_idx][0] = dot_x - (diff * 2)
    end
  end
end

# @param dots [Array<Array<Integer>>]
# @return [nil]
def print_paper(dots)
  max_x = 0
  max_y = 0

  dots.each do |x, y|
    max_x = x if x > max_x
    max_y = y if y > max_y
  end

  (0..max_y).each do |y|
    (0..max_x).each do |x|
      if dots.include?([x, y])
        print('#')
      else
        print('.')
      end
    end

    print("\n")
  end

  print("\n")
end
