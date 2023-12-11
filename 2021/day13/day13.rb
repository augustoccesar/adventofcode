# frozen_string_literal: true

require "set"

require_relative "../task"

class Day13
  include Task

  def part_one
    input = read_input.split("\n\n")
    desired_folds = 1

    dots_positions = input[0].lines.map { |it| it.split(",").map(&:to_i) }
    fold_instructions = input[1].lines.map { |it| /(x|y)=(\d+)/.match(it).captures.collect.to_a }

    folds = 0
    fold_instructions.each do |direction, along_idx|
      dots_positions.each_with_index do |(dot_x, dot_y), dot_idx|
        if direction == "y" && (dot_y > along_idx.to_i)
          diff = dot_y - along_idx.to_i

          dots_positions[dot_idx][1] = dot_y - (diff * 2)
        elsif direction == "x" && (dot_x > along_idx.to_i)
          diff = dot_x - along_idx.to_i
          dots_positions[dot_idx][0] = dot_x - (diff * 2)
        end
      end

      folds += 1
      break if folds == desired_folds
    end

    Set.new(dots_positions).length.to_s
  end

  def part_two
    "-"
  end
end

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
        print("#")
      else
        print(".")
      end
    end

    print("\n")
  end

  print("\n")
end
