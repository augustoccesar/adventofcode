# frozen_string_literal: true

require_relative "../task"

DELIMS = {
  "(" => ")",
  "[" => "]",
  "{" => "}",
  "<" => ">"
}.freeze

ERROR_POINTS = {
  ")" => 3,
  "]" => 57,
  "}" => 1197,
  ">" => 25_137
}.freeze

class Day10
  include Task

  def part_one
    errors = []

    read_input.lines.each do |line|
      delim_stack = []

      line.chars.each do |char|
        if DELIMS.keys.include?(char)
          delim_stack << char
        elsif DELIMS.values.include?(char)
          if char == DELIMS[delim_stack.last]
            delim_stack.pop
            next
          end

          errors << char
          break
        end
      end
    end

    errors.map { |item| ERROR_POINTS[item] }.reduce(&:+).to_s
  end

  def part_two
    "-"
  end
end
