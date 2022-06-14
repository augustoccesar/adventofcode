# frozen_string_literal: true

require_relative "../task"

DELIMS = {
  "(" => ")",
  "[" => "]",
  "{" => "}",
  "<" => ">"
}.freeze

CORRUPTED_ERROR_POINTS = {
  ")" => 3,
  "]" => 57,
  "}" => 1197,
  ">" => 25_137
}.freeze

INCOMPLETE_ERROR_POINTS = {
  ")" => 1,
  "]" => 2,
  "}" => 3,
  ">" => 4
}.freeze

class Day10
  include Task

  def part_one
    errors = []

    read_input.lines.each do |line|
      process_delimiters(line) { |corrupted_char| errors << corrupted_char }
    end

    errors.map { |item| CORRUPTED_ERROR_POINTS[item] }.reduce(&:+).to_s
  end

  def part_two
    scores = []

    read_input.lines.each do |line|
      corrupted = false

      delim_stack = process_delimiters(line) { corrupted = true }

      next if corrupted

      scores << delim_stack.reverse.inject(0) do |total, n|
        (total * 5) + INCOMPLETE_ERROR_POINTS[DELIMS[n]]
      end
    end

    scores.sort[(scores.size - 1) / 2].to_s
  end

  private

  def process_delimiters(chunk, &corrupted_action)
    delim_stack = []

    chunk.chars.each do |char|
      if DELIMS.keys.include?(char)
        delim_stack << char
      elsif DELIMS.values.include?(char)
        if char == DELIMS[delim_stack.last]
          delim_stack.pop
          next
        end

        corrupted_action.call(char)

        break
      end
    end

    delim_stack
  end
end
