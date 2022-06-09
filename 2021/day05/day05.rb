# frozen_string_literal: true

require_relative "../task"

class Day05
  include Task

  def part_one
    lines = parse_input(read_input)
    lines = lines.filter { |item| item.vertical? || item.horizontal? }

    map = {}
    lines.each do |line|
      if line.vertical?
        if line.start_point.y < line.end_point.y
          top = line.start_point.y
          bottom = line.end_point.y
        else
          top = line.end_point.y
          bottom = line.start_point.y
        end

        (top..bottom).each do |y|
          key = "#{line.start_point.x},#{y}"
          map[key] = 0 unless map.key?(key)

          map[key] = map[key] + 1
        end

        next
      end

      if line.horizontal?
        if line.start_point.x < line.end_point.x
          left = line.start_point.x
          right = line.end_point.x
        else
          left = line.end_point.x
          right = line.start_point.x
        end

        (left..right).each do |x|
          key = "#{x},#{line.start_point.y}"
          map[key] = 0 unless map.key?(key)

          map[key] = map[key] + 1
        end

        next
      end

      raise StandardError, "Diagonal"
    end

    map.select { |k, v| v > 1 }.size.to_s
  end

  def part_two
    "-"
  end

  private

  def parse_input(input)
    input.split("\n").map do |item|
      raw_points = item.split(" -> ")

      start_point = Point.from_str(raw_points[0])
      end_point = Point.from_str(raw_points[1])

      Line.new(start_point, end_point)
    end
  end
end

class Point
  attr_reader :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def self.from_str(str)
    tokens = str.split(",")
    self.new(tokens[0].to_i, tokens[1].to_i)
  end
end

class Line
  attr_reader :start_point, :end_point

  def initialize(start_point, end_point)
    @start_point = start_point
    @end_point = end_point
  end

  def horizontal?
    start_point.y == end_point.y
  end

  def vertical?
    start_point.x == end_point.x
  end
end