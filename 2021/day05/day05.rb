# frozen_string_literal: true

require_relative "../task"

class Day05
  include Task

  def part_one
    lines = parse_input(read_input)
    lines = lines.filter { |item| item.vertical? || item.horizontal? }

    run(lines)
  end

  def part_two
    lines = parse_input(read_input)

    run(lines)
  end

  private

  def run(lines)
    map = {}
    lines.each do |line|
      line.point_strs.each do |point_str|
        map[point_str] = 0 unless map.key?(point_str)

        map[point_str] = map[point_str] + 1
      end
    end

    map.select {|_, v| v > 1 }.size.to_s
  end

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

  def to_s
    "#{x},#{y}"
  end

  def ==(other)
    x == other.x && y == other.y
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

  def point_strs
    direction_x = start_point.x > end_point.x ? :left : :right
    direction_y = start_point.y > end_point.y ? :up : :down

    points = []

    if horizontal? || vertical?
      y_iter = start_point.y.send(direction_to_iter(direction_y), end_point.y)
      x_iter = start_point.x.send(direction_to_iter(direction_x), end_point.x)

      y_iter.each do |y|
        x_iter.each do |x|
          points << Point.new(x, y).to_s
        end
      end

      return points
    end

    current = start_point
    points << current.to_s
    while current != end_point
      current = Point.new(
        current.x.send(direction_to_add_sub(direction_x), 1),
        current.y.send(direction_to_add_sub(direction_y), 1),
      )
      points << current.to_s
    end

    points
  end

  private

  def direction_to_iter(direction)
    case direction
    when :left, :up
      :downto
    when :right, :down
      :upto
    else
      raise ArgumentError, "Invalid direction"
    end
  end

  def direction_to_add_sub(direction)
    case direction
    when :left, :up
      :-
    when :right, :down
      :+
    else
      raise ArgumentError, "Invalid direction"
    end
  end
end