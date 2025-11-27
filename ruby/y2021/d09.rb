# frozen_string_literal: true

require_relative '../day'

class Day09 < Day
  def part_one
    map = HeightMap.from_input(read_input)

    map.lowest_points.values.map { |item| item + 1 }.reduce(&:+).to_s
  end

  def part_two
    map = HeightMap.from_input(read_input)

    basins = map.lowest_points.keys
                .map { |key| key.split(',').map(&:to_i) }
                .map { |(x, y)| map.scan_basin(x, y, {}) }

    basins.sort[-3..].reduce(&:*).to_s
  end
end

class HeightMap
  MODIFIERS = [
    [1, 0], # RIGHT
    [-1, 0], # LEFT
    [0, -1], # UP
    [0, 1] # DOWN
  ].freeze

  attr_reader :data, :max_x, :max_y

  def initialize(data, max_x, max_y)
    @data = data
    @max_x = max_x
    @max_y = max_y
  end

  def lowest_points
    points = {}

    data.each do |key, value|
      has_smaller_neighbor = false
      x, y = key.split(',').map(&:to_i)

      neighbors(x, y).each_value do |neighbor|
        if neighbor <= value
          has_smaller_neighbor = true
          break
        end
      end

      next if has_smaller_neighbor

      points["#{x},#{y}"] = value
    end

    points
  end

  def scan_basin(x, y, scanned_map)
    scanned_map["#{x},#{y}"] = true

    total = 1
    neighbors = neighbors(x, y)

    neighbors.each do |neighbor_key, neighbor|
      next if neighbor == 9 || scanned_map.key?(neighbor_key)

      neighbor_x, neighbor_y = neighbor_key.split(',').map(&:to_i)

      total += scan_basin(neighbor_x, neighbor_y, scanned_map)
    end

    total
  end

  def neighbors(x, y)
    res = {}

    MODIFIERS.each do |(x_mod, y_mod)|
      neighbor_x = x + x_mod
      neighbor_y = y + y_mod

      invalid_x = neighbor_x > max_x || neighbor_x.negative?
      invalid_y = neighbor_y > max_y || neighbor_y.negative?
      next if invalid_x || invalid_y

      key = "#{x + x_mod},#{y + y_mod}"
      res[key] = data[key]
    end

    res
  end

  class << self
    def from_input(input)
      heightmap = {}
      max_x = - (1 << 64) - 1
      max_y = - (1 << 64) - 1

      input.split("\n").each_with_index do |row, y|
        row.chars.each_with_index do |item, x|
          heightmap["#{x},#{y}"] = item.to_i

          max_x = x if x > max_x
        end

        max_y = y if y > max_y
      end

      new(heightmap, max_x, max_y)
    end
  end
end
