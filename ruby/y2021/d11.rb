# frozen_string_literal: true

require_relative '../day'

class Day11 < Day
  def part_one
    data = parse_input(read_input)

    flashes = 0
    100.times do
      flashes += check_flashes(data).size
    end

    flashes.to_s
  end

  def part_two
    data = parse_input(read_input)

    i = 0
    loop do
      flashed = check_flashes(data)
      i += 1

      break if flashed.size == data.size * data[0].size
    end

    i.to_s
  end

  private

  def parse_input(input)
    input.split("\n").map { |line| line.chars.map(&:to_i) }
  end

  def check_flashes(data)
    flashed = {}
    (0...data.size).each do |y|
      (0...data[0].size).each do |x|
        next if flashed[pos_to_str(x, y)]

        data[y][x] = data[y][x] + 1
        next if data[y][x] <= 9

        flash(data, x, y, flashed)
      end
    end

    flashed
  end

  def flash(data, x, y, flashed)
    data[y][x] = 0
    flashed[pos_to_str(x, y)] = true

    neighbors = [
      [0, -1], # TOP
      [1, -1], # TOP_RIGHT
      [1, 0], # RIGHT
      [1, 1], # BOTTOM_RIGHT
      [0, 1], # BOTTOM
      [-1, 1], # BOTTOM_LEFT
      [-1, 0], # LEFT
      [-1, -1] # TOP_LEFT
    ]

    neighbors.each do |x_mod, y_mod|
      neighbor_x = x + x_mod
      neighbor_y = y + y_mod

      next if flashed[pos_to_str(neighbor_x, neighbor_y)]
      next if neighbor_x.negative? || neighbor_x >= data[0].size
      next if neighbor_y.negative? || neighbor_y >= data.size

      neighbor_value = data[neighbor_y][neighbor_x]
      data[neighbor_y][neighbor_x] = neighbor_value + 1
      next if data[neighbor_y][neighbor_x] <= 9

      flash(data, neighbor_x, neighbor_y, flashed)
    end
  end

  def pos_to_str(x, y)
    "#{x},#{y}"
  end
end
