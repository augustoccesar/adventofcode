# frozen_string_literal: true

require_relative "../task"

class Day15
  include Task

  DIRECTION_MODIFIERS = [
    [1, 0], [-1, 0],
    [0, 1], [0, -1]
  ].freeze

  def part_one
    map = parse_input(read_input)
    fast_access_map = {}
    unvisited_nodes = {}
    tentative_distances = {}
    next_to_visit = {}

    start_point = point_to_key([0, 0])
    end_point = point_to_key([map[0].size - 1, map[0].size - 1])

    tentative_distances[start_point] = 0
    next_to_visit[start_point] = true

    map.each_with_index do |row, y|
      row.each_with_index do |cost, x|
        point_key = point_to_key([x, y])

        fast_access_map[point_key] = cost
        unvisited_nodes[point_key] = true
        tentative_distances[point_key] = Float::INFINITY unless point_key == start_point
      end
    end

    current_point_key = start_point
    while unvisited_nodes.key?(end_point)
      neighbor_points = neighbors(current_point_key, map[0].size - 1, map.size - 1)

      neighbor_points.each do |point|
        point_key = point_to_key(point)
        current_point_cost = tentative_distances[current_point_key] + fast_access_map[point_key]

        tentative_distances[point_key] = current_point_cost if current_point_cost < tentative_distances[point_key]
        next_to_visit[point_key] = true
      end

      unvisited_nodes.delete(current_point_key)
      next_to_visit.delete(current_point_key)

      current_point_key = next_to_visit.first[0]
    end

    tentative_distances[end_point].to_s
  end

  def part_two
    "-"
  end

  private

  def neighbors(point_key, max_x, max_y)
    point = key_to_point(point_key)
    neighbors_points = DIRECTION_MODIFIERS.map do |(mx, my)|
      [point[0] + mx, point[1] + my]
    end

    neighbors_points.reject! do |x, y|
      x.negative? || y.negative? || x > max_x || y > max_y
    end

    neighbors_points
  end

  def key_to_point(key)
    key.split(",").map(&:to_i)
  end

  def point_to_key(point)
    point.map(&:to_s).join(",")
  end

  def parse_input(input)
    input.lines(chomp: true).map { |line| line.chars.map(&:to_i) }
  end
end
