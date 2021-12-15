# frozen_string_literal: true

require_relative "../task"

class Day15
  include Task

  DIRECTION_MODIFIERS = [
    [1, 0], [-1, 0],
    [0, 1], [0, -1]
  ].freeze

  def part_one
    map, tentative_distances, unvisited_nodes, size = parse_input(read_input)

    current_node = [0, 0]
    target_node = [size - 1, size - 1]

    while unvisited_nodes.key?(target_node)
      neighbors = neighbors(current_node, size)
      neighbors.each do |neighbor|
        next unless unvisited_nodes.key?(neighbor)

        distance = tentative_distances[current_node] + map[neighbor]

        tentative_distances[neighbor] = distance if distance < tentative_distances[neighbor]
        unvisited_nodes[neighbor] = tentative_distances[neighbor] if unvisited_nodes.key?(neighbor)
      end

      unvisited_nodes.delete(current_node)

      current_node = unvisited_nodes.min_by { |_k, v| v }[0] # TODO: Here is probably the main bottleneck
    end

    tentative_distances[target_node].to_s
  end

  def part_two
    "-"
  end

  private

  def neighbors(node, limit)
    neighbors_nodes = DIRECTION_MODIFIERS.map do |(mx, my)|
      [node[0] + mx, node[1] + my]
    end

    neighbors_nodes.reject! do |x, y|
      x.negative? || y.negative? || x >= limit || y >= limit
    end

    neighbors_nodes
  end

  def parse_input(input)
    map = {}
    unvisited_nodes = { [0, 0] => 0 }
    tentative_distances = { [0, 0] => 0 }

    input.lines(chomp: true).each_with_index.map do |line, y|
      line.chars.each_with_index.map do |item, x|
        map[[x, y]] = item.to_i

        tentative_distances[[x, y]] = Float::INFINITY unless [x, y] == [0, 0]
        unvisited_nodes[[x, y]] = Float::INFINITY unless [x, y] == [0, 0]
      end
    end

    [map, tentative_distances, unvisited_nodes, Math.sqrt(map.size).to_i]
  end
end
