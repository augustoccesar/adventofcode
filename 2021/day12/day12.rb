# frozen_string_literal: true

require "set"
require_relative "../task"

class Day12
  include Task

  def part_one
    cave_map = CaveMap.new(read_input)

    cave_map.paths("start", "end").size
  end

  def part_two
    cave_map = CaveMap.new(read_input)

    paths = Set.new
    cave_map.map.keys.filter { |o| o == o.downcase && !%w[start end].include?(o) }.each do |allow_double|
      paths.merge(cave_map.paths("start", "end", allow_double: allow_double))
    end

    paths.size.to_s
  end
end

class CaveMap
  attr_reader :map

  def initialize(input)
    @map = {}

    input.split("\n").each do |line|
      from, to = line.split("-")

      @map.key?(from) ? @map[from] << to : @map[from] = [to]
      @map.key?(to) ? @map[to] << from : @map[to] = [from]
    end
  end

  def paths(from, to, allow_double: nil)
    visited = @map.keys.map { |k| [k, 0] }.to_h
    result = Set.new
    paths_iter(result, from, to, visited, allow_double: allow_double)

    result
  end

  private

  def paths_iter(result, from, to, visited, path = [], allow_double:)
    visited[from] += 1
    path << from

    if from == to
      result.add(path.join(","))
    else
      @map[from].each do |neighbor|
        next if neighbor == neighbor.downcase && visited[neighbor].positive? && neighbor != allow_double
        next if neighbor == allow_double && visited[neighbor] > 1

        paths_iter(result, neighbor, to, visited, path, allow_double: allow_double)
      end
    end

    path.pop
    visited[from] -= 1
  end
end
