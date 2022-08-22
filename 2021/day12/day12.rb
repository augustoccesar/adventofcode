# frozen_string_literal: true

require_relative "../task"

class Day12
  include Task

  def part_one
    caves_map = parse_input(read_input)

    paths = []
    caves_map["start"].trace(paths: paths)

    paths.size.to_s
  end

  def part_two
    caves_map = parse_input(read_input)

    paths = []
    caves_map.keys.filter { |o| o == o.downcase && !%w[start end].include?(o) }.each do |allow_double|
      caves_map["start"].trace(paths: paths, allow_double: allow_double)
    end

    paths.map { |path| path.map(&:name).join(",") }.uniq.size.to_s
  end

  private

  def parse_input(input)
    caves_map = {}

    input.split("\n").each do |line|
      from, to = line.split("-")

      caves_map[from] = Cave.new(from) unless caves_map.key?(from)
      caves_map[to] = Cave.new(to) unless caves_map.key?(to)

      caves_map[from].add_neighbor(caves_map[to])
      caves_map[to].add_neighbor(caves_map[from])
    end

    caves_map
  end
end

class Cave
  attr_reader :name, :neighbors

  def initialize(name)
    @name = name
    @neighbors = []
  end

  def add_neighbor(cave)
    @neighbors << cave
  end

  def trace(current_path = [], visited = {}, paths: [], allow_double: nil)
    visited.key?(@name) ? visited[@name] += 1 : visited[@name] = 1
    current_path << self

    @neighbors.each do |neighbor|
      next if visited.key?(neighbor.name) && neighbor.small? && neighbor.name != allow_double
      next if neighbor.name == allow_double && visited.key?(neighbor.name) && visited[neighbor.name] > 1

      if neighbor.name == "end"
        current_path << neighbor
        paths << current_path
        next
      end

      neighbor.trace(current_path.clone, visited.clone, paths: paths, allow_double: allow_double)
    end
  end

  def small?
    @name == @name.downcase
  end
end
