# frozen_string_literal: true

require_relative "../task"

class Day12
  include Task

  def part_one
    caves_map = {}

    read_input.split("\n").each do |line|
      from, to = line.split("-")

      caves_map[from] = Cave.new(from) unless caves_map.key?(from)
      caves_map[to] = Cave.new(to) unless caves_map.key?(to)

      caves_map[from].add_neighbor(caves_map[to])
      caves_map[to].add_neighbor(caves_map[from])
    end

    paths = []
    caves_map["start"].trace(paths: paths)

    paths.size.to_s
  end

  def part_two
    "-"
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

  def trace(current_path = [], visited = {}, paths: [])
    visited[@name] = true
    current_path << self

    @neighbors.each do |neighbor|
      next if visited[neighbor.name] && neighbor.small?

      if neighbor.name == "end"
        current_path << neighbor
        paths << current_path
        next
      end

      neighbor.trace(current_path.clone, visited.clone, paths: paths)
    end
  end

  def small?
    @name == @name.downcase
  end
end
