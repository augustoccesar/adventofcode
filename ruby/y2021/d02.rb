# frozen_string_literal: true

require_relative '../day'

class Day02 < Day
  def part_one
    action_map = {
      forward: [1, 0],
      down: [0, 1],
      up: [0, -1]
    }.freeze

    xy = [0, 0]

    read_input.lines.map(&:split).each do |command_data|
      action = action_map[command_data[0].to_sym]
      num = command_data[1].to_i

      xy[0] += (action[0] * num)
      xy[1] += (action[1] * num)
    end

    xy.reduce(&:*).to_s
  end

  def part_two
    aim = 0
    xy = [0, 0]

    read_input.lines.map(&:split).map { |item| [item[0], item[1].to_i] }.each do |command_data|
      case command_data[0]
      when 'down'
        aim += command_data[1]
      when 'up'
        aim -= command_data[1]
      when 'forward'
        xy[0] += command_data[1]
        xy[1] += aim * command_data[1]
      end
    end

    xy.reduce(&:*).to_s
  end
end
